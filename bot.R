suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(DBI); library(RSQLite); library(tesseract)
})
need <- function(key) { v <- Sys.getenv(key); if (!nzchar(v)) stop('Missing env: ', key); v }
TG_TOKEN <- need('TELEGRAM_BOT_TOKEN')
TG_CHAT <- need('TELEGRAM_CHAT_ID')
SHEETS_URL <- need('APPS_SCRIPT_URL')
SHEETS_KEY <- need('INVENTORY_API_KEY')
DRY_RUN <- tolower(Sys.getenv('DRY_RUN', 'true')) == 'true'
DB_PATH <- Sys.getenv('DB_PATH', '/app/data/bot.sqlite')
dir.create(dirname(DB_PATH), recursive=TRUE, showWarnings=FALSE)
db <- dbConnect(SQLite(), DB_PATH)
dbExecute(db, 'CREATE TABLE IF NOT EXISTS pending (rid TEXT PRIMARY KEY, chat TEXT, msg TEXT, user TEXT, rs TEXT, stock INTEGER, state TEXT)')
dbExecute(db, 'CREATE TABLE IF NOT EXISTS processed (chat TEXT, msg TEXT, PRIMARY KEY(chat,msg))')

# API calls: all JSON responses checked, no sensitive values printed.
tg <- function(method, data=list()) {
  r <- request(paste0('https://api.telegram.org/bot', TG_TOKEN, '/', method)) |>
    req_body_json(data, auto_unbox=TRUE) |>
    req_timeout(50) |> req_perform()
  x <- resp_body_json(r, simplifyVector=FALSE)
  if (!isTRUE(x$ok)) stop('Telegram error: ', x$description %||% 'unknown')
  x$result
}
`%||%` <- function(a,b) if (is.null(a)) b else a
say <- function(chat, txt, keyboard=NULL) {
  x <- list(chat_id=chat, text=txt)
  if (!is.null(keyboard)) x$reply_markup <- list(inline_keyboard=keyboard)
  tg('sendMessage', x)
}
apps <- function(payload) {
  # Apps Script ContentService responds with HTTPS redirect. httr2 follows redirects.
  payload$api_key <- SHEETS_KEY
  r <- request(SHEETS_URL) |> req_body_json(payload, auto_unbox=TRUE) |>
    req_timeout(60) |> req_perform()
  x <- resp_body_json(r, simplifyVector=FALSE)
  if (!isTRUE(x$ok)) stop(x$error %||% 'unknown sheets error')
  x
}
get_photo <- function(file_id) {
  f <- tg('getFile', list(file_id=file_id))
  url <- paste0('https://api.telegram.org/file/bot', TG_TOKEN, '/', f$file_path)
  dest <- tempfile(fileext='.jpg')
  request(url) |> req_timeout(45) |> req_perform(path=dest)
  dest
}
extract_rs <- function(file) {
  text <- ocr(file, engine=tesseract('rus+eng', options=list(tessedit_pageseg_mode='11')))
  # tolerate spaces; prevent replacing arbitrary OCR characters
  m <- regmatches(tolower(text), gregexpr('rs\\s*[-:]?\\s*[0-9]{4,12}', tolower(text), perl=TRUE))[[1]]
  unique(gsub('[^a-z0-9]', '', m))
}
valid_chat <- function(x) identical(as.character(x), TG_CHAT)
make_rid <- function(chat,msg) paste0('tg_', chat, '_', msg)

on_message <- function(msg) {
  chat <- as.character(msg$chat$id %||% '')
  if (!valid_chat(chat)) return(invisible())
  text <- tolower(paste(msg$caption %||% '', msg$text %||% ''))
  if (!grepl('#закончилось', text, fixed=TRUE)) return(invisible())
  photos <- msg$photo
  if (is.null(photos) || !length(photos)) { say(chat, 'Прикрепите фотографию этикетки с подписью #закончилось'); return() }
  msgid <- as.character(msg$message_id)
  if (nrow(dbGetQuery(db, 'SELECT 1 FROM processed WHERE chat=? AND msg=?', params=list(chat,msgid)))) return()
  dbExecute(db, 'INSERT OR IGNORE INTO processed (chat,msg) VALUES (?,?)', params=list(chat,msgid))
  file <- get_photo(photos[[length(photos)]]$file_id)
  on.exit(unlink(file), add=TRUE)
  ids <- extract_rs(file)
  if (length(ids)!=1) {say(chat, paste0('Нужен ровно один rsID на этикетке. Найдено: ', paste(ids,collapse=', '), '. Списание не выполнено.'));return()}
  # Restrict to Syntol. Unknown maker needs manual intervention.
  txt <- tolower(ocr(file, engine=tesseract('rus+eng')))
  if (!grepl('синтол|syntol|синтол', txt, perl=TRUE)) {
    say(chat, paste0('Найден ', ids, ', но производитель не определён как Синтол. Списание остановлено.'))
    return()
  }
  found <- tryCatch(apps(list(action='lookup',rs=ids)),error=function(e)e)
  if (inherits(found,'error')) {say(chat,paste('Не удалось найти набор:',conditionMessage(found)));return()}
  if (found$stock < 1) {say(chat,paste(ids,'— нулевой остаток.'));return()}
  rid <- make_rid(chat,msgid)
  dbExecute(db, 'INSERT OR REPLACE INTO pending VALUES (?,?,?,?,?,?,?)',
    params=list(rid,chat,msgid,as.character(msg$from$id),ids,as.integer(found$stock),'pending'))
  buttons <- list(list(list(text='✅ Подтвердить',callback_data=paste0('yes:',rid)),
                       list(text='❌ Отмена',callback_data=paste0('no:',rid))))
  say(chat,sprintf('%s (%s)\nСинтол: %d → %d шт.\nСписать 1 набор?',
    found$name,ids,found$stock,found$stock-1L),buttons)
}

on_callback <- function(cb) {
  info <- strsplit(cb$data %||% '', ':',fixed=TRUE)[[1]]
  if (length(info)!=2 || !info[1] %in% c('yes','no')) return()
  row <- dbGetQuery(db,'SELECT * FROM pending WHERE rid=?',params=list(info[2]))
  if (!nrow(row)) {tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Заявка не найдена'));return()}
  p <- row[1,]
  if (!valid_chat(as.character(cb$message$chat$id)) || as.character(cb$from$id)!=p$user) {
    tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Подтверждать может только автор фото'))
    return()
  }
  if (p$state!='pending') {
    tg('answerCallbackQuery',list(callback_query_id=cb$id,text=paste('Уже обработано:',p$state)))
    return()
  }
  if (info[1]=='no') {
    dbExecute(db,"UPDATE pending SET state='cancelled' WHERE rid=?",params=list(p$rid))
    tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Отменено'))
    say(p$chat,paste('Отменено:',p$rs));return()
  }
  tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Проверяю остаток…'))
  if (DRY_RUN) {
    dbExecute(db,"UPDATE pending SET state='dry_run' WHERE rid=?",params=list(p$rid))
    say(p$chat,sprintf('ТЕСТ: %s, %d → %d. Google Sheets не изменён.',p$rs,p$stock,p$stock-1L));return()
  }
  result <- tryCatch(apps(list(action='commit',request_id=p$rid,rs=p$rs,
    expected_stock=as.integer(p$stock),chat_id=p$chat,message_id=p$msg,user_id=p$user)),error=function(e)e)
  if (inherits(result,'error')) {
    say(p$chat,paste('Не удалось подтвердить запись. Проверьте журнал Google Sheets до повторного нажатия:',conditionMessage(result)))
    return()
  }
  dbExecute(db,"UPDATE pending SET state='done' WHERE rid=?",params=list(p$rid))
  say(p$chat,sprintf('✅ Списано: %s, Синтол %d → %d шт.',p$rs,result$before,result$after))
}

cat('Starting Telegram long polling. DRY_RUN=',DRY_RUN,'\n',sep='')
offset <- 0
repeat {
  tryCatch({
    updates <- tg('getUpdates',list(offset=offset,timeout=30,allowed_updates=list('message','callback_query')))
    for (u in updates) {
      offset <- as.numeric(u$update_id)+1
      tryCatch({
        if (!is.null(u$message)) on_message(u$message)
        if (!is.null(u$callback_query)) on_callback(u$callback_query)
      },error=function(e) message('Update error: ',conditionMessage(e)))
    }
  },error=function(e) {message('Polling error: ',conditionMessage(e)); Sys.sleep(5)})
}
