suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(DBI); library(RSQLite); library(tesseract)
})
need <- function(key) { v <- Sys.getenv(key); if (!nzchar(v)) stop('Missing env: ', key); v }
TG_TOKEN <- need('TELEGRAM_BOT_TOKEN')
TG_CHAT <- need('TELEGRAM_CHAT_ID')
SHEETS_URL <- need('APPS_SCRIPT_URL')
SHEETS_KEY <- need('INVENTORY_API_KEY')
DRY_RUN <- tolower(Sys.getenv('DRY_RUN', 'true')) == 'true'
TELEGRAM_PROXY <- Sys.getenv('TELEGRAM_PROXY', '')
telegram_request <- function(url) {
  req <- httr2::request(url)
  if (nzchar(TELEGRAM_PROXY)) req <- httr2::req_proxy(req, url=TELEGRAM_PROXY)
  req
}
DB_PATH <- Sys.getenv('DB_PATH', '/app/data/bot.sqlite')
dir.create(dirname(DB_PATH), recursive=TRUE, showWarnings=FALSE)
db <- dbConnect(SQLite(), DB_PATH)
dbExecute(db, 'CREATE TABLE IF NOT EXISTS pending (rid TEXT PRIMARY KEY, chat TEXT, msg TEXT, user TEXT, rs TEXT, stock INTEGER, state TEXT)')
dbExecute(db, 'CREATE TABLE IF NOT EXISTS processed (chat TEXT, msg TEXT, PRIMARY KEY(chat,msg))')
dbExecute(db, 'CREATE TABLE IF NOT EXISTS manual_requests (chat TEXT NOT NULL, prompt_msg TEXT NOT NULL, photo_msg TEXT NOT NULL, user_id TEXT NOT NULL, state TEXT NOT NULL, PRIMARY KEY(chat,prompt_msg))')

dbExecute(db, 'CREATE TABLE IF NOT EXISTS request_owners (rid TEXT PRIMARY KEY, user_id TEXT NOT NULL)')
dbExecute(db, 'CREATE TABLE IF NOT EXISTS choices (rid TEXT NOT NULL, choice INTEGER NOT NULL, rs TEXT NOT NULL, PRIMARY KEY(rid, choice))')

# A candidate is never automatically written to the inventory: user selection
# merely begins the normal confirmation flow.
rs_distance <- function(a, b) {
  as.numeric(utils::adist(a, b, ignore.case=TRUE)[1L,1L])
}
rank_rs <- function(ocr_ids, catalog, limit=3L) {
  catalog <- unique(tolower(trimws(as.character(catalog))))
  catalog <- catalog[grepl('^rs[0-9]{4,12}$', catalog)]
  evidence <- unique(tolower(trimws(as.character(ocr_ids))))
  evidence <- evidence[grepl('^rs[0-9]{4,12}$', evidence)]
  if (!length(catalog) || !length(evidence)) return(character())
  score <- vapply(catalog, function(candidate) min(vapply(evidence, function(raw) {
    a <- sub('^rs','', raw); b <- sub('^rs','', candidate)
    dist <- rs_distance(a,b)
    # Prefixes are frequent when OCR reads only half the label.
    if (startsWith(b,a)) dist <- min(dist, 0.4 + 0.4*(nchar(b)-nchar(a)))
    dist
  }, numeric(1))), numeric(1))
  ord <- order(score, catalog)
  # Require substantial agreement. Do not recommend a random rs when OCR found nothing.
  allowed <- score[ord] <= pmax(2.0, nchar(sub('^rs','',catalog[ord]))*.4)
  head(catalog[ord][allowed], limit)
}
show_manual_prompt <- function(chat, photo_msg, user_id, intro) {
  prompt <- say(chat,paste0(intro, '\nОтветьте НА ЭТО сообщение полным rsID (например rs1799853). Ввести номер может только автор фотографии.'))
  dbExecute(db, 'INSERT OR REPLACE INTO manual_requests (chat,prompt_msg,photo_msg,user_id,state) VALUES (?,?,?,?,?)',
    params=list(chat,as.character(prompt$message_id),photo_msg,user_id,'waiting'))
}
suggest_rs <- function(chat, photo_msg, user_id, ids, message=NULL) {
  rid <- make_rid(chat, photo_msg)
  result <- tryCatch(apps(list(action='list_rs')),error=function(e)e)
  if (inherits(result,'error')) {
    show_manual_prompt(chat,photo_msg,user_id,paste('Не удалось получить список rsID из Google Sheets:', conditionMessage(result)))
    return(invisible(FALSE))
  }
  options <- rank_rs(ids, unlist(result$rsids, use.names=FALSE), limit=3L)
  if (!length(options)) {
    show_manual_prompt(chat,photo_msg,user_id,paste0(
      'Не удалось уверенно подобрать rsID. Распознано: ',
      if(length(ids)) paste(ids, collapse=', ') else 'ничего', '.'))
    return(invisible(FALSE))
  }
  dbExecute(db, 'DELETE FROM choices WHERE rid=?', params=list(rid))
  for (i in seq_along(options)) dbExecute(db,
    'INSERT INTO choices (rid,choice,rs) VALUES (?,?,?)',params=list(rid,i,options[i]))
  rows <- lapply(seq_along(options),function(i) list(list(
    text=paste0('🔎 ',options[i]), callback_data=paste0('pick:',rid,':',i))))
  # Manual correction is also available as a regular reply to a bot message.
  rows[[length(rows)+1L]] <- list(list(text='✍️ Ввести вручную',callback_data=paste0('manual:',rid)))
  say(chat, paste0('Номер распознан не полностью: ',paste(ids,collapse=', '),
    '\nВыберите подходящий rsID из таблицы или введите его вручную.\nСписания пока не будет.'),rows)
  invisible(TRUE)
}

# API calls: all JSON responses checked, no sensitive values printed.
tg <- function(method, data=list()) {
  r <- telegram_request(paste0('https://api.telegram.org/bot', TG_TOKEN, '/', method)) |>
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
  telegram_request(url) |> req_timeout(45) |> req_perform(path=dest)
  dest
}
extract_rs <- function(file) {
  # First inspect enlarged LABEL crops, not the entire background.
  # This path was checked on the two provided photos (rs1799853, rs9923231).
  # A match must contain an explicit rs prefix; don't guess from bare digits.
  image <- magick::image_read(file)
  metadata <- magick::image_info(image)
  w <- metadata$width[1]; h <- metadata$height[1]
  ocr_engine <- function(psm, language="eng") tesseract::tesseract(
    language, options=list(tessedit_pageseg_mode=as.character(psm)))
  eng6 <- ocr_engine(6)
  eng11 <- ocr_engine(11)
  both11 <- ocr_engine(11, "rus+eng")
  parse_rs <- function(s) {
    s <- tolower(s)
    # A few OCR engines insert spaces between digits. We limit the number
    # of digits to avoid accidentally consuming the next line of text.
    pattern <- "\\brs[[:space:]]*[-:]?[[:space:]]*[0-9][0-9[:space:]]{3,15}"
    hits <- regmatches(s, gregexpr(pattern,s,perl=TRUE))[[1]]
    if (!length(hits)) return(character())
    hits <- gsub("[^0-9]", "", hits)
    hits <- paste0("rs",hits)
    unique(hits[grepl("^rs[0-9]{4,12}$",hits)])
  }
  crop <- function(x0,y0,x1,y1) {
    left <- floor(x0*w); top <- floor(y0*h)
    cw <- max(1L,floor((x1-x0)*w)); ch <- max(1L,floor((y1-y0)*h))
    im <- magick::image_crop(image, magick::geometry_area(cw,ch,left,top))
    # A full-width OCR pass misses rsID on ziplock bags. Enlarging cropped
    # labels exposes the text at about 1000-1500 pixels wide.
    magick::image_resize(im,"1400x")
  }
  # Geometry is expressed as a fraction of the photo, accommodating
  # Telegram's smaller or larger JPEG derivatives.
  regions <- list(c(.20,.48,.80,.72), c(.12,.39,.88,.75),
                  c(.15,.30,.90,.80), c(.08,.25,.95,.82))
  evidence <- character()
  for (k in seq_along(regions)) {
    a <- regions[[k]]
    patch <- crop(a[1],a[2],a[3],a[4])
    # English often recognizes 'rs' more accurately than rus+eng.
    engines <- if (k == 1L) list(eng6,eng11) else list(eng6)
    for (e in engines) {
      txt <- tryCatch(tesseract::ocr(patch,engine=e),error=function(z) "")
      evidence <- c(evidence, parse_rs(txt))
    }
    # Strongest direct evidence: the first, centered crop with an explicit rs.
    # Continue other crops if it is ambiguous, not when it is uniquely read.
    # Continue all passes: partial OCR hits may be misleading.
  }
  if (!length(evidence)) {
    for (e in list(eng6,both11)) {
      txt <- tryCatch(tesseract::ocr(image,engine=e),error=function(z) "")
      evidence <- c(evidence,parse_rs(txt))
    }
  }
  if (!length(evidence)) return(character())
  counts <- sort(table(evidence),decreasing=TRUE)
  # If different rsIDs occur, require manual entry rather than choosing
  # an arbitrary match. Never use only the longest candidate.
  if (length(counts)>1L) return(names(counts))
  names(counts)
}

valid_chat <- function(x) identical(as.character(x), TG_CHAT)
make_rid <- function(chat,msg) paste0('tg_', chat, '_', msg)

# Use a strict ID from the user's reply; no OCR guesses, no bare numeric IDs.
parse_manual_rs <- function(x) {
  x <- trimws(tolower(x %||% ''))
  if (!grepl('^rs[0-9]{4,12}$', x)) return(NULL)
  x
}

# Create the standard confirmation message for OCR and manual identification.
queue_debit <- function(chat, photo_msg, user_id, rs) {
  found <- tryCatch(apps(list(action='lookup', rs=rs)), error=function(e)e)
  if (inherits(found,'error')) {
    say(chat, paste('Не удалось найти набор', rs, ':', conditionMessage(found)))
    return(invisible(FALSE))
  }
  stock <- suppressWarnings(as.integer(found$stock))
  if (length(stock)!=1L || is.na(stock)) {
    say(chat,paste('Некорректный остаток для',rs)); return(invisible(FALSE))
  }
  if (stock < 1L) {say(chat,paste(rs,'— нулевой остаток.')); return(invisible(FALSE))}
  rid <- make_rid(chat,photo_msg)
  # Stable request ID is derived from the ORIGINAL photo, not the manual reply.
  existing <- dbGetQuery(db, 'SELECT state FROM pending WHERE rid=?', params=list(rid))
  if (nrow(existing)) {
    say(chat, 'Для этой фотографии уже создана заявка на списание. Повторное списание не создаётся.')
    return(invisible(FALSE))
  }
  dbExecute(db, 'INSERT INTO pending (rid,chat,msg,user,rs,stock,state) VALUES (?,?,?,?,?,?,?)',
    params=list(rid,chat,photo_msg,user_id,rs,stock,'pending'))
  buttons <- list(list(list(text='✅ Подтвердить',callback_data=paste0('yes:',rid)),
                       list(text='❌ Отмена',callback_data=paste0('no:',rid))))
  say(chat,sprintf('%s (%s)\nСинтол: %d → %d шт.\nСписать 1 набор?',
    found$name,rs,stock,stock-1L),buttons)
  invisible(TRUE)
}

on_manual_reply <- function(msg, chat) {
  reply_id <- as.character(msg$reply_to_message$message_id %||% '')
  if (!nzchar(reply_id)) return(FALSE)
  candidate <- dbGetQuery(db,
    'SELECT * FROM manual_requests WHERE chat=? AND prompt_msg=? AND state=?',
    params=list(chat,reply_id,'waiting'))
  if (!nrow(candidate)) return(FALSE)
  req <- candidate[1,]
  sender <- as.character(msg$from$id %||% '')
  if (!identical(sender, req$user_id)) {
    say(chat, 'Номер rsID для этой фотографии должен отправить автор фотографии ответом на сообщение бота.')
    return(TRUE)
  }
  rs <- parse_manual_rs(msg$text %||% '')
  if (is.null(rs)) {
    say(chat, 'Напишите номер строго в формате rs1799853 (без пробелов), ответом на моё сообщение.')
    return(TRUE)
  }
  # Mark as claimed before lookup. Reopening is possible only through a new photo.
  n <- dbExecute(db,
    "UPDATE manual_requests SET state='used' WHERE chat=? AND prompt_msg=? AND state='waiting'",
    params=list(chat,reply_id))
  if (n == 1L) {
    ok <- queue_debit(chat,req$photo_msg,req$user_id,rs)
    if (!isTRUE(ok)) dbExecute(db,
      "UPDATE manual_requests SET state='waiting' WHERE chat=? AND prompt_msg=? AND state='used'",
      params=list(chat,reply_id))
  }
  TRUE
}

on_message <- function(msg) {
  chat <- as.character(msg$chat$id %||% '')
  if (!valid_chat(chat)) return(invisible())
  if (on_manual_reply(msg,chat)) return(invisible())
  text <- tolower(paste(msg$caption %||% '', msg$text %||% ''))
  if (!grepl('#закончилось', text, fixed=TRUE)) return(invisible())
  photos <- msg$photo
  if (is.null(photos) || !length(photos)) {
    say(chat, 'Прикрепите фотографию этикетки с подписью #закончилось'); return()
  }
  msgid <- as.character(msg$message_id)
  if (nrow(dbGetQuery(db, 'SELECT 1 FROM processed WHERE chat=? AND msg=?', params=list(chat,msgid)))) return()
  dbExecute(db, 'INSERT OR IGNORE INTO processed (chat,msg) VALUES (?,?)', params=list(chat,msgid))
  dbExecute(db, 'INSERT OR IGNORE INTO request_owners (rid,user_id) VALUES (?,?)', params=list(make_rid(chat,msgid),as.character(msg$from$id)))
  file <- get_photo(photos[[length(photos)]]$file_id)
  on.exit(unlink(file), add=TRUE)
  ids <- extract_rs(file)
  if (length(ids)!=1L) {
    suggest_rs(chat,msgid,as.character(msg$from$id),ids)
    return()
  }
  success <- queue_debit(chat,msgid,as.character(msg$from$id),ids[[1]])
  if (!isTRUE(success)) {
    suggest_rs(chat,msgid,as.character(msg$from$id),ids)
  }
}

on_callback <- function(cb) {
  info <- strsplit(cb$data %||% '', ':',fixed=TRUE)[[1]]
  if (!(info[1] %in% c('yes','no','pick','manual'))) return()
  if (info[1] %in% c('yes','no') && length(info)!=2L) return()
  if (info[1] %in% c('pick','manual')) {
    if (length(info) < 2L || length(info) > 3L) return()
    rid <- info[2]
    expected <- make_rid(as.character(cb$message$chat$id), sub('^tg_[^_]+_', '', rid))
    if (!identical(expected,rid) || !valid_chat(as.character(cb$message$chat$id))) return()
    # Only the author may choose; fetch user from Telegram original photo.
    photo_msg <- sub('^tg_[^_]+_', '', rid)
    # When no manual request exists, store author in request_owners (created during photo processing).
    owner <- dbGetQuery(db,'SELECT user_id FROM request_owners WHERE rid=?',params=list(rid))
    if (!nrow(owner) || as.character(owner$user_id[1]) != as.character(cb$from$id)) {
      tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Выбор доступен только автору фотографии'))
      return()
    }
    existing <- dbGetQuery(db, 'SELECT state FROM pending WHERE rid=?',params=list(rid))
    if (nrow(existing)) {
      tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Заявка уже создана'))
      return()
    }
    if (info[1]=='manual') {
      tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Ответьте номером на сообщение бота'))
      show_manual_prompt(TG_CHAT,photo_msg,as.character(cb$from$id),'Введите правильный номер.')
      return()
    }
    if (length(info)!=3L || !grepl('^[1-3]$',info[3])) return()
    chosen <- dbGetQuery(db,'SELECT rs FROM choices WHERE rid=? AND choice=?', params=list(rid,as.integer(info[3])))
    if (!nrow(chosen)) return()
    tg('answerCallbackQuery',list(callback_query_id=cb$id,text=paste('Выбран',chosen$rs[1])))
    ok <- queue_debit(TG_CHAT,photo_msg,as.character(cb$from$id),as.character(chosen$rs[1]))
    if (!isTRUE(ok)) show_manual_prompt(TG_CHAT,photo_msg,as.character(cb$from$id),'Проверьте номер вручную.')
    return()
  }
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
