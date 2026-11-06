suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(DBI); library(RSQLite); library(base64enc)
})
need <- function(key) { v <- Sys.getenv(key); if (!nzchar(v)) stop('Missing env: ', key); v }
TG_TOKEN <- need('TELEGRAM_BOT_TOKEN')
TG_CHATS <- unique(trimws(strsplit(need('TELEGRAM_CHAT_ID'), ',', fixed=TRUE)[[1]]))
if (!length(TG_CHATS) || any(!grepl('^-?[0-9]+$', TG_CHATS)))
  stop('TELEGRAM_CHAT_ID must be a comma-separated list of numeric chat IDs')
SHEETS_URL <- need('APPS_SCRIPT_URL'); SHEETS_KEY <- need('INVENTORY_API_KEY')
DRY_RUN <- tolower(Sys.getenv('DRY_RUN', 'true')) == 'true'
TELEGRAM_PROXY <- Sys.getenv('TELEGRAM_PROXY', '')
OLLAMA_URL <- sub('/+$', '', Sys.getenv('OLLAMA_URL', 'http://host.docker.internal:11434'))
OLLAMA_MODEL <- Sys.getenv('OLLAMA_MODEL', 'longwayxu/xiaomi-ocr-0:bf16')
telegram_request <- function(url) {
  req <- httr2::request(url)
  if (nzchar(TELEGRAM_PROXY)) req <- httr2::req_proxy(req, url=TELEGRAM_PROXY)
  req
}
DB_PATH <- Sys.getenv('DB_PATH', '/app/data/bot.sqlite')
dir.create(dirname(DB_PATH), recursive=TRUE, showWarnings=FALSE)
db <- dbConnect(SQLite(), DB_PATH)
dbExecute(db, 'CREATE TABLE IF NOT EXISTS pending (rid TEXT PRIMARY KEY, chat TEXT, msg TEXT, user TEXT, rs TEXT, expiry TEXT, stock INTEGER, state TEXT)')
# Upgrade existing persistent databases created by the earlier single-row version.
pending_cols <- dbGetQuery(db, 'PRAGMA table_info(pending)')$name
if (!'expiry' %in% pending_cols) dbExecute(db, "ALTER TABLE pending ADD COLUMN expiry TEXT NOT NULL DEFAULT ''")
dbExecute(db, 'CREATE TABLE IF NOT EXISTS processed (chat TEXT, msg TEXT, PRIMARY KEY(chat,msg))')
dbExecute(db, 'CREATE TABLE IF NOT EXISTS request_owners (rid TEXT PRIMARY KEY, user_id TEXT NOT NULL)')
dbExecute(db, "CREATE TABLE IF NOT EXISTS manual_requests (chat TEXT NOT NULL, prompt_msg TEXT NOT NULL, photo_msg TEXT NOT NULL, user_id TEXT NOT NULL, state TEXT NOT NULL, rs TEXT NOT NULL DEFAULT '', PRIMARY KEY(chat,prompt_msg))")
manual_cols <- dbGetQuery(db, 'PRAGMA table_info(manual_requests)')$name
if (!'rs' %in% manual_cols) dbExecute(db, "ALTER TABLE manual_requests ADD COLUMN rs TEXT NOT NULL DEFAULT ''")

`%||%` <- function(a,b) if (is.null(a)) b else a
tg <- function(method, data=list()) {
  r <- telegram_request(paste0('https://api.telegram.org/bot', TG_TOKEN, '/', method)) |>
    req_body_json(data, auto_unbox=TRUE) |> req_timeout(50) |> req_perform()
  x <- resp_body_json(r, simplifyVector=FALSE)
  if (!isTRUE(x$ok)) stop('Telegram error: ', x$description %||% 'unknown')
  x$result
}
say <- function(chat, txt, keyboard=NULL) {
  # Telegram message limit is 4096 characters; split verbose OCR text safely.
  chunks <- if (nchar(txt, type='chars') <= 3900) txt else {
    starts <- seq(1, nchar(txt, type='chars'), by=3900)
    vapply(starts, function(i) substr(txt, i, min(i+3899, nchar(txt, type='chars'))), character(1))
  }
  last <- NULL
  for (i in seq_along(chunks)) {
    x <- list(chat_id=chat, text=chunks[i])
    if (!is.null(keyboard) && i == length(chunks)) x$reply_markup <- list(inline_keyboard=keyboard)
    last <- tg('sendMessage', x)
  }
  last
}
apps <- function(payload) {
  payload$api_key <- SHEETS_KEY
  r <- request(SHEETS_URL) |> req_body_json(payload, auto_unbox=TRUE) |> req_timeout(60) |> req_perform()
  x <- resp_body_json(r, simplifyVector=FALSE)
  if (!isTRUE(x$ok)) stop(x$error %||% 'unknown sheets error')
  x
}
match_dates_in_sheet <- function(rs, dates) {
  result <- tryCatch(apps(list(action='match_dates',rs=rs,dates=dates)),error=function(e)e)
  if (!inherits(result,'error')) return(result)
  # Older deployed Apps Script versions do not yet have match_dates.
  if (!grepl('invalid_action',conditionMessage(result),fixed=TRUE)) stop(conditionMessage(result))
  matches <- lapply(unique(dates),function(expiry) {
    found <- tryCatch(apps(list(action='lookup',rs=rs,expiry=expiry)),error=function(e)e)
    if (!inherits(found,'error')) return(list(expiry=expiry,count=1L))
    count <- if (grepl('ambiguous_lot',conditionMessage(found),fixed=TRUE)) 2L else 0L
    list(expiry=expiry,count=count)
  })
  list(ok=TRUE,rs=rs,matches=matches)
}
get_photo <- function(file_id) {
  f <- tg('getFile', list(file_id=file_id))
  dest <- tempfile(fileext='.jpg')
  telegram_request(paste0('https://api.telegram.org/file/bot', TG_TOKEN, '/', f$file_path)) |>
    req_timeout(45) |> req_perform(path=dest)
  dest
}
normalize_rs <- function(x) {
  x <- tolower(gsub('[[:space:]]', '', as.character(x %||% '')))
  if (grepl('^[0-9]{4,12}$', x)) x <- paste0('rs',x)
  if (grepl('^rs[0-9]{4,12}$', x)) x else ''
}
normalize_date <- function(x) {
  x <- trimws(as.character(x %||% ''))
  # This label family prints expiry as DDMMYY (for example 180825 = 18.08.2025).
  if (grepl('^[0-9]{6}$', x)) {
    compact <- paste0('20',substr(x,5,6),'-',substr(x,3,4),'-',substr(x,1,2))
    d <- suppressWarnings(as.Date(compact, format='%Y-%m-%d'))
    if (!is.na(d) && format(d,'%Y-%m-%d') == compact) return(compact)
    return('')
  }
  fmts <- c('%d.%m.%Y','%d/%m/%Y','%d-%m-%Y','%Y-%m-%d')
  for (fmt in fmts) {
    d <- suppressWarnings(as.Date(x, format=fmt))
    if (!is.na(d) && format(d, fmt) == x) return(format(d, '%Y-%m-%d'))
  }
  ''
}
extract_date_tokens <- function(x) {
  pattern <- '(?<![0-9])(?:[0-9]{6}|[0-9]{1,2}[./-][0-9]{1,2}[./-][0-9]{4}|[0-9]{4}-[0-9]{2}-[0-9]{2})(?![0-9])'
  hits <- regmatches(as.character(x %||% ''),gregexpr(pattern,as.character(x %||% ''),perl=TRUE))[[1]]
  if (!length(hits)) return(character())
  dates <- unique(vapply(hits,function(v) normalize_date(v),character(1)))
  dates[nzchar(dates)]
}
display_date <- function(x) {
  d <- suppressWarnings(as.Date(x, '%Y-%m-%d'))
  if (is.na(d)) x else format(d, '%d.%m.%Y')
}
ocr_photo <- function(file) {
  encoded <- base64enc::base64encode(readBin(file, what='raw', n=file.info(file)$size), linewidth=0)
  prompt <- paste(
    'Распознай исходную фотографию за один проход. Верни в full_text весь фактически видимый текст каждой этикетки, включая мелкую нижнюю часть и подписи полей. Сохрани порядок строк: значение относится только к подписи в той же строке.',
    'В items помести массив объектов по одному на каждую этикетку. В date_candidates перечисли все распознанные значения, похожие на даты, независимо от подписи; не включай температуру или rsID.',
    'Строго сопоставляй подпись и значение в одной строке: Красители → dyes; Серия → lot; Годен до → expiry; Хранить при → storage. Не сдвигай значения между соседними строками: температура хранения никогда не является сроком годности, а дата серии не является сроком годности.',
    'product — название набора/реагента из верхней части этикетки, например Синтол NP-471-100; name — краткое название, если оно напечатано. Не записывай красители в product.',
    'У каждого объекта должны быть строковые поля product, name, dbsnp_id, gene, rsid, lot, expiry, dyes, storage, массив date_candidates и объект additional. В rsid включай префикс rs, если он есть на этикетке.',
    'В каждом поле укажи только прочитанное с изображения; неизвестное строковое поле оставь пустой строкой, date_candidates — пустым массивом, additional — пустым объектом. Не пиши многоточия и описания вроде "название" или "ген" как значения.',
    'Сохраняй символы и цифры rsID точно. expiry заполняй, только если дата рядом с подписью Годен до читается однозначно. Формат ДД.ММ.ГГГГ оставляй так; шесть цифр без разделителей трактуй как ДДММГГ и преобразуй в ДД.ММ.20ГГ, только если это реальная календарная дата. При любой неоднозначности оставь expiry пустым.',
    'Не угадывай и не исправляй нечитаемые символы. Даже если часть данных не читается, все равно верни распознанный full_text и массив items.',
    sep='\n')
  schema <- list(type='object', properties=list(
    full_text=list(type='string'),
    items=list(type='array', items=list(type='object', properties=list(
      product=list(type='string'), name=list(type='string'), dbsnp_id=list(type='string'),
      gene=list(type='string'), rsid=list(type='string'), lot=list(type='string'),
      expiry=list(type='string'), date_candidates=list(type='array',items=list(type='string')),
      dyes=list(type='string'), storage=list(type='string'),
      additional=list(type='object',additionalProperties=list(type='string'))),
      required=list('product','name','dbsnp_id','gene','rsid','lot','expiry','date_candidates','dyes','storage','additional'),
      additionalProperties=FALSE))),
    required=list('full_text','items'), additionalProperties=FALSE)
  images <- list(encoded)
  req <- request(paste0(OLLAMA_URL, '/api/generate')) |>
    req_body_json(list(model=OLLAMA_MODEL, prompt=prompt, images=images, stream=FALSE,
      format=schema, think=FALSE, keep_alive=0, options=list(temperature=0,num_predict=900)), auto_unbox=TRUE) |>
    req_timeout(90) |> req_perform()
  response <- resp_body_json(req, simplifyVector=FALSE)
  if (is.null(response$response)) stop('Ollama вернул пустой ответ')
  raw_response <- as.character(response$response)
  # Some Ollama builds prepend thinking markers or wrap constrained JSON in
  # markdown despite format=schema. Parse the full output first, then its JSON object.
  json_candidates <- raw_response
  json_start <- regexpr('\\{', raw_response)[1]
  json_ends <- gregexpr('\\}', raw_response)[[1]]
  json_end <- if (length(json_ends) && json_ends[1] > 0L) max(json_ends) else -1L
  if (json_start > 0L && json_end >= json_start)
    json_candidates <- c(json_candidates, substr(raw_response, json_start, json_end))
  parsed <- NULL
  for (candidate in unique(json_candidates)) {
    parsed <- tryCatch(fromJSON(candidate, simplifyVector=FALSE), error=function(e) NULL)
    if (is.list(parsed)) break
  }
  if (!is.list(parsed)) parsed <- NULL
  structured <- !is.null(parsed)
  if (is.null(parsed)) {
    # Keep the single-pass OCR useful if the model ignores the JSON schema.
    # The caller will still require an exact rsID/date match in the sheet.
    text_fallback <- gsub('<think>.*?</think>|</?think>', '', raw_response, perl=TRUE)
    text_fallback <- gsub('```[[:alnum:]]*|```', '', text_fallback)
    parsed <- list(full_text=trimws(text_fallback), items=list())
  }
  if (structured && !length(parsed$items %||% list()) && nzchar(parsed$full_text %||% '')) {
    nested <- tryCatch(fromJSON(parsed$full_text, simplifyVector=FALSE),error=function(e)NULL)
    if (is.list(nested) && is.list(nested$items) && length(nested$items)) parsed <- nested
  }
  full_text <- as.character(parsed$full_text %||% '')
  items <- parsed$items %||% list()
  if (!is.list(items) || (structured && (grepl('весь текст с сохранением строк', full_text, ignore.case=TRUE) ||
      grepl('...', response$response, fixed=TRUE))))
    stop('Модель вернула шаблон вместо распознанных данных')
  list(text=full_text, items=items, raw=response$response)
}
valid_chat <- function(x) as.character(x) %in% TG_CHATS
make_rid <- function(chat,msg) paste0('tg_', chat, '_', msg)
parse_manual_pair <- function(x) {
  m <- regexec('^\\s*(rs[0-9]{4,12})\\s+([0-9]{1,2}[./-][0-9]{1,2}[./-][0-9]{4})\\s*$', tolower(x %||% ''), perl=TRUE)
  z <- regmatches(tolower(x %||% ''),m)[[1]]
  if (length(z)!=3L) return(NULL)
  d <- normalize_date(z[3]); if (!nzchar(d)) return(NULL)
  list(rs=z[2], expiry=d)
}
parse_manual_date <- function(x) {
  d <- normalize_date(trimws(x %||% ''))
  if (nzchar(d)) d else NULL
}
show_manual_prompt <- function(chat, photo_msg, user_id, intro, rs='') {
  if (nzchar(rs)) {
    text <- paste0(intro,'\nrsID уже определён: ',rs,'. Ответьте только сроком годности в формате ДД.ММ.ГГГГ. Списание будет предложено после проверки этой даты в таблице. Автор фото должен подтвердить списание.')
  } else {
    text <- paste0(intro,'\nОтветьте на это сообщение парой: rs1799853 05.05.2027. Автор фото должен подтвердить списание.')
  }
  prompt <- say(chat,text)
  dbExecute(db, 'INSERT OR REPLACE INTO manual_requests (chat,prompt_msg,photo_msg,user_id,state,rs) VALUES (?,?,?,?,?,?)',
    params=list(chat,as.character(prompt$message_id),photo_msg,user_id,'waiting',rs))
}
queue_debit <- function(chat, photo_msg, user_id, rs, expiry) {
  rs <- normalize_rs(rs); expiry <- normalize_date(expiry)
  if (!nzchar(rs) || !nzchar(expiry)) {
    show_manual_prompt(chat,photo_msg,user_id,'Не удалось однозначно распознать rsID или дату годности. Списания нет.')
    return(FALSE)
  }
  found <- tryCatch(apps(list(action='lookup',rs=rs,expiry=expiry)),error=function(e)e)
  if (inherits(found,'error')) {
    say(chat,paste('Не удалось найти однозначную партию',rs,display_date(expiry),':',conditionMessage(found)))
    return(FALSE)
  }
  stock <- suppressWarnings(as.integer(found$stock))
  if (length(stock)!=1L || is.na(stock) || stock<1L) {
    say(chat,paste(rs,display_date(expiry),'— в этой партии нет доступного остатка.')); return(FALSE)
  }
  rid <- make_rid(chat,photo_msg)
  existing <- dbGetQuery(db,'SELECT state FROM pending WHERE rid=?',params=list(rid))
  if (nrow(existing)) { say(chat,'Для этого фото заявка уже создана. Повторное списание не создаётся.'); return(FALSE) }
  dbExecute(db,'INSERT INTO pending (rid,chat,msg,user,rs,expiry,stock,state) VALUES (?,?,?,?,?,?,?,?)',
    params=list(rid,chat,photo_msg,user_id,rs,expiry,stock,'pending'))
  buttons <- list(list(list(text='✅ Подтвердить',callback_data=paste0('yes:',rid)),
                       list(text='❌ Отмена',callback_data=paste0('no:',rid))))
  say(chat,sprintf('%s (%s), годен до %s\nПартия: %d → %d шт.\nОбщий остаток rsID: %s шт.\nСписать 1 шт.?',
    found$name,rs,display_date(expiry),stock,stock-1L,found$total),buttons)
  TRUE
}
on_manual_reply <- function(msg, chat) {
  reply_id <- as.character(msg$reply_to_message$message_id %||% '')
  if (!nzchar(reply_id)) return(FALSE)
  candidate <- dbGetQuery(db,'SELECT * FROM manual_requests WHERE chat=? AND prompt_msg=? AND state=?',params=list(chat,reply_id,'waiting'))
  if (!nrow(candidate)) return(FALSE)
  req <- candidate[1,]; sender <- as.character(msg$from$id %||% '')
  if (!identical(sender,req$user_id)) { say(chat,'Уточнение пары rsID и даты может отправить только автор фото.'); return(TRUE) }
  if (nzchar(req$rs)) {
    expiry <- parse_manual_date(msg$text %||% '')
    if (is.null(expiry)) { say(chat,'Введите в ответ только дату Годен до в формате ДД.ММ.ГГГГ, например 05.05.2027'); return(TRUE) }
    pair <- list(rs=req$rs,expiry=expiry)
  } else {
    pair <- parse_manual_pair(msg$text %||% '')
    if (is.null(pair)) { say(chat,'Введите в ответ на сообщение пару строго в формате: rs1799853 05.05.2027'); return(TRUE) }
  }
  dbExecute(db,"UPDATE manual_requests SET state='used' WHERE chat=? AND prompt_msg=? AND state='waiting'",params=list(chat,reply_id))
  queue_debit(chat,req$photo_msg,req$user_id,pair$rs,pair$expiry)
  TRUE
}
on_message <- function(msg) {
  chat <- as.character(msg$chat$id %||% '')
  if (!valid_chat(chat)) return(invisible())
  if (on_manual_reply(msg,chat)) return(invisible())
  caption <- tolower(paste(msg$caption %||% '',msg$text %||% ''))
  if (!grepl('#закончило',caption,fixed=TRUE)) return(invisible())
  photos <- msg$photo
  if (is.null(photos) || !length(photos)) { say(chat,'Прикрепите фото этикетки с подписью #закончило'); return() }
  msgid <- as.character(msg$message_id)
  if (nrow(dbGetQuery(db,'SELECT 1 FROM processed WHERE chat=? AND msg=?',params=list(chat,msgid)))) return()
  dbExecute(db,'INSERT OR IGNORE INTO processed (chat,msg) VALUES (?,?)',params=list(chat,msgid))
  dbExecute(db,'INSERT OR IGNORE INTO request_owners (rid,user_id) VALUES (?,?)',params=list(make_rid(chat,msgid),as.character(msg$from$id)))
  file <- get_photo(photos[[length(photos)]]$file_id); on.exit(unlink(file),add=TRUE)
  ocr <- tryCatch(ocr_photo(file),error=function(e)e)
  if (inherits(ocr,'error')) { show_manual_prompt(chat,msgid,as.character(msg$from$id),paste('Ошибка OCR через Ollama:',conditionMessage(ocr))); return() }
  # Return the full OCR text and structured extraction to the group for checking.
  pretty <- toJSON(list(full_text=ocr$text,items=ocr$items),auto_unbox=TRUE,pretty=TRUE,null='null')
  say(chat,paste0('Результат OCR (текст + JSON):\n',pretty))
  items <- ocr$items
  seen_rs <- character()
  rs_hits <- regmatches(ocr$text,gregexpr('(?i)rs[[:space:]]*[0-9]{4,12}',ocr$text,perl=TRUE))[[1]]
  if (length(rs_hits)) seen_rs <- unique(vapply(rs_hits,normalize_rs,character(1)))
  seen_rs <- seen_rs[nzchar(seen_rs)]
  date_values <- extract_date_tokens(ocr$text)
  direct_expiries <- character()
  for (item in items) {
    rs <- normalize_rs(item$rsid %||% '')
    if (nzchar(rs)) seen_rs <- unique(c(seen_rs,rs))
    expiry <- normalize_date(item$expiry %||% '')
    if (nzchar(expiry)) direct_expiries <- unique(c(direct_expiries,expiry))
    item_dates <- unlist(item$date_candidates %||% list(),use.names=FALSE)
    item_dates <- c(item_dates,item$lot %||% '',item$expiry %||% '')
    item_dates <- c(item_dates,extract_date_tokens(toJSON(item,auto_unbox=TRUE)))
    for (raw_date in item_dates) {
      d <- normalize_date(raw_date)
      if (nzchar(d)) date_values <- unique(c(date_values,d))
    }
  }
  if (length(seen_rs)!=1L) {
    show_manual_prompt(chat,msgid,as.character(msg$from$id),
      if (length(seen_rs)>1L) 'На фото найдено несколько rsID; выбрать автоматически нельзя.' else 'Не удалось распознать rsID.')
    return()
  }
  rs <- seen_rs[[1]]
  if (!length(date_values)) {
    show_manual_prompt(chat,msgid,as.character(msg$from$id),'Не удалось распознать дату Годен до.',rs)
    return()
  }
  checked <- tryCatch(match_dates_in_sheet(rs,date_values),error=function(e)e)
  if (inherits(checked,'error')) {
    show_manual_prompt(chat,msgid,as.character(msg$from$id),paste('Не удалось проверить распознанные даты в таблице:',conditionMessage(checked)),rs)
    return()
  }
  matches <- checked$matches %||% list()
  matched_dates <- character(); unique_dates <- character()
  for (m in matches) {
    count <- suppressWarnings(as.integer(m$count %||% 0))
    if (count>0L) matched_dates <- unique(c(matched_dates,as.character(m$expiry)))
    if (count==1L) unique_dates <- unique(c(unique_dates,as.character(m$expiry)))
  }
  direct <- intersect(direct_expiries,unique_dates)
  if (length(direct)==1L) {
    queue_debit(chat,msgid,as.character(msg$from$id),rs,direct[[1]])
    return()
  }
  if (length(unique_dates)==1L) {
    queue_debit(chat,msgid,as.character(msg$from$id),rs,unique_dates[[1]])
    return()
  }
  suggestions <- if (length(unique_dates)) unique_dates else matched_dates
  if (length(suggestions)) {
    note <- paste0('По rsID ',rs,' в таблице найдены эти даты из OCR: ',paste(vapply(suggestions,display_date,character(1)),collapse=', '),'. Укажите именно дату Годен до.')
  } else {
    note <- paste0('rsID ',rs,' распознан. Даты из OCR (',paste(vapply(date_values,display_date,character(1)),collapse=', '),') не совпали с датами партий в таблице. Укажите дату Годен до вручную.')
  }
  show_manual_prompt(chat,msgid,as.character(msg$from$id),note,rs)
}
on_callback <- function(cb) {
  info <- strsplit(cb$data %||% '',':',fixed=TRUE)[[1]]
  if (!(info[1] %in% c('yes','no')) || length(info)!=2L) return()
  row <- dbGetQuery(db,'SELECT * FROM pending WHERE rid=?',params=list(info[2]))
  if (!nrow(row)) { tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Заявка не найдена')); return() }
  p <- row[1,]
  if (!valid_chat(as.character(cb$message$chat$id)) || as.character(cb$from$id)!=p$user) {
    tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Подтверждать может только автор фото')); return()
  }
  if (p$state!='pending') { tg('answerCallbackQuery',list(callback_query_id=cb$id,text=paste('Уже обработано:',p$state))); return() }
  if (info[1]=='no') {
    dbExecute(db,"UPDATE pending SET state='cancelled' WHERE rid=?",params=list(p$rid))
    tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Отменено')); say(p$chat,paste('Отменено:',p$rs,display_date(p$expiry))); return()
  }
  tg('answerCallbackQuery',list(callback_query_id=cb$id,text='Повторно проверяю партию…'))
  if (DRY_RUN) {
    check <- tryCatch(apps(list(action='lookup',rs=p$rs,expiry=p$expiry)),error=function(e)e)
    if (inherits(check,'error') || as.integer(check$stock)!=as.integer(p$stock)) {
      dbExecute(db,"UPDATE pending SET state='stale' WHERE rid=?",params=list(p$rid))
      say(p$chat,'Остаток партии изменился после появления кнопки. Заявка устарела; списания нет.'); return()
    }
    dbExecute(db,"UPDATE pending SET state='dry_run' WHERE rid=?",params=list(p$rid))
    say(p$chat,sprintf('ТЕСТ: %s, годен до %s, партия %d → %d. Google Sheets не изменена.',p$rs,display_date(p$expiry),p$stock,p$stock-1L)); return()
  }
  result <- tryCatch(apps(list(action='commit',request_id=p$rid,rs=p$rs,expiry=p$expiry,
    expected_stock=as.integer(p$stock),chat_id=p$chat,message_id=p$msg,user_id=p$user)),error=function(e)e)
  if (inherits(result,'error')) { say(p$chat,paste('Не удалось подтвердить списание. Проверьте журнал Google Sheets:',conditionMessage(result))); return() }
  if (!isTRUE(result$ok)) {
    dbExecute(db,"UPDATE pending SET state='stale' WHERE rid=?",params=list(p$rid))
    say(p$chat,'Остаток изменился или партия стала неоднозначной. Заявка устарела; списания нет.'); return()
  }
  dbExecute(db,"UPDATE pending SET state='done' WHERE rid=?",params=list(p$rid))
  say(p$chat,sprintf('✅ Списано: %s, годен до %s. Партия %d → %d; всего по rsID %d → %d шт.',
    p$rs,display_date(p$expiry),result$before,result$after,result$total_before,result$total_after))
}
cat('Starting Telegram polling; allowed chats=',paste(TG_CHATS,collapse=','),
    '; Ollama model=',OLLAMA_MODEL,' DRY_RUN=',DRY_RUN,'\n',sep='')
offset <- 0
repeat {
  tryCatch({
    updates <- tg('getUpdates',list(offset=offset,timeout=30,allowed_updates=list('message','callback_query')))
    for (u in updates) {
      offset <- as.numeric(u$update_id)+1
      tryCatch({ if (!is.null(u$message)) on_message(u$message); if (!is.null(u$callback_query)) on_callback(u$callback_query) },
        error=function(e) message('Update error: ',conditionMessage(e)))
    }
  },error=function(e) { message('Polling error: ',conditionMessage(e)); Sys.sleep(5) })
}
