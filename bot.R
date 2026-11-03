suppressPackageStartupMessages({
  library(httr2); library(jsonlite); library(DBI); library(RSQLite); library(base64enc); library(magick)
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
dbExecute(db, 'CREATE TABLE IF NOT EXISTS manual_requests (chat TEXT NOT NULL, prompt_msg TEXT NOT NULL, photo_msg TEXT NOT NULL, user_id TEXT NOT NULL, state TEXT NOT NULL, PRIMARY KEY(chat,prompt_msg))')

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
display_date <- function(x) {
  d <- suppressWarnings(as.Date(x, '%Y-%m-%d'))
  if (is.na(d)) x else format(d, '%d.%m.%Y')
}
ocr_photo <- function(file) {
  encoded <- base64enc::base64encode(readBin(file, what='raw', n=file.info(file)$size), linewidth=0)
  # Locate whole labels from image content, then crop and enlarge each one.
  image <- magick::image_read(file)
  info <- magick::image_info(image)[1,]
  width <- info$width; height <- info$height
  encode_image <- function(im) {
    path <- tempfile(fileext='.jpg'); on.exit(unlink(path),add=TRUE)
    magick::image_write(im,path=path,format='jpeg',quality=95)
    base64enc::base64encode(readBin(path,what='raw',n=file.info(path)$size),linewidth=0)
  }
  locator_prompt <- paste(
    'Найди целые прямоугольные печатные этикетки наборов реагентов на фото.',
    'Верни границы каждой этикетки целиком, от верхнего до нижнего края, включая мелкие строки. Координаты x0,y0,x1,y1 укажи в диапазоне 0..1000 относительно ширины и высоты изображения.',
    'Не выделяй отдельные слова или фон. Если границы этикеток определить нельзя, верни пустой labels.',sep='\n')
  locator_schema <- list(type='object',properties=list(labels=list(type='array',items=list(
    type='object',properties=list(x0=list(type='number'),y0=list(type='number'),x1=list(type='number'),y1=list(type='number')),
    required=list('x0','y0','x1','y1'),additionalProperties=FALSE))),required=list('labels'),additionalProperties=FALSE)
  locator <- tryCatch({
    r <- request(paste0(OLLAMA_URL,'/api/generate')) |>
      req_body_json(list(model=OLLAMA_MODEL,prompt=locator_prompt,images=list(encoded),stream=FALSE,
        format=locator_schema,keep_alive='30s',options=list(temperature=0)),auto_unbox=TRUE) |>
      req_timeout(300) |> req_perform()
    outer <- resp_body_json(r,simplifyVector=FALSE)
    fromJSON(outer$response,simplifyVector=FALSE)
  },error=function(e) NULL)
  boxes <- list()
  if (!is.null(locator) && is.list(locator$labels)) for (b in locator$labels) {
    xy <- suppressWarnings(as.numeric(unlist(b[c('x0','y0','x1','y1')],use.names=FALSE)))
    if (length(xy)!=4L || any(!is.finite(xy)) || any(xy<0) || any(xy>1000)) next
    b <- xy/1000
    if (b[3]<=b[1] || b[4]<=b[2]) next
    mx <- (b[3]-b[1])*.10; my <- (b[4]-b[2])*.10
    b[c(1,2)] <- pmax(0,b[c(1,2)]-c(mx,my)); b[c(3,4)] <- pmin(1,b[c(3,4)]+c(mx,my))
    if ((b[3]-b[1])<.15 || (b[4]-b[2])<.07 || (b[3]-b[1])/(b[4]-b[2])<.35 || (b[3]-b[1])/(b[4]-b[2])>8) next
    boxes[[length(boxes)+1L]] <- b
    if (length(boxes)>=4L) break
  }
  # If localization fails, scan overlapping bands across the complete image.
  if (!length(boxes)) boxes <- lapply(c(0,.15,.30),function(y) c(0,y,1,min(1,y+.70)))
  focus_encoded <- lapply(boxes,function(b) {
    left <- floor(b[1]*width); top <- floor(b[2]*height)
    cw <- max(1L,floor((b[3]-b[1])*width)); ch <- max(1L,floor((b[4]-b[2])*height))
    patch <- magick::image_crop(image,magick::geometry_area(cw,ch,left,top))
    encode_image(magick::image_resize(patch,'1800x'))
  })
  prompt <- paste(
    'Изображение 1 — полное фото. Следующие изображения — найденные на нем целые этикетки крупным планом либо перекрывающиеся области всего кадра, если прямоугольники этикеток найти не удалось. Используй увеличенные изображения для считывания мелких полей. Не дублируй этикетки, попавшие в перекрывающиеся области.',
    'Верни в full_text весь фактически видимый текст этикетки, включая мелкую нижнюю часть. Сохрани подписи и порядок строк: значение относится только к подписи в той же строке. В items помести массив объектов: по одному объекту для каждого набора/этикетки на фото.',
    'Строго сопоставляй подпись и значение в одной строке: Красители → dyes; Серия → lot; Годен до → expiry; Хранить при → storage. Не сдвигай значения между соседними строками: температура хранения никогда не является сроком годности, а дата серии не является сроком годности.',
    'product — название набора/реагента из верхней части этикетки, например Синтол NP-471-100; name — краткое название, если оно напечатано. Не записывай красители в product.',
    'У каждого объекта должны быть строковые поля product, name, dbsnp_id, gene, rsid, lot, expiry, dyes, storage и объект additional для прочих реально прочитанных полей. В rsid включай префикс rs, если он есть на этикетке.',
    'В каждом поле укажи только прочитанное с изображения; неизвестное поле оставь пустой строкой, additional оставь пустым объектом. Не пиши многоточия и описания вроде "название" или "ген" как значения.',
    'Сохраняй символы и цифры rsID точно. expiry заполняй, только если дата рядом с подписью Годен до читается однозначно. Формат ДД.ММ.ГГГГ оставляй так; шесть цифр без разделителей трактуй как ДДММГГ и преобразуй в ДД.ММ.20ГГ, только если это реальная календарная дата. При любой неоднозначности оставь expiry пустым.',
    'Не угадывай и не исправляй нечитаемые символы. Даже если часть данных не читается, все равно верни распознанный full_text и массив items.',
    sep='\n')
  schema <- list(type='object', properties=list(
    full_text=list(type='string'),
    items=list(type='array', items=list(type='object', properties=list(
      product=list(type='string'), name=list(type='string'), dbsnp_id=list(type='string'),
      gene=list(type='string'), rsid=list(type='string'), lot=list(type='string'),
      expiry=list(type='string'), dyes=list(type='string'), storage=list(type='string'),
      additional=list(type='object')),
      required=list('product','name','dbsnp_id','gene','rsid','lot','expiry','dyes','storage','additional'),
      additionalProperties=FALSE))),
    required=list('full_text','items'), additionalProperties=FALSE)
  images <- c(list(encoded),focus_encoded)
  req <- request(paste0(OLLAMA_URL, '/api/generate')) |>
    req_body_json(list(model=OLLAMA_MODEL, prompt=prompt, images=images, stream=FALSE,
      format=schema, keep_alive=0, options=list(temperature=0)), auto_unbox=TRUE) |>
    req_timeout(300) |> req_perform()
  response <- resp_body_json(req, simplifyVector=FALSE)
  if (is.null(response$response)) stop('Ollama вернул пустой ответ')
  parsed <- tryCatch(fromJSON(response$response, simplifyVector=FALSE), error=function(e) NULL)
  if (is.null(parsed)) stop('Ollama вернул некорректный JSON')
  full_text <- as.character(parsed$full_text %||% '')
  items <- parsed$items %||% list()
  if (!is.list(items) || grepl('весь текст с сохранением строк', full_text, ignore.case=TRUE) ||
      grepl('...', response$response, fixed=TRUE))
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
show_manual_prompt <- function(chat, photo_msg, user_id, intro) {
  prompt <- say(chat,paste0(intro, '\nОтветьте на это сообщение парой: rs1799853 05.05.2027. Автор фото должен подтвердить списание.'))
  dbExecute(db, 'INSERT OR REPLACE INTO manual_requests (chat,prompt_msg,photo_msg,user_id,state) VALUES (?,?,?,?,?)',
    params=list(chat,as.character(prompt$message_id),photo_msg,user_id,'waiting'))
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
  pair <- parse_manual_pair(msg$text %||% '')
  if (is.null(pair)) { say(chat,'Введите в ответ на сообщение пару строго в формате: rs1799853 05.05.2027'); return(TRUE) }
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
  pairs <- list()
  seen_rs <- character()
  for (item in items) {
    rs <- normalize_rs(item$rsid %||% '')
    if (nzchar(rs)) seen_rs <- unique(c(seen_rs,rs))
    expiry <- normalize_date(item$expiry %||% '')
    if (nzchar(rs) && nzchar(expiry)) pairs[[paste(rs,expiry,sep='|')]] <- list(rs=rs,expiry=expiry)
  }
  if (length(pairs)!=1L || length(seen_rs)!=1L) {
    show_manual_prompt(chat,msgid,as.character(msg$from$id),
      if (length(seen_rs)>1L || length(pairs)>1L) 'На фото найдено несколько rsID или пар rsID + срок годности; выбрать автоматически нельзя.' else 'Не удалось однозначно распознать пару rsID + срок годности.')
    return()
  }
  pair <- pairs[[1]]
  queue_debit(chat,msgid,as.character(msg$from$id),pair$rs,pair$expiry)
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
