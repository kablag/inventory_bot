/** Google Sheets inventory API. Bind this script to the inventory spreadsheet. */
const CFG = {
  sheetName: 'Учёт наборов',
  logName: '_bot_log',
  rsColumn: 2,
  syntolColumn: 5,
  headerRow: 1,
};

function setSecretOnce() {
  // Set INVENTORY_API_KEY manually in Project Settings -> Script properties.
  // This function intentionally does NOT print or embed secrets.
  const secret = PropertiesService.getScriptProperties().getProperty('INVENTORY_API_KEY');
  if (!secret || secret.length < 32) throw new Error('Set a random INVENTORY_API_KEY (32+ chars) in Script properties');
}

function respond(obj) {
  return ContentService.createTextOutput(JSON.stringify(obj)).setMimeType(ContentService.MimeType.JSON);
}
function verify(req) {
  const expected = PropertiesService.getScriptProperties().getProperty('INVENTORY_API_KEY');
  // Apps Script does not expose request headers. Supply secret in POST JSON body over HTTPS.
  if (!expected || expected.length < 32 || !req || req.api_key !== expected) throw new Error('unauthorized');
}
function normalizeRs(value) {
  const match = String(value || '').toLowerCase().replace(/\s/g, '').match(/^rs(\d+)$/);
  return match ? `rs${match[1]}` : '';
}
function getSheet() {
  const sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName(CFG.sheetName);
  if (!sheet) throw new Error('Sheet not found: ' + CFG.sheetName);
  return sheet;
}
function locate(rs) {
  const sheet = getSheet();
  const target = normalizeRs(rs);
  if (!target) throw new Error('invalid_rs');
  const last = sheet.getLastRow();
  if (last <= CFG.headerRow) throw new Error('not_found');
  const values = sheet.getRange(CFG.headerRow + 1, CFG.rsColumn, last - CFG.headerRow, 1).getDisplayValues();
  const found = [];
  values.forEach((r, idx) => { if (normalizeRs(r[0]) === target) found.push(CFG.headerRow + idx + 1); });
  if (found.length === 0) throw new Error('not_found');
  if (found.length !== 1) throw new Error('ambiguous_rs');
  const row = found[0];
  const cell = sheet.getRange(row, CFG.syntolColumn);
  const value = cell.getValue();
  if (typeof value !== 'number' || !Number.isInteger(value) || value < 0) {
    throw new Error('non_numeric_stock: ' + cell.getDisplayValue());
  }
  return {sheet, row, cell, stock: value, rs: target,
          name: sheet.getRange(row, 1).getDisplayValue()};
}
function logSheet() {
  const ss = SpreadsheetApp.getActiveSpreadsheet();
  let sheet = ss.getSheetByName(CFG.logName);
  if (!sheet) {
    sheet = ss.insertSheet(CFG.logName);
    sheet.appendRow(['request_id', 'created_at', 'chat_id', 'message_id', 'user_id', 'rs', 'name', 'before', 'after']);
    sheet.hideSheet();
  }
  return sheet;
}
function loggedResult(requestId) {
  const sheet = logSheet(), n = sheet.getLastRow();
  if (n < 2) return null;
  const ids = sheet.getRange(2, 1, n - 1, 1).getValues();
  const ix = ids.findIndex(r => String(r[0]) === requestId);
  if (ix < 0) return null;
  const row = sheet.getRange(ix + 2, 1, 1, 9).getValues()[0];
  return {ok:true, status:'already_applied', rs:String(row[5]), name:String(row[6]),
          before:Number(row[7]), after:Number(row[8])};
}
function execute(req) {
  verify(req);
  if (req.action === 'lookup') {
    const found = locate(req.rs);
    return {ok:true, status:'found', rs:found.rs, name:found.name, stock:found.stock};
  }
  if (req.action !== 'commit') throw new Error('invalid_action');
  if (!/^[-a-zA-Z0-9_:]{5,150}$/.test(String(req.request_id || ''))) throw new Error('invalid_request_id');
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(20000)) throw new Error('busy');
  try {
    const done = loggedResult(req.request_id);
    if (done) return done;
    const found = locate(req.rs);
    if (found.stock !== Number(req.expected_stock)) {
      return {ok:false, error:'stock_changed', current:found.stock, rs:found.rs};
    }
    if (found.stock < 1) throw new Error('out_of_stock');
    const updated = found.stock - 1;
    found.cell.setValue(updated);
    SpreadsheetApp.flush();
    // Record successful operation. If client misses reply, request_id makes retry idempotent.
    logSheet().appendRow([String(req.request_id), new Date(), String(req.chat_id || ''),
      String(req.message_id || ''), String(req.user_id || ''), found.rs, found.name, found.stock, updated]);
    SpreadsheetApp.flush();
    return {ok:true, status:'applied', rs:found.rs, name:found.name, before:found.stock, after:updated};
  } finally { lock.releaseLock(); }
}
function doPost(e) {
  try {
    if (!e || !e.postData || !e.postData.contents) throw new Error('empty_request');
    return respond(execute(JSON.parse(e.postData.contents)));
  } catch (err) {
    return respond({ok:false,error:String(err.message || err)});
  }
}
