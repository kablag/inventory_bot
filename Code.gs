/** Google Sheets API for lot based inventory. Columns follow the new sheet:
 * A dbSNP ID, B rs, C short name, D expiry, E quantity, F/G legacy stock.
 */
const CFG = {
  sheetName: 'Учёт наборов',
  logName: '_bot_log',
  headerRow: 1,
  rsColumn: 2,
  nameColumn: 3,
  expiryColumn: 4,
  stockColumn: 5,
};

function setSecretOnce() {
  const secret = PropertiesService.getScriptProperties().getProperty('INVENTORY_API_KEY');
  if (!secret || secret.length < 32) throw new Error('Set INVENTORY_API_KEY (32+ chars) in Script properties');
}
function respond(obj) {
  return ContentService.createTextOutput(JSON.stringify(obj)).setMimeType(ContentService.MimeType.JSON);
}
function verify(req) {
  const expected = PropertiesService.getScriptProperties().getProperty('INVENTORY_API_KEY');
  if (!expected || expected.length < 32 || !req || req.api_key !== expected) throw new Error('unauthorized');
}
function normalizeRs(value) {
  const match = String(value || '').toLowerCase().replace(/\s/g, '').match(/^rs(\d{4,12})$/);
  return match ? `rs${match[1]}` : '';
}
function normalizeExpiry(value) {
  const s = String(value || '').trim();
  const m = s.match(/^(\d{1,2})[.\-/](\d{1,2})[.\-/](\d{4})$/);
  if (!m) return '';
  const year = Number(m[3]), month = Number(m[2]), day = Number(m[1]);
  const d = new Date(Date.UTC(year, month - 1, day));
  if (d.getUTCFullYear() !== year || d.getUTCMonth() !== month - 1 || d.getUTCDate() !== day) return '';
  return `${year}-${String(month).padStart(2, '0')}-${String(day).padStart(2, '0')}`;
}
function getSheet() {
  const sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName(CFG.sheetName);
  if (!sheet) throw new Error('Sheet not found: ' + CFG.sheetName);
  return sheet;
}
function lotRows(rs, expiry) {
  const sheet = getSheet(), targetRs = normalizeRs(rs), targetExpiry = normalizeExpiry(expiry);
  if (!targetRs) throw new Error('invalid_rs');
  if (!targetExpiry) throw new Error('invalid_expiry');
  const last = sheet.getLastRow();
  if (last <= CFG.headerRow) return [];
  const count = last - CFG.headerRow;
  const vals = sheet.getRange(CFG.headerRow + 1, 1, count, CFG.stockColumn).getValues();
  const disp = sheet.getRange(CFG.headerRow + 1, 1, count, CFG.stockColumn).getDisplayValues();
  const found = [];
  vals.forEach((row, i) => {
    if (normalizeRs(disp[i][CFG.rsColumn - 1]) !== targetRs) return;
    const rowExpiry = normalizeExpiry(disp[i][CFG.expiryColumn - 1]);
    if (rowExpiry === targetExpiry) found.push({row: CFG.headerRow + 1 + i, stock: row[CFG.stockColumn - 1],
      name: disp[i][CFG.nameColumn - 1], expiry: targetExpiry});
  });
  return found;
}
function totalForRs(rs) {
  const sheet = getSheet(), target = normalizeRs(rs), last = sheet.getLastRow();
  if (!target) throw new Error('invalid_rs');
  if (last <= CFG.headerRow) return 0;
  const rows = sheet.getRange(CFG.headerRow + 1, 1, last - CFG.headerRow, CFG.stockColumn).getValues();
  const display = sheet.getRange(CFG.headerRow + 1, CFG.rsColumn, last - CFG.headerRow, 1).getDisplayValues();
  let total = 0;
  rows.forEach((r, i) => {
    if (normalizeRs(display[i][0]) !== target) return;
    const q = r[CFG.stockColumn - 1];
    if (typeof q !== 'number' || !Number.isInteger(q) || q < 0) throw new Error('non_numeric_stock_at_row_' + (CFG.headerRow + i + 1));
    total += q;
  });
  return total;
}
function locate(rs, expiry) {
  const found = lotRows(rs, expiry);
  if (!found.length) throw new Error('not_found');
  if (found.length !== 1) throw new Error('ambiguous_lot');
  const item = found[0];
  if (typeof item.stock !== 'number' || !Number.isInteger(item.stock) || item.stock < 0) throw new Error('non_numeric_stock_at_row_' + item.row);
  return {...item, rs: normalizeRs(rs), cell: getSheet().getRange(item.row, CFG.stockColumn), total: totalForRs(rs)};
}
function logSheet() {
  const ss = SpreadsheetApp.getActiveSpreadsheet();
  let sheet = ss.getSheetByName(CFG.logName);
  if (!sheet) {
    sheet = ss.insertSheet(CFG.logName);
    sheet.appendRow(['request_id', 'created_at', 'chat_id', 'message_id', 'user_id', 'rs', 'expiry', 'name', 'lot_before', 'lot_after', 'total_before', 'total_after']);
    sheet.hideSheet();
  } else {
    const headers = sheet.getRange(1, 1, 1, Math.max(sheet.getLastColumn(), 9)).getDisplayValues()[0];
    // Preserve idempotency records from the previous single-stock log layout.
    if (headers[6] === 'name') {
      sheet.insertColumnBefore(7);
      sheet.getRange(1, 1, 1, 12).setValues([['request_id', 'created_at', 'chat_id', 'message_id', 'user_id', 'rs', 'expiry', 'name', 'lot_before', 'lot_after', 'total_before', 'total_after']]);
    }
  }
  return sheet;
}
function loggedResult(requestId) {
  const sheet = logSheet(), n = sheet.getLastRow();
  if (n < 2) return null;
  const ids = sheet.getRange(2, 1, n - 1, 1).getValues();
  const ix = ids.findIndex(r => String(r[0]) === requestId);
  if (ix < 0) return null;
  const row = sheet.getRange(ix + 2, 1, 1, 12).getValues()[0];
  return {ok:true, status:'already_applied', rs:String(row[5]), expiry:String(row[6]), name:String(row[7]),
    before:Number(row[8]), after:Number(row[9]), total_before:Number(row[10]), total_after:Number(row[11])};
}
function execute(req) {
  verify(req);
  if (req.action === 'lookup') {
    const found = locate(req.rs, req.expiry);
    return {ok:true, status:'found', rs:found.rs, expiry:found.expiry, name:found.name,
      stock:found.stock, total:found.total, row:found.row};
  }
  if (req.action === 'list_rs') {
    const sheet = getSheet(), last = sheet.getLastRow();
    const values = last <= CFG.headerRow ? [] : sheet.getRange(CFG.headerRow + 1, CFG.rsColumn, last - CFG.headerRow, 1).getDisplayValues().flat();
    return {ok:true, rsids:[...new Set(values.map(normalizeRs).filter(Boolean))]};
  }
  if (req.action !== 'commit') throw new Error('invalid_action');
  if (!/^[-a-zA-Z0-9_:]{5,150}$/.test(String(req.request_id || ''))) throw new Error('invalid_request_id');
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(20000)) throw new Error('busy');
  try {
    const done = loggedResult(req.request_id);
    if (done) return done;
    const found = locate(req.rs, req.expiry);
    if (found.stock !== Number(req.expected_stock)) return {ok:false, error:'stock_changed', current:found.stock, rs:found.rs, expiry:found.expiry};
    if (found.stock < 1) throw new Error('out_of_stock');
    const after = found.stock - 1;
    found.cell.setValue(after);
    SpreadsheetApp.flush();
    const totalAfter = totalForRs(found.rs);
    logSheet().appendRow([String(req.request_id), new Date(), String(req.chat_id || ''), String(req.message_id || ''),
      String(req.user_id || ''), found.rs, found.expiry, found.name, found.stock, after, found.total, totalAfter]);
    SpreadsheetApp.flush();
    return {ok:true, status:'applied', rs:found.rs, expiry:found.expiry, name:found.name,
      before:found.stock, after, total_before:found.total, total_after:totalAfter};
  } finally { lock.releaseLock(); }
}
function doPost(e) {
  try {
    if (!e || !e.postData || !e.postData.contents) throw new Error('empty_request');
    return respond(execute(JSON.parse(e.postData.contents)));
  } catch (err) { return respond({ok:false, error:String(err.message || err)}); }
}
