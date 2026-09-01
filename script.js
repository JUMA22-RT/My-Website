/* ══════════════════════════════════════════════
   GLOBAL STATE
══════════════════════════════════════════════ */
let excelWorkbook   = null;
let currentSheet    = null;

/* ══════════════════════════════════════════════
   PAGE NAVIGATION
══════════════════════════════════════════════ */
function showPage(id) {
  document.querySelectorAll('.page').forEach(p => p.classList.remove('active'));
  const page = document.getElementById(id);
  if (page) page.classList.add('active');
  window.scrollTo({ top: 0, behavior: 'smooth' });
}


/* ══════════════════════════════════════════════
   ABOUT TABS
══════════════════════════════════════════════ */
function switchAbout(tab) {
  document.querySelectorAll('.about-panel').forEach(p => p.classList.remove('active'));
  document.querySelectorAll('.about-tab').forEach(t => {
    t.classList.remove('active');
    const fn = t.getAttribute('onclick') || '';
    if (fn.includes(`'${tab}'`)) t.classList.add('active');
  });
  const panel = document.getElementById('about-' + tab);
  if (panel) panel.classList.add('active');
}

/* ══════════════════════════════════════════════
   PROJECT CATEGORY TABS
══════════════════════════════════════════════ */
function switchProjCat(cat) {
  switchCategory(cat);
}

/* ══════════════════════════════════════════════
   GENERIC TAB SWITCH  (data-group / data-target)
══════════════════════════════════════════════ */
function switchTab(group, target) {
  switchTabs(group, target);
}

/* ══════════════════════════════════════════════
   HAMBURGER
══════════════════════════════════════════════ */
const hamburger = document.getElementById('hamburger');
const navLinks  = document.getElementById('navLinks');

if (hamburger) {
  hamburger.addEventListener('click', (e) => {
    e.stopPropagation(); // prevent bubbling to document
    hamburger.classList.toggle('open');
    navLinks && navLinks.classList.toggle('open');
  });
}

function closeNav() {
  hamburger && hamburger.classList.remove('open');
  navLinks  && navLinks.classList.remove('open');
}

// Collapse when clicking outside
document.addEventListener('click', (e) => {
  if (!navLinks.contains(e.target) && !hamburger.contains(e.target)) {
    closeNav();
  }
});

/* ══════════════════════════════════════════════
   TAB BAR CLICK DELEGATION
══════════════════════════════════════════════ */

document.addEventListener('click', (e) => {

  /* =========================
     CATEGORY TABS
  ========================== */
  const catTab = e.target.closest('.cat-tab');
  if (catTab) {
    switchCategory(catTab.dataset.cat);
    return;
  }

  /* =========================
     NORMAL PROJECT TABS
  ========================== */
  const tab = e.target.closest('.tab-btn');
  if (tab) {
    switchTabs(tab.dataset.group, tab.dataset.target);
    return;
  }

  /* =========================
     EMBEDDED TABS (MEAL, SCTO etc.)
  ========================== */
  const embed = e.target.closest('[data-embed-target]');
  if (embed) {
    switchEmbedded(embed);
  }
});

function switchCategory(cat) {

  document.querySelectorAll('.cat-tab')
    .forEach(t => t.classList.remove('active'));

  document.querySelectorAll('.proj-cat')
    .forEach(c => c.classList.remove('active'));

  const activeBtn = document.querySelector(`.cat-tab[data-cat="${cat}"]`);
  const activeCat = document.getElementById(`cat-${cat}`);

  if (activeBtn) activeBtn.classList.add('active');
  if (activeCat) activeCat.classList.add('active');
}

function switchTabs(group, target) {

  document.querySelectorAll(`.tab-btn[data-group="${group}"]`)
    .forEach(b => b.classList.remove('active'));

  document.querySelectorAll(`.tab-panel[data-group="${group}"]`)
    .forEach(p => {
      p.classList.remove('active');
      p.setAttribute('hidden', '');
    });

  const btn = document.querySelector(
    `.tab-btn[data-group="${group}"][data-target="${target}"]`
  );

  const panel = document.getElementById(target);

  if (btn) btn.classList.add('active');
  if (panel) {
    panel.classList.add('active');
    panel.removeAttribute('hidden');
  }
}

function switchEmbedded(el) {
  const tabBar = el.closest('.ip-tab-bar, .form-tab-bar, .kobo-tab-bar');
  if (!tabBar) return;

  const container = el.closest('.reader-card');
  if (!container) return;

  const target = el.dataset.embedTarget;

  tabBar.querySelectorAll('.ip-tab, .form-tab, .kobo-tab')
    .forEach(t => t.classList.remove('active'));

  container.querySelectorAll('.ip-panel, .form-panel, .kobo-panel')
    .forEach(p => p.classList.remove('active'));

  el.classList.add('active');

  const panel = container.querySelector(`#${target}`);
  if (panel) panel.classList.add('active');
}

/* ══════════════════════════════════════════════
   DRAG & DROP HELPERS
══════════════════════════════════════════════ */
function dragOver(e) {
  e.preventDefault();
  e.currentTarget.classList.add('drag-over');
}

function dragLeave(e, zoneId) {
  const zone = document.getElementById(zoneId);
  if (zone) zone.classList.remove('drag-over');
}

function dropFile(e, type, displayId, statusId) {
  e.preventDefault();
  const zone = e.currentTarget;
  zone.classList.remove('drag-over');
  const file = e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files[0];
  if (!file) return;

  const card   = zone.closest('.reader-card');
  const areaEl = card && card.querySelector('.reader-area');
  const fnEl   = card && card.querySelector('.reader-lbl');

  processFile(file, type, displayId, statusId, areaEl, zone, fnEl ? fnEl.id : null);
}

/* ══════════════════════════════════════════════
   MAIN FILE HANDLER
══════════════════════════════════════════════ */
function handleFile(input, type, displayId, statusId, areaId, zoneId, fnameId) {
  const file = input.files && input.files[0];
  if (!file) return;
  const areaEl = document.getElementById(areaId);
  const zoneEl = document.getElementById(zoneId);
  processFile(file, type, displayId, statusId, areaEl, zoneEl, fnameId);
}

function processFile(file, type, displayId, statusId, areaEl, zoneEl, fnameId) {
  const statusEl = document.getElementById(statusId);
  const fnEl     = fnameId ? document.getElementById(fnameId) : null;

  if (statusEl) statusEl.textContent = `Loading ${file.name}…`;
  if (fnEl)     fnEl.textContent     = file.name;

  const dispatch = {
    xlsform:    loadXLSForm,
    excel:      loadExcel,
    word:       loadWord,
    pdf:        loadPDF,
    r:          loadScript,
    python:     loadScript,
    slides:     loadSlides,
    screenshot: loadScreenshot,
  };
  const fn = dispatch[type] || loadScreenshot;
  fn(file, displayId, statusId, areaEl, zoneEl);
}

function showReader(areaEl, zoneEl) {
  if (areaEl)  areaEl.classList.remove('hidden');
  if (zoneEl)  zoneEl.style.display = 'none';
}

/* ──────────────────────────────────────────────
   XLSForm Viewer
────────────────────────────────────────────── */
function loadXLSForm(file, displayId, statusId, areaEl, zoneEl) {
  const reader = new FileReader();
  reader.onload = function(e) {
    try {
      const wb   = XLSX.read(e.target.result, { type: 'array' });
      const disp = document.getElementById(displayId);
      if (!disp) return;
      disp.innerHTML = '';

      const container = document.createElement('div');
      container.className = 'xlsform-display';

      const priorityOrder = ['survey','choices','settings'];
      const names         = wb.SheetNames;
      const ordered       = [
        ...priorityOrder.filter(s => names.some(n => n.toLowerCase() === s)),
        ...names.filter(n => !priorityOrder.includes(n.toLowerCase()))
      ];

      ordered.forEach(sName => {
        const actual = names.find(n => n.toLowerCase() === sName.toLowerCase()) || sName;
        const ws     = wb.Sheets[actual];
        if (!ws) return;
        const rows   = XLSX.utils.sheet_to_json(ws, { defval: '' });

        const label  = document.createElement('div');
        label.className   = 'xf-sheet-label';
        label.textContent = `📋 ${actual}  (${rows.length} rows)`;
        container.appendChild(label);

        if (actual.toLowerCase() === 'survey') {
          rows.forEach(row => {
            const qType   = row.type   || '';
            const qName   = row.name   || '';
            const qLabel  = row['label'] || row['label::English (en)'] || row['label:English'] || '';
            const rel     = row.relevant    || '';
            const cons    = row.constraint  || '';
            const calc    = row.calculation || '';
            const card    = document.createElement('div');
            card.className = 'q-card';
            card.innerHTML =
              `<span class="q-type">${esc(qType)}</span>` +
              `<span class="q-name">${esc(qName)}</span>` +
              `<span class="q-label">${esc(qLabel)}</span>` +
              (rel || cons || calc
                ? `<span class="q-meta">${rel ? '⚡ '+esc(rel)+'&nbsp;' : ''}${cons ? '🔒 '+esc(cons)+'&nbsp;' : ''}${calc ? '🧮 '+esc(calc) : ''}</span>`
                : '');
            container.appendChild(card);
          });
        } else {
          if (rows.length === 0) return;
          const cols    = Object.keys(rows[0]);
          const wrap    = document.createElement('div');
          wrap.className = 'table-scroll';
          wrap.style.borderRadius = '0 0 6px 6px';
          const table   = buildTable(cols, rows);
          wrap.appendChild(table);
          container.appendChild(wrap);
        }
      });

      disp.appendChild(container);
      document.getElementById(statusId).textContent = `✅ Loaded: ${file.name}`;
      showReader(areaEl, zoneEl);
    } catch(err) {
      document.getElementById(statusId).textContent = '❌ ' + err.message;
    }
  };
  reader.readAsArrayBuffer(file);
}


/* ──────────────────────────────────────────────
   Excel Reader + Editor
────────────────────────────────────────────── */
function loadExcel(file, displayId, statusId, areaEl, zoneEl) {
  const reader = new FileReader();
  reader.onload = function(e) {
    try {
      excelWorkbook = XLSX.read(e.target.result, { type: 'array' });
      currentSheet  = excelWorkbook.SheetNames[0];
      renderSheetTabs(excelWorkbook.SheetNames, displayId);
      renderExcelSheet(currentSheet, displayId);
      document.getElementById(statusId).textContent = `✅ Loaded: ${file.name}`;
      showReader(areaEl, zoneEl);
    } catch(err) {
      document.getElementById(statusId).textContent = '❌ ' + err.message;
    }
  };
  reader.readAsArrayBuffer(file);
}

function renderSheetTabs(sheetNames, displayId) {
  const container = document.getElementById('xl-sheet-tabs');
  if (!container) return;
  container.innerHTML = '';
  sheetNames.forEach((name, i) => {
    const btn = document.createElement('button');
    btn.className   = 'sheet-tab-btn' + (i === 0 ? ' active' : '');
    btn.textContent = name;
    btn.onclick = function() {
      container.querySelectorAll('.sheet-tab-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      currentSheet = name;
      renderExcelSheet(name, displayId);
    };
    container.appendChild(btn);
  });
}

function renderExcelSheet(sheetName, displayId) {
  if (!excelWorkbook) return;
  const ws   = excelWorkbook.Sheets[sheetName];
  const rows = XLSX.utils.sheet_to_json(ws, { header: 1, defval: '' });
  const disp = document.getElementById(displayId);
  if (!disp) return;
  disp.innerHTML = '';

  if (rows.length === 0) {
    disp.innerHTML = '<p style="padding:1rem;color:var(--text3)">Empty sheet</p>';
    return;
  }

  const headers = rows[0];
  const table   = document.createElement('table');
  const thead   = document.createElement('thead');
  const htr     = document.createElement('tr');

  const numTh = document.createElement('th');
  numTh.textContent = '#';
  htr.appendChild(numTh);

  headers.forEach(h => {
    const th = document.createElement('th');
    th.textContent = h;
    htr.appendChild(th);
  });
  thead.appendChild(htr);
  table.appendChild(thead);

  const tbody = document.createElement('tbody');
  rows.slice(1).forEach((row, ri) => {
    const tr    = document.createElement('tr');
    const rowTd = document.createElement('td');
    rowTd.textContent = ri + 1;
    rowTd.style.color = 'var(--text3)';
    tr.appendChild(rowTd);

    headers.forEach((_, ci) => {
      const td = document.createElement('td');
      td.textContent      = row[ci] !== undefined ? row[ci] : '';
      td.contentEditable  = true;
      td.dataset.row = ri;
      td.dataset.col = ci;
      td.addEventListener('blur', function() {
        if (!excelWorkbook) return;
        const ws  = excelWorkbook.Sheets[currentSheet];
        const rIdx = parseInt(this.dataset.row) + 1; // +1 for header
        const cIdx = parseInt(this.dataset.col);
        const addr = XLSX.utils.encode_cell({ r: rIdx, c: cIdx });
        if (!ws[addr]) ws[addr] = {};
        ws[addr].v = this.textContent;
        ws[addr].t = 's';
      });
      tr.appendChild(td);
    });
    tbody.appendChild(tr);
  });
  table.appendChild(tbody);
  disp.appendChild(table);
}

function filterTable(input, containerId) {
  const q    = input.value.toLowerCase();
  const cont = document.getElementById(containerId);
  if (!cont) return;
  cont.querySelectorAll('tbody tr').forEach(tr => {
    tr.style.display = tr.textContent.toLowerCase().includes(q) ? '' : 'none';
  });
}

function downloadExcel() {
  if (!excelWorkbook) return;
  XLSX.writeFile(excelWorkbook, 'modified_data.xlsx');
}

/* ──────────────────────────────────────────────
   Word Reader + Editor
────────────────────────────────────────────── */
function loadWord(file, displayId, statusId, areaEl, zoneEl) {
  const reader = new FileReader();
  reader.onload = function(e) {
    mammoth.convertToHtml({ arrayBuffer: e.target.result })
      .then(function(result) {
        const disp = document.getElementById(displayId);
        if (disp) disp.innerHTML = result.value;
        document.getElementById(statusId).textContent = `✅ Loaded: ${file.name}`;
        showReader(areaEl, zoneEl);
      })
      .catch(function(err) {
        document.getElementById(statusId).textContent = '❌ ' + err.message;
      });
  };
  reader.readAsArrayBuffer(file);
}

function downloadWord() {
  const disp = document.getElementById('word-display');
  if (!disp) return;
  const blob = new Blob(['\ufeff' + disp.innerHTML], { type: 'application/msword' });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement('a');
  a.href     = url;
  a.download = 'document.doc';
  a.click();
  URL.revokeObjectURL(url);
}

/* ──────────────────────────────────────────────
   PDF Viewer
────────────────────────────────────────────── */
function loadPDF(file, displayId, statusId, areaEl, zoneEl) {
  const url   = URL.createObjectURL(file);
  const frame = document.getElementById(displayId);
  if (frame) frame.src = url;
  document.getElementById(statusId).textContent = `✅ Loaded: ${file.name}`;
  showReader(areaEl, zoneEl);
}

/* ──────────────────────────────────────────────
   Script Reader  (R / Python)
────────────────────────────────────────────── */
function loadScript(file, displayId, statusId, areaEl, zoneEl) {
  const reader = new FileReader();
  reader.onload = function(e) {
    const code = document.getElementById(displayId);
    if (code) {
      code.textContent = e.target.result;
      if (window.Prism) Prism.highlightElement(code);
    }
    // Update filename in the macOS-style title bar
    const titleId = displayId.replace(/-code$/, '-ftitle');
    const titleEl = document.getElementById(titleId);
    if (titleEl) titleEl.textContent = file.name;

    document.getElementById(statusId).textContent = `✅ Loaded: ${file.name}`;
    showReader(areaEl, zoneEl);
  };
  reader.readAsText(file);
}

/* ──────────────────────────────────────────────
   Slides Reader  (PPTX / PDF)
────────────────────────────────────────────── */
function loadSlides(file, displayId, statusId, areaEl, zoneEl) {
  if (file.name.toLowerCase().endsWith('.pdf')) {
    const url   = URL.createObjectURL(file);
    const disp  = document.getElementById(displayId);
    if (disp) {
      disp.innerHTML = '';
      const frame     = document.createElement('iframe');
      frame.src       = url;
      frame.className = 'doc-frame';
      disp.appendChild(frame);
    }
    document.getElementById(statusId).textContent = `✅ Loaded: ${file.name}`;
    showReader(areaEl, zoneEl);
  } else {
    // PPTX  →  parse with JSZip
    const reader = new FileReader();
    reader.onload = async function(e) {
      try {
        const zip        = await JSZip.loadAsync(e.target.result);
        const slideFiles = Object.keys(zip.files)
          .filter(n => /^ppt\/slides\/slide\d+\.xml$/.test(n))
          .sort((a, b) => {
            const na = parseInt(a.match(/\d+/) || 0);
            const nb = parseInt(b.match(/\d+/) || 0);
            return na - nb;
          });

        const disp = document.getElementById(displayId);
        if (!disp) return;
        disp.innerHTML = '';

        const grid = document.createElement('div');
        grid.style.cssText = 'display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));gap:.9rem;';

        for (let i = 0; i < slideFiles.length; i++) {
          const xml    = await zip.files[slideFiles[i]].async('string');
          const doc    = new DOMParser().parseFromString(xml, 'text/xml');
          const texts  = [...doc.querySelectorAll('t')].map(t => t.textContent.trim()).filter(Boolean);

          const card   = document.createElement('div');
          card.style.cssText = 'background:var(--bg3);border:1px solid var(--border);border-radius:8px;padding:.9rem;overflow:hidden;';
          card.innerHTML     =
            `<div style="font-size:.68rem;color:var(--teal);font-weight:700;margin-bottom:.4rem;text-transform:uppercase;letter-spacing:.07em">Slide ${i + 1}</div>` +
            texts.map((t, j) =>
              `<p style="font-size:${j === 0 ? '.86rem' : '.75rem'};color:${j === 0 ? 'var(--text)' : 'var(--text2)'};margin:.12rem 0;line-height:1.4">${esc(t)}</p>`
            ).join('');
          grid.appendChild(card);
        }

        disp.appendChild(grid);
        document.getElementById(statusId).textContent = `✅ Loaded: ${file.name} — ${slideFiles.length} slides`;
        showReader(areaEl, zoneEl);
      } catch(err) {
        document.getElementById(statusId).textContent = '❌ ' + err.message;
      }
    };
    reader.readAsArrayBuffer(file);
  }
}

/* ──────────────────────────────────────────────
   Screenshot / Image
────────────────────────────────────────────── */
function loadScreenshot(file, displayId, statusId, areaEl, zoneEl) {
  if (file.type === 'application/pdf') {
    loadPDF(file, displayId, statusId, areaEl, zoneEl);
    return;
  }
  const url  = URL.createObjectURL(file);
  const disp = document.getElementById(displayId);
  if (disp) {
    disp.innerHTML  = '';
    const img       = document.createElement('img');
    img.src         = url;
    img.alt         = file.name;
    img.style.cssText = 'width:100%;border-radius:6px;display:block;';
    disp.appendChild(img);
  }
  document.getElementById(statusId).textContent = `✅ Loaded: ${file.name}`;
  showReader(areaEl, zoneEl);
}

/* ══════════════════════════════════════════════
   CLEAR READER
══════════════════════════════════════════════ */
function clearReader(areaId, zoneId, statusId, displayId) {
  const area   = document.getElementById(areaId);
  const zone   = document.getElementById(zoneId);
  const status = document.getElementById(statusId);
  const disp   = document.getElementById(displayId);

  if (area)   area.classList.add('hidden');
  if (zone)   zone.style.display = '';
  if (status) status.textContent = 'No file loaded';
  if (disp)   { disp.innerHTML = ''; if (disp.tagName === 'IFRAME') disp.src = ''; }
}

/* ══════════════════════════════════════════════
   URL EMBED
══════════════════════════════════════════════ */
function embedURL(inputId, frameId, areaId) {
  const url = (document.getElementById(inputId) || {}).value && document.getElementById(inputId).value.trim();
  if (!url) return;
  const frame = document.getElementById(frameId);
  if (frame) frame.src = url;
  const area = document.getElementById(areaId);
  if (area) area.classList.remove('hidden');
}

function clearEmbed(areaId, frameId, inputId) {
  const area  = document.getElementById(areaId);
  const frame = document.getElementById(frameId);
  const input = document.getElementById(inputId);
  if (area)  area.classList.add('hidden');
  if (frame) frame.src = '';
  if (input) input.value = '';
}


function loadEmbeddedProject(key) {
  const input = document.getElementById(`${key}-url`);
  const rawUrl = input && input.value ? input.value.trim() : '';
  if (!rawUrl) {
    setEmbeddedStatus(key, 'Paste a project link first.');
    return;
  }
  showEmbeddedFrame(key, normalizeEmbeddedUrl(rawUrl), 'Loaded link preview.');
}

function previewEmbeddedProjectFile(input, key) {
  const file = input.files && input.files[0];
  if (!file) return;

  const ext = getFileExt(file.name);
  setEmbeddedStatus(key, `Loading ${file.name}...`);
  clearEmbeddedProject(key, false);

  if (file.type === 'application/pdf' || ext === 'pdf' || ext === 'html' || ext === 'htm') {
    showEmbeddedFrame(key, URL.createObjectURL(file), `Loaded: ${file.name}`);
  } else if (file.type.startsWith('image/') || ['png','jpg','jpeg','webp','gif'].includes(ext)) {
    showEmbeddedImage(key, URL.createObjectURL(file), file.name);
  } else if (['xlsx','xlsm','xls','csv'].includes(ext)) {
    renderEmbeddedWorkbook(file, key);
  } else if (ext === 'docx') {
    renderEmbeddedWord(file, key);
  } else if (ext === 'pptx') {
    renderEmbeddedSlides(file, key);
  } else {
    setEmbeddedStatus(key, 'This file type is not supported for embedded preview.');
  }

  input.value = '';
}

function clearEmbeddedProject(key, resetInput = true) {
  const viewer = document.getElementById(`${key}-viewer`);
  const placeholder = document.getElementById(`${key}-placeholder`);
  const frame = document.getElementById(`${key}-frame`);
  const doc = document.getElementById(`${key}-doc`);
  const img = document.getElementById(`${key}-img`);
  const input = document.getElementById(`${key}-url`);

  if (viewer) viewer.classList.remove('is-loaded');
  if (placeholder) placeholder.classList.remove('hidden');
  if (frame) {
    frame.src = '';
    frame.classList.add('hidden');
  }
  if (doc) {
    doc.innerHTML = '';
    doc.classList.add('hidden');
  }
  if (img) {
    img.innerHTML = '';
    img.classList.add('hidden');
  }
  if (resetInput && input) input.value = '';
  setEmbeddedStatus(key, 'Ready for a link or upload.');
}

function showEmbeddedFrame(key, src, message) {
  const viewer = document.getElementById(`${key}-viewer`);
  const placeholder = document.getElementById(`${key}-placeholder`);
  const frame = document.getElementById(`${key}-frame`);
  const doc = document.getElementById(`${key}-doc`);
  const img = document.getElementById(`${key}-img`);

  if (!frame) return;
  if (placeholder) placeholder.classList.add('hidden');
  if (doc) {
    doc.innerHTML = '';
    doc.classList.add('hidden');
  }
  if (img) {
    img.innerHTML = '';
    img.classList.add('hidden');
  }
  frame.src = src;
  frame.classList.remove('hidden');
  if (viewer) viewer.classList.add('is-loaded');
  setEmbeddedStatus(key, message || 'Loaded project preview.');
}

function showEmbeddedImage(key, src, fileName) {
  const viewer = document.getElementById(`${key}-viewer`);
  const placeholder = document.getElementById(`${key}-placeholder`);
  const frame = document.getElementById(`${key}-frame`);
  const doc = document.getElementById(`${key}-doc`);
  const imgWrap = document.getElementById(`${key}-img`);

  if (!imgWrap) return;
  if (placeholder) placeholder.classList.add('hidden');
  if (frame) {
    frame.src = '';
    frame.classList.add('hidden');
  }
  if (doc) {
    doc.innerHTML = '';
    doc.classList.add('hidden');
  }

  imgWrap.innerHTML = '';
  const img = document.createElement('img');
  img.src = src;
  img.alt = fileName || 'Embedded project preview';
  imgWrap.appendChild(img);
  imgWrap.classList.remove('hidden');
  if (viewer) viewer.classList.add('is-loaded');
  setEmbeddedStatus(key, `Loaded: ${fileName}`);
}

function showEmbeddedDocument(key, contentNode, message) {
  const viewer = document.getElementById(`${key}-viewer`);
  const placeholder = document.getElementById(`${key}-placeholder`);
  const frame = document.getElementById(`${key}-frame`);
  const doc = document.getElementById(`${key}-doc`);
  const img = document.getElementById(`${key}-img`);

  if (!doc) return;
  if (placeholder) placeholder.classList.add('hidden');
  if (frame) {
    frame.src = '';
    frame.classList.add('hidden');
  }
  if (img) {
    img.innerHTML = '';
    img.classList.add('hidden');
  }

  doc.innerHTML = '';
  if (typeof contentNode === 'string') {
    doc.innerHTML = contentNode;
  } else {
    doc.appendChild(contentNode);
  }
  doc.classList.remove('hidden');
  if (viewer) viewer.classList.add('is-loaded');
  setEmbeddedStatus(key, message);
}

function renderEmbeddedWorkbook(file, key) {
  const reader = new FileReader();
  reader.onload = function(e) {
    try {
      const workbook = XLSX.read(e.target.result, { type: 'array' });
      const wrap = document.createElement('div');
      const tabs = document.createElement('div');
      const tableWrap = document.createElement('div');
      const note = document.createElement('p');
      tabs.className = 'ip-sheet-tabs';
      tableWrap.className = 'ip-table-scroll';
      note.className = 'ip-table-note';

      function renderSheet(sheetName) {
        const ws = workbook.Sheets[sheetName];
        const rows = XLSX.utils.sheet_to_json(ws, { header: 1, defval: '' });
        tableWrap.innerHTML = '';
        if (!rows.length) {
          tableWrap.innerHTML = '<div class="ip-empty-doc">Empty sheet</div>';
          note.textContent = '';
          return;
        }

        const maxRows = 250;
        const visibleRows = rows.slice(0, maxRows);
        const table = document.createElement('table');
        const tbody = document.createElement('tbody');
        visibleRows.forEach((row, rowIndex) => {
          const tr = document.createElement('tr');
          row.forEach(cell => {
            const cellEl = document.createElement(rowIndex === 0 ? 'th' : 'td');
            cellEl.textContent = cell;
            tr.appendChild(cellEl);
          });
          tbody.appendChild(tr);
        });
        table.appendChild(tbody);
        tableWrap.appendChild(table);
        note.textContent = rows.length > maxRows ? `Showing first ${maxRows} rows of ${rows.length}.` : `${rows.length} rows shown.`;
      }

      workbook.SheetNames.forEach((sheetName, index) => {
        const btn = document.createElement('button');
        btn.type = 'button';
        btn.className = 'sheet-tab-btn' + (index === 0 ? ' active' : '');
        btn.textContent = sheetName;
        btn.addEventListener('click', function() {
          tabs.querySelectorAll('.sheet-tab-btn').forEach(tab => tab.classList.remove('active'));
          btn.classList.add('active');
          renderSheet(sheetName);
        });
        tabs.appendChild(btn);
      });

      wrap.appendChild(tabs);
      wrap.appendChild(tableWrap);
      wrap.appendChild(note);
      renderSheet(workbook.SheetNames[0]);
      showEmbeddedDocument(key, wrap, `Loaded workbook: ${file.name}`);
    } catch (err) {
      setEmbeddedStatus(key, 'Could not preview workbook: ' + err.message);
    }
  };
  reader.readAsArrayBuffer(file);
}

function renderEmbeddedWord(file, key) {
  if (!window.mammoth) {
    setEmbeddedStatus(key, 'Word preview library is not available.');
    return;
  }
  const reader = new FileReader();
  reader.onload = function(e) {
    mammoth.convertToHtml({ arrayBuffer: e.target.result })
      .then(result => {
        showEmbeddedDocument(key, result.value || '<div class="ip-empty-doc">No document text found.</div>', `Loaded Word document: ${file.name}`);
      })
      .catch(err => setEmbeddedStatus(key, 'Could not preview Word document: ' + err.message));
  };
  reader.readAsArrayBuffer(file);
}

function renderEmbeddedSlides(file, key) {
  if (!window.JSZip) {
    setEmbeddedStatus(key, 'PPTX preview library is not available.');
    return;
  }
  const reader = new FileReader();
  reader.onload = async function(e) {
    try {
      const zip = await JSZip.loadAsync(e.target.result);
      const slideFiles = Object.keys(zip.files)
        .filter(name => /^ppt\/slides\/slide\d+\.xml$/.test(name))
        .sort((a, b) => parseInt(a.match(/\d+/) || 0) - parseInt(b.match(/\d+/) || 0));

      const grid = document.createElement('div');
      grid.className = 'ip-slide-grid';
      for (let i = 0; i < slideFiles.length; i++) {
        const xml = await zip.files[slideFiles[i]].async('string');
        const parsed = new DOMParser().parseFromString(xml, 'text/xml');
        const texts = [...parsed.querySelectorAll('t')].map(t => t.textContent.trim()).filter(Boolean);
        const card = document.createElement('div');
        card.className = 'ip-slide-card';
        const label = document.createElement('span');
        label.className = 'ip-slide-label';
        label.textContent = `Slide ${i + 1}`;
        card.appendChild(label);
        if (texts.length) {
          texts.forEach((text, textIndex) => {
            const p = document.createElement('p');
            p.textContent = text;
            if (textIndex === 0) p.style.color = 'var(--text)';
            card.appendChild(p);
          });
        } else {
          const p = document.createElement('p');
          p.textContent = 'No readable text found on this slide.';
          card.appendChild(p);
        }
        grid.appendChild(card);
      }

      showEmbeddedDocument(key, grid, `Loaded presentation: ${file.name}`);
    } catch (err) {
      setEmbeddedStatus(key, 'Could not preview presentation: ' + err.message);
    }
  };
  reader.readAsArrayBuffer(file);
}

function normalizeEmbeddedUrl(rawUrl) {
  let url = rawUrl.trim();
  if (/^www\./i.test(url)) url = 'https://' + url;
  if (!/^(https?:|blob:|data:|file:)/i.test(url)) url = 'https://' + url;

  try {
    const parsed = new URL(url);
    if (parsed.hostname.includes('drive.google.com')) {
      const fileMatch = parsed.pathname.match(/\/file\/d\/([^/]+)/);
      const id = fileMatch && fileMatch[1] ? fileMatch[1] : parsed.searchParams.get('id');
      if (id) return `https://drive.google.com/file/d/${id}/preview`;
    }
    if (parsed.hostname.includes('docs.google.com')) {
      return parsed.href.replace(/\/edit.*$/i, '/preview').replace(/\/view.*$/i, '/preview');
    }
    return parsed.href;
  } catch (err) {
    return rawUrl;
  }
}

function setEmbeddedStatus(key, message) {
  const status = document.getElementById(`${key}-status`);
  if (status) status.textContent = message;
}

function getFileExt(fileName) {
  return (fileName.split('.').pop() || '').toLowerCase();
}

function resolveAssetSrc(src) {
  const value = String(src || '').trim();
  if (!value) return '';
  if (/^(?:[a-z][a-z0-9+.-]*:|\/\/)/i.test(value)) return value;
  return encodeURI(value);
}


document.addEventListener('DOMContentLoaded', buildSpiritualMaterialsSection);

/* ══════════════════════════════════════════════
   COPY CODE
══════════════════════════════════════════════ */
function copyCode(codeId) {
  const el = document.getElementById(codeId);
  if (!el) return;
  navigator.clipboard.writeText(el.textContent || el.innerText).then(function() {
    // find any copy button targeting this id and flash it
    const btn = document.querySelector(`[onclick*="${codeId}"].copy-btn`) ||
                document.querySelector(`button[onclick*="${codeId}"]`);
    if (btn) {
      const orig    = btn.textContent;
      btn.textContent = '✅ Copied!';
      setTimeout(() => { btn.textContent = orig; }, 1500);
    }
  }).catch(function() {
    // fallback
    const range = document.createRange();
    range.selectNode(el);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
    document.execCommand('copy');
    window.getSelection().removeAllRanges();
  });
}

/* ══════════════════════════════════════════════
   GALLERY
══════════════════════════════════════════════ */
function addGalleryFiles(input, galleryId) {
  const files   = Array.from(input.files || []);
  const gallery = document.getElementById(galleryId);
  if (!gallery) return;
  const empty = gallery.querySelector('.gallery-empty');
  if (empty) empty.remove();
  files.forEach(f => addGalleryItem(f, gallery));
}

function dropGalleryFiles(e, galleryId) {
  e.preventDefault();
  e.currentTarget.classList.remove('drag-over');
  const files   = Array.from((e.dataTransfer && e.dataTransfer.files) || []);
  const gallery = document.getElementById(galleryId);
  if (!gallery) return;
  const empty = gallery.querySelector('.gallery-empty');
  if (empty) empty.remove();
  files.forEach(f => addGalleryItem(f, gallery));
}

function addGalleryItem(file, gallery) {
  const item        = document.createElement('div');
  item.className    = 'gallery-item';

  if (file.type === 'application/pdf') {
    item.innerHTML  = `
      <div style="width:100%;height:100%;display:flex;flex-direction:column;
                  align-items:center;justify-content:center;background:var(--bg3);gap:.4rem">
        <span style="font-size:2rem">📄</span>
        <span style="font-size:.7rem;color:var(--text2)">PDF</span>
      </div>`;
  } else {
    const img       = document.createElement('img');
    img.src         = URL.createObjectURL(file);
    img.alt         = file.name;
    item.appendChild(img);
  }

  const label       = document.createElement('div');
  label.className   = 'gallery-item-label';
  label.textContent = file.name;
  item.appendChild(label);

  const del         = document.createElement('div');
  del.className     = 'gallery-item-del';
  del.textContent   = '×';
  del.title         = 'Remove';
  del.onclick       = () => item.remove();
  item.appendChild(del);

  gallery.appendChild(item);
}

/* ══════════════════════════════════════════════
   UTILITY: build a plain <table> from cols + rows array
══════════════════════════════════════════════ */
function buildTable(cols, rows) {
  const table   = document.createElement('table');
  const thead   = document.createElement('thead');
  const htr     = document.createElement('tr');
  cols.forEach(c => { const th = document.createElement('th'); th.textContent = c; htr.appendChild(th); });
  thead.appendChild(htr);
  table.appendChild(thead);
  const tbody   = document.createElement('tbody');
  rows.forEach(row => {
    const tr = document.createElement('tr');
    cols.forEach(c => { const td = document.createElement('td'); td.textContent = row[c]; tr.appendChild(td); });
    tbody.appendChild(tr);
  });
  table.appendChild(tbody);
  return table;
}

function esc(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

/* ══════════════════════════════════════════════
   PARTICLE CANVAS
══════════════════════════════════════════════ */
(function initParticles() {
  const canvas = document.getElementById('particle-canvas');
  if (!canvas) return;
  const ctx    = canvas.getContext('2d');
  let W, H, particles = [];
  const TEAL   = '20,184,166';
  const AMBER  = '245,158,11';
  const COUNT  = 70;
  const LINK   = 110;

  function resize() {
    W = canvas.width  = canvas.offsetWidth;
    H = canvas.height = canvas.offsetHeight;
  }

  function Particle() {
    this.reset = () => {
      this.x     = Math.random() * W;
      this.y     = Math.random() * H;
      this.r     = Math.random() * 1.8 + .5;
      this.vx    = (Math.random() - .5) * .38;
      this.vy    = (Math.random() - .5) * .38;
      this.alpha = Math.random() * .45 + .1;
      this.col   = Math.random() > .72 ? AMBER : TEAL;
    };
    this.reset();
    this.update = () => {
      this.x += this.vx;
      this.y += this.vy;
      if (this.x < 0 || this.x > W || this.y < 0 || this.y > H) this.reset();
    };
    this.draw = () => {
      ctx.beginPath();
      ctx.arc(this.x, this.y, this.r, 0, Math.PI * 2);
      ctx.fillStyle = `rgba(${this.col},${this.alpha})`;
      ctx.fill();
    };
  }

  function init() {
    resize();
    particles = Array.from({ length: COUNT }, () => new Particle());
    loop();
  }

  function loop() {
    requestAnimationFrame(loop);
    ctx.clearRect(0, 0, W, H);
    particles.forEach(p => { p.update(); p.draw(); });
    // draw connecting lines
    for (let i = 0; i < COUNT; i++) {
      for (let j = i + 1; j < COUNT; j++) {
        const dx   = particles[i].x - particles[j].x;
        const dy   = particles[i].y - particles[j].y;
        const dist = Math.sqrt(dx * dx + dy * dy);
        if (dist < LINK) {
          ctx.beginPath();
          ctx.moveTo(particles[i].x, particles[i].y);
          ctx.lineTo(particles[j].x, particles[j].y);
          ctx.strokeStyle = `rgba(${TEAL},${.14 * (1 - dist / LINK)})`;
          ctx.lineWidth   = .6;
          ctx.stroke();
        }
      }
    }
  }

  window.addEventListener('resize', resize);
  // Wait until the landing section is sized
  setTimeout(init, 50);
})();


function buildSpiritualMaterialsSection() {
  const section = document.getElementById('Spiritual-Materials');
  if (!section || section.dataset.spiritBuilt === '1') return;

  // Initialize tab panels - hide all except the first (active) one
  const panels = section.querySelectorAll('.tab-panel');
  panels.forEach((panel, index) => {
    if (index === 0) {
      panel.classList.add('active');
      panel.removeAttribute('hidden');
    } else {
      panel.classList.remove('active');
      panel.setAttribute('hidden', '');
    }
  });

  // Mark as built to avoid re-initialization
  section.dataset.spiritBuilt = '1';
}


document.addEventListener('DOMContentLoaded', function () {
  buildSpiritualMaterialsSection();
  
  const tabBar = document.getElementById('spirit-tabs');
  if (!tabBar) return;

  const tabButtons = tabBar.querySelectorAll('.tab-btn');

  tabButtons.forEach(function (btn) {
    btn.addEventListener('click', function () {
      const group = btn.dataset.group;
      const target = btn.dataset.target;

      // Toggle active button state
      tabBar.querySelectorAll(`.tab-btn[data-group="${group}"]`)
        .forEach(function (b) { b.classList.remove('active'); });
      btn.classList.add('active');

      // Toggle panel visibility
      document.querySelectorAll(`.tab-panel[data-group="${group}"]`)
        .forEach(function (panel) {
          const isTarget = panel.id === target;
          panel.classList.toggle('active', isTarget);
          if (isTarget) {
            panel.removeAttribute('hidden');
          } else {
            panel.setAttribute('hidden', '');
          }
        });
    });
  });
});

