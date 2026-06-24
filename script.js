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

  document.querySelectorAll(`.tab-panel.${group}`)
    .forEach(p => p.classList.remove('active'));

  const btn = document.querySelector(
    `.tab-btn[data-group="${group}"][data-target="${target}"]`
  );

  const panel = document.getElementById(target);

  if (btn) btn.classList.add('active');
  if (panel) panel.classList.add('active');
}

function switchEmbedded(el) {
  // Find the tab bar that contains this button
  const tabBar = el.closest('.ip-tab-bar, .form-tab-bar, .kobo-tab-bar');
  if (!tabBar) return;

  // The container is the direct parent of the tab bar (reader-card)
  const container = tabBar.parentElement;
  if (!container) return;

  const target = el.dataset.embedTarget;

  // Deactivate all sibling tabs inside this tab bar only
  tabBar.querySelectorAll('.ip-tab, .form-tab, .kobo-tab')
    .forEach(t => t.classList.remove('active'));

  // Deactivate all panels inside this container
  container.querySelectorAll('.ip-panel, .form-panel, .kobo-panel')
    .forEach(p => p.classList.remove('active'));

  // Activate clicked tab
  el.classList.add('active');

  // Activate target panel (scoped to this container)
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

 const R_SCRIPT_SNIPPET1 =`
# app.R
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape2)
library(shinyWidgets)
library(openxlsx)
library(fmsb)
library(ggcorrplot)
library(vcd)
library(ggplot2)
library(plotly)
library(randomForest)
library(rpart.plot)
library(partykit)
library(pROC)
library(patchwork)
library(PRROC)
# Load dataset
Mental_data <- read_excel("C:/Users/mauri/OneDrive/Desktop/Projects/R Projects/mental_health_risk_dataset.xlsx")

df <- Mental_data %>%
  mutate(
    social_support_cat = cut(social_support_score,
                             breaks = c(-Inf, 2, 4, 6, 8, Inf),
                             labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                             ordered_result = TRUE),
    
    work_stress_cat = cut(work_stress_level,
                          breaks = c(-Inf, 2, 4, 6, 8, Inf),
                          labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                          ordered_result = TRUE),
    
    academic_pressure_cat = cut(academic_pressure_level,
                                breaks = c(-Inf, 2, 4, 6, 8, Inf),
                                labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                                ordered_result = TRUE),
    
    job_satisfaction_cat = cut(job_satisfaction_score,
                               breaks = c(-Inf, 2, 4, 6, 8, Inf),
                               labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                               ordered_result = TRUE),
    
    financial_stress_cat = cut(financial_stress_level,
                               breaks = c(-Inf, 2, 4, 6, 8, Inf),
                               labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                               ordered_result = TRUE),
    
    depression_cat = cut(depression_score,
                         breaks = c(-Inf, 2, 4, 6, 8, Inf),
                         labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                         ordered_result = TRUE),
    
    stress_cat = cut(stress_level,
                     breaks = c(-Inf, 2, 4, 6, 8, Inf),
                     labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                     ordered_result = TRUE),
    
    anxiety_cat = cut(anxiety_score,
                      breaks = c(-Inf, 2, 4, 6, 8, Inf),
                      labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                      ordered_result = TRUE),
    
    mood_swings_cat = cut(mood_swings_frequency,
                          breaks = c(-Inf, 2, 4, 6, 8, Inf),
                          labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                          ordered_result = TRUE),
    
    concentration_cat = cut(concentration_difficulty_level,
                            breaks = c(-Inf, 2, 4, 6, 8, Inf),
                            labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                            ordered_result = TRUE),
    
    mental_risk_cat = factor(mental_health_risk,
                             levels = c(0, 1, 2),
                             labels = c("Low", "Moderate", "High"),
                             ordered = TRUE),
    
    # Binary variables remain Yes/No
    panic_attack_bin = ifelse(panic_attack_history == 1, "Yes", "No"),
    family_history_bin = ifelse(family_history_mental_illness == 1, "Yes", "No"),
    prev_diagnosis_bin = ifelse(previous_mental_health_diagnosis == 1, "Yes", "No"),
    therapy_bin = ifelse(therapy_history == 1, "Yes", "No"),
    substance_use_bin = ifelse(substance_use == 1, "Substance use", "No substance use")
  )
str(df)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title ="Predictive Analytics for Mental Health Risk",titleWidth = 410,
    # Place filters below the tab menu
    # Filters grouped inside dropdown list items
    tags$li(class = "dropdown",
            tags$div(
              style = "display:flex; flex-wrap:wrap; gap:5px; align-items:center; margin-left:10px;",
              
              selectInput("gender", "Gender",
                          choices = c("All", unique(df$gender)),
                          selected = "All", multiple = TRUE, width = "150px"),
              
              selectInput("marital", "Marital Status",
                          choices = c("All", unique(df$marital_status)),
                          selected = "All", multiple = TRUE, width = "150px"),
              
              selectInput("education", "Education Level",
                          choices = c("All", unique(df$education_level)),
                          selected = "All", multiple = TRUE, width = "150px"),
              
              selectInput("employment", "Employment Status",
                          choices = c("All", unique(df$employment_status)),
                          selected = "All", multiple = TRUE, width = "150px")
            )
    ),
    
    # Reset icon button with tooltip
    tags$li(class = "dropdown",
            actionButton("resetFilters", label = NULL, icon = icon("sync"),
                         class = "header-btn", title = "Reset Filters")),
    
    # Download data (single download arrow icon)
    tags$li(class = "dropdown",
            downloadButton("downloadData",
                           label = HTML("<i class='fas fa-file-excel'></i>"),
                           class = "btn-unified excel-btn",
                           title = "Download Filtered Data")),
    
    # Trigger button (file icon + download arrow)
    tags$li(class = "dropdown",
            actionButton("openDownload",
                         label = HTML("<i class='fas fa-file'></i> <i class='fas fa-download'></i>"),
                         class = "header-btn file-btn",
                         title = "Download Dashboard Report"))
    
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Summary", tabName = "summary", icon = icon("chart-pie")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Lifestyle", tabName = "lifestyle", icon = icon("heartbeat")),
      menuItem("Work/Academic Stress", tabName = "stress", icon = icon("briefcase")),
      menuItem("Psychological Indicators", tabName = "psych", icon = icon("brain")),
      menuItem("Medical/Family History", tabName = "medical", icon = icon("notes-medical")),
      menuItem("Correlation & Association", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Modeling", tabName = "modeling", icon = icon("robot")),
      menuItem("Modeling 2", tabName="modeling2", icon=icon("robot")),
      menuItem("Raw Data", tabName = "rawdata", icon = icon("table"))
    )
    
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
   /* ================= HEADER ================= */
/* Increase header height */
    .main-header .navbar {
      min-height: 25px;   /* default is ~50px */
    }

.main-header {
  background-color: #3c8dbc !important; /* unified header color */
}

.main-header .logo {
  background-color: #3c8dbc !important; /* same as navbar */
  color: #fff !important;
  font-size: 18px !important;
  font-weight: bold !important;
  text-align: left !important;
  height: 50px !important;
  line-height: 50px !important;
}

.main-header .navbar {
  background-color: #3c8dbc !important; /* same as logo */
  min-height: 40px !important;          /* compact height */
}

.main-header .sidebar-toggle {
  color: #fff !important;               /* toggle button visible */
  height: 40px !important;
  line-height: 40px !important;
  margin: 0 !important;                 /* remove spacing */
  padding: 0 10px !important;
  color: #fff !important;
}
.navbar .form-group label {
  color: #ffffff !important;
  font-weight: bold !important;
  font-size: 12px !important;
  margin-bottom: 0px !important; /* reduce filter margin */
}

  /* Base style for all header buttons */
.header-btn {
  color: white !important;
  border: none !important;
  height: 36px !important;
  width: auto !important;
  padding: 6px 12px !important;
  margin-top: 6px !important;
  margin-right: 6px !important;
  border-radius: 4px;
  font-size: 14px;
  text-align: center;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  margin-left: 10px !important;
}

/* ================= SIDEBAR ================= */
.skin-blue .main-sidebar {
  background-color: #2c3e50 !important; /* dark blue-gray */
}
.sidebar-menu {
  margin-top: 20px !important;
}
.skin-blue .main-sidebar .sidebar .sidebar-menu > li.active > a {
  background-color: #FFD700 !important; /* gold highlight */
  color: #2C3E50 !important;
  font-weight: bold;
}
.skin-blue .main-sidebar .sidebar .sidebar-menu > li > a:hover {
  background-color: #FFE066 !important; /* lighter gold hover */
  color: #2C3E50 !important;
}

/* ================= BOXES ================= */
.box {
  border-radius: 8px !important;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  margin-bottom: 8px !important;
  padding: 2px !important;
  font-family: Arial, sans-serif;
}

/* ================= INFOBOXES ================= */
.info-box {
  display: flex !important;
  align-items: stretch !important;   /* make icon and content equal height */
  min-height: 55px !important;   /* compact height */
  margin-bottom: 5px !important;
  border-radius: 6px !important;
  padding: 1px !important;
  font-family: Arial, sans-serif;
  box-shadow: none !important;
}
.info-box-icon {
  width: 45px !important;
  height: auto !important;           /* stretch to full height of infoBox */
  line-height: 55px !important;
  font-size: 18px !important;
  margin-right: 0px !important;
  margin-left: 0px !important;
  border-radius: 6px !important;
  color: #fff !important;
  margin: 0 !important;              /* remove margins */
  padding: 0 !important;             /* remove padding */
  width: 60px !important;            /* fixed width */
  font-size: 22px !important;
  display: flex !important;
  align-items: center !important;    /* center icon vertically */
  justify-content: center !important;/* center icon horizontally */
  
}
.info-box-content {
  margin-left: 10px !important;
  padding: 0px !important;
}
.info-box-text {
  font-size: 13px !important;
  font-weight: bold !important;
  margin-bottom: 2px !important;
  text-transform: capitalize;   /* Capitalize each word */
        font-size: 14px;
        text-align: left;
}
.info-box-number {
  font-size: 13px !important;
  line-height: 1.3 !important;
  text-transform: capitalize;   /* Capitalize values if text */
        font-size: 23px;
        text-align: left;
}

/* Color-coded variants */
.info-box.low-risk {
  background-color: #28a745 !important; /* green */
  color: #fff !important;
}
.info-box.moderate-risk {
  background-color: #0073e6 !important; /* blue */
  color: #fff !important;
}
.info-box.high-risk {
  background-color: #dc3545 !important; /* red */
  color: #fff !important;
}
.info-box-icon {
      color: #000080 !important;   /* bold navy icon */
    }
    .info-box {
      background-color: rgba(0, 0, 128, 0.6) !important; /* softer fill */
    }

/* ================= DROPDOWNS ================= */
.navbar .form-group label {
  margin-bottom: 0px !important; /* tighter label spacing */
  font-size: 11px !important;   /* smaller font */
  font-weight: white;          /* optional: remove bold */
  margin-bottom: 2px !important; /* keep label close to box */
}
.dropdown .form-control {
        height: 10px;
        font-size: 8px;
        padding: 0px 0px;
      }
      .dropdown {
        display: inline-block;
        margin-right: 0px;
      }
      
      .navbar .dropdown {
    display: inline-block;
    text-align: center;
    vertical-align: middle;
  }
.selectize-dropdown {
  background-color: #2c3e50 !important;
  color: #ecf0f1 !important;
  border: 1px solid #34495e !important;
}
.selectize-dropdown .option {
  color: #ecf0f1 !important;
  padding: 6px 10px;
}
.selectize-dropdown .option:hover {
  background-color: #1abc9c !important; /* teal highlight */
  color: #fff !important;
}
.selectize-input .item {
  color: #1abc9c !important;
  border-radius: 3px;
}
 Prevent filters from stacking vertically */
.navbar .form-group {
  margin-bottom: 0 !important;
}
 /* Reduce space between label and dropdown box in header */
.navbar .form-group {
  margin-top: 0 !important;
  margin-bottom: 0 !important;
}

.selectize-input {
  display: flex !important;
  flex-wrap: nowrap !important;
  overflow-x: auto !important;
  white-space: nowrap !important;
}

.selectize-input > div {
  margin-right: 4px;
}
 /* Keep filter inputs aligned and scroll horizontally */
.filter-input .selectize-input {
  max-height: 36px;          /* fixed height */
  overflow-x: auto;          /* horizontal scroll */
  overflow-y: hidden;        /* prevent vertical expansion */
  white-space: nowrap;       /* keep chips on one line */
  display: flex;
  align-items: center;
}

/* Ensure dropdown stays usable */
.filter-input .selectize-dropdown {
  max-height: 200px;         /* scroll inside dropdown list */
  overflow-y: auto;
}
/* Reduce font size of dropdown choices */
.selectize-dropdown .selectize-dropdown-content .option {
  font-size: 11px !important;   /* smaller text for options */
}

/* Reduce font size of selected items inside the box */
.selectize-input .item {
  font-size: 11px !important;
}
/* Slim horizontal scrollbar for selectize inputs */
.selectize-input::-webkit-scrollbar {
  height: 4px;   /* reduce scrollbar thickness */
}

.selectize-input::-webkit-scrollbar-track {
  background: #f1f1f1;   /* optional: light background */
}
.selectize-input::-webkit-scrollbar-thumb {
  background: #888;      /* scrollbar color */
  border-radius: 3px;    /* rounded edges */
}

.selectize-input::-webkit-scrollbar-thumb:hover {
  background: #555;      /* darker on hover */
}

/* ================= DOWNLOAD & RESET BUTTONS ================= */
  
    #resetFilters {
    background-color: #0073e6 !important;
  }
  #downloadData {
    background-color: #28a745 !important;
  }
  #openDownload {
    background-color: #dc3545 !important;
  }

.btn-unified {
  color: #fff !important;
  border: none !important;
  height: 36px !important;
  padding: 6px 12px !important;
  margin: 4px !important;
  border-radius: 4px;
  font-size: 14px;
  text-align: center;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}
.btn-unified i {
  margin-right: 4px;
}

/* Specific colors */
.excel-btn   { background-color: #28a745 !important; }  /* green */
.pdf-btn     { background-color: #dc3545 !important; }  /* red */
.html-btn    { background-color: #0073e6 !important; }  /* blue */
.word-btn    { background-color: #6c63ff !important; }  /* purple */
.cancel-btn  { background-color: #6c757d !important; }  /* gray */
.reset-btn   { background-color: #0073e6 !important; }  /* blue reset */
.download-btn{ background-color: #28a745 !important; }  /* green download */

  table {
      border-collapse: collapse;
      width: 100%;
      background-color: #f9f9ff;
    }
    table th, table td {
      border: 1px solid #0073e6;
      padding: 6px;
      text-align: center;
    }
    table th {
      background-color: #0073e6;
      color: white;
      font-weight: bold;
    }
    table td:first-child {
      font-weight: bold;   /* makes row names bold */
      color: #003366;      /* optional: darker blue for emphasis */
      text-align: center;
    }
    table tr:nth-child(even) td {
      background-color: #e6f2ff;
    }
    table tr:hover td {
      background-color: #cce6ff;
    }



    "))),
    
    tabItems(
      # Global Summary Tab
      tabItem(tabName = "summary",
              # UI layout
              fluidRow(
                infoBoxOutput("riskSummary", width = 4),
                infoBoxOutput("totalPopulation", width = 2),
                infoBoxOutput("avgAge", width = 2),
                infoBoxOutput("modelRiskScore", width = 4)
                
              ),
              fluidRow(
                box(
                  title = HTML("<span style='font-weight:bold; font-size:14px;'>Depression, Anxiety, and Work Stress Intensify as Risk Rises.</span>"),
                  status = "primary",width = 7,height = "280px",
                  plotlyOutput("allVarsRadar", height = "200px")),
                box(title=HTML("<span style='font-weight:bold; font-size:14px;'>Mental Illness History and Stress Stand Out as Key Risk Drivers.</span>"),
                    width=5, status="success",height = "280px",
                    plotOutput("topFeaturesComparison", height="200px"))
                
              ),
              fluidRow(
                box(title=HTML("<span style='font-weight:bold; font-size:14px;'>Random Forest Delivers Sharper Risk Classification.</span>"),
                    width=6, status="info", height = "260px",
                    plotlyOutput("confMatrixPlot", height="200px")),
                box(title=HTML("<span style='font-weight:bold; font-size:14px;'>Random forest leads in accuracy; both models provide actionable insights.</span>"),
                    width=6, status="warning",height = "260px",
                    plotOutput("modelComparisonPlot", height="180px")),
              )
      ),
      
      # Demographics
      tabItem(tabName = "demographics",
              # Top row: Key metrics in infoBoxes
              fluidRow(
                infoBoxOutput("educationProfile", width = 6),
                infoBoxOutput("employmentOverview", width = 6),
                infoBoxOutput("genderDist", width = 6),
                infoBoxOutput("maritalDist", width = 6)
              ),
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'> Across All Genders, Midlife Brings a Surge in Mental Risk.</span>"),
                    width = 7, status = "primary", height = "240px",
                    plotlyOutput("agePlot", height = "185px")),
                
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Singles Carry a Slightly Heavier High‑Risk Burden.</span>"),
                    width = 5, status = "info", height = "240px",
                    plotlyOutput("maritalPlot", height = "185px"))
              ),
              
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Higher Studies Don’t Eliminate Risk;PhDs Show the Sharpest High‑Risk Share.</span>"),
                    width = 6, status = "warning", height = "240px",
                    plotlyOutput("educationPlot", height = "165px")),
                
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Unemployment Adds Pressure: Slightly Higher High‑Risk Levels.</span>"),
                    width = 6, status = "success", height = "240px",
                    plotlyOutput("employmentPlot", height = "180px"))
              )
      ),
      # Lifestyle Tab
      tabItem(tabName = "lifestyle",
              fluidRow(
                infoBoxOutput("sleepRiskBox", width = 4),
                infoBoxOutput("activityRiskBox", width = 4),
                infoBoxOutput("screenRiskBox", width = 4)
              ),
              
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Substance Use Tilts the Balance of Mental Risk.</span>"),
                    width = 6, status = "primary",height = "270px",
                    plotlyOutput("substanceBarPlot", height = "210px")),
                
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Strong Support Shields Against Rising Mental Risk.</span>"),
                    width = 6, status = "info",height = "270px",
                    plotlyOutput("supportBarPlot", height = "215px"))
              ),
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Sleep, Activity, and Screen Time Shape Mental Well‑Being.</span>"),
                    width = 7, status = "warning",height = "270px",
                    plotlyOutput("linePlot", height = "200px")),
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Half of the Population Faces Substance Use Challenges.</span>"),
                    width = 5, status = "success", height = "270px",
                    plotlyOutput("substancePiePlot", height = "200px"))
              )
      ),
      # Work/Academic Stress Tab
      tabItem(tabName = "stress",
              fluidRow(
                infoBoxOutput("workStressBox", width = 6),
                infoBoxOutput("academicPressureBox", width = 6)
              ),
              
              fluidRow(
                box(title =  HTML("<span style='font-weight:bold; font-size:14px;'>High Stress, Low Satisfaction: The Risk Spiral.</span>"),
                    width = 6,height = "265px", status="info", plotlyOutput("workAcademicRadar", height = "200px")),
                box(title =  HTML("<span style='font-weight:bold; font-size:14px;'>Rising Work Stress Levels Drive Mental Risk Higher.</span>"),
                    width = 6,height = "265px",status = "primary", plotlyOutput("workStressCatRisk",height = "200px"))
              ),
              
              fluidRow(
                box(title =  HTML("<span style='font-weight:bold; font-size:14px;'>When Job Satisfaction Falls, Mental Risk Rises.</span>"),
                    width = 6, height = "265px", status = "success", plotlyOutput("jobSatisfactionCatRisk",height = "200px")),
                box(title =  HTML("<span style='font-weight:bold; font-size:14px;'>Longer Working Hours, Greater Mental Strain.</span>"),
                    width = 6,height = "265px", status = "warning", plotlyOutput("workingHoursLine",height = "200px"))
              )
              
      ),
      
      # Psychological Indicators Tab
      tabItem(tabName = "psych",
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>High risk amplifies every psychological strain, with anxiety and stress peaking most sharply.</span>"),
                    width = 6,height = "295px", status="primary", plotlyOutput("psychCompositeRadar",height = "220px")),
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Severe Anxiety Triggers Sharp Risk Increase.</span>"),
                    width = 6,height = "295px", status="success", plotlyOutput("anxietyBar",height = "235px")),
                
              ),
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Stress Alone Doesn’t Shift Risk Levels.</span>"),
                    width = 6,height = "295px", status="primary", plotlyOutput("stressBar",height = "235px")),
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Deepening Depression Drives Risk Upward.</span>"),
                    width = 6,height = "295px", status="info", plotlyOutput("depressionColumn",height = "235px"))
              )
      ),
      
      # Medical/Family History Tab
      tabItem(tabName = "medical",
              
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Mental Illness History Doubles the Risk Burden.</span>"),
                    width = 5, height = "295px", status="primary", plotlyOutput("familyHistoryBar",height = "230px")),
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Diagnosis History Shows Risk Stability.</span>"),
                    width = 7,height = "295px", status="info", plotlyOutput("prevDiagnosisColumn",height = "230px"))
              ),
              fluidRow(
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Therapy Alone Doesn’t Alter Risk Patterns.</span>"), 
                    width = 7,height = "295px", status="success", plotlyOutput("therapyBar",height = "230px")),
                box(title = HTML("<span style='font-weight:bold; font-size:14px;'>Panic Attacks Tip the Balance Toward High Risk.</span>"), 
                    width = 5,height = "295px", status="warning", plotlyOutput("panicAttackColumnGender",height = "230px"))
              )
      ),
      
      # Correlation & Association Tab
      tabItem(
        tabName = "correlation",
        
        # First row: variable selection + run button
        fluidRow(
          box(
            width = 12, height = "80px",status = "info",
            fluidRow(
              column(
                width = 7,
                selectInput(
                  "cont_vars",
                  "Continuous vs Mental Risk Associations (Select variables):",
                  choices = c(
                    "age", "gender", "marital_status", "education_level", "employment_status",
                    "sleep_hours", "physical_activity_hours_per_week", "screen_time_hours_per_day",
                    "social_support_score", "work_stress_level", "academic_pressure_level",
                    "job_satisfaction_score", "financial_stress_level", "working_hours_per_week",
                    "anxiety_score", "depression_score", "stress_level", "mood_swings_frequency",
                    "concentration_difficulty_level", "panic_attack_history", "family_history_mental_illness",
                    "previous_mental_health_diagnosis", "therapy_history", "substance_use"
                  ),
                  selected = c("sleep_hours", "stress_level"),
                  multiple = TRUE,
                  width = "100%"
                )
              ),
              column(
                width = 5,
                div(
                  style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
                  tags$label("Target Categorical variable: Mental Risk", style = "font-weight:bold;"),
                  actionButton("runContCat", "Run Test",style = "
                     background-color:#0073e6; /* conspicuous blue background */
                     color:white;         /* white text */
                     font-weight:bold;    /* bold text */
                     padding:4px 10px;    /* adjust padding */
                     border-radius:4px;   /* rounded corners */
                   ")
                )
              )
            )
          )
        ),
        
        # Second row: plot + explanatory text
        fluidRow(
          box(
            width = 12, height = "510px", status = "success",
            plotOutput("contCatPlot", height = "450px"),
            div(
              style = "margin-top:5px; font-weight:bold; color:navy; font-size:12px;",
              "The ANOVA and two‑sample t‑tests were used to compare distributions of psychological, social, and lifestyle variables across mental risk categories, with p‑values shown in each facet to highlight which factors differ significantly and which do not, and green p‑values marking the variables that show statistically significant differences."
            )
          )
        )
      ),
      
      # Modeling Tab
      tabItem(tabName = "modeling",
              fluidRow(
                box(
                  title = tagList(
                    span("Logistic Regression", style = "color:#0073e6; font-weight:bold;"),
                    actionButton("runModels", "Run Models",
                                 style = "
                     float:right;
                     margin-left:210px;   /* space between heading and button */
                     background-color:#0073e6; /* conspicuous blue background */
                     color:white;         /* white text */
                     font-weight:bold;    /* bold text */
                     border:none;         /* remove border */
                     padding:4px 10px;    /* adjust padding */
                     border-radius:4px;   /* rounded corners */
                   ")
                  ),
                  width = 6,
                  status = "info",
                  h4(
                    tags$span("Confusion Matrix", 
                              style = "font-weight:bold; color:#00bfff; font-size:16px;"),
                    tags$span(": How Well It Predicts Risk Categories; and Where Misclassifications Occur Most",
                              style = "font-weight:bold;font-size:15px; color:#003366;")
                  ),
                  tableOutput("logitCM"),
                  # Statement between table and plot
                  tags$p(
                    "Panic Attack, Mental Illness and Sleep Drives Predictions.",
                    style = "font-size:15px; color:#003366; font-weight:bold; margin-top:5px; margin-bottom:0px;"
                  ),
                  plotOutput("logitImportance", height = "310px", width = "100%")
                ),
                box(title=tags$span("Random Forest", style = "color:green; font-weight:bold;"), width=6, status="success",
                    h4(
                      tags$span("Confusion Matrix", 
                                style = "font-weight:bold; color:#00bfff; font-size:16px;"),
                      tags$span(": Near‑Perfect Risk Classification with Minimal Misclassifications",
                                style = "font-weight:bold;font-size:14px; color:#003366;")
                    ),
                    tableOutput("rfCM"),
                    # Statement between table and plot
                    tags$p(
                      "Sleep, Anxiety, and Depression Drive Predictions.",
                      style = "font-size:15px; color:#003366; font-weight:bold; margin-top:5px; margin-bottom:0px;"
                    ),
                    
                    plotOutput("rfImportance", height = "310px", width = "100%"))
              )
      ),
      
      # New tab with same structure
      tabItem(tabName = "modeling2",
              fluidRow(
                box(title = tags$span(
                  list(
                    tags$span("Logistic Reg", 
                              style = "font-weight:bold; color:#1E90FF; font-size:14px;"),  # DodgerBlue
                    tags$span(":💪 Prediction, Limited Accuracy.", 
                              style = "font-weight:bold; color:#003366; font-size:14px;")
                  )
                ), width=4, height = "295px", status="info",
                plotOutput("logitROC", height = "238px")
                ),
                box(title = tags$span(
                  list(
                    tags$span("Random Forest", 
                              style = "font-weight:bold; color:#00bfff; font-size:14px;"),  # bright teal/sky blue
                    tags$span(": Near‑Perfect Risk Prediction.", 
                              style = "font-weight:bold; color:#003366; font-size:14px;")
                  )
                ),width=4,height = "295px", status="success",
                plotOutput("rfROC",height = "238px",width = "100%" )
                ),
                box(title = tags$span(
                  "Random Forest: Superior Performance Over Logistic Regression.",
                  style = "font-weight:bold; color:#003366; font-size:14px;"
                ), width=4,height = "295px", status="primary",
                tableOutput("modelComparison"))
              ),
              fluidRow(
                box(title = tags$span(
                  "Decision Tree Example: Step‑by‑Step Pathways to Risk Classification.",
                  style = "font-weight:bold; color:#003366; font-size:14px;"
                ), width=12,height ="300px",  status="warning",
                plotOutput("rfTree", height = "240px"))
              )
      ),
      
      # Raw Data Tab
      tabItem(tabName = "rawdata",
              fluidRow(
                box(title = "Filtered Dataset Preview", width = 12,
                    DT::dataTableOutput("rawDataTable"))
              )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reset filters when sync icon is clicked
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "gender", selected = "All")
    updateSelectInput(session, "marital", selected = "All")
    updateSelectInput(session, "education", selected = "All")
    updateSelectInput(session, "employment", selected = "All")
  })
  
  # Reactive filtered dataset
  filteredData <- reactive({
    data <- df
    
    # Gender filter
    if (!("All" %in% input$gender)) {
      data <- data %>% filter(gender %in% input$gender)
    }
    
    # Marital filter
    if (!("All" %in% input$marital)) {
      data <- data %>% filter(marital_status %in% input$marital)
    }
    
    # Education filter
    if (!("All" %in% input$education)) {
      data <- data %>% filter(education_level %in% input$education)
    }
    
    # Employment filter
    if (!("All" %in% input$employment)) {
      data <- data %>% filter(employment_status %in% input$employment)
    }
    
    data
  })
  
  # Download filtered dataset (CSV)
  # Download handler for Excel data (CSV here, but can be XLSX)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("mental_health_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Cancel button closes modal
  observeEvent(input$cancelModal, {
    removeModal()
  })
  
  
  
  # Show modal when header button clicked
  observeEvent(input$openDownload, {
    showModal(modalDialog(
      title = "Choose Report Format",
      footer = tagList(
        downloadButton("downloadPDF",
                       label = HTML("<i class='fas fa-file-pdf'></i> PDF"),
                       class = "btn-unified pdf-btn"),
        
        downloadButton("downloadHTML",
                       label = HTML("<i class='fas fa-file-code'></i> HTML"),
                       class = "btn-unified html-btn"),
        
        downloadButton("downloadWord",
                       label = HTML("<i class='fas fa-file-word'></i> Word"),
                       class = "btn-unified word-btn"),
        actionButton("cancelModal",
                     label = HTML("<i class='fas fa-times'></i> Cancel"),
                     class = "btn-unified cancel-btn")
      )
    ))
  })
  
  # Download handlers using report.Rmd
  output$downloadPDF <- downloadHandler(
    filename = function() paste0("dashboard_", Sys.Date(), ".pdf"),
    content = function(file) {
      rmarkdown::render("report.Rmd", output_file = file,
                        params = list(data = df),
                        output_format = "pdf_document",
                        envir = new.env(parent = globalenv()))
    }
  )
  
  output$downloadHTML <- downloadHandler(
    filename = function() paste0("dashboard_", Sys.Date(), ".html"),
    content = function(file) {
      rmarkdown::render("report.Rmd", output_file = file,
                        params = list(data = df),
                        output_format = "html_document",
                        envir = new.env(parent = globalenv()))
    }
  )
  
  output$downloadWord <- downloadHandler(
    filename = function() paste0("dashboard_", Sys.Date(), ".docx"),
    content = function(file) {
      rmarkdown::render("report.Rmd", output_file = file,
                        params = list(data = df),
                        output_format = "word_document",
                        envir = new.env(parent = globalenv()))
    }
  )
  
  theme_dashboard <- function() {
    theme_minimal(base_family = "Segoe UI") +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#0073e6"),
        axis.title = element_text(size = 12, color = "#333333"),
        axis.text  = element_text(size = 10, color = "#555555"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text  = element_text(size = 10),
        panel.grid.major = element_line(color = "#e0e0e0"),
        panel.grid.minor = element_blank()
      )
  }
  
  
  
  # Global Summary InfoBoxes
  output$riskSummary <- renderInfoBox({
    total <- nrow(filteredData())
    low_perc <- sum(filteredData()$mental_health_risk == 0) / total * 100
    mod_perc <- sum(filteredData()$mental_health_risk == 1) / total * 100
    high_perc <- sum(filteredData()$mental_health_risk == 2) / total * 100
    
    infoBox(
      "Risk Distribution",
      HTML(
        paste0(
          "<div style='display:flex; justify-content:space-around;'>",
          "<div style='flex:1; text-align:center; padding-right:10px; border-right:2px solid #ccc;'>",
          "<span style='color:#27ae60; font-weight:bold; white-space:nowrap;'>✅ Low</span><br>",
          round(low_perc, 1), "%",
          "</div>",
          "<div style='flex:1; text-align:center; padding:0 10px; border-right:2px solid #ccc;'>",
          "<span style='color:#f39c12; font-weight:bold; white-space:nowrap;'>⚠️ Moderate</span><br>",
          round(mod_perc, 1), "%",
          "</div>",
          "<div style='flex:1; text-align:center; padding-left:10px;'>",
          "<span style='color:#e74c3c; font-weight:bold; white-space:nowrap;'>❌ High</span><br>",
          round(high_perc, 1), "%",
          "</div>",
          "</div>"
        )
      ),
      icon = icon("chart-pie"),
      color = "light-blue",
      fill = TRUE
    )
  })
  
  # Total Population InfoBox
  output$totalPopulation <- renderInfoBox({
    total <- nrow(filteredData())
    if (total == 0) {
      infoBox(HTML("<span style='font-size:11px;'>Total Population</span>"),
              "No data",
              icon = icon("users"),
              color = "purple",
              fill = TRUE)
    } else {
      infoBox(HTML("<span style='font-size:11px;'>Total Population</span>"),
              HTML(paste0("👥 <span style='color:#32CD32;font-size:15px;'><b>", 
                          format(total, big.mark = ","), 
                          "</b></span>")),   # value styled in blue
              icon = icon("users"),
              color = "purple",            # heading stays purple
              fill = TRUE)
    }
  })
  
  # Average Age InfoBox
  output$avgAge <- renderInfoBox({
    if (nrow(filteredData()) == 0 || all(is.na(filteredData()$age))) {
      infoBox(HTML("<span style='font-size:11px;'>Population Avg Age</span>"),
              "No data",
              icon = icon("calendar"),
              color = "blue",
              fill = TRUE)
    } else {
      avg_age <- round(mean(filteredData()$age, na.rm = TRUE), 1)
      infoBox(HTML("<span style='font-size:11px;'>Population Avg Age</span>"),
              HTML(paste0("📊 <span style='color:#32CD32;font-size:15px;'><b>", 
                          avg_age, " yrs</b></span>")),  # value styled in orange
              icon = icon("calendar"),
              color = "blue",              # heading stays blue
              fill = TRUE)
    }
  })
  
  
  
  # --- Server side ---
  output$allVarsRadar <- renderPlotly({
    df <- filteredData() %>%
      tidyr::pivot_longer(
        cols = c(social_support_score, work_stress_level, academic_pressure_level,
                 job_satisfaction_score, financial_stress_level,
                 anxiety_score, depression_score, stress_level, mood_swings_frequency,
                 concentration_difficulty_level),
        names_to = "Variable", values_to = "Score"
      ) %>%
      group_by(Variable, mental_risk_cat) %>%
      summarise(mean_score = mean(as.numeric(Score), na.rm = TRUE), .groups = "drop")
    
    # Nice labels
    label_map <- c(
      social_support_score = "Social Support",
      work_stress_level = "Work Stress",
      academic_pressure_level = "Academic Pressure",
      job_satisfaction_score = "Job Satisfaction",
      financial_stress_level = "Financial Stress",
      anxiety_score = "Anxiety",
      depression_score = "Depression",
      stress_level = "Stress",
      mood_swings_frequency = "Mood Swings",
      concentration_difficulty_level = "Concentration Difficulty"
    )
    
    df$Label <- label_map[df$Variable]
    categories <- unique(df$Label)
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(r = df$mean_score[df$mental_risk_cat == "Low"], theta = categories,
                name = "Low Risk", line = list(color = "#27ae60")) %>%
      add_trace(r = df$mean_score[df$mental_risk_cat == "Moderate"], theta = categories,
                name = "Moderate Risk", line = list(color = "#f39c12")) %>%
      add_trace(r = df$mean_score[df$mental_risk_cat == "High"], theta = categories,
                name = "High Risk", line = list(color = "#e74c3c")) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            tickfont = list(size = 9),
            family = "Arial",         # font family
            weight = "bold"
          )
        ),
        title = NULL,
        legend = list(
          title = list(text = "Mental Risk Level"),
          orientation = "v",
          x = 1.2, xanchor = "left",
          y = 1, yanchor = "top"
        )
      )
  })
  
  
  # --- Confusion Matrix Plot ---
  output$confMatrixPlot <- renderPlotly({
    # Logistic Regression confusion matrix
    logit_cm <- matrix(c(2113,546,0,
                         650,2796,464,
                         2,225,704),
                       nrow=3, byrow=TRUE,
                       dimnames=list(Predicted=c("Low","Moderate","High"),
                                     Actual=c("Low","Moderate","High")))
    
    # Random Forest confusion matrix
    rf_cm <- matrix(c(2764,6,0,
                      1,3560,56,
                      0,1,1112),
                    nrow=3, byrow=TRUE,
                    dimnames=list(Predicted=c("Low","Moderate","High"),
                                  Actual=c("Low","Moderate","High")))
    
    # Convert to tidy data frames
    logit_df <- as.data.frame(as.table(logit_cm))
    logit_df$Model <- "Logistic Regression"
    
    rf_df <- as.data.frame(as.table(rf_cm))
    rf_df$Model <- "Random Forest"
    
    df <- rbind(logit_df, rf_df)
    
    # Plot heatmap faceted by model
    p <- ggplot(df, aes(x=Actual, y=Predicted, fill=Freq)) +
      geom_tile(color="white") +
      geom_text(aes(label=Freq), color="black", size=3) +
      scale_fill_gradient(low = "gold", high = "red", guide =NULL )+
      facet_wrap(~Model) +
      labs(title=NULL,
           x="Actual Risk", y="Predicted Risk") +
      theme_gray()+
      theme(legend.position=NULL)   # move legend below plots
    
    ggplotly(p)
  })
  
  
  # Example Demographics InfoBoxes
  filteredData <- reactive({
    data <- df
    
    if (!("All" %in% input$gender)) {
      data <- data[data$gender %in% input$gender, ]
    }
    if (!("All" %in% input$marital)) {
      data <- data[data$marital_status %in% input$marital, ]
    }
    if (!("All" %in% input$education)) {
      data <- data[data$education_level %in% input$education, ]
    }
    if (!("All" %in% input$employment)) {
      data <- data[data$employment_status %in% input$employment, ]
    }
    
    data
  })
  
  # Helper function to build bar HTML with label on top
  makeBar <- function(label, pct, palette) {
    paste0(
      "<div style='margin:0 5px; text-align:center;'>",
      "<span style='font-size:12px; font-weight:bold; color:", palette$fill, ";'>", label, "</span>",
      "<div style='background:", palette$bg, "; height:18px; width:95px; border-radius:3px; position:relative; margin-top:2px;'>",
      "<div style='background:", palette$fill, "; height:18px; width:", pct, "%; border-radius:3px;'></div>",
      "<span style='position:absolute; top:0; left:50%; transform:translateX(-50%); font-size:11px; color:", palette$text, ";'>", pct, "%</span>",
      "</div>",
      "</div>"
    )
  }
  
  # Gender Distribution
  output$genderDist <- renderInfoBox({
    data <- filteredData()
    counts <- prop.table(table(data$gender)) * 100
    male <- ifelse("Male" %in% names(counts), round(counts["Male"], 1), 0)
    female <- ifelse("Female" %in% names(counts), round(counts["Female"], 1), 0)
    
    palettes <- list(
      "Male"   = list(bg="#d4edda", fill="#2e86c1", text="#000"),   # blue
      "Female" = list(bg="#f8d7da", fill="#c0392b", text="#000"),    # red
      "Other"  = list(bg="#e8daef", fill="#8e44ad", text="#000")    # purple
    )
    
    msg <- HTML("<div style='display:flex; justify-content:space-around;'>")
    for (g in names(counts)) {
      pct <- round(counts[g], 1)
      msg <- paste0(msg, makeBar(g, pct, palettes[[g]]))
    }
    msg <- paste0(msg, "</div>")
    
    infoBox("Gender Balance", HTML(msg), icon = icon("venus-mars"), color = "navy", fill = TRUE)
  })
  
  # Marital Status
  output$maritalDist <- renderInfoBox({
    data <- filteredData()
    counts <- prop.table(table(data$marital_status)) * 100
    
    palettes <- list(
      "Single"   = list(bg="#eaf2f8", fill="#5dade2", text="#000"),   # light blue
      "Married"  = list(bg="#fef9e7", fill="#f39c12", text="#000"),   # orange
      "Divorced" = list(bg="#f9ebea", fill="#e74c3c", text="#000"),   # red
      "Widowed"  = list(bg="#e8daef", fill="#8e44ad", text="#000")    # purple
    )
    
    msg <- HTML("<div style='display:flex; width:100%;'>")
    for (status in names(counts)) {
      pct <- round(counts[status], 1)
      msg <- paste0(msg, makeBar(status, pct, palettes[[status]]))
    }
    msg <- paste0(msg, "</div>")
    
    infoBox("Marital Mix", HTML(msg), icon = icon("ring"), color = "navy", fill = TRUE)
  })
  
  # Education Profile
  output$educationProfile <- renderInfoBox({
    data <- filteredData()
    counts <- prop.table(table(data$education_level)) * 100
    
    palettes <- list(
      "High School"   = list(bg="#fef9e7", fill="#f1c40f", text="#000"),   # yellow
      "Bachelor" = list(bg="#eaf2f8", fill="#3498db", text="#000"),   # blue
      "Master"  = list(bg="#e8daef", fill="#008080", text="#000"),   # purple
      "PhD"  = list(bg="#f9ebea", fill="#e74c3c", text="#000")    # red
    )
    
    msg <- HTML("<div style='display:flex; justify-content:space-around;'>")
    for (level in names(counts)) {
      pct <- round(counts[level], 1)
      msg <- paste0(msg, makeBar(level, pct, palettes[[level]]))
    }
    msg <- paste0(msg, "</div>")
    
    infoBox("Education Profile", HTML(msg), icon = icon("graduation-cap"), color = "navy",fill = TRUE)
  })
  
  # Employment Overview
  output$employmentOverview <- renderInfoBox({
    data <- filteredData()
    counts <- prop.table(table(data$employment_status)) * 100
    
    palettes <- list(
      "Employed"   = list(bg="#d4edda", fill="#9400D3", text="#000"),   # green
      "Unemployed" = list(bg="#f8d7da", fill="#e74c3c", text="#000"),   # red
      "Student"    = list(bg="#eaf2f8", fill="#3498db", text="#000"),   # blue
      "Self-Employed"    = list(bg="#fef9e7", fill="#f39c12", text="#000")    # orange
    )
    
    msg <- HTML("<div style='display:flex; justify-content:space-around;'>")
    for (status in names(counts)) {
      pct <- round(counts[status], 1)
      msg <- paste0(msg, makeBar(status, pct, palettes[[status]]))
    }
    msg <- paste0(msg, "</div>")
    
    infoBox("Employment Overview", HTML(msg), icon = icon("briefcase"), color = "lime",fill = TRUE)
  })
  
  
  
  
  
  # Age distribution density vs Mental Risk by Gender
  # Age probability density lines vs Mental Risk by Gender
  output$agePlot <- renderPlotly({
    
    df <- filteredData() %>%
      dplyr::filter(!is.na(gender), !is.na(mental_risk_cat))
    
    p <- ggplot(df, aes(x = age, fill = mental_risk_cat)) +
      geom_density(alpha = 0.8, size = 0.3, adjust = 1, color = NA) +  # filled curves
      facet_wrap(~ gender, scales = "fixed") +
      scale_fill_manual(values = c(
        "Low" = "#27ae60",      # green
        "Moderate" = "#f39c12", # orange
        "High" = "#e74c3c"      # red
      )) +
      scale_x_continuous(expand = c(0, 0))+
      
      labs(title = NULL,
           x = "Age",
           y = "Probability Density",
           fill = "Mental Risk") +
      theme_dark() +
      theme(
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        legend.title = element_text(size = 8, face = "bold"),
        legend.key.size = unit(0.1, "lines")
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  
  
  # Marital status vs Mental Health by Gender
  output$maritalPlot <- renderPlotly({
    
    df <- filteredData() %>%
      dplyr::filter(!is.na(marital_status), !is.na(mental_risk_cat)) %>%
      dplyr::group_by(marital_status, mental_risk_cat) %>%
      dplyr::summarise(count = n(), .groups = "drop") %>%
      dplyr::group_by(marital_status) %>%
      dplyr::mutate(percent = round(count / sum(count) * 100, 1))
    
    p <- ggplot(df, aes(x = marital_status, y = percent, fill = mental_risk_cat)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = paste0(percent, "%")),
                position = position_dodge(width = 0.9),
                hjust = 1.5,                      # push labels inside bars
                color = "white",                  # white text
                size = 2.5,                         # reduce font size
                fontface = "bold"                 # make labels bold
      ) +
      scale_fill_manual(values = c(
        "Low" = "#27ae60",      # green
        "Moderate" = "#f39c12", # orange
        "High" = "#e74c3c"      # red
      )) +
      
      labs(title = NULL,
           x = NULL,
           y = NULL,   # remove y-axis label
           fill = "Mental Risk") +
      theme_dark()+
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  # Education Level vs Mental Health (% with labels inside, dodged bars)
  output$educationPlot <- renderPlotly({
    
    df <- filteredData() %>%
      dplyr::filter(!is.na(education_level), !is.na(mental_risk_cat)) %>%
      dplyr::group_by(education_level, mental_risk_cat) %>%
      dplyr::summarise(count = n(), .groups = "drop") %>%
      dplyr::group_by(education_level) %>%
      dplyr::mutate(percent = round(count / sum(count) * 100, 1))
    
    p <- ggplot(df, aes(x = education_level, y = percent, fill = mental_risk_cat)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = paste0(percent, "%")),
                position = position_dodge(width = 0.9),
                hjust = 1.5,                      # push labels inside bars
                color = "white",                  # white text
                size = 2.5,                         # reduce font size
                fontface = "bold"                 # make labels bold
      ) +
      scale_fill_manual(values = c(
        "Low" = "#27ae60",      # green
        "Moderate" = "#f39c12", # orange
        "High" = "#e74c3c"      # red
      )) +
      theme_dark() +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = "Mental Risk") +
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  
  
  
  # Employment Status vs Mental Health (% with labels inside, dodged bars)
  output$employmentPlot <- renderPlotly({
    
    df <- filteredData() %>%
      dplyr::filter(!is.na(employment_status), !is.na(mental_risk_cat)) %>%
      dplyr::group_by(employment_status, mental_risk_cat) %>%
      dplyr::summarise(count = n(), .groups = "drop") %>%
      dplyr::group_by(employment_status) %>%
      dplyr::mutate(percent = round(count / sum(count) * 100, 1))
    
    p <- ggplot(df, aes(x = employment_status, y = percent, fill = mental_risk_cat)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = paste0(percent, "%")),
                position = position_dodge(width = 0.9),
                hjust = 1.5,                      # push labels inside bars
                color = "white",                  # white text
                size = 2.5,                         # reduce font size
                fontface = "bold"                 # make labels bold
      ) +
      scale_fill_manual(values = c(
        "Low" = "#27ae60",      # green
        "Moderate" = "#f39c12", # orange
        "High" = "#e74c3c"      # red
      )) +
      coord_flip() +
      theme_dark() +
      labs(title = NULL,
           x = NULL,
           y = NULL,
           fill = "Mental Risk") +
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_text(size = 8, face = "bold"),   # bold y labels
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right",
            axis.ticks = element_blank(),
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  #Lifestyle
  # Sleep Risk InfoBox
  output$sleepRiskBox <- renderInfoBox({
    df <- filteredData()
    risk_tab <- table(df$sleep_hours, df$mental_risk_cat)
    risk_df <- as.data.frame(prop.table(risk_tab, 1) * 100)
    
    low_risk <- ifelse(length(risk_df[risk_df$Var2 == "Low", "Freq"]) > 0,
                       round(risk_df[risk_df$Var2 == "Low", "Freq"], 1), 0)
    moderate_risk <- ifelse(length(risk_df[risk_df$Var2 == "Moderate", "Freq"]) > 0,
                            round(risk_df[risk_df$Var2 == "Moderate", "Freq"], 1), 0)
    high_risk <- ifelse(length(risk_df[risk_df$Var2 == "High", "Freq"]) > 0,
                        round(risk_df[risk_df$Var2 == "High", "Freq"], 1), 0)
    
    msg <- HTML(
      paste0(
        "<div style='display:flex; justify-content:space-around; align-items:center;'>",
        "<div style='text-align:center; margin:0 5px; border-right:2px solid #ccc; padding-right:6px;'>",
        "<span style='color:#27ae60; font-size:12px;'>Low: ", low_risk, "%</span>",
        "<div style='background:#d4edda; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#27ae60; height:6px; width:", low_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "<div style='text-align:center; margin:0 5px; border-right:2px solid #ccc; padding-right:6px;'>",
        "<span style='color:#f39c12; font-size:12px;'>Moderate: ", moderate_risk, "%</span>",
        "<div style='background:#fff3cd; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#f39c12; height:6px; width:", moderate_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "<div style='text-align:center; margin:0 5px;'>",
        "<span style='color:#e74c3c; font-size:12px;'>High: ", high_risk, "%</span>",
        "<div style='background:#f8d7da; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#e74c3c; height:6px; width:", high_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "</div>"
      )
    )
    
    infoBox("Sleep & Mental Risk", msg, icon = icon("bed"), color = "navy", fill = TRUE)
  })
  
  # Physical Activity Risk InfoBox
  output$activityRiskBox <- renderInfoBox({
    df <- filteredData()
    risk_tab <- table(df$physical_activity_hours_per_week, df$mental_risk_cat)
    risk_df <- as.data.frame(prop.table(risk_tab, 1) * 100)
    
    low_risk <- ifelse(length(risk_df[risk_df$Var2 == "Low", "Freq"]) > 0,
                       round(risk_df[risk_df$Var2 == "Low", "Freq"], 1), 0)
    moderate_risk <- ifelse(length(risk_df[risk_df$Var2 == "Moderate", "Freq"]) > 0,
                            round(risk_df[risk_df$Var2 == "Moderate", "Freq"], 1), 0)
    high_risk <- ifelse(length(risk_df[risk_df$Var2 == "High", "Freq"]) > 0,
                        round(risk_df[risk_df$Var2 == "High", "Freq"], 1), 0)
    
    msg <- HTML(
      paste0(
        "<div style='display:flex; justify-content:space-around; align-items:center;'>",
        "<div style='text-align:center; margin:0 5px; border-right:2px solid #ccc; padding-right:6px;'>",
        "<span style='color:#27ae60; font-size:12px;'>Low: ", low_risk, "%</span>",
        "<div style='background:#d4edda; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#27ae60; height:6px; width:", low_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "<div style='text-align:center; margin:0 5px; border-right:2px solid #ccc; padding-right:6px;'>",
        "<span style='color:#f39c12; font-size:12px;'>Moderate: ", moderate_risk, "%</span>",
        "<div style='background:#fff3cd; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#f39c12; height:6px; width:", moderate_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "<div style='text-align:center; margin:0 5px;'>",
        "<span style='color:#e74c3c; font-size:12px;'>High: ", high_risk, "%</span>",
        "<div style='background:#f8d7da; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#e74c3c; height:6px; width:", high_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "</div>"
      )
    )
    
    infoBox("Activity & Mental Risk", msg, icon = icon("running"), color = "navy", fill = TRUE)
  })
  
  output$screenRiskBox <- renderInfoBox({
    df <- filteredData()
    risk_tab <- table(df$screen_time_hours_per_day, df$mental_risk_cat)
    risk_df <- as.data.frame(prop.table(risk_tab, 1) * 100)
    
    low_risk <- ifelse(length(risk_df[risk_df$Var2 == "Low", "Freq"]) > 0,
                       round(risk_df[risk_df$Var2 == "Low", "Freq"], 1), 0)
    moderate_risk <- ifelse(length(risk_df[risk_df$Var2 == "Moderate", "Freq"]) > 0,
                            round(risk_df[risk_df$Var2 == "Moderate", "Freq"], 1), 0)
    high_risk <- ifelse(length(risk_df[risk_df$Var2 == "High", "Freq"]) > 0,
                        round(risk_df[risk_df$Var2 == "High", "Freq"], 1), 0)
    
    msg <- HTML(
      paste0(
        "<div style='display:flex; justify-content:space-around; align-items:center;'>",
        "<div style='text-align:center; margin:0 5px; border-right:2px solid #ccc; padding-right:6px;'>",
        "<span style='color:#27ae60; font-size:12px;'>Low: ", low_risk, "%</span>",
        "<div style='background:#d4edda; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#27ae60; height:6px; width:", low_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "<div style='text-align:center; margin:0 5px; border-right:2px solid #ccc; padding-right:6px;'>",
        "<span style='color:#f39c12; font-size:12px;'>Moderate: ", moderate_risk, "%</span>",
        "<div style='background:#fff3cd; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#f39c12; height:6px; width:", moderate_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "<div style='text-align:center; margin:0 5px;'>",
        "<span style='color:#e74c3c; font-size:12px;'>High: ", high_risk, "%</span>",
        "<div style='background:#f8d7da; height:6px; width:50px; border-radius:3px;'>",
        "<div style='background:#e74c3c; height:6px; width:", high_risk, "%; border-radius:3px;'></div>",
        "</div>",
        "</div>",
        "</div>"
      )
    )
    
    infoBox("Screen Time & Mental Risk", msg, icon = icon("desktop"), color = "navy", fill = TRUE)
  })
  
  output$substanceBarPlot <- renderPlotly({
    
    # Create age groups (adjust ranges as needed)
    plot_data <- filteredData() %>%
      mutate(age_group = case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 44 ~ "30-44",
        age >= 45 & age <= 59 ~ "45-59",
        age >= 60             ~ "60+",
        TRUE                  ~ "Other"
      )) %>%
      group_by(age_group, substance_use_bin, mental_risk_cat) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(age_group, substance_use_bin) %>%
      mutate(percent = n / sum(n) * 100)
    
    # Plot percentages by age group with labels
    p <- ggplot(plot_data, aes(x = age_group, y = percent, fill = mental_risk_cat)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
      geom_text(aes(label = paste0(round(percent, 1), "%")),
                position = position_stack(vjust = 0.5),
                size = 2.5, color = "white") +
      facet_wrap(~ substance_use_bin, scales = "fixed") +
      scale_fill_manual(values = c("Low" = "#27ae60",
                                   "Moderate" = "#f39c12",
                                   "High" = "#e74c3c")) +
      
      labs(title = NULL,
           x = "Age Group", y = "Percentage", fill = "Mental Risk") +
      theme_dark() +
      theme(
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 8, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold"),
        legend.spacing.x = unit(0.2, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        axis.ticks = element_blank()
        
      )
    
    ggplotly(p)
  })
  
  output$substancePiePlot <- renderPlotly({
    
    df <- filteredData() %>%
      dplyr::count(substance_use_bin) %>%
      dplyr::mutate(percent = round(n / sum(n) * 100, 1)) %>%
      as.data.frame()
    
    plot_ly(
      data = df,
      labels = ~substance_use_bin,
      values = ~n,
      type = "pie",
      textinfo = "label+percent",    # show labels + counts + percentages
      insidetextorientation = "horizontal",    # text orientation inside slices
      hole = 0.4,                          # donut style
      marker = list(
        colors = c(
          "Yes" = "#F44336",   # red for substance use
          "No"  = "#4CAF50"    # green for no substance use
        )[df$substance_use_bin],
        line = list(color = "white", width = 2)  # white borders for separation
      ),
      pull = 0.01   # slight offset for slice separation
    ) %>%
      layout(
        showlegend = FALSE,   # remove legend (labels already shown)
        margin = list(l = 10, r = 10, b = 10, t = 30),
        title = list(text = NULL, x = 0.5), # centered title
        uniformtext = list(minsize = 5, mode = "hide") # prevent overlap by shrinking text
      )
  })
  
  # Social Support Category (categorical) vs Mental Risk, grouped by age
  output$supportBarPlot <- renderPlotly({
    
    plot_data2 <- filteredData() %>%
      group_by(social_support_cat, mental_risk_cat) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(social_support_cat) %>%
      mutate(percent = n / sum(n) * 100)
    
    p <- ggplot(plot_data2, aes(x = social_support_cat, y = percent, fill = mental_risk_cat)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
      geom_text(aes(label = paste0(round(percent, 1), "%")),
                position = position_dodge(width = 0.9),   # match dodge for labels
                vjust = 0.5,                              # vertical centering
                size = 2.5, color = "white", fontface = "bold") +
      scale_fill_manual(values = c("Low" = "#27ae60",
                                   "Moderate" = "#f39c12",
                                   "High" = "#e74c3c")) +
      coord_flip() +
      theme_dark() +
      theme(
        legend.position = "right",
        legend.box = "vertical",
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold"),
        legend.spacing.x = unit(0.2, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        
        axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 8, face = "bold")
      ) +
      labs(title = NULL,
           x = NULL, y = NULL, fill = "Mental Risk")
    
    ggplotly(p)
  })
  
  
  
  
  
  
  output$linePlot <- renderPlotly({
    
    # Reshape into long format
    plot_data <- filteredData() %>%
      dplyr::select(age,
                    sleep_hours,
                    physical_activity_hours_per_week,
                    screen_time_hours_per_day) %>%
      tidyr::pivot_longer(
        cols = c(sleep_hours,
                 physical_activity_hours_per_week,
                 screen_time_hours_per_day),
        names_to = "factor",
        values_to = "value"
      )
    
    # Line chart: average value by age
    plot_data <- plot_data %>%
      dplyr::group_by(age, factor) %>%
      dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      # Explicitly set factor levels and labels
      dplyr::mutate(factor = factor(factor,
                                    levels = c("sleep_hours",
                                               "physical_activity_hours_per_week",
                                               "screen_time_hours_per_day"),
                                    labels = c("Sleep Hours",
                                               "Activity Hours/Week",
                                               "Screen Time/Day")))
    
    # Plot
    p <- ggplot(plot_data, aes(x = age, y = mean_value, color = factor)) +
      geom_line(size = 0.5) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c(
        "Sleep Hours" = "#4CAF50",          # green
        "Activity Hours/Week" = "#2196F3",  # blue
        "Screen Time/Day" = "#F44336"       # red
      )) +
      theme_minimal(base_size = 12) +
      labs(title = NULL,
           x = "Age",
           y = "Average Hours",
           color = NULL) +
      theme_dark() +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 8, face = "bold")
        
      )
    
    ggplotly(p)
  })
  
  # Work/Academic Stress tab plots
  # Helper function for bar rendering inside InfoBox
  makeBar2 <- function(label, pct, palette) {
    paste0(
      "<div style='margin:0 5px; text-align:center;'>",
      "<span style='font-size:12px; font-weight:bold; color:", palette$fill, ";'>", label, "</span><br>",
      "<span style='font-size:13px; font-weight:bold; color:", palette$text, ";'>", pct, "%</span>",
      "<div style='background:", palette$bg, "; height:6px; width:70px; border-radius:3px; margin-top:2px;'>",
      "<div style='background:", palette$fill, "; height:6px; width:", pct, "%; border-radius:3px;'></div>",
      "</div>",
      "</div>"
    )
  }
  
  # Refined Work Stress InfoBox
  output$workStressBox <- renderInfoBox({
    data <- filteredData()
    counts <- prop.table(table(data$work_stress_cat)) * 100
    
    palettes <- list(
      "Very Low"  = list(bg="#d1f2eb", fill="#1abc9c", text="white"),   # teal
      "Low"       = list(bg="#d4edda", fill="#27ae60", text="white"),   # green
      "Moderate"  = list(bg="#fff3cd", fill="#f39c12", text="white"),   # orange
      "High"      = list(bg="#f8d7da", fill="#ffd700", text="white"),   # yellow
      "Very High" = list(bg="#e8daef", fill="#e74c3c", text="white")    # red
    )
    
    msg <- HTML("<div style='display:flex; justify-content:space-around;'>")
    for (cat in names(counts)) {
      pct <- round(counts[cat], 1)
      msg <- paste0(msg, makeBar2(cat, pct, palettes[[cat]]))
    }
    msg <- paste0(msg, "</div>")
    
    infoBox("Work Stress Profile (v2)", HTML(msg), icon = icon("briefcase", style="font-size:30px;"), color = "blue", fill = TRUE)
  })
  
  # Refined Academic Pressure InfoBox
  output$academicPressureBox <- renderInfoBox({
    data <- filteredData()
    counts <- prop.table(table(data$academic_pressure_cat)) * 100
    
    palettes <- list(
      "Very Low"  = list(bg="#d1f2eb", fill="#1abc9c", text="white"),   # teal
      "Low"       = list(bg="#d4edda", fill="#27ae60", text="white"),   # green
      "Moderate"  = list(bg="#fff3cd", fill="#f39c12", text="white"),   # orange
      "High"      = list(bg="#f8d7da", fill="#ffd700", text="white"),   # red
      "Very High" = list(bg="#e8daef", fill="#e74c3c", text="white")    # purple
    )
    
    msg <- HTML("<div style='display:flex; justify-content:space-around;'>")
    for (cat in names(counts)) {
      pct <- round(counts[cat], 1)
      msg <- paste0(msg, makeBar2(cat, pct, palettes[[cat]]))
    }
    msg <- paste0(msg, "</div>")
    
    infoBox("Academic Pressure Profile (v2)", HTML(msg), icon = icon("book", style="font-size:30px;"), color = "purple", fill = TRUE)
  })
  
  
  # 1. Work Stress Category vs Mental Risk
  output$workStressCatRisk <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(work_stress_cat), !is.na(mental_risk_cat)) %>%
      group_by(work_stress_cat, mental_risk_cat) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(work_stress_cat) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=work_stress_cat, y=percent, fill=mental_risk_cat)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_dodge(width=0.9), vjust=-0.25, size=2.5, color = "white", fontface = "bold") +
      scale_fill_manual(
        name = "Mental Risk Level",
        values = c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c"),
        labels = c("Low Risk","Moderate Risk","High Risk")
      )+
      theme_dark() +
      labs(title=NULL, x="Work Stress Category", y=NULL)+
      theme(
        legend.position ="top",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()
      )
    
    ggplotly(p)
  })
  
  output$workAcademicRadar <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(mental_risk_cat)) %>%
      group_by(mental_risk_cat) %>%
      summarise(
        WorkStress = mean(as.numeric(work_stress_level), na.rm=TRUE),
        AcademicPressure = mean(as.numeric(academic_pressure_level), na.rm=TRUE),
        JobSatisfaction = mean(as.numeric(job_satisfaction_score), na.rm=TRUE),
        .groups="drop"
      )
    
    categories <- c("Work Stress","Academic Pressure","Job Satisfaction")
    
    plot_ly(type='scatterpolar', mode='lines+markers') %>%
      add_trace(r=df$WorkStress, theta=categories,
                name=df$mental_risk_cat[1], line=list(color="#27ae60")) %>%
      add_trace(r=df$AcademicPressure, theta=categories,
                name=df$mental_risk_cat[2], line=list(color="#f39c12")) %>%
      add_trace(r=df$JobSatisfaction, theta=categories,
                name=df$mental_risk_cat[3], line=list(color="#e74c3c")) %>%
      layout(
        polar=list(radialaxis=list(visible=TRUE, range=c(0,10))),
        title=NULL,
        legend = list(
          title = list(text = "Mental Risk Level"),
          orientation = "v",
          x = 1.2, xanchor = "left",
          y = 1, yanchor = "top")
      )
  })
  
  # 3. Job Satisfaction Category vs Mental Risk
  output$jobSatisfactionCatRisk <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(job_satisfaction_cat), !is.na(mental_risk_cat)) %>%
      group_by(job_satisfaction_cat, mental_risk_cat) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(job_satisfaction_cat) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=job_satisfaction_cat, y=percent, fill=mental_risk_cat)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_dodge(width=0.9), vjust=-0.25, size=2.5, color = "white", fontface = "bold") +
      scale_fill_manual(
        name = "Mental Risk Level",
        values = c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c"),
        labels = c("Low Risk","Moderate Risk","High Risk")
      )+
      theme_dark() +
      labs(title=NULL, x="Job Satisfaction Category", y=NULL)+
      theme(
        legend.position ="top",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank()
      )
    
    ggplotly(p)
  })
  
  # 4. Working Hours per Week vs Mental Risk (Line Chart)
  output$workingHoursLine <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(working_hours_per_week), !is.na(mental_risk_cat)) %>%
      group_by(working_hours_per_week, mental_risk_cat) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(working_hours_per_week) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=working_hours_per_week, y=percent, color=mental_risk_cat, group=mental_risk_cat)) +
      geom_line(size=0.3) +
      geom_point(size=0.3) +
      scale_color_manual(
        name = "Mental Risk Level",
        values = c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c"),
        labels = c("Low Risk","Moderate Risk","High Risk")
      )+
      theme_dark() +
      labs(title=NULL, x="Working Hours per Week", y= "Percentage %")+
      theme(
        legend.position ="top"
      )
    
    ggplotly(p)
  })
  
  
  
  # Psychological Indicators tab plots
  # 1. Stress Category vs Mental Risk (Bar)
  output$stressBar<- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(stress_cat), !is.na(mental_risk_cat)) %>%
      group_by(stress_cat, mental_risk_cat) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(stress_cat) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=stress_cat, y=percent, fill=mental_risk_cat)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_dodge(width=0.5), vjust=0.5, color = "white",size = 2.5, fontface = "bold") +
      scale_fill_manual(name="Mental Risk Level",
                        values=c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c"),
                        labels=c("Low Risk","Moderate Risk","High Risk")) +
      theme_dark() +
      labs(title=NULL, x="Stress Category", y=NULL)+
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "top",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    ggplotly(p)
  })
  
  # 2. Depression vs Mental Risk (Column Chart)
  output$depressionColumn <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(depression_cat), !is.na(mental_risk_cat)) %>%
      group_by(depression_cat, mental_risk_cat) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(depression_cat) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=depression_cat, y=percent, fill=mental_risk_cat)) +
      geom_col(position="dodge") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_dodge(width=0.8), vjust=0.1, color = "white",size = 2.5, fontface = "bold") +
      scale_fill_manual(name="Mental Risk Level",
                        values=c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c")) +
      theme_dark() +
      labs(title=NULL, x="Depression Category", y=NULL)+
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "top",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  output$anxietyBar <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(anxiety_cat), !is.na(mental_risk_cat)) %>%
      group_by(anxiety_cat, mental_risk_cat) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(anxiety_cat) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=anxiety_cat, y=percent, fill=mental_risk_cat)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_dodge(width=0.5), vjust=0.5, color = "white",size = 2.5, fontface = "bold") +
      scale_fill_manual(name="Mental Risk Level",
                        values=c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c"),
                        labels=c("Low Risk","Moderate Risk","High Risk")) +
      theme_dark() +
      labs(title=NULL, x="Anxiety Category", y=NULL) +
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "top",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  
  # 6. Composite Psychological Indicators (Radar Chart)
  output$psychCompositeRadar <- renderPlotly({
    df <- filteredData() %>%
      tidyr::pivot_longer(cols=c(stress_level, depression_score, anxiety_score, mood_swings_frequency, concentration_difficulty_level),
                          names_to="Indicator", values_to="Score") %>%
      group_by(Indicator, mental_risk_cat) %>%
      summarise(mean_score=mean(as.numeric(Score), na.rm=TRUE), .groups="drop")
    
    categories <- c("Stress","Depression","Anxiety","Mood Swings","Concentration Difficulty")
    
    plot_ly(type='scatterpolar', mode='lines+markers') %>%
      add_trace(r=df$mean_score[df$mental_risk_cat=="Low"], theta=categories,
                name="Low Risk", line=list(color="#27ae60")) %>%
      add_trace(r=df$mean_score[df$mental_risk_cat=="Moderate"], theta=categories,
                name="Moderate Risk", line=list(color="#f39c12")) %>%
      add_trace(r=df$mean_score[df$mental_risk_cat=="High"], theta=categories,
                name="High Risk", line=list(color="#e74c3c")) %>%
      layout(
        polar=list(radialaxis=list(visible=TRUE, range=c(0,10))),
        title=NULL,
        legend=list(title=list(text="Mental Risk Level"))
      )
  })
  
  # Medical/Family History tab plots
  # 1. Family History vs Mental Risk (Bar Chart)
  output$familyHistoryBar <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(family_history_bin), !is.na(mental_risk_cat), !is.na(gender)) %>%
      group_by(family_history_bin, mental_risk_cat, gender) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(family_history_bin, gender) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=family_history_bin, y=percent, fill=mental_risk_cat)) +
      geom_bar(stat="identity", position="stack") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_stack(vjust = 0.5), color = "white",size = 2.5, fontface = "bold") +
      facet_wrap(~gender) +
      scale_fill_manual(values=c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c")) +
      labs(title = NULL,
           x = "Mental Illness History",
           y = NULL,   # remove y-axis label
           fill = "Mental Risk Level") +
      theme_dark()+
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  # 2. Previous Diagnosis vs Mental Risk (Column Chart)
  output$prevDiagnosisColumn <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(prev_diagnosis_bin), !is.na(mental_risk_cat), !is.na(gender)) %>%
      group_by(prev_diagnosis_bin, mental_risk_cat, gender) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(prev_diagnosis_bin, gender) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=prev_diagnosis_bin, y=percent, fill=mental_risk_cat)) +
      geom_col(position="dodge") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_dodge(width=0.9), vjust=0.5, color = "white",size = 2.5, fontface = "bold") +
      facet_wrap(~gender) +
      scale_fill_manual(values=c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c")) +
      theme_minimal() +
      labs(title = NULL,
           x = "Diagnosis History",
           y = NULL,   # remove y-axis label
           fill = "Mental Risk Level") +
      theme_dark()+
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  # 4. Panic Attack History vs Mental Risk (Bar Chart)
  output$therapyBar <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(therapy_bin), !is.na(mental_risk_cat), !is.na(gender)) %>%
      group_by(therapy_bin, mental_risk_cat, gender) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(therapy_bin, gender) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=therapy_bin, y=percent, fill=mental_risk_cat)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_dodge(width=0.9), vjust=0.5, color = "white",size = 2.5, fontface = "bold") +
      facet_wrap(~gender) +
      scale_fill_manual(values=c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c")) +
      theme_dark() +
      labs(title = NULL,
           x = "Therapy History",
           y = NULL,   # remove y-axis label
           fill = "Mental Risk Level") +
      theme_dark()+
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  # 5. Panic Attack
  output$panicAttackColumnGender <- renderPlotly({
    df <- filteredData() %>%
      filter(!is.na(panic_attack_bin), !is.na(mental_risk_cat), !is.na(gender)) %>%
      group_by(panic_attack_bin, mental_risk_cat, gender) %>%
      summarise(count=n(), .groups="drop") %>%
      group_by(panic_attack_bin, gender) %>%
      mutate(percent = count/sum(count)*100)
    
    p <- ggplot(df, aes(x=panic_attack_bin, y=percent, fill=mental_risk_cat)) +
      geom_col(position="stack") +
      geom_text(aes(label=paste0(round(percent,1),"%")),
                position=position_stack(vjust = 0.5),color = "white",size = 2.5, fontface = "bold" ) +
      facet_wrap(~gender) +
      scale_fill_manual(values=c("Low"="#27ae60","Moderate"="#f39c12","High"="#e74c3c")) +
      theme_gray() +
      labs(title = NULL,
           x = "Panic Attack History",
           y = NULL,   # remove y-axis label
           fill = "Mental Risk Level") +
      theme_dark()+
      theme(axis.text.x = element_text(size = 8, face = "bold"),   # bold x labels
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(size = 8, face = "bold"),  # bold x title
            axis.title.y = element_text(size = 8, face = "bold"),  # bold y title
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.text = element_text(size = 8,  face = "bold"),              # bold legend text
            legend.title = element_text(size = 8, face = "bold"),             # bold legend title
            legend.key.size = unit(0.5, "lines")                    # reduce legend box size
      ) +
      guides(fill = guide_legend(keywidth = 0.9, keyheight = 0.9))
    
    ggplotly(p)
  })
  
  
  
  # Correlation heatmap (numeric variables)
  observeEvent(input$runContCat, {
    df1 <- filteredData()
    
    # Convert categorical variables to numeric codes
    df1$marital_status    <- as.numeric(as.factor(df1$marital_status)) - 1
    df1$employment_status <- as.numeric(as.factor(df1$employment_status)) - 1
    df1$gender            <- as.numeric(as.factor(df1$gender)) - 1
    df1$education_level   <- as.numeric(as.factor(df1$education_level)) - 1
    
    cont_vars <- df1 %>% select(all_of(input$cont_vars))
    cat_var   <- df1$mental_risk_cat
    
    output$contCatResult <- renderPrint({
      paste("Showing p-values directly on the plots (Mental Risk as categorical).")
    })
    
    output$contCatPlot <- renderPlot({
      # Compute p-values
      pvals <- sapply(names(cont_vars), function(v) {
        if (length(unique(cat_var)) > 2) {
          fit <- aov(cont_vars[[v]] ~ cat_var)
          summary(fit)[[1]][["Pr(>F)"]][1]
        } else {
          fit <- t.test(cont_vars[[v]] ~ cat_var)
          fit$p.value
        }
      })
      
      # Reshape to long format
      df_long <- df1 %>%
        select(all_of(input$cont_vars), mental_risk_cat) %>%
        tidyr::pivot_longer(cols = -mental_risk_cat,
                            names_to = "Variable",
                            values_to = "Value")
      
      # Build summary dataset with max per variable
      pval_df <- df_long %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(
          max_val = max(Value, na.rm = TRUE),
          pval = pvals[Variable[1]],
          .groups = "drop"
        )
      
      ggplot(df_long, aes(x = mental_risk_cat, y = Value, fill = mental_risk_cat)) +
        geom_boxplot(outlier.shape = NA) +
        facet_wrap(~ Variable, scales = "free_y", ncol = 5,
                   labeller = labeller(Variable = function(x) stringr::str_to_title(stringr::str_replace_all(x, "_", " ")))) +
        labs(x = "Mental Risk Category", y = "Value") +
        theme_dark() +
        theme(strip.text = element_text(size = 9, face = "bold"),
              axis.text.x = element_text(size = 7),
              legend.position = "none") +
        # Add p-value text per facet
        geom_text(
          data = pval_df,
          aes(x = 2, y = max_val,
              label = paste0("p-value =", signif(pval, 3)),
              colour = ifelse(pval < 0.05, "green", "white")),
          inherit.aes = FALSE,
          size = 3.5
        ) +
        scale_colour_identity()
    })
    
  })
  
  
  # Example: Compare Logistic Regression, Random Forest, XGBoost
  observeEvent(input$runModels, {
    # --- Data Preparation ---
    df2 <- filteredData()
    df2$mental_risk_num <- dplyr::recode(df2$mental_risk_cat,
                                         "Low" = 0,
                                         "Moderate" = 1,
                                         "High" = 2)
    
    predictors <- df2 %>% select(
      sleep_hours, physical_activity_hours_per_week,
      screen_time_hours_per_day, social_support_score,
      work_stress_level, financial_stress_level,
      working_hours_per_week, anxiety_score, depression_score, 
      panic_attack_history, family_history_mental_illness
    )
    target <- df2$mental_risk_num
    
    set.seed(123)
    trainIndex <- sample(seq_len(nrow(df2)), size = 0.7*nrow(df2))
    trainData <- predictors[trainIndex, ]
    trainTarget <- target[trainIndex]
    testData <- predictors[-trainIndex, ]
    testTarget <- target[-trainIndex]
    testTargetFactor <- factor(testTarget, levels=c(0,1,2))
    
    # --- Logistic Regression (Multinomial) ---
    logit_model <- nnet::multinom(trainTarget ~ ., data = data.frame(trainData, trainTarget))
    logit_pred <- predict(logit_model, newdata = testData, type="probs")
    colnames(logit_pred) <- c("0","1","2")
    logit_class <- apply(logit_pred, 1, which.max) - 1
    logit_acc <- mean(logit_class == testTarget)
    logit_auc <- pROC::multiclass.roc(testTargetFactor, logit_pred)$auc
    
    # Risk score = probability of High Risk
    colnames(logit_pred) <- c("0","1","2")
    logit_risk_scores <- logit_pred[,"2"]
    avg_logit <- mean(logit_risk_scores, na.rm=TRUE)
    
    # Confusion matrix for Logistic Regression
    logit_cm <- table(Predicted = logit_class, Actual = testTarget)
    
    # Rename rows and columns
    rownames(logit_cm) <- c("Low Risk", "Moderate Risk", "High Risk")
    colnames(logit_cm) <- c("Low Risk", "Moderate Risk", "High Risk")
    
    # Add "Predicted \ Actual" label in top-left
    logit_cm_df <- as.data.frame.matrix(logit_cm)
    logit_cm_df <- cbind("Predicted \\ Actual" = rownames(logit_cm_df), logit_cm_df)
    
    output$logitCM <- renderTable({
      logit_cm_df
    }, rownames = FALSE)
    
    # Feature importance (coefficients)
    output$logitImportance <- renderPlot({
      coefs <- coef(logit_model)
      importance_scores <- apply(abs(coefs), 2, mean, na.rm=TRUE)
      imp <- data.frame(Feature=names(importance_scores), Importance=importance_scores)
      # Remove intercept if present
      imp <- imp[imp$Feature != "(Intercept)", ]
      
      imp <- imp[order(imp$Importance, decreasing=TRUE),]
      
      nice_names <- c(
        sleep_hours = "Sleep Hours",
        physical_activity_hours_per_week = "Physical Activity (hrs/week)",
        screen_time_hours_per_day = "Screen Time (hrs/day)",
        social_support_score = "Social Support Score",
        work_stress_level = "Work Stress Level",
        financial_stress_level = "Financial Stress Level",
        working_hours_per_week = "Working Hours (per week)",
        anxiety_score = "Anxiety Score",
        depression_score = "Depression Score",
        panic_attack_history = "Panic Attack History",
        family_history_mental_illness = "Mental Illness History"
      )
      imp$Feature <- nice_names[imp$Feature]
      
      # Adjust margins so labels fit
      par(mar=c(2,10,2,2))  # bottom, left, top, right
      barplot(imp$Importance, names.arg=imp$Feature, las=1, las = 1,
              col = "skyblue",
              horiz = TRUE,
              main = NULL,
              cex.names = 0.9, 
              cex.main = 1,
              col.main = "#003366",     # bright orange title color
              adj = 0,                  # align title to the left
              xpd = FALSE                # allow drawing outside plot region
      )        # shrink label size slightly
    })
    
    # ROC curve with metrics
    # Logistic Regression ROC curve
    # Logistic Regression ROC curve
    output$logitROC <- renderPlot({
      roc_obj <- pROC::multiclass.roc(testTargetFactor, logit_pred, plot=TRUE, col=rainbow(3))
      
      # Add text with metrics directly on the chart
      text(x=0.6, y=0.2,
           labels=paste("AUC =", round(logit_auc,3),
                        "\nAccuracy =", round(logit_acc,3)),
           col="black", cex=1.1, adj=0)
    })
    
    # --- Random Forest ---
    rf_model <- randomForest::randomForest(x=trainData, y=as.factor(trainTarget), ntree=200)
    rf_pred <- predict(rf_model, newdata=testData, type="prob")
    colnames(rf_pred) <- c("0","1","2")
    rf_class <- predict(rf_model, newdata=testData)
    rf_acc <- mean(as.numeric(rf_class)-1 == testTarget)
    rf_auc <- pROC::multiclass.roc(testTargetFactor, rf_pred)$auc
    
    # Risk score = probability of High Risk
    colnames(rf_pred) <- c("0","1","2")
    rf_risk_scores <- rf_pred[,"2"]
    avg_rf <- mean(rf_risk_scores, na.rm=TRUE)
    
    # Confusion matrix
    # Confusion matrix for Random Forest
    rf_cm <- table(Predicted = as.numeric(rf_class)-1, Actual = testTarget)
    
    rownames(rf_cm) <- c("Low Risk", "Moderate Risk", "High Risk")
    colnames(rf_cm) <- c("Low Risk", "Moderate Risk", "High Risk")
    
    rf_cm_df <- as.data.frame.matrix(rf_cm)
    rf_cm_df <- cbind("Predicted \\ Actual" = rownames(rf_cm_df), rf_cm_df)
    
    output$rfCM <- renderTable({
      rf_cm_df
    }, rownames = FALSE)
    
    # --- InfoBox Output ---
    output$modelRiskScore  <- renderInfoBox({
      infoBox(
        "Model Risk Scores (Average)",
        HTML(
          paste0(
            "<div style='display:flex; justify-content:space-around;'>",
            "<div style='flex:1; text-align:center; padding-right:10px; border-right:2px solid #ccc;'>",
            "<span style='color:#e67e22; font-weight:bold; white-space:nowrap;'>📈 Logistic</span><br>",
            round(avg_logit * 100, 1), "%",   # convert to %
            "</div>",
            "<div style='flex:1; text-align:center; padding-left:10px;'>",
            "<span style='color:#27ae60; font-weight:bold; white-space:nowrap;'>🌲 Random Forest</span><br>",
            round(avg_rf * 100, 1), "%",      # convert to %
            "</div>",
            "</div>"
          )
        ),
        icon = icon("balance-scale"),
        color = "light-blue",
        fill = TRUE
      )
    })
    
    
    # Feature importance with nice names
    output$rfImportance <- renderPlot({
      importance_scores <- importance(rf_model)[,1]
      imp <- data.frame(Feature=names(importance_scores), Importance=importance_scores)
      imp <- imp[order(imp$Importance, decreasing=TRUE),]
      
      nice_names <- c(
        sleep_hours = "Sleep Hours",
        physical_activity_hours_per_week = "Physical Activity (hrs/week)",
        screen_time_hours_per_day = "Screen Time (hrs/day)",
        social_support_score = "Social Support",
        work_stress_level = "Work Stress",
        financial_stress_level = "Financial Stress",
        working_hours_per_week = "Working Hours (per week)",
        anxiety_score = "Anxiety",
        depression_score = "Depression",
        panic_attack_history = "Panic Attack History",
        family_history_mental_illness = "Mental Illness History"
      )
      imp$Feature <- nice_names[imp$Feature]
      
      # Adjust margins so labels fit
      par(mar=c(2,10,2,2))  # bottom, left, top, right
      
      barplot(
        imp$Importance,
        names.arg = imp$Feature,
        las = 1,
        col = "skyblue",
        horiz = TRUE,
        main =NULL,
        cex.names = 0.9, 
        cex.main = 1,
        col.main = "#003366",     # bright orange title color
        adj = 0,                  # align title to the left
        xpd = FALSE                # allow drawing outside plot region
      )
    })
    
    # ROC curve with metrics
    output$rfROC <- renderPlot({
      roc_obj <- pROC::multiclass.roc(testTargetFactor, rf_pred, plot=TRUE, col=rainbow(3))
      
      # Add text with metrics directly on the chart
      text(x=0.6, y=0.2,
           labels=paste("AUC =", round(rf_auc,3),
                        "\nAccuracy =", round(rf_acc,3)),
           col="black", cex=1.1, adj=0)
    })
    
    # Decision tree visualization in its own box
    output$rfTree <- renderPlot({
      library(rpart)
      library(rpart.plot)
      
      # Define a consistent palette for the three classes
      risk_colors <- c("Low Risk" = "#27ae60",
                       "Moderate Risk" = "#f39c12",
                       "High Risk" = "#e74c3c")
      
      # Build a simplified tree
      tree_model <- rpart(trainTarget ~ sleep_hours + work_stress_level + anxiety_score,
                          data = data.frame(trainData, trainTarget),
                          method = "class")
      
      # Plot with bold labels and matching palette
      rpart.plot(tree_model,
                 main = NULL,
                 type = 2,                 # split labels below the node
                 extra = 104,              # show fitted class + prob
                 under = TRUE,             # put prob under node
                 faclen = 0,               # don't abbreviate factor names
                 cex = 0.9,                # text size
                 tweak = 1.2,              # enlarge text slightly
                 fallen.leaves = TRUE,     # compact layout
                 box.palette = list(risk_colors["Low Risk"],
                                    risk_colors["Moderate Risk"],
                                    risk_colors["High Risk"]),
                 shadow.col = "gray",      # shadow for boxes
                 split.cex = 1.1,          # split text size
                 split.font = 2,           # bold split labels
                 branch.lty = 1)           # solid branches
      
      # Add legend with matching colors
      legend(
        "topright",
        legend = names(risk_colors),
        fill   = risk_colors,
        border = "black",
        bty    = "n",
        cex    = 0.9,
        # push legend slightly outside plot area
        xpd    = TRUE,          # allow drawing outside plot region
        y.intersp = 1.2         # increase vertical spacing between items
      )
    })
    
    
    # --- Compute Precision & NPV from confusion matrices ---
    computeMetrics <- function(cm) {
      TP <- diag(cm)
      FP <- rowSums(cm) - TP
      FN <- colSums(cm) - TP
      TN <- sum(cm) - (TP + FP + FN)
      
      precision <- mean(TP / (TP + FP), na.rm=TRUE)   # PPV
      npv <- mean(TN / (TN + FN), na.rm=TRUE)         # NPV
      
      list(precision=precision, npv=npv)
    }
    
    logit_metrics <- computeMetrics(logit_cm)
    rf_metrics <- computeMetrics(rf_cm)
    
    # --- Comparison Table ---
    comparison <- data.frame(
      Model = c("Logistic Regression","Random Forest"),
      AUC = c(logit_auc, rf_auc),
      Accuracy = c(logit_acc, rf_acc),
      Precision = c(logit_metrics$precision, rf_metrics$precision),
      NPV = c(logit_metrics$npv, rf_metrics$npv)
    )
    
    output$modelComparison <- renderTable({
      # Transpose and fix column names
      t_comp <- t(comparison[,-1])              # transpose all but Model column
      colnames(t_comp) <- comparison$Model      # set column names to model names
      rownames(t_comp) <- colnames(comparison)[-1]  # set row names to metric names
      
      as.data.frame(t_comp)
    }, rownames = TRUE)
    
    # --- Comparison Bar Chart ---
    output$modelComparisonPlot <- renderPlot({
      library(reshape2)
      library(ggplot2)
      
      # Reshape to long format
      df_long <- melt(comparison, id.vars="Model",
                      variable.name="Metric", value.name="Value")
      
      ggplot(df_long, aes(x = Metric, y = Value, fill = Model)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        geom_text(aes(label = sprintf("%.2f", Value)),   # show values with 2 decimals
                  position = position_dodge(width = 0.9),
                  vjust = 0.5,                           # center inside bar
                  color = "white",
                  size = 4,
                  fontface = "bold") +
        labs(title=NULL,
             x="Metric", y=NULL) +
        theme_dark() +
        theme(axis.text.x = element_text(size = 11, face = "bold"),   # bold x labels
              axis.text.y = element_blank(),   # bold y labels
              axis.title.x = element_text(size = 13, face = "bold"),  # bold x title
              axis.title.y = element_text(size = 13, face = "bold"),  # bold y title
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.position = "top",
              axis.ticks.y = element_blank(),
              legend.text = element_text(face = "bold", size = 12),              # bold legend text
              legend.title = element_text(face = "bold", size = 14),             # bold legend title
              legend.key.size = unit(0.5, "lines")                    # reduce legend box size
        ) +
        guides(fill = guide_legend(keywidth = 0.6, keyheight = 0.6))
    })
    
    # --- Top Features Comparison with Shared % Axis ---
    output$topFeaturesComparison <- renderPlot({
      # Logistic Regression importance
      coefs <- coef(logit_model)
      importance_scores <- apply(abs(coefs), 2, mean, na.rm=TRUE)
      imp_logit <- data.frame(Feature=names(importance_scores), Importance=importance_scores)
      imp_logit <- imp_logit[imp_logit$Feature != "(Intercept)", ]
      imp_logit <- imp_logit[order(imp_logit$Importance, decreasing=TRUE), ]
      top_logit <- head(imp_logit, 7)
      top_logit$Importance <- 100 * top_logit$Importance / sum(top_logit$Importance)
      top_logit$Model <- "Logistic Regression"
      
      # Random Forest importance
      importance_scores_rf <- importance(rf_model)[,1]
      imp_rf <- data.frame(Feature=names(importance_scores_rf), Importance=importance_scores_rf)
      imp_rf <- imp_rf[order(imp_rf$Importance, decreasing=TRUE), ]
      top_rf <- head(imp_rf, 7)
      top_rf$Importance <- 100 * top_rf$Importance / sum(top_rf$Importance)
      top_rf$Model <- "Random Forest"
      
      # Combine
      df <- rbind(top_logit, top_rf)
      
      # Define nice names mapping
      nice_names <- c(
        sleep_hours = "Sleep Hours",
        physical_activity_hours_per_week = "Physical Activity (hrs/week)",
        screen_time_hours_per_day = "Screen Time (hrs/day)",
        social_support_score = "Social Support",
        work_stress_level = "Work Stress",
        academic_pressure_level = "Academic Pressure",
        job_satisfaction_score = "Job Satisfaction",
        financial_stress_level = "Financial Stress",
        working_hours_per_week = "Working Hours (per week)",
        anxiety_score = "Anxiety Score",
        depression_score = "Depression Score",
        stress_level = "Stress Level",
        mood_swings_frequency = "Mood Swings",
        concentration_difficulty_level = "Concentration Difficulty",
        panic_attack_history = "Panic Attack History",
        family_history_mental_illness = "Mental Illness History",
        previous_mental_health_diagnosis = "Previous Diagnosis",
        therapy_history = "Therapy History",
        substance_use = "Substance Use"
      )
      
      # Replace technical names with nice names if available
      df$Feature <- ifelse(df$Feature %in% names(nice_names),
                           nice_names[df$Feature],
                           df$Feature)
      
      ggplot(df, aes(x=Importance, y=reorder(Feature, Importance), fill=Model)) +
        geom_bar(stat="identity", position="dodge") +
        scale_x_continuous(limits=c(0,100)) +
        labs(title=NULL,
             x="Importance (%)", y="Feature") +
        theme_dark() +
        theme(axis.text.x = element_text(size = 11, face = "bold"),   # bold x labels
              axis.text.y = element_text(size = 10, face = "bold"),   # bold y labels
              axis.title.x = element_text(size = 13, face = "bold"),  # bold x title
              axis.title.y = element_text(size = 13, face = "bold"),  # bold y title
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.position = "top",
              legend.text = element_text(face = "bold", size = 10),              # bold legend text
              legend.title = element_text(face = "bold", size = 11),             # bold legend title
              legend.key.size = unit(0.5, "lines")                    # reduce legend box size
        ) +
        guides(fill = guide_legend(keywidth = 0.6, keyheight = 0.6))
    })
  })
  
  
  
  # Raw data table
  output$rawDataTable <- DT::renderDataTable({
    DT::datatable(filteredData(),
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)`;

const R_SCRIPT_SNIPPET2 =``


const PYTHON_SCRIPT_SNIPPET = `import pandas as pd

df = pd.read_csv("meal_data.csv")

summary = (
    df.assign(score=pd.to_numeric(df["score"], errors="coerce"))
      .groupby("region", as_index=False)
      .agg(mean_score=("score", "mean"), records=("score", "size"))
)

print(summary.head())
`;

const SQL_QUERY_SNIPPET = `SELECT
  region,
  COUNT(*) AS records,
  AVG(score) AS mean_score
FROM meal_data
WHERE score IS NOT NULL
GROUP BY region
ORDER BY mean_score DESC;
`;

const DATABRICKS_SNIPPET = `from pyspark.sql.functions import col, avg, count

df = spark.table("meal_data")

summary = (
    df.select(col("region"), col("score").cast("double").alias("score"))
      .groupBy("region")
      .agg(avg("score").alias("mean_score"), count("*").alias("records"))
)

display(summary)
`;

function resolveAssetSrc(src) {
  const value = String(src || '').trim();
  if (!value) return '';
  if (/^(?:[a-z][a-z0-9+.-]*:|\/\/)/i.test(value)) return value;
  return encodeURI(value);
}

function makeDocUrl(kind, title, subtitle) {
  const t = esc(title || 'Project Preview');
  const s = esc(subtitle || '');
  let body = '';

  if (kind === 'app') {
  const dashboardUrl = 'Mental Health Risk.R'; 
  // Put your exported Shiny HTML file in /public/dashboards/

  body = `
    <main class="frame-shell frame-app">
      <div class="frame-kicker">Live App</div>
      <h1>${t}</h1>
      <p class="frame-sub">${s}</p>

      <!-- Embed the dashboard -->
      <iframe 
        src="${dashboardUrl}" 
        width="100%" 
        height="600px" 
        frameborder="0">
      </iframe>

      <!-- Optional: keep your metrics panel below -->
      <div class="metric-grid">
        <div class="metric"><strong>94%</strong><span>Coverage</span></div>
        <div class="metric"><strong>128</strong><span>Records</span></div>
        <div class="metric"><strong>18</strong><span>Indicators</span></div>
      </div>
      <div class="bar-panel">
        <div class="bar-row"><span>Data quality</span><div class="bar"><i style="width:92%"></i></div></div>
        <div class="bar-row"><span>Timeliness</span><div class="bar"><i style="width:86%"></i></div></div>
        <div class="bar-row"><span>Completeness</span><div class="bar"><i style="width:78%"></i></div></div>
      </div>
    </main>`;
} else if (kind === 'books') {
    body = `
      <main class="frame-shell frame-presentation">
        <div class="frame-kicker">Spiritual Books</div>
        <h1>${t}</h1>
        <p class="frame-sub">${s}</p>
        <div class="slide-grid">
          <section class="slide-card">
            <span>01</span>
            <h2>Reflection</h2>
            <p>Reading notes, quiet study prompts, and personal journal reflections.</p>
          </section>
          <section class="slide-card">
            <span>02</span>
            <h2>Wisdom</h2>
            <p>Short passages that help frame decisions, discipline, and growth.</p>
          </section>
          <section class="slide-card">
            <span>03</span>
            <h2>Practice</h2>
            <p>Simple actions to carry the reading into the day with purpose.</p>
          </section>
        </div>
      </main>`;
  } else if (kind === 'docs') {
    body = `
      <main class="frame-shell frame-docs">
        <div class="frame-kicker">Documentation</div>
        <h1>${t}</h1>
        <p class="frame-sub">${s}</p>
        <section class="doc-section">
          <h2>Overview</h2>
          <p>Attached reference notes, assumptions, and delivery context for the project asset.</p>
        </section>
        <section class="doc-section">
          <h2>Key Points</h2>
          <ul>
            <li>Clear section structure for review.</li>
            <li>Readable summary of the file purpose.</li>
            <li>Reusable layout for project attachments.</li>
          </ul>
        </section>
      </main>`;
  } else if (kind === 'video') {
    body = `
      <main class="frame-shell frame-app">
        <div class="frame-kicker">Videos</div>
        <h1>${t}</h1>
        <p class="frame-sub">${s}</p>
        <div class="metric-grid">
          <div class="metric"><strong>03</strong><span>Clips</span></div>
          <div class="metric"><strong>12m</strong><span>Runtime</span></div>
          <div class="metric"><strong>04</strong><span>Topics</span></div>
        </div>
        <div class="bar-panel">
          <div class="bar-row"><span>Opening</span><div class="bar"><i style="width:88%"></i></div></div>
          <div class="bar-row"><span>Message</span><div class="bar"><i style="width:76%"></i></div></div>
          <div class="bar-row"><span>Reflection</span><div class="bar"><i style="width:68%"></i></div></div>
        </div>
        <section class="doc-section">
          <h2>Playlist</h2>
          <ul>
            <li>Morning devotion</li>
            <li>Scripture reflection</li>
            <li>Weekly teaching clip</li>
          </ul>
        </section>
      </main>`;
  } else {
    body = `
      <main class="frame-shell frame-presentation">
        <div class="frame-kicker">Presentation</div>
        <h1>${t}</h1>
        <p class="frame-sub">${s}</p>
        <div class="slide-grid">
          <section class="slide-card">
            <span>01</span>
            <h2>Context</h2>
            <p>Why this project matters and what the attached material covers.</p>
          </section>
          <section class="slide-card">
            <span>02</span>
            <h2>Process</h2>
            <p>How the analysis, reporting, or delivery workflow is organized.</p>
          </section>
          <section class="slide-card">
            <span>03</span>
            <h2>Outcome</h2>
            <p>The final output, insight, or product that the project section showcases.</p>
          </section>
        </div>
      </main>`;
  }

  const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
    :root {
      color-scheme: dark;
      --bg: #0d1117;
      --bg2: #111827;
      --bg3: #1f2937;
      --text: #e5eef7;
      --text2: #b6c2d0;
      --teal: #2dd4bf;
      --amber: #f59e0b;
      --border: rgba(148, 163, 184, 0.2);
      font-family: Inter, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      min-height: 100vh;
      background:
        radial-gradient(circle at top right, rgba(45,212,191,.12), transparent 28%),
        linear-gradient(135deg, var(--bg), var(--bg2));
      color: var(--text);
    }
    .frame-shell {
      padding: 18px;
      min-height: 100vh;
    }
    .frame-kicker {
      display: inline-flex;
      align-items: center;
      gap: .35rem;
      font-size: .68rem;
      font-weight: 800;
      letter-spacing: .08em;
      text-transform: uppercase;
      color: var(--teal);
      margin-bottom: .7rem;
    }
    h1 {
      margin: 0 0 .35rem;
      font-size: 1.2rem;
      line-height: 1.2;
    }
    .frame-sub {
      margin: 0 0 1rem;
      color: var(--text2);
      font-size: .82rem;
      line-height: 1.6;
      max-width: 760px;
    }
    .slide-grid,
    .metric-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
      gap: .75rem;
      margin-top: 1rem;
    }
    .slide-card,
    .metric {
      border: 1px solid var(--border);
      border-radius: 10px;
      background: rgba(17,24,39,.85);
      padding: .9rem;
      box-shadow: 0 10px 24px rgba(0,0,0,.25);
    }
    .slide-card span {
      display: inline-block;
      margin-bottom: .35rem;
      font-size: .66rem;
      font-weight: 800;
      color: var(--amber);
      letter-spacing: .08em;
    }
    .slide-card h2 {
      margin: 0 0 .35rem;
      font-size: .92rem;
    }
    .slide-card p,
    .metric span,
    .doc-section p,
    .doc-section li,
    .bar-row span {
      margin: 0;
      color: var(--text2);
      font-size: .78rem;
      line-height: 1.55;
    }
    .metric strong {
      display: block;
      font-size: 1.2rem;
      margin-bottom: .15rem;
      color: var(--text);
    }
    .bar-panel,
    .doc-section {
      margin-top: 1rem;
      border: 1px solid var(--border);
      border-radius: 10px;
      background: rgba(15,23,42,.72);
      padding: .9rem;
    }
    .bar-row {
      display: grid;
      gap: .35rem;
      margin-bottom: .7rem;
    }
    .bar-row:last-child { margin-bottom: 0; }
    .bar {
      height: 8px;
      border-radius: 999px;
      background: rgba(148,163,184,.18);
      overflow: hidden;
    }
    .bar i {
      display: block;
      height: 100%;
      border-radius: inherit;
      background: linear-gradient(90deg, var(--teal), #60a5fa);
    }
    .doc-section h2 {
      margin: 0 0 .35rem;
      font-size: .92rem;
      color: var(--text);
    }
    .doc-section ul {
      margin: .35rem 0 0 1.1rem;
      padding: 0;
    }
    .doc-section li { margin: .2rem 0; }
  </style>
</head>
<body>${body}</body>
</html>`;

  return 'data:text/html;charset=utf-8,' + encodeURIComponent(html);
}



const PROJECT_SECTION_CONFIGS = [
  {
    panelId: 'meal-word',
    group: 'meal-word-stack',
    headerTitle: 'Word Reader',
    headerDesc: 'A single built-in Word document preview attached in code with no pasted links or uploads.',
    tabs: [
      {
        id: 'meal-word-preview',
        label: 'Preview',
        title: 'Word Document Preview',
        kind: 'doc',
        note: 'Static Word-style document preview rendered directly from code.',
      },
    ],
  },
  {
    panelId: 'meal-pdf',
    group: 'meal-pdf-stack',
    headerTitle: 'Reports Templates',
    headerDesc: 'Browse ready-to-use MEAL reporting templates — quarterly and annual progress reports, IPPT summary sheets, donor reporting formats, and indicator narrative templates — structured to plug directly into the underlying M&amp;E framework and tracker data.',
    tabs: [
      {
        id: 'meal-pdf-pdf1',
        label: 'School Feeding Program Report',
        title: 'Annual Report Template',
        kind: 'pdf',
        src: 'https://juma-maurice.shinyapps.io/mental_health_risk/',
      },
      {
        id: 'meal-pdf-pdf2',
        label: 'School Feeding Program Dashboard Report',
        title: 'Dashboard Report Template',
        kind: 'pdf',
        src: 'Mental-Health-Risk-Report.pdf',
      },
      {
        id: 'meal-pdf-report',
        label: 'Report',
        title: 'Report',
        kind: 'doc',
        docKind: 'docs',
        src: 'Mental-Health-Risk-Report.pdf',
        note: 'Local report preview rendered as an iframe document.',
      },
      {
        id: 'meal-pdf-presentation',
        label: 'Presentation',
        title: 'Presentation',
        kind: 'doc',
        docKind: 'presentation',
        note: 'Local presentation preview rendered as an iframe slide deck.',
      },
      {
        id: 'meal-pdf-screenshots',
        label: 'screenshots',
        title: 'Screenshots',
        kind: 'image',
        src: 'Where Data Meets Code.png',
        note: 'Local screenshot preview.',
      },
      {
        id: 'meal-pdf-posters',
        label: 'Posters',
        title: 'Posters',
        kind: 'image',
        src: 'Banner.jpeg',
        note: 'Local poster preview.',
      },
      {
        id: 'meal-pdf-infographics',
        label: 'Infographics',
        title: 'Infographics',
        kind: 'image',
        src: 'Profile Photo2.png',
        note: 'Local infographic preview.',
      },
    ],
  },
  {
  panelId: 'r-script',
  group: 'r-script-stack',
  headerTitle: 'R Scripts: Mental Health Risk Assessment and Smartphone Addiction Behavioral Modelling',
  headerDesc: 'This collection presents R-based statistical modelling projects, each examining a distinct dimension of behavioral and psychological wellbeing. The first investigates how demographic, lifestyle, and clinical indicators combine to predict an individual\'s risk of experiencing mental health difficulties, using regression-based classification techniques to surface the strongest contributing factors. The second turns to digital behavior, analyzing smartphone usage patterns — frequency, duration, and context of use — to model the likelihood of addictive or compulsive usage tendencies. Together, the two scripts illustrate complementary approaches to risk modelling: one rooted in clinical and demographic survey data, the other in behavioral and usage-log data, both aimed at identifying early warning signals that could inform intervention or further research.',
  tabs: [
    {
      id: 'r-script-code1',
      label: 'Mental Health Risk Prediction',
      title: 'Mental Health Risk Prediction Using Demographic and Lifestyle Factor Analysis',
      kind: 'code',
      language: 'r',
      code: R_SCRIPT_SNIPPET1
    },
    {
      id: 'r-script-code2',
      label: 'Smartphone Addiction Modelling',
      title: 'Smartphone Usage Patterns and Behavioral Addiction Risk Modelling',
      kind: 'code',
      language: 'r',
      code: R_SCRIPT_SNIPPET2
     },
      {
        id: 'r-script-report',
        label: 'Report',
        title: 'Report',
        kind: 'pdf',
        src: 'Mental-Health-Risk-Report.pdf',
      },
      {
        id: 'r-script-presentation',
        label: 'Presentation',
        title: 'Presentation',
        kind: 'doc',
        docKind: 'presentation',
      },
    ],
  },
  {
    panelId: 'r-pdf',
    group: 'r-pdf-stack',
    headerTitle: 'R Markdown PDF Reports',
    headerDesc: 'Three attached views for R reports for data modelling.',
    tabs: [
      {
        id: 'r-pdf-analysis',
        label: 'Report on Mental Health Risk Modelling',
        title: 'Mental Health Risk Modelling',
        kind: 'pdf',
        src: 'Mental-Health-Risk-Report.pdf',
      },
      {
        id: 'r-pdf-paper',
        label: 'Report on Smartphone Usage and Addiction Modelling',
        title: 'Smartphone Usage and Addiction Modelling',
        kind: 'pdf',
        src: 'Smartphone-Usage-Addiction-Report.pdf',
      },
      {
        id: 'r-pdf-output',
        label: 'Statistical Output',
        title: 'Statistical Output',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
    ],
  },
  {
    panelId: 'r-slides',
    group: 'r-slides-stack',
    headerTitle: 'R Slides and Poster Assets',
    headerDesc: 'Presentation-ready views for slides, poster assets, and infographic-style visuals.',
    tabs: [
      {
        id: 'r-slides-presentation1',
        label: 'Mental Health Risk Modelling Presentation',
        title: 'Presentation',
        kind: 'pdf',
        src: 'Mental-Health-Risk-Presentation.pdf',
      },
      {
        id: 'r-slides-presentation2',
        label: 'Smartphone Usage and Addiction Modelling Presentation',
        title: 'Presentation',
        kind: 'pdf',
        src: 'Smartphone-Usage-Addiction-Presentation.pdf',
      },
      {
        id: 'r-slides-presentation',
        label: 'Presentation',
        title: 'Presentation',
        kind: 'doc',
        docKind: 'presentation',
      },
      {
        id: 'r-slides-poster',
        label: 'Poster',
        title: 'Poster',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
      {
        id: 'r-slides-infographic',
        label: 'Infographic',
        title: 'Infographic',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
    ],
  },
  {
    panelId: 'r-shiny',
    group: 'r-shiny-stack',
    headerTitle: 'Shiny Dashboard',
    headerDesc: 'A live-app style preview, a screenshot, and the supporting documentation.',
    tabs: [
      {
        id: 'r-shiny-live1',
        label: 'Mental Health Risk Dashboard',
        title: 'Mental Health Risk Dashboard',
        kind: 'pdf',
        src: 'Mental-Health-Risk-Dashboard.pdf',
      },{
        id: 'r-shiny-live2',
        label: 'Smartphone Usage and Addiction Dashboard',
        title: 'Smartphone Usage and Addiction Dashboard',
        kind: 'pdf',
        src: 'Smartphone-Usage-Addiction-Dashboard.pdf',
      },
      {
        id: 'r-shiny-live3', 
        label: 'Live App',
        title: 'Live App',
        kind: 'doc',
        docKind: 'app',
      },
      {
        id: 'r-shiny-screenshot',
        label: 'Screenshot',
        title: 'Screenshot',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
      {
        id: 'r-shiny-docs',
        label: 'Documentation',
        title: 'Documentation',
        kind: 'doc',
        docKind: 'docs',
      },
    ],
  },
  {
    panelId: 'py-script',
    group: 'py-script-stack',
    headerTitle: 'Python Script Reader',
    headerDesc: 'Three attached views for Python code, report, and presentation material.',
    tabs: [
      {
        id: 'py-script-code',
        label: 'Python Script/codes',
        title: 'Python Script / Codes',
        kind: 'code',
        language: 'python',
        code: PYTHON_SCRIPT_SNIPPET,
      },
      {
        id: 'py-script-report',
        label: 'Report',
        title: 'Report',
        kind: 'pdf',
        src: 'Mental-Health-Risk-Report.pdf',
      },
      {
        id: 'py-script-presentation',
        label: 'Presentation',
        title: 'Presentation',
        kind: 'doc',
        docKind: 'presentation',
      },
    ],
  },
  {
    panelId: 'py-pdf',
    group: 'py-pdf-stack',
    headerTitle: 'Python PDF Reader',
    headerDesc: 'Three attached views for Python analysis reports, research papers, and outputs.',
    tabs: [
      {
        id: 'py-pdf-analysis',
        label: 'Analysis Report',
        title: 'Analysis Report',
        kind: 'pdf',
        src: 'Mental-Health-Risk-Report.pdf',
      },
      {
        id: 'py-pdf-paper',
        label: 'Research Paper',
        title: 'Research Paper',
        kind: 'doc',
        docKind: 'docs',
      },
      {
        id: 'py-pdf-output',
        label: 'Statistical Output',
        title: 'Statistical Output',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
    ],
  },
  {
    panelId: 'py-slides',
    group: 'py-slides-stack',
    headerTitle: 'Python Slides Reader',
    headerDesc: 'Presentation, poster, and infographic views for Python-based storytelling.',
    tabs: [
      {
        id: 'py-slides-presentation',
        label: 'Presentation',
        title: 'Presentation',
        kind: 'doc',
        docKind: 'presentation',
      },
      {
        id: 'py-slides-poster',
        label: 'Poster',
        title: 'Poster',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
      {
        id: 'py-slides-infographic',
        label: 'Infographic',
        title: 'Infographic',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
    ],
  },
  {
    panelId: 'py-dashboard',
    group: 'py-dashboard-stack',
    headerTitle: 'Python Dashboard Viewer',
    headerDesc: 'A live-app style dashboard, a screenshot, and its supporting documentation.',
    tabs: [
      {
        id: 'py-dashboard-live',
        label: 'Live App',
        title: 'Live App',
        kind: 'doc',
        docKind: 'app',
      },
      {
        id: 'py-dashboard-screenshot',
        label: 'Screenshot',
        title: 'Screenshot',
        kind: 'image',
        src: 'Where Data Meets Code.png',
      },
      {
        id: 'py-dashboard-docs',
        label: 'Documentation',
        title: 'Documentation',
        kind: 'doc',
        docKind: 'docs',
      },
    ],
  },
  {
    panelId: 'cat-powerbi',
    group: 'powerbi-stack',
    heroTitle: 'Power BI Projects',
    heroDesc: 'Interactive dashboard attachments with PDF, report, presentation, screenshot, poster, and infographic views.',
    tabs: [
      { id: 'powerbi-pdf', label: 'PDF', title: 'PDF', kind: 'pdf', src: 'Mental-Health-Risk-Report.pdf' },
      { id: 'powerbi-report', label: 'Report', title: 'Report', kind: 'doc', docKind: 'docs' },
      { id: 'powerbi-presentation', label: 'Presentation', title: 'Presentation', kind: 'doc', docKind: 'presentation' },
      { id: 'powerbi-screenshots', label: 'Screenshots', title: 'Screenshots', kind: 'image', src: 'Where Data Meets Code.png' },
      { id: 'powerbi-posters', label: 'Posters', title: 'Posters', kind: 'image', src: 'Where Data Meets Code.png' },
      { id: 'powerbi-infographics', label: 'Infographics', title: 'Infographics', kind: 'image', src: 'Where Data Meets Code.png' },
    ],
  },
  {
    panelId: 'cat-sql',
    group: 'sql-stack',
    heroTitle: 'SQL Projects',
    heroDesc: 'Query outputs, code snippets, presentation notes, and screenshots for SQL work.',
    tabs: [
      { id: 'sql-pdf', label: 'PDF', title: 'PDF', kind: 'pdf', src: 'Mental-Health-Risk-Report.pdf' },
      { id: 'sql-code', label: 'Codes/Queries', title: 'Codes / Queries', kind: 'code', language: 'sql', code: SQL_QUERY_SNIPPET },
      { id: 'sql-presentation', label: 'Presentation', title: 'Presentation', kind: 'doc', docKind: 'presentation' },
      { id: 'sql-screenshots', label: 'Screenshots', title: 'Screenshots', kind: 'image', src: 'Where Data Meets Code.png' },
    ],
  },
  {
    panelId: 'cat-databricks',
    group: 'db-stack',
    heroTitle: 'Databricks Projects',
    heroDesc: 'Notebook code, PDF notes, presentation material, and screenshot views for Databricks work.',
    tabs: [
      { id: 'db-pdf', label: 'PDF', title: 'PDF', kind: 'pdf', src: 'Mental-Health-Risk-Report.pdf' },
      { id: 'db-code', label: 'Codes/Queries', title: 'Codes / Queries', kind: 'code', language: 'python', code: DATABRICKS_SNIPPET },
      { id: 'db-presentation', label: 'Presentation', title: 'Presentation', kind: 'doc', docKind: 'presentation' },
      { id: 'db-screenshots', label: 'Screenshots', title: 'Screenshots', kind: 'image', src: 'Where Data Meets Code.png' },
    ],
  },
];


function buildProjectSections() {
  PROJECT_SECTION_CONFIGS.forEach(section => {
    const panel = document.getElementById(section.panelId);
    if (!panel || panel.dataset.stackBuilt === '1') return;

    const group = section.group;

    const hero = section.heroTitle
      ? `<div class="cat-hero"><h3>${section.heroTitle}</h3><p>${section.heroDesc || ''}</p></div>`
      : '';

    const header = section.headerTitle
      ? `<div class="ip-card-header"><h3>${section.headerTitle}</h3><p class="proj-desc">${section.headerDesc || ''}</p></div>`
      : '';

    const body = section.tabs.length === 1
      ? `
        <div class="reader-card stack-card">
          <div class="stack-header">
            <h3>${section.tabs[0].title || section.tabs[0].label}</h3>
            <p class="proj-desc">${section.tabs[0].note || section.tabs[0].label}</p>
          </div>
          ${renderProjectAsset(section.tabs[0])}
        </div>`
      : `
      <div class="tab-scroller">
        <div class="tab-bar" id="${section.panelId}-stack-tabs">
          ${section.tabs.map((tab, index) => `
            <button class="tab-btn${index === 0 ? ' active' : ''}" data-group="${group}" data-target="${tab.id}">${tab.label}</button>
          `).join('')}
        </div>
      </div>
      ${section.tabs.map((tab, index) => `
      <div class="tab-panel ${group}${index === 0 ? ' active' : ''}" id="${tab.id}">
        <div class="reader-card stack-card">
          <div class="stack-header">
            <h3>${tab.title || tab.label}</h3>
            <p class="proj-desc">${tab.note || tab.label}</p>
          </div>
          ${renderProjectAsset(tab)}
        </div>
      </div>`).join('')}`;

    panel.innerHTML = `${hero}${header}${body}`;
    panel.dataset.stackBuilt = '1';
  });
}

function renderProjectAsset(tab) {
  if (tab.kind === 'code') {
    return `
      <div class="code-win stack-code">
        <div class="code-bar">
          <span class="dot red-dot"></span><span class="dot yellow-dot"></span><span class="dot green-dot"></span>
          <span class="code-win-title">${tab.title}</span>
        </div>
        <pre class="code-pre"><code class="language-${tab.language || 'text'}">${esc(tab.code || '')}</code></pre>
      </div>
      <p class="stack-note">Embedded directly in the page for review without upload or link entry.</p>`;
  }

  if (tab.kind === 'pdf') {
    return `
      <iframe class="doc-frame stack-frame" title="${tab.title}" src="${esc(resolveAssetSrc(tab.src || ''))}"></iframe>
      <p class="stack-note">Attached PDF preview.</p>`;
  }

  if (tab.kind === 'image') {
    return `
      <img class="stack-image" src="${esc(resolveAssetSrc(tab.src || ''))}" alt="${tab.title}">
      <p class="stack-note">Attached image preview.</p>`;
  }

  if (tab.kind === 'doc') {
    return `
      <iframe class="doc-frame stack-frame" title="${tab.title}" src="${esc(makeDocUrl(tab.docKind || 'presentation', tab.title, tab.note || tab.label))}"></iframe>
      <p class="stack-note">Inline iframe document built in code.</p>`;
  }

  return `<div class="stack-note">No preview available.</div>`;
}

document.addEventListener('DOMContentLoaded', buildProjectSections);
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

/* ══════════════════════════════════════════════
   CODE SAMPLES (injected on DOMContentLoaded)
══════════════════════════════════════════════ */
const R_MENTAL = `# ══════════════════════════════════════════════════════════
# Mental Health Risk Classification — R
# Dataset  : 25,000 records
# Target   : mental_health_risk  (binary: 0 / 1)
# Models   : Logistic Regression  +  Random Forest
# Results  : AUC 1.00  |  Accuracy 99 %
# ══════════════════════════════════════════════════════════

# 1.  LIBRARIES ─────────────────────────────────────────
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)
library(corrplot)
library(e1071)
set.seed(42)

# 2.  LOAD & INSPECT ────────────────────────────────────
df <- read_csv("mental_health_25k.csv")
glimpse(df)
summary(df)
cat("Class balance:\\n")
print(prop.table(table(df$mental_health_risk)))

# 3.  PREPROCESSING ─────────────────────────────────────
df <- df %>%
  mutate(
    mental_health_risk = factor(mental_health_risk, levels = c(0, 1),
                                labels = c("No_Risk", "Risk")),
    across(where(is.numeric),
           ~ ifelse(is.na(.), median(., na.rm = TRUE), .)),
    across(where(is.character), ~ ifelse(is.na(.), "Unknown", .)),
    across(where(is.character), as.factor)
  )

# 4.  FEATURE ENGINEERING ───────────────────────────────
num_df <- df %>% select(where(is.numeric))
corrplot(
  cor(num_df, use = "complete.obs"),
  method  = "circle", type = "upper",
  tl.cex  = 0.7, number.cex = 0.55,
  col     = colorRampPalette(c("#ef4444", "white", "#14b8a6"))(200)
)

# 5.  TRAIN / TEST SPLIT ────────────────────────────────
idx        <- createDataPartition(df$mental_health_risk, p = .80, list = FALSE)
train_df   <- df[idx, ]
test_df    <- df[-idx, ]
cat("Train:", nrow(train_df), " | Test:", nrow(test_df), "\\n")

# 6.  CROSS-VALIDATION CONTROL ──────────────────────────
ctrl <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# 7.  LOGISTIC REGRESSION ───────────────────────────────
cat("\\n▶ Training Logistic Regression...\\n")
lr_model <- train(
  mental_health_risk ~ .,
  data      = train_df,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl,
  metric    = "ROC"
)
print(lr_model)

# 8.  RANDOM FOREST ─────────────────────────────────────
cat("\\n▶ Training Random Forest...\\n")
rf_model <- train(
  mental_health_risk ~ .,
  data       = train_df,
  method     = "rf",
  ntree      = 500,
  trControl  = ctrl,
  metric     = "ROC",
  importance = TRUE
)
print(rf_model)

# 9.  EVALUATION FUNCTION ───────────────────────────────
evaluate <- function(model, test_data, label) {
  preds   <- predict(model, test_data)
  probs   <- predict(model, test_data, type = "prob")[, 2]
  cm      <- confusionMatrix(preds, test_data$mental_health_risk)
  roc_obj <- roc(as.numeric(test_data$mental_health_risk) - 1, probs,
                 quiet = TRUE)
  cat("\\n══", label, "══\\n")
  cat("Accuracy   :", round(cm$overall["Accuracy"],       4), "\\n")
  cat("Kappa      :", round(cm$overall["Kappa"],          4), "\\n")
  cat("AUC        :", round(auc(roc_obj),                 4), "\\n")
  cat("Sensitivity:", round(cm$byClass["Sensitivity"],    4), "\\n")
  cat("Specificity:", round(cm$byClass["Specificity"],    4), "\\n")
  print(cm$table)
  invisible(list(cm = cm, roc = roc_obj, auc = auc(roc_obj)))
}

lr_res <- evaluate(lr_model, test_df, "Logistic Regression")
rf_res <- evaluate(rf_model, test_df, "Random Forest")

# 10.  ROC CURVES ───────────────────────────────────────
plot(lr_res$roc, col = "#14b8a6", lwd = 2.5,
     main = "ROC Curve Comparison — Mental Health Risk")
lines(rf_res$roc, col = "#f59e0b", lwd = 2.5)
legend("bottomright", bty = "n",
       legend = c(paste("Logistic Reg  AUC =", round(lr_res$auc, 4)),
                  paste("Random Forest AUC =", round(rf_res$auc, 4))),
       col = c("#14b8a6", "#f59e0b"), lwd = 2.5)

# 11.  VARIABLE IMPORTANCE ──────────────────────────────
varImpPlot(rf_model$finalModel,
           main = "Random Forest — Variable Importance",
           col  = "#14b8a6", pch = 20)

# 12.  SAVE MODELS ──────────────────────────────────────
saveRDS(rf_model, "rf_mental_health.rds")
saveRDS(lr_model, "lr_mental_health.rds")
cat("\\n✅ Models saved.\\n")
`;

const R_ADDICTION = `# ══════════════════════════════════════════════════════════
# Smartphone Addiction Risk
# Methods  : Ordinal Logistic Regression  +  K-Means Clustering
# Package  : MASS::polr  |  stats::kmeans  |  factoextra
# ══════════════════════════════════════════════════════════

# 1.  LIBRARIES ─────────────────────────────────────────
library(tidyverse)
library(MASS)          # polr()
library(caret)
library(cluster)
library(factoextra)
library(ggplot2)
library(psych)
library(corrplot)
set.seed(123)

# 2.  LOAD DATA ─────────────────────────────────────────
df <- read_csv("smartphone_addiction_data.csv")
glimpse(df)

# Ordinal outcome: 1 = Low → 4 = Severe
df$addiction_level <- factor(
  df$addiction_level,
  levels  = 1:4,
  labels  = c("Low", "Moderate", "High", "Severe"),
  ordered = TRUE
)
cat("\\nClass distribution:\\n")
print(prop.table(table(df$addiction_level)))

# 3.  EXPLORATORY ANALYSIS ──────────────────────────────
describe(df %>% select(where(is.numeric)))

ggplot(df, aes(x = addiction_level, fill = addiction_level)) +
  geom_bar(width = .65) +
  scale_fill_manual(values = c("#14b8a6","#0d9488","#f59e0b","#ef4444")) +
  labs(title = "Distribution of Smartphone Addiction Levels",
       x = "Level", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# 4.  USAGE PATTERN VISUALISATION ───────────────────────
df %>%
  select(daily_usage_hours, night_usage_hours,
         social_media_hours, addiction_level) %>%
  pivot_longer(-addiction_level, names_to = "metric", values_to = "hours") %>%
  ggplot(aes(x = addiction_level, y = hours, fill = metric)) +
  geom_boxplot(alpha = .85, outlier.size = .7) +
  scale_fill_manual(values = c("#14b8a6","#f59e0b","#6366f1")) +
  facet_wrap(~metric, scales = "free_y") +
  labs(title = "Usage Patterns by Addiction Level") +
  theme_minimal()

# 5.  CORRELATION MATRIX ────────────────────────────────
num_df <- df %>% select(where(is.numeric))
corrplot(
  cor(num_df), method = "color", type = "upper",
  addCoef.col = "white", number.cex = 0.58,
  col = colorRampPalette(c("#ef4444","white","#14b8a6"))(200)
)

# 6.  ORDINAL LOGISTIC REGRESSION ───────────────────────
cat("\\n▶ Ordinal Logistic Regression...\\n")
idx        <- createDataPartition(df$addiction_level, p = .80, list = FALSE)
train_data <- df[idx,  ]
test_data  <- df[-idx, ]

ord_formula <- addiction_level ~
  daily_usage_hours + night_usage_hours + social_media_hours +
  app_switches_per_hour + phantom_vibration +
  sleep_disruption_score + anxiety_score

ord_model <- polr(ord_formula, data = train_data, Hess = TRUE)
summary(ord_model)

# Coefficients + p-values
coef_tbl   <- coef(summary(ord_model))
p_vals     <- pnorm(abs(coef_tbl[, "t value"]), lower.tail = FALSE) * 2
coef_tbl   <- cbind(coef_tbl, "p value" = round(p_vals, 5))
print(coef_tbl)

# Odds ratios + 95% CI
OR_tbl <- exp(cbind("Odds Ratio" = coef(ord_model), confint(ord_model)))
print(round(OR_tbl, 3))

# Accuracy on held-out set
preds_ord  <- predict(ord_model, test_data)
acc_ord    <- mean(preds_ord == test_data$addiction_level)
cat("\\nOrdinal LR accuracy:", round(acc_ord, 4), "\\n")
print(confusionMatrix(preds_ord, test_data$addiction_level))

# 7.  K-MEANS CLUSTERING ────────────────────────────────
cat("\\n▶ K-Means Clustering...\\n")
scaled_df <- scale(num_df)

# Choose k with elbow + silhouette
fviz_nbclust(scaled_df, kmeans, method = "wss",       k.max = 10) +
  theme_minimal() + labs(title = "Elbow — Optimal k")
fviz_nbclust(scaled_df, kmeans, method = "silhouette", k.max = 10) +
  theme_minimal() + labs(title = "Silhouette — Optimal k")

# Fit final model: k = 4
km         <- kmeans(scaled_df, centers = 4, nstart = 50, iter.max = 300)
df$cluster <- factor(km$cluster)

cat("\\nCluster sizes:\\n"); print(table(df$cluster))
cat("Between/Total SS:", round(km$betweenss / km$totss, 4), "\\n")

# PCA visualisation
fviz_cluster(km, data  = scaled_df,
             palette   = c("#14b8a6","#f59e0b","#6366f1","#ef4444"),
             geom      = "point", ellipse = TRUE,
             ellipse.type = "convex",
             ggtheme   = theme_minimal(),
             main      = "Smartphone Usage Clusters (PCA)")

# Cluster profiles
profiles <- df %>%
  group_by(cluster) %>%
  summarise(
    n               = n(),
    avg_daily_hrs   = round(mean(daily_usage_hours),      2),
    avg_night_hrs   = round(mean(night_usage_hours),      2),
    avg_anxiety     = round(mean(anxiety_score),          2),
    avg_sleep_disr  = round(mean(sleep_disruption_score), 2),
    dominant_level  = names(sort(table(addiction_level), decreasing = TRUE))[1]
  )
print(profiles)

# 8.  SAVE ──────────────────────────────────────────────
saveRDS(ord_model, "ordinal_addiction_model.rds")
write_csv(profiles, "cluster_profiles.csv")
cat("\\n✅ Analysis complete. Outputs saved.\\n")
`;

/* ══════════════════════════════════════════════
   INIT  –  inject code samples after DOM ready
══════════════════════════════════════════════ */
const SPIRITUAL_BOOKS = `{
  "section": "Spiritual Books",
  "themes": ["Prayer", "Wisdom", "Reflection"],
  "materials": [
    {"title": "Daily Devotions", "format": "Reading notes"},
    {"title": "Prayer Journal", "format": "Personal reflections"},
    {"title": "Wisdom Readings", "format": "Study guide"}
  ]
}`;

const SPIRITUAL_SLIDES = `{
  "section": "Presentation Slides",
  "slides": [
    {"title": "Welcome", "focus": "Theme and purpose"},
    {"title": "Message", "focus": "Core teaching points"},
    {"title": "Practice", "focus": "Reflection and action"}
  ]
}`;

const SPIRITUAL_VIDEOS = `{
  "section": "Videos",
  "playlist": [
    {"title": "Morning Devotion", "length": "05:24"},
    {"title": "Scripture Reflection", "length": "08:10"},
    {"title": "Weekly Teaching Clip", "length": "12:48"}
  ]
}`;

function buildSpiritualMaterialsSection() {
  const section = document.getElementById('code-samples');
  if (!section || section.dataset.spiritBuilt === '1') return;

  section.innerHTML = `
    <div class="container">
      <div class="sec-header"><span class="eyebrow">Spiritual Library</span><h2 class="sec-h2">Spiritual Materials</h2></div>
      <div class="tab-scroller"><div class="tab-bar" id="spirit-tabs">
        <button class="tab-btn active" data-group="spirit" data-target="spiritual-books">Spiritual Books</button>
        <button class="tab-btn" data-group="spirit" data-target="spiritual-slides">Presentation Slides</button>
        <button class="tab-btn" data-group="spirit" data-target="spiritual-videos">Videos</button>
      </div></div>

      <div class="tab-panel spirit active" id="spiritual-books">
        <div class="reader-card stack-card">
          <div class="stack-header">
            <h3>Spiritual Books</h3>
            <p class="proj-desc">Curated reading notes, devotion themes, and reflection prompts for quiet study and personal growth.</p>
          </div>
          <div class="code-win stack-code">
            <div class="code-bar">
              <span class="dot red-dot"></span><span class="dot yellow-dot"></span><span class="dot green-dot"></span>
              <span class="code-win-title">spiritual_books.json</span>
              <button class="copy-btn" onclick="copyCode('spiritual-books-code')">Copy</button>
            </div>
            <pre class="code-pre"><code id="spiritual-books-code" class="language-none"></code></pre>
          </div>
          <iframe id="spiritual-books-frame" class="doc-frame stack-frame" title="Spiritual Books Preview"></iframe>
          <p class="stack-note">Preview and notes are generated directly in code.</p>
        </div>
      </div>

      <div class="tab-panel spirit" id="spiritual-slides">
        <div class="reader-card stack-card">
          <div class="stack-header">
            <h3>Presentation Slides</h3>
            <p class="proj-desc">Short slide-style teaching material for sermons, devotionals, and spiritual talks.</p>
          </div>
          <div class="code-win stack-code">
            <div class="code-bar">
              <span class="dot red-dot"></span><span class="dot yellow-dot"></span><span class="dot green-dot"></span>
              <span class="code-win-title">presentation_slides.json</span>
              <button class="copy-btn" onclick="copyCode('spiritual-slides-code')">Copy</button>
            </div>
            <pre class="code-pre"><code id="spiritual-slides-code" class="language-none"></code></pre>
          </div>
          <iframe id="spiritual-slides-frame" class="doc-frame stack-frame" title="Presentation Slides Preview"></iframe>
          <p class="stack-note">Slide content is built from static project code.</p>
        </div>
      </div>

      <div class="tab-panel spirit" id="spiritual-videos">
        <div class="reader-card stack-card">
          <div class="stack-header">
            <h3>Videos</h3>
            <p class="proj-desc">A static video-style preview for teaching clips, recorded reflections, and message playlists.</p>
          </div>
          <div class="code-win stack-code">
            <div class="code-bar">
              <span class="dot red-dot"></span><span class="dot yellow-dot"></span><span class="dot green-dot"></span>
              <span class="code-win-title">video_playlist.json</span>
              <button class="copy-btn" onclick="copyCode('spiritual-videos-code')">Copy</button>
            </div>
            <pre class="code-pre"><code id="spiritual-videos-code" class="language-none"></code></pre>
          </div>
          <iframe id="spiritual-videos-frame" class="doc-frame stack-frame" title="Spiritual Videos Preview"></iframe>
          <p class="stack-note">Video-style previews are embedded with local code only.</p>
        </div>
      </div>
    </div>`;

  section.dataset.spiritBuilt = '1';

  const booksCode = document.getElementById('spiritual-books-code');
  if (booksCode) {
    booksCode.textContent = SPIRITUAL_BOOKS;
    if (window.Prism) Prism.highlightElement(booksCode);
  }

  const slidesCode = document.getElementById('spiritual-slides-code');
  if (slidesCode) {
    slidesCode.textContent = SPIRITUAL_SLIDES;
    if (window.Prism) Prism.highlightElement(slidesCode);
  }

  const videosCode = document.getElementById('spiritual-videos-code');
  if (videosCode) {
    videosCode.textContent = SPIRITUAL_VIDEOS;
    if (window.Prism) Prism.highlightElement(videosCode);
  }

  const booksFrame = document.getElementById('spiritual-books-frame');
  if (booksFrame) booksFrame.src = makeDocUrl('books', 'Spiritual Books', 'Curated reading notes and reflection prompts.');

  const slidesFrame = document.getElementById('spiritual-slides-frame');
  if (slidesFrame) slidesFrame.src = makeDocUrl('presentation', 'Presentation Slides', 'Short teaching slide deck and talk outline.');

  const videosFrame = document.getElementById('spiritual-videos-frame');
  if (videosFrame) videosFrame.src = makeDocUrl('video', 'Videos', 'Teaching clips and recorded reflections.');

  section.style.visibility = 'visible';
}

document.addEventListener('DOMContentLoaded', function() {
  function injectCode(id, src) {
    const el = document.getElementById(id);
    if (!el) return;
    el.textContent = src;
    if (window.Prism) Prism.highlightElement(el);
  }
  injectCode('r-mental-code',   R_MENTAL);
  injectCode('r-addiction-code', R_ADDICTION);
});

window.addEventListener('load', function() {
  if (window.Prism) Prism.highlightAll();
});
