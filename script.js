document.addEventListener("DOMContentLoaded", function () {

// ================================
// GLOBAL + MOBILE MENU
// ================================
const nav = document.getElementById("navbar");

window.toggleMenu = function() {
  if (nav) nav.classList.toggle("active");
}

// ================================
// HELPERS
// ================================
function showSection(sectionId, groupClass) {
  document.querySelectorAll("." + groupClass).forEach(el => {
    el.classList.remove("active");
    el.style.display = "none";
  });
  const target = document.getElementById(sectionId);
  if (target) {
    target.classList.add("active");
    target.style.display = "block";
  }
}

// ================================
// NAVIGATION LISTENERS
// ================================

// Top-level links (Home, About)
document.querySelectorAll('nav > .tab-link').forEach(link => {
  if (!link.closest('.dropdown')) {
    link.addEventListener('click', function(e) {
      e.preventDefault();
      document.querySelectorAll('.tab-link').forEach(l => l.classList.remove('active'));
      document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
      this.classList.add('active');
      const target = document.querySelector(this.getAttribute('href'));
      if (target) target.classList.add('active');
      if (nav) nav.classList.remove("active");
    });
  }
});

// Parent links in dropdowns
document.querySelectorAll('.dropdown > .tab-link').forEach(parentLink => {
  parentLink.addEventListener('click', function(e) {
    e.preventDefault();
    const isMobile = window.innerWidth <= 768;
    const dropdown = this.closest('.dropdown');
    const sectionId = this.getAttribute('href').substring(1);

    if (isMobile) {
      document.querySelectorAll('.dropdown').forEach(d => {
        if (d !== dropdown) d.classList.remove('active');
      });
      dropdown.classList.toggle("active");
      document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
      document.querySelectorAll('.tab-link').forEach(l => l.classList.remove('active'));
      const target = document.getElementById(sectionId);
      if (target) target.classList.add('active');
      this.classList.add('active');
      return;
    }

    document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
    document.querySelectorAll('.tab-link').forEach(l => l.classList.remove('active'));
    const target = document.getElementById(sectionId);
    if (target) {
      target.classList.add('active');
      this.classList.add('active');
      const firstSub = target.querySelector('.project-section, .resume-section, .code-section');
      if (firstSub) {
        const groupClass = Array.from(firstSub.classList).find(cls =>
          ['project-section','resume-section','code-section'].includes(cls)
        );
        if (groupClass) showSection(firstSub.id, groupClass);
        firstSub.classList.add('active');
      }
    }
  });
});

// Child links in dropdowns
document.querySelectorAll('.dropdown-content > .tab-link').forEach(link => {
  link.addEventListener('click', function(e) {
    e.preventDefault();
    const sectionId = this.getAttribute('href').substring(1);
    const target = document.getElementById(sectionId);
    if (!target) return;

    if (target.classList.contains('dropdown-button')) {
      toggleCodeDropdown(target);
    } else {
      const groupClass = Array.from(target.classList).find(cls =>
        ['project-section','resume-section','code-section'].includes(cls)
      );
      if (groupClass) showSection(sectionId, groupClass);
    }

    document.querySelectorAll('.dropdown-content > .tab-link')
      .forEach(l => l.classList.remove('active'));

    this.classList.add('active');

    if (window.innerWidth <= 768) {
      const dropdown = this.closest('.dropdown');
      if (dropdown) dropdown.classList.remove('active');
      if (nav) nav.classList.remove('active');
    }
  });
});

// ================================
// DEFAULT ACTIVE SECTION
// ================================
const home = document.getElementById('home');
if (home) home.classList.add('active');

const navLink = document.querySelector('.tab-link[href="#home"]');
if (navLink) navLink.classList.add('active');

// ================================
// CODE LOADER
// ================================
// Toggle project panels
window.toggleProject = function toggleProject(button) {
  const panel = button.nextElementSibling;
  const section = button.closest(".code-section");

  // Close other panels in same section
  section.querySelectorAll(".dropdown-panel").forEach(p => {
    if (p !== panel) {
      p.style.maxHeight = null;
      p.classList.remove("open");
    }
  });

  // Toggle current
  const isOpen = panel.style.maxHeight;

  if (isOpen) {
    panel.style.maxHeight = null;
    panel.classList.remove("open");
    button.classList.remove("active");
  } else {
    panel.style.maxHeight = panel.scrollHeight + "px";
    panel.classList.add("open");
    button.classList.add("active");
  }
}

// Copy button
window.copyCode = function copyCode(id) {
  const text = document.getElementById(id).innerText;
  navigator.clipboard.writeText(text).then(() => {
    alert("Copied!");
  });
}

// ================= LOAD FILES =================

// R
fetch("https://raw.githubusercontent.com/JUMA22-RT/DATA-SCIENCE/main/Mental%20Health%20Risk.R")
  .then(r => r.text())
  .then(data => {
    document.getElementById("rcode").textContent = data;
    Prism.highlightAll();
  });

fetch("https://raw.githubusercontent.com/JUMA22-RT/DATA-SCIENCE/main/Smartphone%20Addiction.R")
  .then(r => r.text())
  .then(data => {
    const el = document.getElementById("rcode2");
    if (el) el.textContent = data;
  });

// Python
fetch("https://raw.githubusercontent.com/YOUR_USERNAME/YOUR_REPO/main/script.py")
  .then(r => r.text())
  .then(data => {
    const el = document.getElementById("pycode");
    if (el) el.textContent = data;
  });

// SQL (example)
document.getElementById("sqlcode").textContent = `
SELECT school_id, COUNT(*) AS attendance
FROM attendance_table
GROUP BY school_id;
`;

document.getElementById("sqlcode2").textContent = `
SELECT *
FROM students
WHERE age IS NULL;
`;
});