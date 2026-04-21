// Toggle menu for mobile
function toggleMenu() {
  const navbar = document.getElementById("navbar");
  if (navbar) navbar.classList.toggle("active");
}

const nav = document.getElementById("navbar");

// Generic section switcher for child sections
function showSection(sectionId, groupClass) {
  // Hide all in the group
  document.querySelectorAll("." + groupClass).forEach(el => {
    el.classList.remove("active");
    el.style.display = "none";
  });

  // Show the target section
  const target = document.getElementById(sectionId);
  if (target) {
    target.classList.add("active");
    target.style.display = "block";
  }
}

// ===== ATTACH ALL LISTENERS NOW =====

// Home and About links (top-level)
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

// Parent links in dropdowns (Projects, Resume, Code Samples)
document.querySelectorAll('.dropdown > .tab-link').forEach(parentLink => {
  parentLink.addEventListener('click', function(e) {
    e.preventDefault();
    const isMobile = window.innerWidth <= 768;
    const dropdown = this.closest('.dropdown');
    const href = this.getAttribute('href');
    const sectionId = href.substring(1);

    if (isMobile) {
      // Mobile: toggle dropdown AND activate parent section
      dropdown.classList.toggle("active");

      document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
      document.querySelectorAll('.tab-link').forEach(l => l.classList.remove('active'));

      const target = document.getElementById(sectionId);
      if (target) target.classList.add('active');
      this.classList.add('active');
      return;
    }

    // Desktop: switch to parent section (unchanged)
    document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
    document.querySelectorAll('.tab-link').forEach(l => l.classList.remove('active'));

    const target = document.getElementById(sectionId);
    if (target) target.classList.add('active');
    this.classList.add('active');

    // Show first sub-section
    const firstSub = target.querySelector('.project-section, .resume-section, .code-section');
    if (firstSub) {
      showSection(firstSub.id, firstSub.className.split(' ')[0]);
      firstSub.classList.add('active');
    }
  });
});

// Child links in dropdowns (Education, Experience, Resume Skills, Mental Health, Addiction)
document.querySelectorAll('.dropdown-content > .tab-link').forEach(childLink => {
  childLink.addEventListener('click', function(e) {
    e.preventDefault();
    const href = this.getAttribute('href');
    const sectionId = href.substring(1);
    const target = document.getElementById(sectionId);

    if (!target) return;

    // Determine the subsection class (project-section, resume-section, code-section)
    const subsectionClass = target.className.split(' ').find(cls => 
      cls === 'project-section' || cls === 'resume-section' || cls === 'code-section'
    );

    if (subsectionClass) {
      // Hide all subsections of this type
      document.querySelectorAll('.' + subsectionClass).forEach(el => {
        el.classList.remove('active');
        el.style.display = 'none';
      });

      // Show the target subsection
      target.classList.add('active');
      target.style.display = 'block';

      // Update active link styling in the dropdown
      document.querySelectorAll('.dropdown-content > .tab-link').forEach(link => {
        link.classList.remove('active');
      });
      this.classList.add('active');
    }

    if (nav) nav.classList.remove("active");
  });
});


// ===== SET UP DEFAULT ACTIVE SECTIONS ON PAGE LOAD =====
// Ensure Home is active by default
window.addEventListener('DOMContentLoaded', () => {
  try {
    const ids = ['home'];
    ids.forEach(id => {
      const section = document.getElementById(id);
      if (section) section.classList.add('active');

      const navLink = document.querySelector(`.tab-link[href="#${id}"]`);
      if (navLink) navLink.classList.add('active');
    });
  } catch (err) {
    console.warn("Default section activation skipped:", err);
  }
});


// Toggle menu for mobile
function toggleMenu() {
  const navbar = document.querySelector("nav"); // match CSS selector
  if (navbar) navbar.classList.toggle("active");
}

// Collapse nav when a sub-link is clicked
document.querySelectorAll('.dropdown-content .tab-link').forEach(subLink => {
  subLink.addEventListener('click', function(e) {
    e.preventDefault();
    e.stopPropagation();

    const href = this.getAttribute('href');
    const sectionId = href.substring(1);

    // Show the section
    showSection(sectionId, 'project-section'); // adjust groupClass as needed

    // Collapse dropdown
    const dropdown = this.closest('.dropdown');
    if (dropdown) dropdown.classList.remove('active');

    // Collapse nav back to hamburger
    const navbar = document.querySelector("nav");
    if (navbar) navbar.classList.remove("active");
  });
});

// Load R code dynamically
fetch("https://raw.githubusercontent.com/JUMA22-RT/DATA-SCIENCE/main/Mental%20Health%20Risk.R")
    .then(response => response.text())
    .then(data => {
      const codeBlock = document.getElementById("rcode");
      codeBlock.textContent = data;
      Prism.highlightElement(codeBlock);
    })
    .catch(err => console.error("Error loading R file:", err));

