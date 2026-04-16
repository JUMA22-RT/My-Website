// Toggle menu for mobile
function toggleMenu() {
  document.getElementById("navbar").classList.toggle("active");
}

// Main tab switching
document.querySelectorAll('.tab-link').forEach(link => {
  link.addEventListener('click', function(e) {
    e.preventDefault();
    document.querySelectorAll('.tab-link').forEach(l => l.classList.remove('active'));
    document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
    this.classList.add('active');
    const target = document.querySelector(this.getAttribute('href'));
    target.classList.add('active');
    document.getElementById("navbar").classList.remove("active"); // close nav on mobile
  });
});

// Generic sub-tab switching
function showSubTab(section, event, sectionClass, tabClass) {
  document.querySelectorAll(sectionClass).forEach(sec => sec.classList.remove('active'));
  document.getElementById(section).classList.add('active');
  document.querySelectorAll(tabClass + ' button').forEach(btn => btn.classList.remove('active'));
  event.target.classList.add('active');
}

// Projects
function showProject(section, event) {
  showSubTab(section, event, '.project-section', '.project-tabs');
}

// Resume
function showResume(section, event) {
  showSubTab(section, event, '.resume-section', '.resume-tabs');
}

// Load R code dynamically
fetch("https://raw.githubusercontent.com/JUMA22-RT/DATA-SCIENCE/main/Mental%20Health%20Risk.R")
    .then(response => response.text())
    .then(data => {
      const codeBlock = document.getElementById("rcode");
      codeBlock.textContent = data;
      Prism.highlightElement(codeBlock);
    })
    .catch(err => console.error("Error loading R file:", err));

// Enable dropdown toggle on mobile
document.querySelectorAll(".dropdown > a").forEach(link => {
  link.addEventListener("click", function(e) {
    if (window.innerWidth <= 768) {
      e.preventDefault(); // prevent navigation
      this.parentElement.classList.toggle("active");
    }
  });
});

// Generic tab switcher
function showSection(sectionId, groupClass, event) {
  if (event) event.preventDefault();

  // Hide all sections in the group
  document.querySelectorAll("." + groupClass).forEach(el => {
    el.classList.remove("active");
    el.style.display = "none";
  });

  // Show the clicked section
  const target = document.getElementById(sectionId);
  if (target) {
    target.classList.add("active");
    target.style.display = "block";
  }

  // Remove highlight from all dropdown links
  document.querySelectorAll(".dropdown-content a").forEach(link => {
    link.classList.remove("active");
  });

  // Highlight the clicked child link
  if (event && event.target) {
    event.target.classList.add("active");
  }
}