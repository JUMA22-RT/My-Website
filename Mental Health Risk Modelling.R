
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
Mental_data <- read_excel("C:/Users/mauri/OneDrive/Desktop/Projects/Datasets/mental_health_risk_dataset.xlsx")

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
shinyApp(ui = ui, server = server)
