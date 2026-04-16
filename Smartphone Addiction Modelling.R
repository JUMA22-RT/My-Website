# app.R
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(pROC)
library(pscl)

# Load data
data <- readxl::read_excel("C:/Users/mauri/OneDrive/Desktop/Projects/Datasets/Smartphone_Usage_And_Addiction_Analysis.xlsx")
data %>% head()
View(data)

ui <- dashboardPage(
  dashboardHeader(title = "Smartphone Usage, Addiction, and Lifestyle Impact",
                  titleWidth = 500, # increase width so the full title shows
                  # Filters in header bar with labels on the left
                  tags$li(
                    class = "dropdown",
                    div(
                      style = "display: flex; align-items: center; gap: 10px; padding: 1px 0px; background-color: transparent;",
                      
                      # Gender filter
                      div(
                        style = "display: flex; align-items: center; gap: 4px;",
                        tags$label("Gender:", style = "color: white; font-weight: bold; text-align: center;"),
                        selectInput("gender_filter", NULL,
                                    choices = c("All", unique(data$gender)),
                                    selected = "All", width = "90px")
                      ),
                      
                    )
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Usage Behavior and Device Interaction", tabName = "usage", icon = icon("mobile")),
      menuItem("Lifestyle Factors and Impact Indicators", tabName = "lifestyle", icon = icon("bed")),
      menuItem("Correlation & Association", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("GLM Modeling", tabName = "glm", icon = icon("chart-line")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    # CSS tweaks to reduce spacing
    tags$style(HTML("
     .skin-blue .main-sidebar {
     background-color: near-black;
     }
     .content-wrapper {
          padding: 0px;
        }
        .box {
          margin: 0px;
        }

       /* Force header title flush left */
    .main-header .logo {
      width: 30 !important;       /* shrink to fit text */
      margin: 5 !important;         /* remove default margins */
      padding-left: 5 !important;   /* no left padding */
      text-align: left !important;  /* align text left */
      height: 53px;       /* match navbar height */
      line-height: 53px;  /* vertically center text */
      font-size: 19px !important;   /* increase font size */
      font-weight: bold !important; /* make it bold */
      text-align: left !important;  /* keep it left aligned */
      color: white !important;      /* ensure it stays visible on dark header */
    }
    /* Increase header height */
    .main-header .navbar {
      min-height: 25px;   /* default is ~50px */
    }

    .main-header .sidebar-toggle {
      height: 20px;
      line-height: 20px;
      width: 10px;
    }
    /* Adjust dropdown filter alignment */
    .main-header .navbar .dropdown {
      margin-top: 0px;   /* push filters down a bit */
      margin-bottom: 0px
    }
    
    * Style the selectInput boxes */
    .main-header .navbar .dropdown .form-control {
      font-size: 0px;           /* smaller text inside dropdown */
      height: 28px;             /* reduce height */
      padding: 0px 0px;          /* tighter padding */
      width: 50px;              /* fixed width */
    }
    
    .sidebar-menu {
      margin-top: 30px;   /* push items down by 30px */
    }
    
    /* Style header filter labels */
    .main-header .dropdown label {
      color: white !important;
      font-weight: bold !important;
      text-align: center !important;
      font-size: 14px !important;   /* increase font size */
    }

      .box { margin-bottom: 2px; }         /* reduce space below boxes */
      /* Center the icon inside valueBox/infoBox */
    
      .box-header { padding-bottom: 0px; }   /* add space below box heading */
      .plot-title { margin-top: 0px; }      /* add space above plot title */
      .box { margin-right: 0px !important; }
      .content-wrapper, .right-side {padding-top: 0px !important;   /* remove top padding */}
      .box {margin-top: 0px !important;    /* remove margin above boxes */}
      .box {box-shadow: none !important;   /* remove shadow */border: 1px solid #d2d6de;     /* optional: keep a clean border */}
      .skin-blue .main-header .logo {
          background-color: #3c8dbc;}
      .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;}
       /* Move sidebar toggle button to the right */
      .sidebar-toggle {
        float: right !important;
        margin-right: 10px;
        margin-left: 0 !important;
      }
      
      .info-box-text {
        text-transform: capitalize;   /* Capitalize each word */
        font-size: 13px;
        text-align: left;
      }
      .info-box-number {
        text-transform: capitalize;   /* Capitalize values if text */
        font-size: 23px;
        text-align: left;
      }
      .info-box {
        min-height: 60px;             /* reduce height */
        margin-bottom: 2px;           /* reduce spacing */
        margin-left: 0px !important;
        margin-right: 0px !important;
        
      }
      /* Remove padding inside rows */
      .row {
        margin-left: 0px !important;
        margin-right: 0px !important;
      }

      .info-box-icon {
        height: 60px;
        line-height: 60px;
        width: 40px;
        font-size: 23px;
        margin-right: 5px;

      }
      .info-box-content {
        padding: 0px 0px;
        text-align: left;
        margin-left: 40px;    /* increase spacing between icon and text */

      }
      
      /* Allow sidebar menu text to wrap */
    .sidebar-menu > li > a {
      white-space: normal !important;   /* allow wrapping */
      word-wrap: break-word !important; /* break long words if needed */
      line-height: 1.2em;               /* tighter line spacing for wrapped text */
      padding-right: 10px;              /* add some breathing room */
    }
    
    /* Bright highlight for active sidebar menu item */
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li.active > a {
      background-color: #FFD700 !important;  /* bright gold background */
      color: #2C3E50 !important;             /* dark text for contrast */
      font-weight: bold;                     /* emphasize active item */
    }

    /* Optional: hover effect for better UX */
    .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a:hover {
      background-color: #FFE066 !important;  /* lighter gold on hover */
      color: #2C3E50 !important;
    }



    ")),
    
    tabItems(
      # Overview
      tabItem(tabName = "overview",
              fluidRow(
                infoBoxOutput("totalRecords", width = 3),
                infoBoxOutput("avgScreenTime", width = 3),
                infoBoxOutput("avgSleep", width = 3),
                infoBoxOutput("avgGaming", width = 3)
              ),
              fluidRow(
                infoBoxOutput("avgWork", width = 3),
                infoBoxOutput("avgMedia", width = 3),
                infoBoxOutput("avgWeekend", width = 3),
                infoBoxOutput("avgAddictionScore", width = 3)
              ),
              fluidRow(
                box(
                  title = NULL,
                  width = 7, height = "220px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "The age breakdown shows a youthful population, with men, women, and others evenly represented across all age ranges."
                  ),
                  plotlyOutput("genderPlot", height = "165px")
                ),
                box(
                  title = NULL,
                  width = 5, height = "220px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Most participants fall into moderate or severe addiction, while only a small share report no addiction at all."
                  ),
                  plotlyOutput("addictionScorePlot", height = "165px")
                ),
                
                box(
                  title = NULL,
                  width = 5, height = "210px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Overall, a clear majority are classified as addicted, underscoring the scale of the issue."
                  ),
                  plotlyOutput("addictionClassPlot", height = "150px")
                ),
                
                box(
                  title = NULL,
                  width = 7, height = "210px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Across genders, addiction strongly overlaps with academic impact, showing consistent patterns of disruption regardless of group."
                  ),
                  plotlyOutput("addictionStatusPlot", height = "158px")
                ),
                box(
                  title = NULL,
                  width = 12, height = "100px",
                  div(
                    style = "padding:0px; text-align:left; width:100%; font-size:13px; font-weight:bold; color:#2C3E50;",
                    p(
                      span("Summary: ",
                           style = "color:#E74C3C; font-weight:bold; font-size:14px;"),
                      span("Overall, the data suggests that higher screen time is associated with reduced sleep and elevated addiction scores.This overview provides a quick snapshot of digital habits and their potential impact on lifestyle and well-being.",
                           style = "color:#2E86C1; font-size:13px;")
                    )
                  )
                )
              )
      ),
      
      # Usage Behavior and Device Interactions
      tabItem(tabName = "usage",
              fluidRow(
                box(
                  title = NULL,
                  width = 6, height = "270px",
                  div(
                    style = "padding:5px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "The age curve shows a youthful population, with most participants clustered around their early thirties."
                  ),
                  plotlyOutput("agePlot", height = "200px")
                ),
                
                box(
                  title = NULL,
                  width = 6, height = "270px",
                  div(
                    style = "padding:5px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Screen habits differ slightly across genders, but all groups spend significant hours daily on devices."
                  ),
                  plotlyOutput("screenPlot", height = "200px")
                ),
                
                box(
                  title = NULL,
                  width = 5, height = "270px",
                  div(
                    style = "padding:5px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Social media and sleep dominate routines, while gaming and work vary modestly between genders."
                  ),
                  plotlyOutput("usageCategoryPlot", height = "200px")
                ),
                
                box(
                  title = NULL,
                  width = 7, height = "270px",
                  div(
                    style = "padding:5px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Across ages, notifications consistently outpace app opens, highlighting how constant alerts drive engagement."
                  ),
                  plotlyOutput("notifPlot", height = "200px")
                ),
                
                box(
                  title = NULL,
                  width = 12, height = "70px",
                  div(
                    style = "padding:8px; text-align:left; width:100%; font-size:14px; font-weight:bold; color:#2C3E50;",
                    p(
                      span("Summary: ",
                           style = "color:#E74C3C; font-weight:bold; font-size:14px;"),
                      span("This report provides a holistic view of digital behavior, showing how age and gender influence stress, addiction, and academic performance, while lifestyle patterns in screen time, sleep, and notifications reveal the broader impact of constant engagement on wellbeing.",
                           style = "color:#2E86C1; font-size:14px;")
                    )
                  )
                )
              )
      ),
      
      # Lifestyle Factors and Impact Indicators
      tabItem(tabName = "lifestyle",
              fluidRow(
                box(
                  title = NULL,
                  width = 6, height = "270px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Stress shifts with age and gender, showing who carries the heavier emotional load."
                  ),
                  plotlyOutput("stressPlot", height = "209px")
                ),
                
                box(
                  title = NULL,
                  width = 6, height = "270px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Addiction patterns emerge differently across groups, from mild habits to severe dependence."
                  ),
                  plotlyOutput("addictionPlot", height = "209px")
                ),
                
                box(
                  title = NULL,
                  width = 6, height = "270px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Stress and addiction ripple into academics, with some groups more affected than others."
                  ),
                  plotlyOutput("impactPlot", height = "209px")
                ),
                
                box(
                  title = NULL,
                  width = 6, height = "270px",
                  div(
                    style = "padding:0px; font-size:14px; font-weight:bold; color:#2C3E50; text-align:left;",
                    "Daily routines reveal rising screen hours and shrinking sleep, shaping overall wellbeing."
                  ),
                  plotlyOutput("weekendPlot", height = "210px")
                ),
                
                box(
                  title = NULL,
                  width = 12, height = "70px",
                  div(
                    style = "padding:0px; text-align:left; width:100%; font-size:14px; font-weight:bold; color:#2C3E50;",
                    p(
                      span("Summary: ",
                           style = "color:#E74C3C; font-weight:bold; font-size:14px;"),
                      span("The findings illustrate clear age and gender variations in stress, addiction, and academic impact, alongside lifestyle trends in screen use and sleeping hours that shape overall behavioral outcomes.",
                           style = "color:#2E86C1; font-size:14px;")
                    )
                  )
                )
                
              )),
      
      # Correlation & Association
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  width = 7, height = "280px",
                  
                  # Flexbox layout for dropdown (left) and button (right)
                  div(style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
                      selectInput("cor_vars", "Correlation Matrix (Select variables):",
                                  choices = c("daily_screen_time_hours", "social_media_hours",
                                              "gaming_hours", "work_study_hours", "sleep_hours",
                                              "notifications_per_day", "app_opens_per_day", "weekend_screen_time"),
                                  selected = c("daily_screen_time_hours", "sleep_hours"),
                                  multiple = TRUE,
                                  width = "80%"),   # dropdown takes most of the row
                      actionButton("runCorrelation", "Generate")  # button floats right
                  ),
                  
                  verbatimTextOutput("corResult"),
                  plotOutput("corPlot", height = "170px", width = "100%")
                ),
                
                box(
                  width = 5, height = "280px",
                  
                  # Flexbox layout for dropdown (left) and button (right)
                  div(style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
                      selectInput("assoc_vars", "Association Matrix (Select variables):",
                                  choices = c("addiction_level", "addicted_status", "stress_level", "academic_work_impact"),
                                  selected = c("addiction_level", "academic_work_impact"),
                                  multiple = TRUE,
                                  width = "75%"),   # dropdown takes most of the row
                      actionButton("runAssociation", "Generate")  # button floats right
                  ),
                  
                  verbatimTextOutput("assocResult"),
                  plotOutput("assocPlot", height = "170px", width = "100%")
                ),
                
                box(
                  width = 12, height = "270px",
                  
                  # Side-by-side layout for continuous dropdown (left) and categorical dropdown + button (right)
                  fluidRow(
                    column(width = 8,
                           selectInput("cont_vars", "Continuous vs Categorical Associations(Select continuous variables):",
                                       choices = c("daily_screen_time_hours", "social_media_hours",
                                                   "gaming_hours", "work_study_hours", "sleep_hours",
                                                   "notifications_per_day", "app_opens_per_day", "weekend_screen_time"),
                                       selected = c("daily_screen_time_hours", "sleep_hours"),
                                       multiple = TRUE,
                                       width = "100%")
                    ),
                    column(width = 4,
                           div(style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
                               selectInput("cat_var", "(Select categorical variable):",
                                           choices = c("addiction_level", "addicted_status", "stress_level", "academic_work_impact"),
                                           selected = "addicted_status",
                                           width = "70%"),
                               actionButton("runContCat", "Run Test")
                           )
                    )
                  ),
                  
                  # Results and plot
                  plotOutput("contCatPlot", height = "160px", width = "100%")
                ),
                box(
                  title = NULL,
                  width = 12, height = "90px",
                  div(
                    style = "padding:0px; text-align:left; width:100%; font-size:14px; font-weight:bold;",
                    p(
                      span("Summary: ",
                           style = "color:#E74C3C; font-weight:bold; font-size:14px;"),
                      span("The analysis reveals strong links between weekend screen time and daily screen time, ",
                           style = "color:#2E86C1; font-size:14px;"),
                      span("significant associations between addiction status and sleep/social media use, ",
                           style = "color:#2E86C1; font-size:14px;"),
                      span("and measurable academic impacts across age and gender groups.",
                           style = "color:#2E86C1; font-size:14px;")
                    )
                  )
                )
              )
              
      ),
      
      tabItem(tabName = "glm",
              fluidRow(
                box(
                  title = tagList(
                    span("GLM Predictor Selection"),
                    div(style = "float:right; margin-right:10px; font-size:10px;",
                        actionButton("runGLM", "Run GLM")
                    )
                  ),
                  width = 6, height = "125px",
                  selectInput("glm_predictors", "Select predictors:",
                              choices = c("daily_screen_time_hours", "social_media_hours",
                                          "gaming_hours", "work_study_hours", "sleep_hours",
                                          "notifications_per_day", "app_opens_per_day", "weekend_screen_time"),
                              selected = c("daily_screen_time_hours", "social_media_hours"),
                              multiple = TRUE)
                ),
                box(title = "Confusion Matrix", width = 6,height = "125px",
                    div(style = "width:100%; font-size:12px;",
                        tableOutput("glmConfMatTable"))
                ),
                
                fluidRow(
                  box(title = "Model Summary", width = 7, height = "215px",
                      div(style= "width:100%;",
                          tableOutput("glmSummaryTable")),
                  ),
                  
                  box(title = "Predicted vs Actual with Logistic Curve", width = 5, height = "215px",
                      plotOutput("glmPredVsActual", height = "170px"))
                )),
              
              fluidRow(
                box(title = "ROC Curve", width = 4, height = "215px",
                    plotOutput("glmROC", height = "170px")),
                box(title = "Performance Metrics", width = 8, height = "215px",
                    div(style = "width:100%;",
                        tableOutput("glmConfMetricsTable"))
                ),
                box(
                  title = NULL,
                  width = 12, height = "120px",
                  div(
                    style = "padding:0px;text-align:left;,height:50%; width:100%; font-size:12px;font-weight:bold;",
                    p(
                      span("Summary: ", 
                           style = "color:#E74C3C; font-weight:bold; font-size:14px;"),
                      span("This GLM analysis demonstrates that daily screen time, social media use, and sleep hours are strong predictors of addiction status, achieving high accuracy (≈90%) with excellent sensitivity and overall model performance.", 
                           style = "color:#2E86C1; font-size:14px;")
                    )
                  )
                )
                
              )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Raw Data",
            width = 12, height = "400px",
            DTOutput("dataTable")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered dataset
  filteredData <- reactive({
    df <- data
    if (input$gender_filter != "All") {
      df <- df %>% filter(gender == input$gender_filter)
    }
    # Mutate addicted_label into categorical labels
    df <- df %>%
      mutate(addicted_status = ifelse(addicted_label == 1, "Addicted", "Not Addicted"))
    df
  })
  
  # Overview
  # InfoBoxes
  output$totalRecords <- renderInfoBox({
    infoBox("Total Records", nrow(filteredData()), icon = icon("database"),
            color = "blue", fill = TRUE)
  })
  
  output$avgScreenTime <- renderInfoBox({
    infoBox("Avg Daily Screen Time (hrs)",
            round(mean(filteredData()$daily_screen_time_hours, na.rm=TRUE),1),
            icon = icon("mobile"), color = "green", fill = TRUE)
  })
  
  output$avgSleep <- renderInfoBox({
    infoBox("Avg Sleep Duration (hrs)",
            round(mean(filteredData()$sleep_hours, na.rm=TRUE),1),
            icon = icon("bed"), color = "purple", fill = TRUE)
  })
  
  output$avgGaming <- renderInfoBox({
    infoBox("Avg Gaming Duration (hrs)",
            round(mean(filteredData()$gaming_hours, na.rm=TRUE),1),
            icon = icon("gamepad"), color = "orange", fill = TRUE)
  })
  
  output$avgWork <- renderInfoBox({
    infoBox("Avg Work Study Time (hrs)",
            round(mean(filteredData()$work_study_hours, na.rm=TRUE),1),
            icon = icon("briefcase"), color = "aqua", fill = TRUE)
  })
  
  output$avgMedia <- renderInfoBox({
    infoBox("Avg Social Media (hrs)",
            round(mean(filteredData()$social_media_hours, na.rm=TRUE),1),
            icon = icon("comments"), color = "red", fill = TRUE)
  })
  
  output$avgWeekend <- renderInfoBox({
    infoBox("Avg Weekend Screen Time (hrs)",
            round(mean(filteredData()$weekend_screen_time, na.rm=TRUE),1),
            icon = icon("tv"), color = "yellow", fill = TRUE)
  })
  
  output$avgAddictionScore <- renderInfoBox({
    df <- filteredData()
    
    # Fit logistic regression on binary addicted_label
    # Add predictors you want to include
    model <- glm(addicted_label ~ stress_level + academic_work_impact,
                 data = df, family = binomial())
    
    # Predicted probabilities for each observation
    preds <- predict(model, type = "response")
    
    # Average predicted probability = model-based addiction score
    avg_score <- mean(preds, na.rm = TRUE)
    
    infoBox("Addiction Score (Logistic GLM)",
            round(avg_score, 2),
            icon = icon("exclamation-triangle"),
            color = "maroon", fill = TRUE)
  })
  
  # Overview
  
  output$genderPlot <- renderPlotly({
    df <- filteredData() %>%
      mutate(AgeGroup = cut(age,
                            breaks = c(10,16,21,26,31,36,41),
                            labels = c("10–15","16–20","21–25","26–30","31–35","36–40"))) %>%
      count(AgeGroup, gender) %>%
      group_by(AgeGroup) %>%
      mutate(pct = n / sum(n))
    
    ggplotly(
      ggplot(df, aes(x = AgeGroup, y = pct, fill = gender)) +
        geom_col(position = position_dodge(width = 0.9), width = 0.8) +
        geom_text(aes(y = pct/2,   # place text in the middle of each bar
                      label = paste0(round(pct * 100), "%")),
                  position = position_dodge(width = 1),
                  vjust = 0.5,   # centers text vertically inside bars
                  hjust = 0.5,   # centers horizontally
                  color = "white",
                  size = 2     # reduced font size to fit
        ) +
        labs(x = "Age Group", y = NULL, fill = "Gender Group") +   # remove y-axis title
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),breaks = NULL)+
        theme_gray()+
        theme(
          axis.text.x = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 8, face = "bold"),
          legend.position = "right",
          legend.box = "vertical",
          legend.text = element_text(size = 5),   # reduce legend label font size
          legend.title = element_text(size = 8)   # reduce legend title font size
        )
    )
  })
  
  output$addictionScorePlot <- renderPlotly({
    df <- filteredData() %>%
      dplyr::count(addiction_level) %>%
      dplyr::mutate(percent = round(n / sum(n) * 100, 1)) %>%
      as.data.frame()
    
    plot_ly(
      data = df,
      labels = ~addiction_level,
      values = ~n,
      type = "pie",
      textinfo = "label+percent",
      textposition = "inside",
      insidetextorientation = "radial",
      marker = list(
        colors = c(
          "None" = "#B0BEC5",     # grey (neutral baseline)
          "Mild" = "#4CAF50",     # green (low severity)
          "Moderate" = "#FFC107", # amber/yellow (medium severity)
          "Severe" = "#F44336"    # red (high severity)
        )[df$addiction_level],
        line = list(color = "white", width = 0.1)  # white borders between slices
      ),
      pull = 0.01   # offset slices slightly to create spacing
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
  })
  
  output$addictionStatusPlot <- renderPlotly({
    df_summary <- filteredData() %>%
      dplyr::mutate(
        addiction_status = ifelse(addicted_label == 1, "Addicted", "Not Addicted"),
        academic_work_impact = factor(academic_work_impact, levels = c("Yes", "No"))
      ) %>%
      dplyr::count(gender, academic_work_impact, addiction_status) %>%
      group_by(gender, academic_work_impact) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    
    p <- ggplot(df_summary, aes(x = academic_work_impact, y = pct, fill = addiction_status)) +
      geom_col(position = "stack") +
      geom_text(aes(y = pct/2, label = paste0(round(pct * 100), "%")),
                position = position_stack(vjust = 0.5),
                color = "white",
                size = 2.5) +
      facet_wrap(~ gender, strip.position = "bottom") +
      labs(
        x = "Academic Work Impact",
        y = NULL,   # remove y-axis title
        fill = "Addiction Status"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = NULL) +
      theme_gray()+
      
      theme(
        axis.text.x = element_text(size = 6, face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"),
        legend.position = "right",
        legend.box = "vertical",
        legend.text = element_text(size = 7),   # reduce legend label font size
        legend.title = element_text(size = 9),   # reduce legend title font size
        strip.placement = "outside")
    
    ggplotly(p)
  })
  output$addictionClassPlot <- renderPlotly({
    df <- filteredData() %>%
      dplyr::mutate(
        addiction_status = ifelse(addicted_label == 1, "Addicted", "Not Addicted")
      ) %>%
      dplyr::count(addiction_status) %>%
      dplyr::mutate(percent = round(n / sum(n) * 100, 1)) %>%
      as.data.frame()
    
    plot_ly(
      data = df,
      labels = ~addiction_status,
      values = ~n,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hole = 0.5,   # donut style
      marker = list(
        colors = c(
          "Addicted" = "#F44336",   # red (high severity)
          "Not Addicted" = "#4CAF50" # green (low severity)
        )[df$addiction_status],
        line = list(color = "white", width = 3)  # white borders → spacing between slices
      ),
      pull = 0.01   # offset slices slightly for separation
    ) %>%
      layout(
        showlegend = FALSE,   # remove legend
        margin = list(l = 0, r = 0, b = 0, t = 0)
      )
  })
  
  # Usage Behavior and Device Interactions
  
  output$agePlot <- renderPlotly({
    df <- filteredData()
    mu <- mean(df$age, na.rm = TRUE)
    sigma <- sd(df$age, na.rm = TRUE)
    
    ggplotly(
      ggplot(df, aes(x = age)) +
        # Histogram
        geom_histogram(
          binwidth = 3,
          fill = "steelblue",
          alpha = 0.6,
          color = "white"
        ) +
        # Normal distribution curve scaled to histogram counts
        stat_function(
          fun = function(x) {
            dnorm(x, mean = mu, sd = sigma) * length(df$age) * 3
          },
          color = "red",
          size = 0.5,        # make line thin
          linetype = "solid"
        ) +
        labs(x = "Age", y = "Count") +
        theme_gray() +
        theme(
          axis.text.x = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 8, face = "bold"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = 7),   # reduce legend label font size
          legend.title = element_text(size = 9),   # reduce legend title font size
          plot.margin = unit(c(0, 0, 0, 0), "cm"),   # flush with box edges
          axis.title.y = element_text(size = 8, face = "bold"),
        )
    )
  })
  
  output$screenPlot <- renderPlotly({
    df <- filteredData() %>%
      mutate(ScreenGroup = cut(
        daily_screen_time_hours,
        breaks = c(0, 2, 4, 6, 8, 10, 12, 24),
        labels = c("0–2","2–4","4–6","6–8","8–10","10–12","12+")
      ))
    
    ggplotly(
      ggplot(df, aes(x = ScreenGroup, fill = gender)) +
        geom_bar(position = "fill") +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 0.1),
          breaks = NULL
        ) +
        geom_text(
          aes(
            label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 0.1)
          ),
          stat = "count",
          position = position_fill(vjust = 0.5),
          color = "white",
          size = 3
        ) +
        scale_x_discrete(
          labels = c(
            "0–2"="0–2 hrs","2–4"="2–4 hrs","4–6"="4–6 hrs",
            "6–8"="6–8 hrs","8–10"="8–10 hrs","10–12"="10–12 hrs","12+"="12+ hrs"
          )
        ) +
        # Rename legend title and labels
        labs(x = "Screen Time Group (hrs)", y = NULL, fill = "Gender Category:") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 8, face = "bold"),
          legend.box = "vertical",
          legend.text = element_text(size = 7),   # reduce legend label font size
          legend.title = element_text(size = 9),   # reduce legend title font size
          legend.position = "top",
          plot.margin = unit(c(0,0,0,0), "cm")
        )
    )
  })
  
  output$usageCategoryPlot <- renderPlotly({
    usage <- filteredData() %>%
      select(social_media_hours, gaming_hours, work_study_hours, sleep_hours, gender) %>%
      group_by(gender) %>%
      summarise(across(everything(), mean, na.rm = TRUE))
    
    usage_long <- tidyr::pivot_longer(
      usage,
      cols = c(social_media_hours, gaming_hours, work_study_hours, sleep_hours),
      names_to = "Category",
      values_to = "Hours"
    ) %>%
      mutate(Category = factor(
        Category,
        levels = c("social_media_hours", "gaming_hours", "work_study_hours", "sleep_hours"),
        labels = c("Social Media", "Gaming", "Work/Study", "Sleep")
      ))
    
    ggplotly(
      ggplot(usage_long, aes(x = Category, y = Hours, fill = Category)) +
        geom_bar(stat = "identity") +
        facet_wrap(~gender, strip.position = "bottom") +   # facet labels at bottom
        scale_fill_manual(
          values = c(
            "Social Media" = "steelblue",
            "Gaming" = "darkred",
            "Work/Study" = "darkgreen",
            "Sleep" = "purple"
          )
        ) +
        labs(x = NULL, y = "Average Hours", fill = "Usage type:") +
        theme_gray() +
        theme(
          axis.text.y = element_text(size = 8, face = "bold"),
          axis.title.y = element_text(size = 8),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = 7),   # reduce legend label font size
          legend.title = element_text(size = 9),   # reduce legend title font size
          
          axis.text.x = element_blank(),
          strip.placement = "outside",         # push facet strips outside plot
          strip.text = element_text(face = "bold")
        )
    )
  })
  
  # Notifications per Day plot
  output$notifPlot <- renderPlotly({
    df <- filteredData()
    
    # Reshape into long format
    df_long <- df %>%
      dplyr::select(age, notifications_per_day, app_opens_per_day) %>%
      tidyr::pivot_longer(
        cols = c(notifications_per_day, app_opens_per_day),
        names_to = "metric",
        values_to = "value"
      )
    
    # Calculate average per age group and metric
    df_avg <- df_long %>%
      group_by(age, metric) %>%
      summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")
    
    # Convert averages to percentages within each age group
    df_percent <- df_avg %>%
      group_by(age) %>%
      mutate(percent = avg_value / sum(avg_value, na.rm = TRUE) * 100) %>%
      ungroup() %>%
      # Convert metric into factor with clean labels
      mutate(metric = factor(metric,
                             levels = c("notifications_per_day", "app_opens_per_day"),
                             labels = c("Daily Average Notifications", "Daily Average App Opens")))
    
    # Build combined line chart
    p <- ggplot(df_percent, aes(x = age, y = percent, color = metric, group = metric)) +
      geom_line(size = 0.5) +
      geom_point(size = 1) +
      labs(x = "Age", y = "Percentage (%)", color = NULL) +   # legend title
      scale_color_manual(
        values = c("Daily Average Notifications" = "blue", "Daily Average App Opens" = "red")
      ) +
      theme_gray() +
      theme(
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 8, face = "bold"),
        legend.position = "top",
        legend.box = "vertical",
        legend.text = element_text(size = 7),   # reduce legend label font size
        legend.title = element_text(size = 9)   # reduce legend title font size
        
        
      ) +
      scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
      scale_x_continuous(
        expand = c(0,0),
        limits = c(min(df_percent$age), max(df_percent$age)),
        breaks = unique(df_percent$age)   # show all age values
      )
    
    ggplotly(p, tooltip = c("age", "metric", "percent"))
  })
  
  # App Opens per Day plot
  output$appOpensPlot <- renderPlotly({
    df <- filteredData()
    
    df_app <- df %>%
      dplyr::group_by(age, gender) %>%
      dplyr::summarise(total = sum(app_opens_per_day, na.rm = TRUE)) %>%
      dplyr::mutate(percent = total / sum(total) * 100) %>%
      dplyr::ungroup()
    
    p2 <- ggplot(df_app, aes(x = age, y = percent, fill = gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(round(percent, 1), "%")),
                position = position_dodge(width = 0.9),
                vjust = -0.3, size = 3.5, fontface = "bold") +
      labs(x = "Age", y = "Percentage (%)", title = "App Opens per Day") +
      theme_minimal()+
      theme(
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 8, face = "bold"),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 7),   # reduce legend label font size
        legend.title = element_text(size = 9)   # reduce legend title font size
        
      )
    
    ggplotly(p2)
  })
  
  #Lifestyle Factors and Impact Indicators
  
  output$stressPlot <- renderPlotly({
    df <- filteredData() %>%
      dplyr::mutate(
        age_group = dplyr::case_when(
          age <= 20 ~ "≤20",
          age >= 21 & age <= 30 ~ "21–30",
          age > 30 ~ ">30"
        ),
        age_group = factor(age_group, levels = c("≤20", "21–30", ">30"))
      ) %>%
      dplyr::count(age_group, gender, stress_level) %>%
      dplyr::group_by(age_group, gender) %>%
      dplyr::mutate(pct = n / sum(n)) %>%
      dplyr::ungroup()
    
    p <- ggplot(df, aes(x = gender, y = pct, fill = stress_level)) +
      geom_col(position = "stack", width = 0.9) +
      geom_text(
        aes(label = paste0(round(pct * 100), "%")),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 2.5
      ) +
      facet_wrap(~ age_group) +
      labs(x = NULL, y = NULL, fill = "Stress Level:") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = NULL) +
      scale_fill_manual(
        values = c(
          "Low" = "#4CAF50",     # green (low severity)
          "Medium" = "#FFC107", # amber/yellow (medium severity)
          "High" = "#F44336"    # red (high severity)
        )
      ) +
      theme_gray() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 8, face = "bold"),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 7),   # reduce legend label font size
        legend.title = element_text(size = 9)   # reduce legend title font size
        
      )
    
    ggplotly(p)
  })
  
  
  output$addictionPlot <- renderPlotly({
    df <- filteredData() %>%
      dplyr::mutate(
        age_group = dplyr::case_when(
          age <= 20 ~ "≤20",
          age >= 21 & age <= 30 ~ "21–30",
          age > 30 ~ ">30"
        ),
        age_group = factor(age_group, levels = c("≤20", "21–30", ">30"))
      ) %>%
      dplyr::count(age_group, gender, addiction_level) %>%
      dplyr::group_by(age_group, gender) %>%
      dplyr::mutate(pct = n / sum(n)) %>%
      dplyr::ungroup()
    
    p <- ggplot(df, aes(x = gender, y = pct, fill = addiction_level)) +
      geom_col(position = "stack", width = 0.9) +
      geom_text(
        aes(label = paste0(round(pct * 100), "%")),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 3
      ) +
      facet_wrap(~ age_group) +
      labs(x = NULL, y = NULL, fill = "Addiction Level:") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = NULL) +
      # Severity color progression: None → Mild → Moderate → Severe
      scale_fill_manual(
        values = c(
          "None" = "#B0BEC5",     # grey (neutral)
          "Mild" = "#4CAF50",     # green (low severity)
          "Moderate" = "#FFC107", # amber/yellow (medium severity)
          "Severe" = "#F44336"    # red (high severity)
        )
      ) +
      theme_gray() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 8, face = "bold"),
        legend.position = "bottom",
        legend.box = "vertical",        
        legend.text = element_text(size = 7),   # reduce legend label font size
        legend.title = element_text(size = 9)   # reduce legend title font size
        
      )
    
    ggplotly(p)
  })
  
  
  
  # Impact Indicators
  output$impactPlot <- renderPlotly({
    df <- filteredData() %>%
      dplyr::mutate(
        age_group = dplyr::case_when(
          age <= 20 ~ "≤20",
          age >= 21 & age <= 30 ~ "21–30",
          age > 30 ~ ">30"
        ),
        age_group = factor(age_group, levels = c("≤20", "21–30", ">30"))
      ) %>%
      dplyr::count(age_group, gender, academic_work_impact) %>%
      dplyr::group_by(age_group, gender) %>%
      dplyr::mutate(pct = n / sum(n)) %>%
      dplyr::ungroup()
    
    p <- ggplot(df, aes(x = gender, y = pct, fill = academic_work_impact)) +
      geom_col(position = position_dodge(width = 0.9), width = 1) +
      geom_text(aes(y = pct/2, label = paste0(round(pct * 100), "%")),
                position = position_dodge(width = 0.8),
                color = "white",
                size = 2,
                vjust = 0.5) +
      facet_wrap(~ age_group) +
      labs(x = NULL , y = NULL, fill = "Academic Work Impact:") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks = NULL) +
      scale_fill_manual(
        values = c(
          "No" = "#4CAF50",     # green (No Impact)
          "Yes" = "#F44336"    # red (Impact)
        )
      ) +
      theme_gray()+
      theme(strip.text = element_text(size = 10, face = "bold"), # facet labels
            axis.text.x = element_text(size = 8, face = "bold"),
            axis.title.x = element_text(size = 8, face = "bold"),
            legend.position = "bottom",
            legend.box = "vertical",        
            legend.text = element_text(size = 7),   # reduce legend label font size
            legend.title = element_text(size = 9)   # reduce legend title font size
            
      )
    
    ggplotly(p)
  })
  
  output$weekendPlot <- renderPlotly({
    df <- filteredData()
    
    # Reshape into long format for the three metrics
    df_long <- df %>%
      dplyr::select(age, daily_screen_time_hours, weekend_screen_time, sleep_hours) %>%
      tidyr::pivot_longer(
        cols = c(daily_screen_time_hours, weekend_screen_time, sleep_hours),
        names_to = "metric",
        values_to = "value"
      )
    
    # Calculate average per age group and metric
    df_avg <- df_long %>%
      group_by(age, metric) %>%
      summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")
    
    # Convert metric into factor with clean labels
    df_avg <- df_avg %>%
      mutate(metric = factor(metric,
                             levels = c("daily_screen_time_hours", "weekend_screen_time", "sleep_hours"),
                             labels = c("Avg Daily Screen Time", "Avg Weekend Screen Time", "Avg Sleeping Time")))
    
    # Build combined line chart
    p <- ggplot(df_avg, aes(x = age, y = avg_value, color = metric, group = metric)) +
      geom_line(size = 0.8) +
      geom_point(size = 1.5) +
      labs(x = "Age", y = "Average Hours", color = NULL) +
      scale_color_manual(
        values = c("Avg Daily Screen Time" = "blue",
                   "Avg Weekend Screen Time" = "red",
                   "Avg Sleeping Time" = "green")
      ) +
      theme_gray() +
      theme(
        legend.position = "top"
      ) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(
        expand = c(0,0),
        limits = c(min(df_avg$age), max(df_avg$age)),
        breaks = unique(df_avg$age)
      )
    
    ggplotly(p, tooltip = c("age", "metric", "avg_value"))
  })
  
  # Correlation Matrix
  observeEvent(input$runCorrelation, {
    df <- filteredData()
    numeric_vars <- df %>% select(all_of(input$cor_vars))
    
    cor_matrix <- cor(numeric_vars, use = "complete.obs", method = "pearson")
    
    # Create shorter names
    short_names <- c(
      daily_screen_time_hours = "ScreenTime",
      social_media_hours      = "SocialMedia",
      gaming_hours            = "Gaming",
      work_study_hours        = "WorkStudy",
      sleep_hours             = "Sleep",
      notifications_per_day   = "Notifs",
      app_opens_per_day       = "AppOpens",
      weekend_screen_time     = "Weekend"
    )
    
    # Apply shorter names to matrix
    colnames(cor_matrix) <- short_names[colnames(cor_matrix)]
    rownames(cor_matrix) <- short_names[rownames(cor_matrix)]
    
    output$corPlot <- renderPlot({
      corrplot(cor_matrix,
               method = "color", type = "lower",
               tl.col = "black", tl.srt = 20,
               addCoef.col = "black", number.cex = 0.7,
               col = colorRampPalette(c("red", "green", "blue"))(200),
               cl.pos = "n"   # removes the color scale legend
      )
    })
  })
  
  # Association Matrix (Cramer's V)
  observeEvent(input$runAssociation, {
    df <- filteredData()
    cat_vars <- df %>% select(all_of(input$assoc_vars))
    
    var_names <- names(cat_vars)
    assoc_matrix <- matrix(NA, nrow = length(var_names), ncol = length(var_names),
                           dimnames = list(var_names, var_names))
    
    for (i in 1:length(var_names)) {
      for (j in 1:length(var_names)) {
        if (i != j) {
          tbl <- table(cat_vars[[i]], cat_vars[[j]])
          chi <- suppressWarnings(chisq.test(tbl))
          n <- sum(tbl)
          phi2 <- chi$statistic / n
          r <- nrow(tbl)
          k <- ncol(tbl)
          assoc_matrix[i, j] <- sqrt(phi2 / min(k - 1, r - 1))
        } else {
          assoc_matrix[i, j] <- 1
        }
      }
    }
    
    # Define shorter names
    short_names <- c(
      addiction_level     = "AddictLvl",
      addicted_status     = "AddictStat",
      stress_level        = "StressLvl",
      academic_work_impact= "AcadImpact"
    )
    
    # Apply shorter names
    colnames(assoc_matrix) <- short_names[colnames(assoc_matrix)]
    rownames(assoc_matrix) <- short_names[rownames(assoc_matrix)]
    
    output$assocPlot <- renderPlot({
      corrplot(assoc_matrix,
               method = "color", type = "lower",
               tl.col = "black", tl.srt = 20,
               addCoef.col = "black", number.cex = 0.7,
               col = colorRampPalette(c("red", "white", "blue"))(200),
               is.corr = FALSE,
               cl.pos = "n")   # removes scale legend
    })
  })
  
  # Continuous vs Categorical Association
  # Continuous vs Categorical Association with p-values in boxplots
  observeEvent(input$runContCat, {
    df <- filteredData()
    cont_vars <- df %>% select(all_of(input$cont_vars))
    cat_var <- df[[input$cat_var]]
    
    output$contCatResult <- renderPrint({
      paste("Showing p-values directly on the plots.")
    })
    
    output$contCatPlot <- renderPlot({
      par(mfrow = c(1, length(cont_vars)))
      for (v in names(cont_vars)) {
        # Run appropriate test
        if (length(unique(cat_var)) > 2) {
          fit <- aov(cont_vars[[v]] ~ cat_var)
          pval <- summary(fit)[[1]][["Pr(>F)"]][1]
        } else {
          fit <- t.test(cont_vars[[v]] ~ cat_var)
          pval <- fit$p.value
        }
        
        # Draw boxplot
        boxplot(cont_vars[[v]] ~ cat_var,
                xlab = NULL, ylab = v,
                col = "lightblue")
        
        # Add p-value text
        mtext(paste("p =", signif(pval, 3)), side = 3, line = 0.5, col = "red", cex = 0.9)
      }
    })
  })
  # -------------------------
  # GLM Modeling
  # -------------------------
  observeEvent(input$runGLM, {
    df <- data
    
    # Train/Test Split
    set.seed(123)
    trainIndex <- caret::createDataPartition(df$addicted_label, p = 0.7, list = FALSE)
    trainData <- df[trainIndex, ]
    testData  <- df[-trainIndex, ]
    
    # Fit GLM
    formula <- as.formula(
      paste("addicted_label ~", paste(input$glm_predictors, collapse = " + "))
    )
    glm_model <- glm(formula, data = trainData, family = binomial)
    
    # Predictions
    testData$pred_prob <- predict(glm_model, newdata = testData, type = "response")
    testData$pred_class <- ifelse(testData$pred_prob > 0.5, 1, 0)
    
    # Confusion Matrix
    cm <- caret::confusionMatrix(factor(testData$pred_class),
                                 factor(testData$addicted_label),
                                 positive = "1")
    
    # Contingency table: Prediction rows, Reference columns
    # Contingency table: Predictions as rows, Reference (Actuals) as columns
    output$glmConfMatTable <- renderTable({
      mat <- cm$table
      
      # Rename rows (Predictions) and columns (Reference)
      rownames(mat) <- ifelse(rownames(mat) == "1", "Predicted: Addicted", "Predicted: Not Addicted")
      colnames(mat) <- ifelse(colnames(mat) == "1", "Actual: Addicted", "Actual: Not Addicted")
      
      # Convert to data frame for display
      as.data.frame.matrix(mat)
    }, rownames = TRUE)
    
    # Full metrics table
    # Accuracy, sensitivity, specificity, etc.
    output$glmConfMetricsTable <- renderTable({
      
      # Helper function: format numeric or return "Not Available"
      fmt <- function(x, digits = 4) {
        if (is.na(x)) {
          return("Not Available")
        } else {
          return(sprintf(paste0("%.", digits, "f"), x))
        }
      }
      
      # Build table with fallbacks
      df <- data.frame(
        Metric = c("Accuracy", "95% CI", "McNemar's Test P-Value",
                   "Sensitivity", "Specificity",
                   "Precision (PPV)", "Negative Predictive Value (NPV)"),
        Value = c(
          fmt(cm$overall["Accuracy"]),
          paste0("(", fmt(cm$overall["AccuracyLower"]), ", ", fmt(cm$overall["AccuracyUpper"]), ")"),
          ifelse(is.na(cm$overall["McnemarPValue"]), "Not Available", as.character(signif(cm$overall["McnemarPValue"], 3))),
          fmt(cm$byClass["Sensitivity"]),
          fmt(cm$byClass["Specificity"]),
          fmt(cm$byClass["Pos Pred Value"]),
          fmt(cm$byClass["Neg Pred Value"])
        ),
        stringsAsFactors = FALSE
      )
      
      # Transpose: metrics become column names
      df_t <- as.data.frame(t(df$Value))
      colnames(df_t) <- df$Metric
      rownames(df_t) <- NULL
      
      df_t
    })
    
    # Model Summary with stars
    output$glmSummaryTable <- renderTable({
      coef_summary <- summary(glm_model)$coefficients
      data.frame(
        Predictor = rownames(coef_summary),
        Estimate = round(coef_summary[,1], 3),
        Std_Error = round(coef_summary[,2], 3),
        Z_value = round(coef_summary[,3], 3),
        P_value = signif(coef_summary[,4], 3),
        row.names = NULL
      )
    })
    
    # ROC Curve
    output$glmROC <- renderPlot({
      roc_obj <- pROC::roc(testData$addicted_label, testData$pred_prob)
      auc_val <- pROC::auc(roc_obj)
      plot(roc_obj, col = "blue", lwd = 2,
           main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))
      abline(a = 0, b = 1, lty = 2, col = "gray")
    })
    
    # Predicted vs Actual with Logistic Curve
    output$glmPredVsActual <- renderPlot({
      ggplot(testData, aes(x = daily_screen_time_hours, y = addicted_label)) +
        geom_point(color = "darkblue", alpha = 0.6) +
        geom_smooth(method = "glm", method.args = list(family = "binomial"),
                    se = TRUE, color = "red", lwd = 1.2) +
        labs(title = "Predicted vs Actual with Logistic Curve",
             x = "Daily Screen Time (hours)",
             y = "Addicted Label (0/1)")
    })
  })
  # Raw Data (with sideways scroll)
  output$dataTable <- renderDT({
    datatable(
      filteredData(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
}

shinyApp(ui, server)



