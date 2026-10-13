# ==============================================================================
# Household Water Leakage Pattern Analysis Using R Programming
# File: app.R
# Author: Academic Project Team
# Description: Fully functional Shiny dashboard for analyzing household water
#              consumption, identifying unusual consumption patterns, and
#              detecting possible water leakages using rule-based statistical
#              methods (Mean, Standard Deviation, Consecutive Days, Night Flow).
# ==============================================================================

# Load Required Libraries
suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(DT)
  library(grid)
  library(gridExtra)
})

# Source external helper scripts
if (!file.exists("water_usage.csv")) {
  if (file.exists("generate_dataset.R")) {
    source("generate_dataset.R")
  }
}

if (file.exists("reports/generate_pdf_report.R")) {
  source("reports/generate_pdf_report.R")
}

# ------------------------------------------------------------------------------
# UI DEFINITION
# ------------------------------------------------------------------------------
ui <- navbarPage(
  title = div(
    class = "navbar-brand-content",
    tags$span(style = "font-size: 1.4rem; margin-right: 8px;", "💧"),
    tags$strong("AquaTrace | Household Water Leakage Analysis")
  ),
  id = "nav_tabs",
  windowTitle = "Household Water Leakage Pattern Analysis",
  collapsible = TRUE,
  
  # Include Custom Stylesheet & Meta
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$style(HTML("
      /* Inline fallbacks if style.css is caching */
      .stat-card { border-radius: 12px; padding: 18px; margin-bottom: 16px; background: #fff; box-shadow: 0 2px 5px rgba(0,0,0,0.06); }
      .badge-normal { background-color: #ecfdf5; color: #059669; padding: 4px 10px; border-radius: 999px; font-weight: 600; font-size: 0.8rem; }
      .badge-warning { background-color: #fffbeb; color: #d97706; padding: 4px 10px; border-radius: 999px; font-weight: 600; font-size: 0.8rem; }
      .badge-leakage { background-color: #fef2f2; color: #dc2626; padding: 4px 10px; border-radius: 999px; font-weight: 700; font-size: 0.8rem; }
    "))
  ),
  
  # ============================================================================
  # TAB 1: HOME DASHBOARD
  # ============================================================================
  tabPanel(
    title = tagList(tags$span("🏠"), " Home Dashboard"),
    value = "tab_home",
    
    fluidPage(
      # Hero Banner
      div(
        class = "hero-banner",
        h3("💧 Household Water Leakage Pattern Analysis System"),
        p(
          "Welcome to the AquaTrace monitoring system. This academic project utilizes descriptive ",
          "statistics (historical mean, standard deviation thresholds, consecutive persistence, ",
          "and night-time baseline tracking) to monitor daily water usage, spot anomalies, and alert ",
          "householders to potential plumbing leaks before extensive damage or utility waste occurs."
        ),
        div(
          style = "display: flex; gap: 10px; flex-wrap: wrap; margin-top: 10px;",
          actionButton("btn_go_data", "📋 View Water Data", class = "btn btn-sm btn-water-primary"),
          actionButton("btn_go_analysis", "📊 Explore Visualizations", class = "btn btn-sm btn-water-secondary"),
          actionButton("btn_go_alerts", "🚨 Check Leakage Alerts", class = "btn btn-sm btn-water-secondary")
        )
      ),
      
      # Insufficient Data Notice if applicable
      uiOutput("home_data_warning"),
      
      # KPI Summary Cards (4 Cards)
      fluidRow(
        column(
          width = 3,
          div(
            class = "stat-card card-blue",
            div(class = "card-label", "Total Water Consumed"),
            div(class = "card-value", textOutput("kpi_total_consumption")),
            div(class = "card-subtext", textOutput("kpi_total_volume_m3")),
            div(class = "card-icon-bg", "🚰")
          )
        ),
        column(
          width = 3,
          div(
            class = "stat-card card-cyan",
            div(class = "card-label", "Average Daily Usage"),
            div(class = "card-value", textOutput("kpi_avg_daily")),
            div(class = "card-subtext", textOutput("kpi_sd_daily")),
            div(class = "card-icon-bg", "📊")
          )
        ),
        column(
          width = 3,
          div(
            class = "stat-card card-amber",
            div(class = "card-label", "Highest Water Usage"),
            div(class = "card-value", textOutput("kpi_highest_usage")),
            div(class = "card-subtext", textOutput("kpi_highest_date")),
            div(class = "card-icon-bg", "⚡")
          )
        ),
        column(
          width = 3,
          div(
            class = "stat-card card-red",
            div(class = "card-label", "Possible Leakage Alerts"),
            div(class = "card-value", textOutput("kpi_alert_count")),
            div(class = "card-subtext", textOutput("kpi_alert_breakdown")),
            div(class = "card-icon-bg", "🚨")
          )
        )
      ),
      
      # Main Home Grid: System Overview & Quick Alerts Preview
      fluidRow(
        column(
          width = 7,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("System Methodology & Academic Purpose")
            ),
            p(
              strong("Why Statistical Rule-Based Detection? "),
              "Unlike complex black-box machine learning models, statistical techniques provide ",
              "transparent, interpretable thresholds that household residents and municipal engineers can verify."
            ),
            tags$ul(
              tags$li(strong("Daily Baseline (Mean & SD): "), "Computes household baseline consumption (μ) and standard deviation (σ). Days exceeding μ + 2σ are flagged."),
              tags$li(strong("Percentage Threshold: "), "Applies a minimum configured increase threshold (e.g. +25% above average) to prevent false positives during low-variance periods."),
              tags$li(strong("Consecutive Day Persistence: "), "A single day spike is labeled a WARNING (often guests or laundry), whereas 2+ consecutive abnormal days trigger a POSSIBLE LEAKAGE alert."),
              tags$li(strong("Night Flow Audit: "), "Monitors night consumption (midnight to 6 AM). A surge in night usage is the primary physical signature of continuously leaking fixtures.")
            ),
            div(
              class = "disclaimer-card",
              strong("Academic & Practical Disclaimer: "),
              "Statistical anomalies indicate mathematical deviations from typical household routines and do ",
              "not constitute absolute proof of a physical pipe fracture. Elevated consumption can also stem ",
              "from legitimate household activities such as garden irrigation, hosting visitors, or filling pools."
            )
          )
        ),
        column(
          width = 5,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Recent Flagged Anomalies"),
              tags$small(class = "text-muted", "Latest Alerts")
            ),
            uiOutput("home_recent_alerts_ui")
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # TAB 2: WATER USAGE DATA
  # ============================================================================
  tabPanel(
    title = tagList(tags$span("📋"), " Water Usage Data"),
    value = "tab_data",
    
    fluidPage(
      fluidRow(
        # Sidebar for Controls, Upload & Add Record
        column(
          width = 4,
          # CSV Upload Card
          div(
            class = "sidebar-panel",
            div(class = "sidebar-header", "📁 Upload Water Consumption CSV"),
            fileInput(
              "file_upload", 
              label = "Select CSV File:",
              accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            helpText(
              "File must contain: Date, Household_ID, Morning_Usage, Afternoon_Usage, Night_Usage."
            ),
            div(
              style = "display: flex; gap: 8px; margin-top: 10px;",
              actionButton("btn_reset_sample", "🔄 Restore Sample Data", class = "btn btn-sm btn-water-secondary"),
              downloadButton("btn_download_sample_template", "📥 Download Template", class = "btn btn-sm btn-water-secondary")
            )
          ),
          
          # Add New Record Form Card
          div(
            class = "sidebar-panel",
            div(class = "sidebar-header", "➕ Add New Water Usage Record"),
            dateInput("input_add_date", "Date:", value = Sys.Date(), format = "yyyy-mm-dd"),
            selectInput("input_add_hh", "Household ID:", choices = c("HH001", "HH002", "HH003")),
            numericInput("input_add_morning", "Morning Usage (litres):", value = 140, min = 0, step = 1),
            numericInput("input_add_afternoon", "Afternoon Usage (litres):", value = 110, min = 0, step = 1),
            numericInput("input_add_night", "Night Usage (litres):", value = 45, min = 0, step = 1),
            p(
              style = "font-size: 0.85rem; color: #64748b; margin-top: 5px;",
              strong("Calculated Total: "), 
              textOutput("add_record_total_preview", inline = TRUE)
            ),
            actionButton("btn_submit_record", "💾 Save Record", class = "btn btn-water-primary btn-block", style = "width: 100%;")
          ),
          
          # Filters Card
          div(
            class = "sidebar-panel",
            div(class = "sidebar-header", "🔍 Filter Records"),
            selectInput("filter_household", "Select Household:", choices = c("All Households", "HH001", "HH002", "HH003"), selected = "HH001"),
            dateRangeInput("filter_date_range", "Date Range:", start = NULL, end = NULL, format = "yyyy-mm-dd")
          )
        ),
        
        # Main Table Display
        column(
          width = 8,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Daily Water Consumption Dataset"),
              tags$span(textOutput("table_record_count", inline = TRUE), class = "badge bg-primary")
            ),
            p(
              style = "font-size: 0.9rem; color: #64748b;",
              "Interactive data view. Total daily consumption is automatically calculated as ",
              "Morning + Afternoon + Night water usage. Search, sort, or paginate below."
            ),
            DTOutput("table_water_records")
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # TAB 3: WATER CONSUMPTION ANALYSIS
  # ============================================================================
  tabPanel(
    title = tagList(tags$span("📊"), " Consumption Analysis"),
    value = "tab_analysis",
    
    fluidPage(
      # Controls Bar for Analysis Tab
      fluidRow(
        column(
          width = 12,
          div(
            class = "content-card",
            style = "padding: 1rem; margin-bottom: 1rem;",
            fluidRow(
              column(
                width = 4,
                selectInput("analysis_household_select", "Household for Analysis:", choices = c("HH001", "HH002", "HH003"), selected = "HH001")
              ),
              column(
                width = 4,
                selectInput("chart_theme_view", "Chart Color Mode:", choices = c("Standard Water Blue", "High Contrast"), selected = "Standard Water Blue")
              ),
              column(
                width = 4,
                div(
                  style = "padding-top: 25px;",
                  textOutput("analysis_summary_banner")
                )
              )
            )
          )
        )
      ),
      
      # Chart 1: Daily Water Consumption Timeline (Full Width)
      fluidRow(
        column(
          width = 12,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Figure 1: Daily Water Consumption Trends vs. Statistical Baseline"),
              tags$span(class = "badge bg-info text-dark", "Daily Timeline")
            ),
            p(
              style = "font-size: 0.88rem; color: #64748b; margin-bottom: 0.8rem;",
              "The dashed green line represents the historical daily mean. The dashed red line marks ",
              "the anomaly threshold (Mean + 2 SD). Shaded band displays normal expected variation."
            ),
            plotOutput("plot_daily_trend", height = "360px")
          )
        )
      ),
      
      # Charts 2 & 3: Weekly / Day of Week & Time-of-Day Breakdown
      fluidRow(
        column(
          width = 6,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Figure 2: Weekly Water Usage Comparisons"),
              tags$span(class = "badge bg-secondary", "Day of Week")
            ),
            p(
              style = "font-size: 0.85rem; color: #64748b;",
              "Average water consumption compared across days of the week to reveal weekly domestic patterns."
            ),
            plotOutput("plot_weekly_comparison", height = "320px")
          )
        ),
        column(
          width = 6,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Figure 3: Time-of-Day Consumption Breakdown"),
              tags$span(class = "badge bg-secondary", "Morning / Afternoon / Night")
            ),
            p(
              style = "font-size: 0.85rem; color: #64748b;",
              "Proportion and volume of water utilized during morning, afternoon, and night-time periods."
            ),
            plotOutput("plot_time_of_day_breakdown", height = "320px")
          )
        )
      ),
      
      # Charts 4 & 5: Average Household Water Consumption & Normal vs Unusual
      fluidRow(
        column(
          width = 6,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Figure 4: Average Household Water Consumption & Distribution"),
              tags$span(class = "badge bg-secondary", "Histogram & Density")
            ),
            p(
              style = "font-size: 0.85rem; color: #64748b;",
              "Distribution of daily water volume showing central tendencies (Mean vs Median) and skewness."
            ),
            plotOutput("plot_usage_distribution", height = "320px")
          )
        ),
        column(
          width = 6,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Figure 5: Normal vs. Unusual Water Usage Summary"),
              tags$span(class = "badge bg-secondary", "Proportions")
            ),
            p(
              style = "font-size: 0.85rem; color: #64748b;",
              "Comparison of normal consumption days versus flagged warning and leakage days."
            ),
            plotOutput("plot_normal_vs_unusual", height = "320px")
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # TAB 4: POSSIBLE LEAKAGE DETECTION
  # ============================================================================
  tabPanel(
    title = tagList(tags$span("🔍"), " Leakage Detection"),
    value = "tab_detection",
    
    fluidPage(
      fluidRow(
        # Configuration Settings
        column(
          width = 4,
          div(
            class = "sidebar-panel",
            div(class = "sidebar-header", "⚙️ Rule-Based Threshold Parameters"),
            p(
              style = "font-size: 0.85rem; color: #64748b;",
              "Configure statistical rules to adjust sensitivity for anomalous consumption detection."
            ),
            sliderInput(
              "param_sd_multiplier",
              "1. Standard Deviation Multiplier (k):",
              min = 1.0, max = 3.5, value = 2.0, step = 0.1
            ),
            helpText("Default 2.0 SD covers ~95.4% of normal Gaussian variation."),
            
            sliderInput(
              "param_min_pct_increase",
              "2. Minimum % Increase Above Mean:",
              min = 10, max = 100, value = 25, step = 5, post = "%"
            ),
            helpText("Prevents false flags when historical variance (SD) is very low."),
            
            sliderInput(
              "param_consecutive_days",
              "3. Consecutive Days for Leakage Warning:",
              min = 2, max = 5, value = 2, step = 1
            ),
            helpText("Persistent spikes for N days elevate WARNING to POSSIBLE LEAKAGE."),
            
            checkboxInput(
              "param_enable_night_audit",
              "4. Enable Night-Time Flow Rule",
              value = TRUE
            ),
            helpText("Flags abnormal continuous water consumption between midnight and dawn.")
          ),
          
          # Baseline Summary Card
          div(
            class = "sidebar-panel",
            div(class = "sidebar-header", "📊 Calculated Baseline Metrics"),
            uiOutput("detection_baseline_metrics_ui")
          )
        ),
        
        # Detection Visualizations and Results
        column(
          width = 8,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Anomaly Detection Timeline & Status Classification"),
              tags$span(textOutput("detection_status_badge", inline = TRUE))
            ),
            p(
              style = "font-size: 0.88rem; color: #64748b;",
              "Points are classified into NORMAL, WARNING (single-day spike), or POSSIBLE LEAKAGE (consecutive persistent surge)."
            ),
            plotOutput("plot_leakage_detection_timeline", height = "340px")
          ),
          
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Night-Time Consumption Flow Pattern"),
              tags$span(class = "badge bg-dark", "Night Water Audit")
            ),
            p(
              style = "font-size: 0.88rem; color: #64748b;",
              "Continuous night-time water flow is the most definitive indicator of active leaks ",
              "(e.g., faulty toilet flapper valves or broken pipes) since occupants are typically asleep."
            ),
            plotOutput("plot_night_flow_analysis", height = "280px")
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # TAB 5: LEAKAGE ALERT PANEL
  # ============================================================================
  tabPanel(
    title = tagList(tags$span("🚨"), " Leakage Alert Panel"),
    value = "tab_alerts",
    
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            class = "hero-banner",
            style = "background: #fef2f2; border-color: #fecaca;",
            h4(style = "color: #991b1b; font-weight: 700;", "🚨 Active Leakage Alert & Inspection Center"),
            p(
              style = "color: #7f1d1d;",
              "This panel summarizes all dates exhibiting abnormal water consumption, computes excess ",
              "volume beyond expected usage, assigns an alert severity level, and provides practical troubleshooting steps."
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 8,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Flagged Consumption Incidents"),
              div(
                style = "display: flex; gap: 8px;",
                selectInput("filter_alert_severity", NULL, 
                            choices = c("All Alerts", "POSSIBLE LEAKAGE", "WARNING"), 
                            selected = "All Alerts", width = "180px")
              )
            ),
            DTOutput("table_alert_records")
          )
        ),
        column(
          width = 4,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("🛠️ Recommended Action Protocol")
            ),
            uiOutput("alert_recommended_actions_ui")
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # TAB 6: REPORTS & EXPORTS
  # ============================================================================
  tabPanel(
    title = tagList(tags$span("📄"), " Reports & Downloads"),
    value = "tab_reports",
    
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Comprehensive Household Water Audit Report")
            ),
            p(
              style = "font-size: 0.95rem; color: #475569;",
              "Review the executive summary statistics below and download complete analytical outputs ",
              "in CSV or PDF report format for record keeping or academic project submission."
            ),
            
            # Action Buttons for Downloads
            div(
              style = "display: flex; gap: 15px; margin-bottom: 1.5rem;",
              downloadButton("btn_download_csv", "📥 Download Filtered Data (CSV)", class = "btn btn-water-primary"),
              downloadButton("btn_download_pdf", "📄 Download Formal Report (PDF)", class = "btn btn-water-primary")
            )
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Statistical Summary Table")
            ),
            tableOutput("table_statistical_summary")
          )
        ),
        column(
          width = 6,
          div(
            class = "content-card",
            div(
              class = "content-card-header",
              tags$span("Estimated Financial & Ecological Impact")
            ),
            uiOutput("report_impact_summary_ui")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value storing the master water usage dataset
  water_data_val <- reactiveVal({
    # Initialize from default file if present, else generate fresh
    if (file.exists("water_usage.csv")) {
      read.csv("water_usage.csv", stringsAsFactors = FALSE)
    } else if (file.exists("sample_data/water_usage.csv")) {
      read.csv("sample_data/water_usage.csv", stringsAsFactors = FALSE)
    } else {
      # Fallback generate dataset
      source("generate_dataset.R")
      read.csv("water_usage.csv", stringsAsFactors = FALSE)
    }
  })
  
  # ----------------------------------------------------------------------------
  # Navigation button shortcuts
  # ----------------------------------------------------------------------------
  observeEvent(input$btn_go_data, {
    updateNavbarPage(session, "nav_tabs", selected = "tab_data")
  })
  observeEvent(input$btn_go_analysis, {
    updateNavbarPage(session, "nav_tabs", selected = "tab_analysis")
  })
  observeEvent(input$btn_go_alerts, {
    updateNavbarPage(session, "nav_tabs", selected = "tab_alerts")
  })
  
  # ----------------------------------------------------------------------------
  # File Upload Handler
  # ----------------------------------------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      uploaded_df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      
      # Validate required columns
      required_cols <- c("Date", "Household_ID", "Morning_Usage", "Afternoon_Usage", "Night_Usage")
      missing_cols <- setdiff(required_cols, names(uploaded_df))
      
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Upload error: Missing required column(s):", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = 8
        )
        return()
      }
      
      # Validate dates
      uploaded_dates <- as.Date(uploaded_df$Date)
      if (any(is.na(uploaded_dates))) {
        showNotification("Warning: Some dates could not be parsed. Expected YYYY-MM-DD format.", 
                         type = "warning", duration = 6)
      }
      
      # Handle negative usage or missing numeric values
      uploaded_df$Morning_Usage   <- pmax(0, as.numeric(uploaded_df$Morning_Usage))
      uploaded_df$Afternoon_Usage <- pmax(0, as.numeric(uploaded_df$Afternoon_Usage))
      uploaded_df$Night_Usage     <- pmax(0, as.numeric(uploaded_df$Night_Usage))
      
      # Automatically compute or recalculate Total Daily Usage
      uploaded_df$Total_Daily_Usage <- round(uploaded_df$Morning_Usage + 
                                               uploaded_df$Afternoon_Usage + 
                                               uploaded_df$Night_Usage, 1)
      
      if (!"Demonstration_Tag" %in% names(uploaded_df)) {
        uploaded_df$Demonstration_Tag <- "User Uploaded Data"
      }
      
      water_data_val(uploaded_df)
      
      # Update household selectors with newly uploaded households
      hh_choices <- unique(uploaded_df$Household_ID)
      updateSelectInput(session, "filter_household", choices = c("All Households", hh_choices), selected = hh_choices[1])
      updateSelectInput(session, "analysis_household_select", choices = hh_choices, selected = hh_choices[1])
      updateSelectInput(session, "input_add_hh", choices = hh_choices, selected = hh_choices[1])
      
      # Update date filters
      valid_dates <- na.omit(as.Date(uploaded_df$Date))
      if (length(valid_dates) > 0) {
        updateDateRangeInput(session, "filter_date_range", 
                             start = min(valid_dates), 
                             end = max(valid_dates),
                             min = min(valid_dates),
                             max = max(valid_dates))
      }
      
      showNotification(
        sprintf("Successfully loaded %d records from '%s'!", nrow(uploaded_df), input$file_upload$name),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Error reading CSV file:", e$message), type = "error", duration = 8)
    })
  })
  
  # ----------------------------------------------------------------------------
  # Reset to Sample Data Handler
  # ----------------------------------------------------------------------------
  observeEvent(input$btn_reset_sample, {
    if (file.exists("generate_dataset.R")) {
      source("generate_dataset.R")
    }
    sample_df <- read.csv("water_usage.csv", stringsAsFactors = FALSE)
    water_data_val(sample_df)
    
    hh_choices <- unique(sample_df$Household_ID)
    updateSelectInput(session, "filter_household", choices = c("All Households", hh_choices), selected = "HH001")
    updateSelectInput(session, "analysis_household_select", choices = hh_choices, selected = "HH001")
    
    valid_dates <- as.Date(sample_df$Date)
    updateDateRangeInput(session, "filter_date_range", 
                         start = min(valid_dates), end = max(valid_dates))
    
    showNotification("Restored default 90-day sample dataset for HH001, HH002, HH003.", type = "message")
  })
  
  # Download Sample Template
  output$btn_download_sample_template <- downloadHandler(
    filename = function() { "water_usage_template.csv" },
    content = function(file) {
      sample_template <- data.frame(
        Date = c("2026-04-01", "2026-04-02", "2026-04-03"),
        Household_ID = c("HH_USER", "HH_USER", "HH_USER"),
        Morning_Usage = c(130.5, 142.0, 138.0),
        Afternoon_Usage = c(105.0, 98.5, 112.0),
        Night_Usage = c(45.0, 40.5, 48.0),
        stringsAsFactors = FALSE
      )
      write.csv(sample_template, file, row.names = FALSE)
    }
  )
  
  # ----------------------------------------------------------------------------
  # Add New Record Handler
  # ----------------------------------------------------------------------------
  output$add_record_total_preview <- renderText({
    m <- ifelse(is.na(input$input_add_morning), 0, input$input_add_morning)
    a <- ifelse(is.na(input$input_add_afternoon), 0, input$input_add_afternoon)
    n <- ifelse(is.na(input$input_add_night), 0, input$input_add_night)
    sprintf("%.1f Litres", m + a + n)
  })
  
  observeEvent(input$btn_submit_record, {
    # Validate inputs
    if (is.null(input$input_add_date) || is.na(input$input_add_date)) {
      showNotification("Please select a valid date.", type = "error")
      return()
    }
    
    m <- as.numeric(input$input_add_morning)
    a <- as.numeric(input$input_add_afternoon)
    n <- as.numeric(input$input_add_night)
    
    if (is.na(m) || is.na(a) || is.na(n) || m < 0 || a < 0 || n < 0) {
      showNotification("Usage values must be valid non-negative numbers.", type = "error")
      return()
    }
    
    new_row <- data.frame(
      Date = format(as.Date(input$input_add_date), "%Y-%m-%d"),
      Household_ID = as.character(input$input_add_hh),
      Morning_Usage = round(m, 1),
      Afternoon_Usage = round(a, 1),
      Night_Usage = round(n, 1),
      Total_Daily_Usage = round(m + a + n, 1),
      Demonstration_Tag = "User Added Record",
      stringsAsFactors = FALSE
    )
    
    curr <- water_data_val()
    # Check if duplicate date and household
    dup <- which(curr$Date == new_row$Date & curr$Household_ID == new_row$Household_ID)
    if (length(dup) > 0) {
      curr[dup, ] <- new_row
      showNotification(sprintf("Updated existing record for %s on %s.", new_row$Household_ID, new_row$Date), type = "message")
    } else {
      curr <- rbind(curr, new_row)
      # Re-order by date
      curr <- curr[order(as.Date(curr$Date)), ]
      showNotification(sprintf("Added new record for %s on %s (%.1f L).", new_row$Household_ID, new_row$Date, new_row$Total_Daily_Usage), type = "message")
    }
    water_data_val(curr)
  })
  
  # Initialize date range filter once data is available
  observe({
    df <- water_data_val()
    dates <- as.Date(df$Date)
    dates <- dates[!is.na(dates)]
    if (length(dates) > 0) {
      isolate({
        if (is.null(input$filter_date_range[1])) {
          updateDateRangeInput(session, "filter_date_range",
                               start = min(dates),
                               end = max(dates),
                               min = min(dates),
                               max = max(dates))
        }
      })
    }
  })
  
  # Sync Household Selection across tabs if appropriate
  observeEvent(input$filter_household, {
    if (input$filter_household != "All Households") {
      updateSelectInput(session, "analysis_household_select", selected = input$filter_household)
    }
  })
  
  # ----------------------------------------------------------------------------
  # FILTERED DATASET (Base for displays)
  # ----------------------------------------------------------------------------
  filtered_raw_data <- reactive({
    df <- water_data_val()
    req(nrow(df) > 0)
    
    # Household filter
    if (input$filter_household != "All Households") {
      df <- df %>% filter(Household_ID == input$filter_household)
    }
    
    # Date range filter
    if (!is.null(input$filter_date_range[1]) && !is.na(input$filter_date_range[1])) {
      df <- df %>% filter(as.Date(Date) >= as.Date(input$filter_date_range[1]))
    }
    if (!is.null(input$filter_date_range[2]) && !is.na(input$filter_date_range[2])) {
      df <- df %>% filter(as.Date(Date) <= as.Date(input$filter_date_range[2]))
    }
    
    df <- df[order(as.Date(df$Date)), ]
    df
  })
  
  # ----------------------------------------------------------------------------
  # STATISTICAL LEAKAGE DETECTION ENGINE
  # ----------------------------------------------------------------------------
  # This reactive function performs the rule-based statistical anomaly detection
  # on the currently selected household dataset.
  analyzed_data_reactive <- reactive({
    # We evaluate for the household selected in analysis or primary filter
    target_hh <- input$analysis_household_select
    df <- water_data_val() %>% 
      filter(Household_ID == target_hh)
    
    # Apply date range if set
    if (!is.null(input$filter_date_range[1]) && !is.na(input$filter_date_range[1])) {
      df <- df %>% filter(as.Date(Date) >= as.Date(input$filter_date_range[1]))
    }
    if (!is.null(input$filter_date_range[2]) && !is.na(input$filter_date_range[2])) {
      df <- df %>% filter(as.Date(Date) <= as.Date(input$filter_date_range[2]))
    }
    
    df <- df[order(as.Date(df$Date)), ]
    n_days <- nrow(df)
    
    # Check if insufficient data
    if (n_days < 7) {
      df$Status <- "INSUFFICIENT DATA"
      df$Severity <- "None"
      df$Excess_Usage <- 0
      df$Expected_Usage <- ifelse(n_days > 0, mean(df$Total_Daily_Usage, na.rm = TRUE), 0)
      df$Recommended_Action <- "Accumulate at least 7 days of historical consumption to calculate baseline."
      return(list(
        data = df,
        insufficient = TRUE,
        mean_usage = ifelse(n_days > 0, mean(df$Total_Daily_Usage, na.rm = TRUE), 0),
        sd_usage = ifelse(n_days > 1, sd(df$Total_Daily_Usage, na.rm = TRUE), 0),
        threshold = 0,
        pct_threshold = 0,
        night_mean = 0,
        night_sd = 0,
        n_days = n_days
      ))
    }
    
    # 1. Calculate Historical Mean and Standard Deviation
    mu <- mean(df$Total_Daily_Usage, na.rm = TRUE)
    sigma <- sd(df$Total_Daily_Usage, na.rm = TRUE)
    
    # 2. Night Baseline
    mu_night <- mean(df$Night_Usage, na.rm = TRUE)
    sigma_night <- sd(df$Night_Usage, na.rm = TRUE)
    
    # 3. User Configured Thresholds
    k <- input$param_sd_multiplier
    pct_inc <- input$param_min_pct_increase
    consec_req <- input$param_consecutive_days
    night_enabled <- input$param_enable_night_audit
    
    stat_threshold <- mu + (k * sigma)
    pct_threshold  <- mu * (1 + (pct_inc / 100))
    
    # Combined daily anomaly condition:
    # Must exceed BOTH the SD threshold AND the percentage threshold
    is_anomaly <- (df$Total_Daily_Usage > stat_threshold) & (df$Total_Daily_Usage > pct_threshold)
    
    # Night flow anomaly condition:
    # Night usage exceeds night mean by more than 1.8 SD
    is_night_anomaly <- (df$Night_Usage > (mu_night + 1.8 * sigma_night))
    
    # 4. Consecutive Days Anomaly Tracking
    # Identify runs of anomalous days using rle
    rle_res <- rle(is_anomaly)
    consec_streak <- rep(0, n_days)
    current_idx <- 1
    for (i in seq_along(rle_res$lengths)) {
      len <- rle_res$lengths[i]
      val <- rle_res$values[i]
      if (val) {
        consec_streak[current_idx:(current_idx + len - 1)] <- len
      }
      current_idx <- current_idx + len
    }
    
    # 5. Assign Status, Severity, and Recommended Actions
    status_vec <- rep("NORMAL", n_days)
    severity_vec <- rep("Normal", n_days)
    action_vec <- rep("Routine monitoring: Water consumption is within expected statistical limits.", n_days)
    
    for (i in 1:n_days) {
      if (is_anomaly[i]) {
        if (consec_streak[i] >= consec_req) {
          status_vec[i] <- "POSSIBLE LEAKAGE"
          severity_vec[i] <- "High Priority"
          
          if (night_enabled && is_night_anomaly[i]) {
            action_vec[i] <- "High Priority: Persistent surge with elevated night-time flow. Conduct overnight meter test; inspect toilet flappers and valves immediately."
          } else {
            action_vec[i] <- "High Priority: Persistent abnormal usage for consecutive days. Inspect visible pipes, outdoor hose bibbs, and pressure valves."
          }
        } else {
          status_vec[i] <- "WARNING"
          severity_vec[i] <- "Moderate Priority"
          
          if (night_enabled && is_night_anomaly[i]) {
            action_vec[i] <- "Moderate Priority: Isolated usage spike with noticeable night flow. Monitor fixture drips and check toilet tank seals."
          } else {
            action_vec[i] <- "Moderate Priority: Isolated usage spike. Check if related to household chores (laundry, gardening). Monitor for 48 hours."
          }
        }
      }
    }
    
    df$Status <- status_vec
    df$Severity <- severity_vec
    df$Expected_Usage <- round(mu, 1)
    df$Excess_Usage <- round(pmax(0, df$Total_Daily_Usage - mu), 1)
    df$Recommended_Action <- action_vec
    df$Is_Night_Anomaly <- is_night_anomaly
    
    list(
      data = df,
      insufficient = FALSE,
      mean_usage = mu,
      sd_usage = sigma,
      threshold = stat_threshold,
      pct_threshold = pct_threshold,
      night_mean = mu_night,
      night_sd = sigma_night,
      n_days = n_days
    )
  })
  
  # ----------------------------------------------------------------------------
  # HOME TAB OUTPUTS
  # ----------------------------------------------------------------------------
  output$home_data_warning <- renderUI({
    engine <- analyzed_data_reactive()
    if (engine$insufficient) {
      div(
        class = "alert alert-warning",
        style = "border-radius: 10px;",
        strong("⚠️ Insufficient Historical Data Notice: "),
        sprintf("Currently only %d day(s) of records are available for %s. At least 7 consecutive days of historical data are required to establish a reliable statistical baseline.",
                engine$n_days, input$analysis_household_select)
      )
    } else {
      NULL
    }
  })
  
  # Summary KPI Cards
  output$kpi_total_consumption <- renderText({
    df <- analyzed_data_reactive()$data
    tot <- sum(df$Total_Daily_Usage, na.rm = TRUE)
    sprintf("%s L", format(round(tot, 1), big.mark = ","))
  })
  
  output$kpi_total_volume_m3 <- renderText({
    df <- analyzed_data_reactive()$data
    tot <- sum(df$Total_Daily_Usage, na.rm = TRUE)
    sprintf("Equiv: %.2f m³ (cubic meters)", tot / 1000)
  })
  
  output$kpi_avg_daily <- renderText({
    engine <- analyzed_data_reactive()
    sprintf("%.1f L / day", engine$mean_usage)
  })
  
  output$kpi_sd_daily <- renderText({
    engine <- analyzed_data_reactive()
    sprintf("Std Dev: ±%.1f L", engine$sd_usage)
  })
  
  output$kpi_highest_usage <- renderText({
    df <- analyzed_data_reactive()$data
    if (nrow(df) == 0) return("0 L")
    mx <- max(df$Total_Daily_Usage, na.rm = TRUE)
    sprintf("%.1f L", mx)
  })
  
  output$kpi_highest_date <- renderText({
    df <- analyzed_data_reactive()$data
    if (nrow(df) == 0) return("N/A")
    idx <- which.max(df$Total_Daily_Usage)
    sprintf("Peak Date: %s", df$Date[idx])
  })
  
  output$kpi_alert_count <- renderText({
    df <- analyzed_data_reactive()$data
    n_alerts <- sum(df$Status %in% c("WARNING", "POSSIBLE LEAKAGE"))
    sprintf("%d Alerts", n_alerts)
  })
  
  output$kpi_alert_breakdown <- renderText({
    df <- analyzed_data_reactive()$data
    n_leak <- sum(df$Status == "POSSIBLE LEAKAGE")
    n_warn <- sum(df$Status == "WARNING")
    sprintf("%d Leakage | %d Warning", n_leak, n_warn)
  })
  
  # Recent Alerts snapshot on Home Page
  output$home_recent_alerts_ui <- renderUI({
    df <- analyzed_data_reactive()$data
    alerts <- df %>% filter(Status %in% c("WARNING", "POSSIBLE LEAKAGE"))
    
    if (nrow(alerts) == 0) {
      div(
        class = "action-box action-box-normal",
        tags$span("✅ "),
        strong("Clean Record: "),
        "No unusual water usage or leakage alerts detected for the current period."
      )
    } else {
      # Show up to 4 most recent alerts
      recent <- tail(alerts, 4)
      recent <- recent[order(as.Date(recent$Date), decreasing = TRUE), ]
      
      tagList(
        lapply(seq_len(nrow(recent)), function(i) {
          row <- recent[i, ]
          box_class <- ifelse(row$Status == "POSSIBLE LEAKAGE", "action-box-leak", "action-box-warning")
          badge_class <- ifelse(row$Status == "POSSIBLE LEAKAGE", "badge-status-leak", "badge-status-warning")
          
          div(
            class = paste("action-box", box_class),
            style = "margin-bottom: 8px; padding: 10px 14px;",
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
              tags$strong(row$Date),
              tags$span(class = paste("badge-status", badge_class), row$Status)
            ),
            div(
              style = "font-size: 0.85rem;",
              sprintf("Recorded: %.1f L | Expected: %.1f L (Excess: +%.1f L)", 
                      row$Total_Daily_Usage, row$Expected_Usage, row$Excess_Usage)
            ),
            div(
              style = "font-size: 0.8rem; margin-top: 4px; font-style: italic;",
              row$Recommended_Action
            )
          )
        })
      )
    }
  })
  
  # ----------------------------------------------------------------------------
  # TAB 2: DATA RECORDS TABLE
  # ----------------------------------------------------------------------------
  output$table_record_count <- renderText({
    df <- filtered_raw_data()
    sprintf("%d Records Displayed", nrow(df))
  })
  
  output$table_water_records <- renderDT({
    df <- filtered_raw_data()
    req(nrow(df) > 0)
    
    # Calculate status for display if for single household
    display_df <- df
    display_df$Date <- as.character(display_df$Date)
    
    datatable(
      display_df,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        order = list(list(0, 'asc')),
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      colnames = c("Date", "Household ID", "Morning (L)", "Afternoon (L)", "Night (L)", "Total Daily (L)", "Data Tag")
    ) %>%
      formatRound(columns = c("Morning_Usage", "Afternoon_Usage", "Night_Usage", "Total_Daily_Usage"), digits = 1) %>%
      formatStyle(
        'Total_Daily_Usage',
        background = styleColorBar(range(display_df$Total_Daily_Usage, na.rm = TRUE), '#bae6fd'),
        backgroundSize = '95% 70%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # ----------------------------------------------------------------------------
  # TAB 3: CONSUMPTION ANALYSIS CHARTS (ggplot2)
  # ----------------------------------------------------------------------------
  output$analysis_summary_banner <- renderText({
    engine <- analyzed_data_reactive()
    sprintf("Analyzed Household: %s | Records: %d days | Mean: %.1f L/day | SD: %.1f L",
            input$analysis_household_select, engine$n_days, engine$mean_usage, engine$sd_usage)
  })
  
  # Custom ggplot2 theme for clean water design
  aqua_theme <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", color = "#0f172a", size = 13),
        plot.subtitle = element_text(color = "#64748b", size = 10, margin = margin(b = 10)),
        axis.title = element_text(face = "bold", color = "#334155", size = 10),
        axis.text = element_text(color = "#475569"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#f1f5f9"),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 9),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA)
      )
  }
  
  # Figure 1: Daily Water Consumption Trends vs. Statistical Baseline
  output$plot_daily_trend <- renderPlot({
    engine <- analyzed_data_reactive()
    df <- engine$data
    req(nrow(df) > 0)
    
    df$Date_Parsed <- as.Date(df$Date)
    
    p <- ggplot(df, aes(x = Date_Parsed, y = Total_Daily_Usage)) +
      # Ribbon indicating standard variation band
      geom_ribbon(aes(ymin = pmax(0, engine$mean_usage - engine$sd_usage), 
                      ymax = engine$threshold), 
                  fill = "#e0f2fe", alpha = 0.55) +
      # Trend Line
      geom_line(color = "#0284c7", linewidth = 0.9) +
      # Points colored by status
      geom_point(aes(color = Status), size = 2.8, alpha = 0.9) +
      # Baseline Mean line
      geom_hline(yintercept = engine$mean_usage, linetype = "dashed", color = "#059669", linewidth = 0.8) +
      # Upper Threshold line
      geom_hline(yintercept = engine$threshold, linetype = "dashed", color = "#dc2626", linewidth = 0.9) +
      scale_color_manual(
        values = c("NORMAL" = "#059669", "WARNING" = "#d97706", "POSSIBLE LEAKAGE" = "#dc2626", "INSUFFICIENT DATA" = "#94a3b8"),
        drop = FALSE
      ) +
      annotate("text", x = min(df$Date_Parsed), y = engine$threshold + (engine$sd_usage * 0.25), 
               label = sprintf("Threshold: Mean + %.1f SD = %.1f L", input$param_sd_multiplier, engine$threshold), 
               color = "#dc2626", fontface = "bold", hjust = 0, size = 3.6) +
      annotate("text", x = min(df$Date_Parsed), y = engine$mean_usage + (engine$sd_usage * 0.2), 
               label = sprintf("Historical Baseline Mean = %.1f L", engine$mean_usage), 
               color = "#059669", fontface = "bold", hjust = 0, size = 3.6) +
      labs(
        title = sprintf("Daily Water Consumption Timeline for %s", input$analysis_household_select),
        subtitle = "Blue band: Expected normal variation range | Dashed lines: Mean and Anomaly Threshold",
        x = "Date",
        y = "Total Daily Consumption (Litres)",
        color = "Status Classification"
      ) +
      aqua_theme()
    
    p
  })
  
  # Figure 2: Weekly Water Usage Comparisons
  output$plot_weekly_comparison <- renderPlot({
    df <- analyzed_data_reactive()$data
    req(nrow(df) > 0)
    
    df$Date_Parsed <- as.Date(df$Date)
    df$DayOfWeek <- factor(weekdays(df$Date_Parsed), 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    dow_agg <- df %>%
      group_by(DayOfWeek) %>%
      summarise(
        Avg_Usage = mean(Total_Daily_Usage, na.rm = TRUE),
        Max_Usage = max(Total_Daily_Usage, na.rm = TRUE),
        .groups = "drop"
      )
    
    overall_mean <- mean(df$Total_Daily_Usage, na.rm = TRUE)
    
    p <- ggplot(dow_agg, aes(x = DayOfWeek, y = Avg_Usage)) +
      geom_col(fill = "#0284c7", width = 0.58, alpha = 0.9) +
      geom_hline(yintercept = overall_mean, linetype = "dashed", color = "#d97706", linewidth = 0.8) +
      geom_text(aes(label = sprintf("%.0f L", Avg_Usage)), vjust = -0.5, fontface = "bold", size = 3.5, color = "#0f172a") +
      annotate("text", x = 1, y = overall_mean + 12, label = sprintf("Avg: %.0f L", overall_mean), color = "#d97706", hjust = 0, size = 3.2) +
      labs(
        title = "Average Daily Water Consumption by Day of Week",
        subtitle = "Identifies domestic cycles (e.g., weekend spikes vs. weekday baselines)",
        x = "Day of Week",
        y = "Average Daily Usage (Litres)"
      ) +
      ylim(0, max(dow_agg$Avg_Usage, na.rm = TRUE) * 1.25) +
      aqua_theme()
    
    p
  })
  
  # Figure 3: Time-of-Day Consumption Breakdown
  output$plot_time_of_day_breakdown <- renderPlot({
    df <- analyzed_data_reactive()$data
    req(nrow(df) > 0)
    
    breakdown <- data.frame(
      TimePeriod = factor(c("Morning", "Afternoon", "Night"), levels = c("Morning", "Afternoon", "Night")),
      Average_L = c(mean(df$Morning_Usage, na.rm = TRUE),
                    mean(df$Afternoon_Usage, na.rm = TRUE),
                    mean(df$Night_Usage, na.rm = TRUE)),
      Total_L = c(sum(df$Morning_Usage, na.rm = TRUE),
                  sum(df$Afternoon_Usage, na.rm = TRUE),
                  sum(df$Night_Usage, na.rm = TRUE))
    )
    
    p <- ggplot(breakdown, aes(x = TimePeriod, y = Average_L, fill = TimePeriod)) +
      geom_col(width = 0.52, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.1f L\n(%.1f%%)", Average_L, (Total_L / sum(Total_L)) * 100)), 
                vjust = -0.3, fontface = "bold", size = 3.5, color = "#0f172a") +
      scale_fill_manual(values = c("Morning" = "#38bdf8", "Afternoon" = "#0284c7", "Night" = "#0f172a")) +
      labs(
        title = "Time-of-Day Water Consumption Breakdown",
        subtitle = "Average usage and percentage share by daily period",
        x = "Period of the Day",
        y = "Average Usage (Litres)"
      ) +
      ylim(0, max(breakdown$Average_L) * 1.3) +
      aqua_theme()
    
    p
  })
  
  # Figure 4: Average Household Water Consumption & Distribution
  output$plot_usage_distribution <- renderPlot({
    df <- analyzed_data_reactive()$data
    req(nrow(df) > 0)
    
    mu <- mean(df$Total_Daily_Usage, na.rm = TRUE)
    med <- median(df$Total_Daily_Usage, na.rm = TRUE)
    
    p <- ggplot(df, aes(x = Total_Daily_Usage)) +
      geom_histogram(fill = "#e0f2fe", color = "#0284c7", bins = 20, alpha = 0.8) +
      geom_vline(xintercept = mu, linetype = "dashed", color = "#059669", linewidth = 1) +
      geom_vline(xintercept = med, linetype = "dotted", color = "#d97706", linewidth = 1) +
      annotate("text", x = mu, y = Inf, label = sprintf(" Mean: %.1f L", mu), 
               color = "#059669", fontface = "bold", hjust = -0.1, vjust = 2, size = 3.5) +
      annotate("text", x = med, y = Inf, label = sprintf(" Median: %.1f L", med), 
               color = "#d97706", fontface = "bold", hjust = 1.1, vjust = 4, size = 3.5) +
      labs(
        title = "Frequency Distribution of Daily Water Consumption",
        subtitle = "Shows skewness caused by persistent leaks or high-use days",
        x = "Total Daily Water Usage (Litres)",
        y = "Number of Days (Frequency)"
      ) +
      aqua_theme()
    
    p
  })
  
  # Figure 5: Normal vs Unusual Water Usage Summary
  output$plot_normal_vs_unusual <- renderPlot({
    df <- analyzed_data_reactive()$data
    req(nrow(df) > 0)
    
    status_summary <- df %>%
      group_by(Status) %>%
      summarise(
        DayCount = n(),
        TotalLitres = sum(Total_Daily_Usage, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(status_summary, aes(x = Status, y = DayCount, fill = Status)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%d Days\n(%.1f%%)", DayCount, (DayCount / sum(DayCount)) * 100)),
                vjust = -0.3, fontface = "bold", size = 3.5) +
      scale_fill_manual(values = c("NORMAL" = "#10b981", "WARNING" = "#f59e0b", "POSSIBLE LEAKAGE" = "#ef4444", "INSUFFICIENT DATA" = "#94a3b8")) +
      labs(
        title = "Days Classified by Water Consumption Status",
        subtitle = "Normal baseline compliance vs. detected anomaly frequency",
        x = "Operational Status",
        y = "Day Count"
      ) +
      ylim(0, max(status_summary$DayCount) * 1.25) +
      aqua_theme()
    
    p
  })
  
  # ----------------------------------------------------------------------------
  # TAB 4: POSSIBLE LEAKAGE DETECTION OUTPUTS
  # ----------------------------------------------------------------------------
  output$detection_baseline_metrics_ui <- renderUI({
    engine <- analyzed_data_reactive()
    tagList(
      div(style = "margin-bottom: 8px;", tags$strong("Baseline Mean (μ): "), sprintf("%.1f L / day", engine$mean_usage)),
      div(style = "margin-bottom: 8px;", tags$strong("Standard Deviation (σ): "), sprintf("±%.1f L", engine$sd_usage)),
      div(style = "margin-bottom: 8px;", tags$strong("SD Threshold (μ + kσ): "), sprintf("%.1f L / day (k = %.1f)", engine$threshold, input$param_sd_multiplier)),
      div(style = "margin-bottom: 8px;", tags$strong("Min % Threshold: "), sprintf("%.1f L / day (+%d%%)", engine$pct_threshold, input$param_min_pct_increase)),
      div(style = "margin-bottom: 8px;", tags$strong("Night Baseline Mean: "), sprintf("%.1f L / night", engine$night_mean)),
      div(style = "margin-bottom: 8px;", tags$strong("Night Anomaly Cutoff: "), sprintf("%.1f L / night", engine$night_mean + 1.8 * engine$night_sd))
    )
  })
  
  output$detection_status_badge <- renderText({
    df <- analyzed_data_reactive()$data
    n_leak <- sum(df$Status == "POSSIBLE LEAKAGE")
    if (n_leak > 0) {
      sprintf("⚠️ %d LEAKAGE INCIDENTS FLAGGED", n_leak)
    } else {
      "✅ SYSTEM NORMAL"
    }
  })
  
  # Detection Timeline Plot
  output$plot_leakage_detection_timeline <- renderPlot({
    engine <- analyzed_data_reactive()
    df <- engine$data
    req(nrow(df) > 0)
    
    df$Date_Parsed <- as.Date(df$Date)
    
    p <- ggplot(df, aes(x = Date_Parsed, y = Total_Daily_Usage)) +
      geom_line(color = "#94a3b8", linewidth = 0.7) +
      geom_point(aes(color = Status, size = Status), alpha = 0.95) +
      geom_hline(yintercept = engine$threshold, linetype = "dashed", color = "#ef4444", linewidth = 0.9) +
      geom_hline(yintercept = engine$mean_usage, linetype = "dotted", color = "#10b981", linewidth = 0.8) +
      scale_color_manual(values = c("NORMAL" = "#10b981", "WARNING" = "#f59e0b", "POSSIBLE LEAKAGE" = "#ef4444", "INSUFFICIENT DATA" = "#94a3b8")) +
      scale_size_manual(values = c("NORMAL" = 2.0, "WARNING" = 3.5, "POSSIBLE LEAKAGE" = 4.5, "INSUFFICIENT DATA" = 2.0)) +
      labs(
        title = "Statistical Leakage Anomaly Classification",
        subtitle = "Points sized and colored by rule classification severity",
        x = "Date",
        y = "Total Consumption (Litres)",
        color = "Anomaly Status",
        size = "Anomaly Status"
      ) +
      aqua_theme()
    
    p
  })
  
  # Night-Time Water Flow Analysis Plot
  output$plot_night_flow_analysis <- renderPlot({
    engine <- analyzed_data_reactive()
    df <- engine$data
    req(nrow(df) > 0)
    
    df$Date_Parsed <- as.Date(df$Date)
    night_cutoff <- engine$night_mean + 1.8 * engine$night_sd
    
    p <- ggplot(df, aes(x = Date_Parsed, y = Night_Usage)) +
      geom_line(color = "#0f172a", linewidth = 0.8) +
      geom_point(aes(color = Night_Usage > night_cutoff), size = 2.5) +
      geom_hline(yintercept = engine$night_mean, linetype = "dashed", color = "#059669") +
      geom_hline(yintercept = night_cutoff, linetype = "dashed", color = "#dc2626") +
      scale_color_manual(values = c("FALSE" = "#0284c7", "TRUE" = "#dc2626"), 
                         labels = c("Normal Night Flow", "Abnormal Night Surge")) +
      annotate("text", x = min(df$Date_Parsed), y = night_cutoff + 5, 
               label = sprintf("Night Threshold (%.1f L)", night_cutoff), 
               color = "#dc2626", hjust = 0, size = 3.3, fontface = "bold") +
      labs(
        title = "Overnight Water Consumption Flow (Midnight - 6:00 AM)",
        subtitle = "Persistent overnight flow strongly indicates leaking toilets or supply pipes",
        x = "Date",
        y = "Night Water Usage (Litres)",
        color = "Night Status"
      ) +
      aqua_theme()
    
    p
  })
  
  # ----------------------------------------------------------------------------
  # TAB 5: LEAKAGE ALERT PANEL
  # ----------------------------------------------------------------------------
  alert_records_reactive <- reactive({
    df <- analyzed_data_reactive()$data
    alerts <- df %>% filter(Status %in% c("WARNING", "POSSIBLE LEAKAGE"))
    
    if (input$filter_alert_severity != "All Alerts") {
      alerts <- alerts %>% filter(Status == input$filter_alert_severity)
    }
    
    alerts[order(as.Date(alerts$Date), decreasing = TRUE), ]
  })
  
  output$table_alert_records <- renderDT({
    alerts <- alert_records_reactive()
    
    if (nrow(alerts) == 0) {
      empty_df <- data.frame(
        Notice = "No alerts found matching current filter criteria."
      )
      return(datatable(empty_df, rownames = FALSE, options = list(dom = 't')))
    }
    
    display_alerts <- alerts %>%
      select(Date, Household_ID, Total_Daily_Usage, Expected_Usage, Excess_Usage, Status, Severity, Recommended_Action)
    
    datatable(
      display_alerts,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ftp'
      ),
      colnames = c("Alert Date", "Household", "Recorded (L)", "Expected (L)", "Excess (L)", "Status", "Severity", "Recommended Action")
    ) %>%
      formatRound(columns = c("Total_Daily_Usage", "Expected_Usage", "Excess_Usage"), digits = 1) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c("WARNING", "POSSIBLE LEAKAGE"),
          c("#fffbeb", "#fef2f2")
        ),
        color = styleEqual(
          c("WARNING", "POSSIBLE LEAKAGE"),
          c("#b45309", "#b91c1c")
        ),
        fontWeight = 'bold'
      )
  })
  
  output$alert_recommended_actions_ui <- renderUI({
    alerts <- alert_records_reactive()
    n_leak <- sum(alerts$Status == "POSSIBLE LEAKAGE")
    
    tagList(
      div(
        class = "action-box action-box-leak",
        h5(style = "margin-top: 0; font-weight: bold;", "1. Overnight Meter Test"),
        p("Record the water meter dial reading right before retiring to bed. Ensure no appliances (washers, dishwashers) run overnight. Read the meter immediately upon waking. Any advance in numbers confirms continuous leakage.")
      ),
      div(
        class = "action-box action-box-warning",
        h5(style = "margin-top: 0; font-weight: bold;", "2. Toilet Flapper Food Dye Test"),
        p("Put 4 to 6 drops of dark food coloring in the toilet water tank. Wait 15-20 minutes without flushing. If color seeps into the bowl, the flapper valve is deteriorated and silently wasting 100-300+ litres per day.")
      ),
      div(
        class = "action-box action-box-normal",
        h5(style = "margin-top: 0; font-weight: bold;", "3. Visible Fixture & Outdoor Audit"),
        p("Check under-sink P-traps, washing machine supply hoses, outdoor garden hose spigots, and water heater temperature-pressure relief valves for corrosion, moisture, or dripping.")
      )
    )
  })
  
  # ----------------------------------------------------------------------------
  # TAB 6: REPORTS & STATISTICAL EXPORTS
  # ----------------------------------------------------------------------------
  output$table_statistical_summary <- renderTable({
    engine <- analyzed_data_reactive()
    df <- engine$data
    req(nrow(df) > 0)
    
    n_days <- nrow(df)
    total_vol <- sum(df$Total_Daily_Usage, na.rm = TRUE)
    mean_val <- mean(df$Total_Daily_Usage, na.rm = TRUE)
    median_val <- median(df$Total_Daily_Usage, na.rm = TRUE)
    sd_val <- sd(df$Total_Daily_Usage, na.rm = TRUE)
    min_val <- min(df$Total_Daily_Usage, na.rm = TRUE)
    max_val <- max(df$Total_Daily_Usage, na.rm = TRUE)
    
    n_alerts <- sum(df$Status %in% c("WARNING", "POSSIBLE LEAKAGE"))
    n_leak <- sum(df$Status == "POSSIBLE LEAKAGE")
    total_excess <- sum(df$Excess_Usage, na.rm = TRUE)
    
    data.frame(
      Metric = c(
        "Total Days Analyzed",
        "Total Water Consumed",
        "Mean Daily Usage",
        "Median Daily Usage",
        "Standard Deviation (SD)",
        "Minimum Recorded Usage",
        "Maximum Recorded Usage",
        "Upper Anomaly Threshold (Mean + 2SD)",
        "Total Possible Leakage Incidents",
        "Estimated Excess Water Volume"
      ),
      Value = c(
        sprintf("%d Days", n_days),
        sprintf("%s Litres (%.2f m³)", format(round(total_vol, 1), big.mark = ","), total_vol / 1000),
        sprintf("%.1f Litres / day", mean_val),
        sprintf("%.1f Litres / day", median_val),
        sprintf("%.1f Litres", sd_val),
        sprintf("%.1f Litres", min_val),
        sprintf("%.1f Litres", max_val),
        sprintf("%.1f Litres / day", engine$threshold),
        sprintf("%d Days", n_leak),
        sprintf("%.1f Litres (%.2f m³)", total_excess, total_excess / 1000)
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$report_impact_summary_ui <- renderUI({
    engine <- analyzed_data_reactive()
    df <- engine$data
    total_vol <- sum(df$Total_Daily_Usage, na.rm = TRUE)
    total_excess <- sum(df$Excess_Usage, na.rm = TRUE)
    excess_pct <- ifelse(total_vol > 0, (total_excess / total_vol) * 100, 0)
    
    div(
      p(
        strong("Excess Water Loss: "), 
        sprintf("%.1f Litres", total_excess),
        sprintf(" (accounting for %.1f%% of all water consumed across the period).", excess_pct)
      ),
      p(
        strong("Ecological Impact: "),
        "Unaddressed household leaks typically waste between 2,000 and 10,000 litres of clean potable water monthly per fixture, placing unnecessary strain on municipal treatment infrastructure."
      ),
      p(
        strong("Recommended Next Step: "),
        "Generate and download the formal PDF report below for plumbing inspection documentation."
      )
    )
  })
  
  # Download Filtered CSV Handler
  output$btn_download_csv <- downloadHandler(
    filename = function() {
      sprintf("water_consumption_audit_%s_%s.csv", 
              input$analysis_household_select, format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      export_df <- analyzed_data_reactive()$data
      write.csv(export_df, file, row.names = FALSE)
    }
  )
  
  # Download PDF Report Handler
  output$btn_download_pdf <- downloadHandler(
    filename = function() {
      sprintf("Water_Leakage_Report_%s_%s.pdf", 
              input$analysis_household_select, format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      engine <- analyzed_data_reactive()
      df <- engine$data
      
      # Prepare stats list
      stats_list <- list(
        start_date = min(df$Date),
        end_date = max(df$Date),
        n_days = nrow(df),
        total_usage = sum(df$Total_Daily_Usage, na.rm = TRUE),
        mean_usage = engine$mean_usage,
        sd_usage = engine$sd_usage,
        threshold = engine$threshold,
        max_usage = max(df$Total_Daily_Usage, na.rm = TRUE),
        max_date = df$Date[which.max(df$Total_Daily_Usage)],
        n_alerts = sum(df$Status %in% c("WARNING", "POSSIBLE LEAKAGE")),
        n_leakage = sum(df$Status == "POSSIBLE LEAKAGE"),
        n_warning = sum(df$Status == "WARNING"),
        total_excess = sum(df$Excess_Usage, na.rm = TRUE)
      )
      
      # Prepare alerts dataframe
      alerts <- df %>% 
        filter(Status %in% c("WARNING", "POSSIBLE LEAKAGE")) %>%
        transmute(
          Date = Date,
          Household_ID = Household_ID,
          Recorded_Usage = Total_Daily_Usage,
          Expected_Usage = Expected_Usage,
          Excess_Usage = Excess_Usage,
          Severity = Severity,
          Recommended_Action = Recommended_Action
        )
      
      # Call the generator
      generate_water_pdf_report(df, stats_list, alerts, file, household_id = input$analysis_household_select)
    }
  )
}

# ------------------------------------------------------------------------------
# LAUNCH APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

