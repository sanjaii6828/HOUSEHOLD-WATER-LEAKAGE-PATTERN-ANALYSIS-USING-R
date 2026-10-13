# ==============================================================================
# Household Water Leakage Pattern Analysis
# Script: reports/generate_pdf_report.R
# Purpose: Generate a professional, multi-page PDF report containing executive
#          water consumption summaries, ggplot2 trend visualizations, and
#          flagged leakage anomaly records with recommended troubleshooting.
# ==============================================================================

library(ggplot2)
library(grid)
library(gridExtra)

generate_water_pdf_report <- function(df_data, stats_list, alerts_df, output_path, household_id = "HH001") {
  # Set up PDF graphics device (A4 landscape or standard letter portrait: 8.5 x 11 inches)
  pdf(output_path, width = 8.5, height = 11, onefile = TRUE)
  
  # Colors for charts
  col_primary   <- "#0284c7"
  col_dark      <- "#0f172a"
  col_threshold <- "#ef4444"
  col_mean      <- "#059669"
  col_accent    <- "#06b6d4"
  
  # ----------------------------------------------------------------------------
  # PAGE 1: Executive Summary & Consumption Trends
  # ----------------------------------------------------------------------------
  grid.newpage()
  
  # Page 1 Header Banner
  pushViewport(viewport(x = 0.5, y = 0.94, width = 0.9, height = 0.1))
  grid.rect(gp = gpar(fill = "#0284c7", col = NA))
  grid.text("HOUSEHOLD WATER LEAKAGE PATTERN ANALYSIS", 
            x = 0.05, y = 0.65, just = "left", 
            gp = gpar(col = "white", fontsize = 16, fontface = "bold"))
  grid.text(sprintf("Comprehensive Consumption Audit & Leakage Anomaly Report | Household: %s", household_id), 
            x = 0.05, y = 0.35, just = "left", 
            gp = gpar(col = "#e0f2fe", fontsize = 9))
  grid.text(sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M")), 
            x = 0.95, y = 0.35, just = "right", 
            gp = gpar(col = "#bae6fd", fontsize = 8))
  popViewport()
  
  # Executive Key Metrics Table
  pushViewport(viewport(x = 0.5, y = 0.82, width = 0.9, height = 0.12))
  
  summary_matrix <- matrix(c(
    "Analyzed Period", sprintf("%s to %s (%d Days)", stats_list$start_date, stats_list$end_date, stats_list$n_days),
    "Total Water Consumed", sprintf("%s Litres (%.2f m³)", format(round(stats_list$total_usage, 1), big.mark = ","), stats_list$total_usage / 1000),
    "Average Daily Usage", sprintf("%.1f Litres / day", stats_list$mean_usage),
    "Standard Deviation (SD)", sprintf("%.1f Litres", stats_list$sd_usage),
    "Anomaly Threshold (Mean+2SD)", sprintf("%.1f Litres / day", stats_list$threshold),
    "Peak Recorded Usage", sprintf("%.1f Litres (%s)", stats_list$max_usage, stats_list$max_date),
    "Possible Leakage Alerts", sprintf("%d Alerts (%d Possible Leak, %d Warning)", 
                                       stats_list$n_alerts, stats_list$n_leakage, stats_list$n_warning),
    "Estimated Excess Volume", sprintf("%.1f Litres", stats_list$total_excess)
  ), ncol = 4, byrow = FALSE)
  
  t_summary <- tableGrob(summary_matrix, 
                         cols = c("Metric", "Value", "Statistical Baseline", "Observed Impact"),
                         theme = ttheme_minimal(
                           core = list(
                             fg_params = list(fontsize = 8, hjust = 0, x = 0.05),
                             bg_params = list(fill = c("#f8fafc", "#f0f9ff"))
                           ),
                           colhead = list(
                             fg_params = list(fontsize = 8.5, fontface = "bold", col = "#0f172a"),
                             bg_params = list(fill = "#e2e8f0")
                           )
                         ))
  grid.draw(t_summary)
  popViewport()
  
  # Plot 1: Daily Water Consumption Timeline with Baseline & Threshold
  df_plot <- df_data
  df_plot$Date_Parsed <- as.Date(df_plot$Date)
  
  p1 <- ggplot(df_plot, aes(x = Date_Parsed, y = Total_Daily_Usage)) +
    geom_ribbon(aes(ymin = pmax(0, stats_list$mean_usage - stats_list$sd_usage), 
                    ymax = stats_list$threshold), 
                fill = "#e0f2fe", alpha = 0.5) +
    geom_line(color = col_primary, linewidth = 0.8) +
    geom_point(aes(color = Status), size = 2) +
    geom_hline(yintercept = stats_list$mean_usage, linetype = "dashed", color = col_mean, linewidth = 0.7) +
    geom_hline(yintercept = stats_list$threshold, linetype = "dashed", color = col_threshold, linewidth = 0.8) +
    scale_color_manual(values = c("NORMAL" = "#059669", "WARNING" = "#d97706", "POSSIBLE LEAKAGE" = "#dc2626"),
                       drop = FALSE) +
    annotate("text", x = min(df_plot$Date_Parsed), y = stats_list$threshold + 20, 
             label = sprintf("Threshold (Mean+2SD = %.1f L)", stats_list$threshold), 
             color = col_threshold, hjust = 0, size = 2.8, fontface = "bold") +
    annotate("text", x = min(df_plot$Date_Parsed), y = stats_list$mean_usage + 15, 
             label = sprintf("Baseline Mean (%.1f L)", stats_list$mean_usage), 
             color = col_mean, hjust = 0, size = 2.8) +
    labs(
      title = "Figure 1: Daily Water Consumption Timeline & Anomaly Threshold",
      subtitle = "Shaded band indicates expected variation; points indicate statistical status.",
      x = "Date",
      y = "Total Usage (Litres)",
      color = "Status"
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(face = "bold", color = col_dark, size = 10),
      plot.subtitle = element_text(color = "#64748b", size = 7.5),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  
  pushViewport(viewport(x = 0.5, y = 0.52, width = 0.9, height = 0.28))
  grid.draw(ggplotGrob(p1))
  popViewport()
  
  # Plot 2: Time-of-Day Usage Breakdown (Morning, Afternoon, Night)
  time_breakdown <- data.frame(
    Period = factor(c("Morning", "Afternoon", "Night"), levels = c("Morning", "Afternoon", "Night")),
    Total_L = c(sum(df_plot$Morning_Usage, na.rm = TRUE),
                sum(df_plot$Afternoon_Usage, na.rm = TRUE),
                sum(df_plot$Night_Usage, na.rm = TRUE)),
    Avg_L = c(mean(df_plot$Morning_Usage, na.rm = TRUE),
              mean(df_plot$Afternoon_Usage, na.rm = TRUE),
              mean(df_plot$Night_Usage, na.rm = TRUE))
  )
  
  p2 <- ggplot(time_breakdown, aes(x = Period, y = Avg_L, fill = Period)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f L", Avg_L)), vjust = -0.4, size = 3, fontface = "bold") +
    scale_fill_manual(values = c("Morning" = "#38bdf8", "Afternoon" = "#0284c7", "Night" = "#1e293b")) +
    labs(
      title = "Figure 2: Average Consumption by Time of Day",
      x = "Time Period",
      y = "Average Litres"
    ) +
    ylim(0, max(time_breakdown$Avg_L) * 1.25) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(face = "bold", color = col_dark, size = 9),
      panel.grid.minor = element_blank()
    )
  
  # Plot 3: Day of the Week Usage
  df_plot$DayOfWeek <- factor(weekdays(df_plot$Date_Parsed), 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  dow_summary <- aggregate(Total_Daily_Usage ~ DayOfWeek, data = df_plot, FUN = mean)
  
  p3 <- ggplot(dow_summary, aes(x = DayOfWeek, y = Total_Daily_Usage)) +
    geom_col(fill = "#06b6d4", width = 0.6) +
    geom_text(aes(label = sprintf("%.0f", Total_Daily_Usage)), vjust = -0.4, size = 2.7) +
    labs(
      title = "Figure 3: Average Daily Usage by Day of Week",
      x = "Day of Week",
      y = "Avg Litres"
    ) +
    ylim(0, max(dow_summary$Total_Daily_Usage) * 1.2) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(face = "bold", color = col_dark, size = 9),
      axis.text.x = element_text(angle = 25, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  # Draw Figure 2 (Left)
  pushViewport(viewport(x = 0.27, y = 0.20, width = 0.44, height = 0.24))
  grid.draw(ggplotGrob(p2))
  popViewport()
  
  # Draw Figure 3 (Right)
  pushViewport(viewport(x = 0.73, y = 0.20, width = 0.44, height = 0.24))
  grid.draw(ggplotGrob(p3))
  popViewport()
  
  # Page 1 Footer
  pushViewport(viewport(x = 0.5, y = 0.04, width = 0.9, height = 0.04))
  grid.text("Household Water Leakage Pattern Analysis | Page 1 of 2", 
            x = 0.5, y = 0.5, gp = gpar(col = "#94a3b8", fontsize = 8))
  popViewport()
  
  # ----------------------------------------------------------------------------
  # PAGE 2: Leakage Alert Log & Practical Recommendations
  # ----------------------------------------------------------------------------
  grid.newpage()
  
  # Page 2 Header Banner
  pushViewport(viewport(x = 0.5, y = 0.94, width = 0.9, height = 0.08))
  grid.rect(gp = gpar(fill = "#0369a1", col = NA))
  grid.text("LEAKAGE ALERT LOG & ACTIONABLE RECOMMENDATIONS", 
            x = 0.05, y = 0.65, just = "left", 
            gp = gpar(col = "white", fontsize = 14, fontface = "bold"))
  grid.text("Statistical Anomaly Flagging | Rule: Daily Usage > Historical Mean + 2 SD & Night Pattern Analysis", 
            x = 0.05, y = 0.35, just = "left", 
            gp = gpar(col = "#e0f2fe", fontsize = 8.5))
  popViewport()
  
  # Alerts Table
  pushViewport(viewport(x = 0.5, y = 0.65, width = 0.9, height = 0.42))
  if (nrow(alerts_df) > 0) {
    # Limit table rows to top 10 for printable formatting
    display_alerts <- alerts_df[1:min(10, nrow(alerts_df)), ]
    table_data <- data.frame(
      Date = display_alerts$Date,
      Recorded_L = sprintf("%.1f", display_alerts$Recorded_Usage),
      Expected_L = sprintf("%.1f", display_alerts$Expected_Usage),
      Excess_L = sprintf("+%.1f", display_alerts$Excess_Usage),
      Severity = display_alerts$Severity,
      Action = substr(display_alerts$Recommended_Action, 1, 48) # compact text
    )
    names(table_data) <- c("Alert Date", "Recorded (L)", "Expected (L)", "Excess (L)", "Severity", "Recommended Action")
    
    t_alerts <- tableGrob(table_data, rows = NULL,
                          theme = ttheme_default(
                            core = list(
                              fg_params = list(fontsize = 7.5, hjust = 0, x = 0.03),
                              bg_params = list(fill = c("#ffffff", "#f8fafc"))
                            ),
                            colhead = list(
                              fg_params = list(fontsize = 8, fontface = "bold", col = "white"),
                              bg_params = list(fill = "#0284c7")
                            )
                          ))
    grid.draw(t_alerts)
  } else {
    grid.rect(gp = gpar(fill = "#ecfdf5", col = "#a7f3d0"))
    grid.text("[NORMAL] No anomalous water leakage events were detected during this period.",
              gp = gpar(col = "#059669", fontsize = 10, fontface = "bold"))
  }
  popViewport()
  
  # Practical Recommendations Box
  pushViewport(viewport(x = 0.5, y = 0.28, width = 0.9, height = 0.22))
  grid.rect(gp = gpar(fill = "#f0f9ff", col = "#bae6fd", lwd = 1.5))
  grid.text("PRACTICAL STEP-BY-STEP LEAKAGE TROUBLESHOOTING PROTOCOL", 
            x = 0.04, y = 0.88, just = "left", 
            gp = gpar(col = "#0369a1", fontsize = 9.5, fontface = "bold"))
  
  rec_text <- c(
    "1. OVERNIGHT METER TEST: Note water meter readings right before bed and first thing in the morning,",
    "   ensuring no fixtures/appliances run overnight. Any change confirms continuous unmetered leakage.",
    "2. TOILET TANK FLAPPER INSPECTION: Place 4-5 drops of food coloring into the toilet tank without flushing.",
    "   If color appears in the toilet bowl within 15 minutes, the flapper valve is worn and leaking.",
    "3. FAUCET & APPLIANCE AUDIT: Check washers, under-sink valves, washing machine hoses, and water heaters.",
    "4. OUTDOOR & IRRIGATION: Inspect garden hose bibbs and drip lines for damp ground or pressure drops."
  )
  for (i in seq_along(rec_text)) {
    grid.text(rec_text[i], x = 0.04, y = 0.72 - (i - 1) * 0.12, just = "left", 
              gp = gpar(col = "#1e293b", fontsize = 7.8))
  }
  popViewport()
  
  # Academic Disclaimer Box
  pushViewport(viewport(x = 0.5, y = 0.11, width = 0.9, height = 0.09))
  grid.rect(gp = gpar(fill = "#fffbeb", col = "#fde68a"))
  grid.text("ACADEMIC & METHODOLOGICAL DISCLAIMER", 
            x = 0.04, y = 0.75, just = "left", 
            gp = gpar(col = "#b45309", fontsize = 8, fontface = "bold"))
  grid.text("This statistical analysis flags unusual consumption exceeding historical averages (Mean + 2 SD) and abnormal night-time", 
            x = 0.04, y = 0.50, just = "left", gp = gpar(col = "#78350f", fontsize = 7.2))
  grid.text("flow. Statistical deviations do not prove an actual physical leak; elevated usage may result from visitors, gardening, or cleaning.", 
            x = 0.04, y = 0.25, just = "left", gp = gpar(col = "#78350f", fontsize = 7.2))
  popViewport()
  
  # Page 2 Footer
  pushViewport(viewport(x = 0.5, y = 0.03, width = 0.9, height = 0.03))
  grid.text("Household Water Leakage Pattern Analysis | Page 2 of 2", 
            x = 0.5, y = 0.5, gp = gpar(col = "#94a3b8", fontsize = 8))
  popViewport()
  
  dev.off()
  return(output_path)
}
