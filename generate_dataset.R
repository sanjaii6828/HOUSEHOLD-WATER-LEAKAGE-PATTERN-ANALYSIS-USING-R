# ==============================================================================
# Household Water Leakage Pattern Analysis
# Script: generate_dataset.R
# Purpose: Automatically generate a realistic sample dataset containing at
#          least 60 days of household water consumption records with normal
#          usage, isolated high consumption spikes, and simulated periods of
#          persistent water leakage.
# ==============================================================================

# Ensure reproducibility
set.seed(42)

generate_water_data <- function(output_file = "water_usage.csv", n_days = 90) {
  # Base start date
  start_date <- as.Date("2026-01-01")
  dates <- seq(from = start_date, by = "day", length.out = n_days)
  
  # ----------------------------------------------------------------------------
  # Household 1 (Primary Demo Household - contains persistent leakage demonstration)
  # ----------------------------------------------------------------------------
  # Normal baseline parameters:
  # Morning: ~135 L (SD ~12)
  # Afternoon: ~105 L (SD ~10)
  # Night: ~45 L (SD ~6)
  # Total Normal: ~285 L
  
  hh1_morning   <- round(rnorm(n_days, mean = 135, sd = 12), 1)
  hh1_afternoon <- round(rnorm(n_days, mean = 105, sd = 10), 1)
  hh1_night     <- round(rnorm(n_days, mean = 45,  sd = 6),  1)
  hh1_tag       <- rep("Normal Usage", n_days)
  
  # Ensure no negative values
  hh1_morning   <- pmax(hh1_morning, 40)
  hh1_afternoon <- pmax(hh1_afternoon, 30)
  hh1_night     <- pmax(hh1_night, 15)
  
  # 1. Occasional isolated high-consumption days (e.g., heavy laundry or hosting guests)
  # Day 15 (Single Day High Usage)
  hh1_morning[15]   <- 210.5
  hh1_afternoon[15] <- 245.0
  hh1_night[15]     <- 62.0
  hh1_tag[15]       <- "Demonstration: Isolated High Consumption"
  
  # Day 30 (Single Day High Usage)
  hh1_morning[30]   <- 195.0
  hh1_afternoon[30] <- 230.5
  hh1_night[30]     <- 58.0
  hh1_tag[30]       <- "Demonstration: Isolated High Consumption"

  # 2. Simulated Persistent Leakage Incident (Days 42 to 47 = 6 consecutive days)
  # Cause: Faulty toilet flapper valve / pipe hairline fracture causing continuous flow
  # Note significant night-time spike (180 - 230 L instead of normal 45 L)
  leak_indices <- 42:47
  hh1_morning[leak_indices]   <- round(rnorm(length(leak_indices), mean = 240, sd = 15), 1)
  hh1_afternoon[leak_indices] <- round(rnorm(length(leak_indices), mean = 220, sd = 12), 1)
  hh1_night[leak_indices]     <- round(rnorm(length(leak_indices), mean = 210, sd = 18), 1) # Severe night leak
  hh1_tag[leak_indices]       <- "Demonstration: Simulated Persistent Leakage"

  # 3. Simulated Secondary Minor Leakage Incident (Days 72 to 74 = 3 consecutive days)
  # Cause: Outdoor hose bib / garden valve slow drip
  minor_leak_indices <- 72:74
  hh1_morning[minor_leak_indices]   <- round(rnorm(length(minor_leak_indices), mean = 185, sd = 10), 1)
  hh1_afternoon[minor_leak_indices] <- round(rnorm(length(minor_leak_indices), mean = 175, sd = 10), 1)
  hh1_night[minor_leak_indices]     <- round(rnorm(length(minor_leak_indices), mean = 135, sd = 12), 1)
  hh1_tag[minor_leak_indices]       <- "Demonstration: Simulated Moderate Leakage"

  hh1_total <- round(hh1_morning + hh1_afternoon + hh1_night, 1)

  df_hh1 <- data.frame(
    Date = format(dates, "%Y-%m-%d"),
    Household_ID = "HH001",
    Morning_Usage = hh1_morning,
    Afternoon_Usage = hh1_afternoon,
    Night_Usage = hh1_night,
    Total_Daily_Usage = hh1_total,
    Demonstration_Tag = hh1_tag,
    stringsAsFactors = FALSE
  )

  # ----------------------------------------------------------------------------
  # Household 2 (Smaller 2-person household - steady normal baseline)
  # ----------------------------------------------------------------------------
  hh2_morning   <- round(rnorm(n_days, mean = 90, sd = 8), 1)
  hh2_afternoon <- round(rnorm(n_days, mean = 65, sd = 7), 1)
  hh2_night     <- round(rnorm(n_days, mean = 30, sd = 4), 1)
  hh2_tag       <- rep("Normal Usage", n_days)
  
  hh2_morning   <- pmax(hh2_morning, 25)
  hh2_afternoon <- pmax(hh2_afternoon, 20)
  hh2_night     <- pmax(hh2_night, 10)
  
  # Single weekend spike
  hh2_morning[22]   <- 150.0
  hh2_afternoon[22] <- 140.0
  hh2_tag[22]       <- "Demonstration: Isolated High Consumption"
  
  hh2_total <- round(hh2_morning + hh2_afternoon + hh2_night, 1)

  df_hh2 <- data.frame(
    Date = format(dates, "%Y-%m-%d"),
    Household_ID = "HH002",
    Morning_Usage = hh2_morning,
    Afternoon_Usage = hh2_afternoon,
    Night_Usage = hh2_night,
    Total_Daily_Usage = hh2_total,
    Demonstration_Tag = hh2_tag,
    stringsAsFactors = FALSE
  )

  # ----------------------------------------------------------------------------
  # Household 3 (Family household - 4-5 people, persistent leak at end of month)
  # ----------------------------------------------------------------------------
  hh3_morning   <- round(rnorm(n_days, mean = 180, sd = 15), 1)
  hh3_afternoon <- round(rnorm(n_days, mean = 140, sd = 12), 1)
  hh3_night     <- round(rnorm(n_days, mean = 60,  sd = 8),  1)
  hh3_tag       <- rep("Normal Usage", n_days)
  
  hh3_morning   <- pmax(hh3_morning, 60)
  hh3_afternoon <- pmax(hh3_afternoon, 40)
  hh3_night     <- pmax(hh3_night, 20)
  
  # Persistent leak on days 80 to 83 (4 days)
  hh3_leak <- 80:83
  hh3_morning[hh3_leak]   <- round(rnorm(length(hh3_leak), mean = 270, sd = 15), 1)
  hh3_afternoon[hh3_leak] <- round(rnorm(length(hh3_leak), mean = 240, sd = 12), 1)
  hh3_night[hh3_leak]     <- round(rnorm(length(hh3_leak), mean = 200, sd = 15), 1)
  hh3_tag[hh3_leak]       <- "Demonstration: Simulated Persistent Leakage"
  
  hh3_total <- round(hh3_morning + hh3_afternoon + hh3_night, 1)

  df_hh3 <- data.frame(
    Date = format(dates, "%Y-%m-%d"),
    Household_ID = "HH003",
    Morning_Usage = hh3_morning,
    Afternoon_Usage = hh3_afternoon,
    Night_Usage = hh3_night,
    Total_Daily_Usage = hh3_total,
    Demonstration_Tag = hh3_tag,
    stringsAsFactors = FALSE
  )

  # Combine all households
  combined_df <- rbind(df_hh1, df_hh2, df_hh3)
  
  # Ensure target directories exist
  if (!dir.exists("sample_data")) {
    dir.create("sample_data", recursive = TRUE)
  }
  
  # Save to primary location and sample_data directory
  write.csv(combined_df, output_file, row.names = FALSE)
  write.csv(combined_df, file.path("sample_data", basename(output_file)), row.names = FALSE)
  
  cat(sprintf("[SUCCESS] Generated %d records across %d days for 3 households.\n", 
              nrow(combined_df), n_days))
  cat(sprintf("[SAVED] Files written to '%s' and 'sample_data/%s'.\n", 
              output_file, basename(output_file)))
  
  return(invisible(combined_df))
}

# Run directly if invoked as script
if (!interactive() || identical(environment(), globalenv())) {
  generate_water_data("water_usage.csv", n_days = 90)
}

