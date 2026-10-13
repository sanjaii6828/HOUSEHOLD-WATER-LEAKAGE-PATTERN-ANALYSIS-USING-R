# 💧 Household Water Leakage Pattern Analysis Using R Programming

An end-to-end, beginner-friendly, and fully functional **R Programming** academic project designed to monitor household water consumption, visualize daily usage trends, identify unusual consumption anomalies, and detect potential domestic water leakages using transparent statistical methods.

Built specifically for college coursework, academic demonstrations, and smart home water conservation studies.

---

## 🌟 Key Highlights

- **100% R-Powered**: Developed purely in R using RStudio and Shiny. No Python or complex black-box machine learning models required.
- **Statistical Leakage Detection Engine**: Rule-based detection using historical mean ($\mu$), standard deviation ($\sigma$), user-configurable sensitivity, consecutive-day persistence, and overnight flow auditing.
- **Modern Water-Themed UI**: Clean blue-and-white aesthetic with KPI summary cards, water-drop badges, searchable data tables, and dynamic visual status indicators.
- **Rich Visualizations**: Interactive `ggplot2` graphs covering timelines, weekly day-of-week cycles, time-of-day breakdowns, usage distributions, and anomaly classifications.
- **Built-in Sample & Custom Data**: Includes a pre-configured 90-day multi-household dataset with simulated real-world leakage events, plus an interactive form and CSV uploader to test custom data.
- **Instant PDF & CSV Reporting**: Zero-configuration PDF report generation using R's built-in graphics engine and `gridExtra` (no external LaTeX/Pandoc installation required).

---

## 📁 Project Folder Structure

```
Household_Water_Leakage_Analysis/
│
├── app.R                       # Main Shiny web dashboard application
├── generate_dataset.R          # Script to generate reproducible 90-day sample data
├── water_usage.csv             # Primary sample dataset (90 days, 3 households)
├── requirements.txt            # Package reference list (explains R package usage)
├── README.md                   # Complete documentation and execution guide
│
├── www/
│   └── style.css               # Water-themed modern dashboard stylesheet
│
├── reports/
│   └── generate_pdf_report.R   # Multi-page PDF report generation script
│
└── sample_data/
    └── water_usage.csv         # Backup copy of the standard sample dataset
```

### Purpose of Each File:
1. **`app.R`**: The core application file containing both the user interface (`ui`) and reactive server logic (`server`). Defines all dashboard tabs, interactive graphs, data inputs, and statistical calculations.
2. **`generate_dataset.R`**: Standalone generator script producing 90 days of realistic consumption data across 3 households (`HH001`, `HH002`, `HH003`) with baseline usage, single-day spikes, and simulated multi-day leakage incidents.
3. **`water_usage.csv`**: The default dataset loaded on startup.
4. **`www/style.css`**: Custom styling that gives the application a polished blue-and-white water appearance, rounded KPI cards, and animated alert badges.
5. **`reports/generate_pdf_report.R`**: Helper script that builds a formal, multi-page executive PDF audit report with tables and embedded `ggplot2` figures.
6. **`requirements.txt`**: Informational package list explaining dependencies.

---

## 🛠️ Technologies Used

| Technology | Purpose |
| :--- | :--- |
| **R (v4.0+)** | Primary programming language |
| **RStudio** | Recommended integrated development environment |
| **Shiny** | Interactive web dashboard framework |
| **ggplot2** | Publication-quality statistical visualizations |
| **dplyr** | Fast, intuitive tabular data manipulation |
| **readr** | Robust CSV parsing and file input/output |
| **DT** | Interactive, searchable, and paginated HTML tables |
| **gridExtra / grid** | High-resolution multi-page PDF reporting |

---

## 🔬 Statistical Leakage Detection Methodology

The application detects anomalies through transparent, verifiable descriptive statistics:

### 1. Historical Daily Baseline
For a selected household with $N \ge 7$ days of data, the system calculates:
- **Baseline Mean Daily Consumption**: 
  $$\mu = \frac{1}{N} \sum_{i=1}^{N} \text{Total\_Daily\_Usage}_i$$
- **Standard Deviation**: 
  $$\sigma = \sqrt{\frac{1}{N-1} \sum_{i=1}^{N} (\text{Total\_Daily\_Usage}_i - \mu)^2}$$

### 2. Dual Threshold Anomaly Rule
To minimize false alarms while catching real spikes, a day is flagged as anomalous if it simultaneously satisfies:
1. **Statistical Variance Threshold**: $\text{Daily Usage} > \mu + (k \times \sigma)$ *(Default multiplier $k = 2.0$)*
2. **Minimum Percentage Increase**: $\text{Daily Usage} > \mu \times (1 + \text{Min\_Pct\_Increase} / 100)$ *(Default $+25\%$)*

### 3. Consecutive Day Persistence & Severity Escalation
- **NORMAL**: Consumption falls within expected statistical limits ($\le \mu + k\sigma$).
- **WARNING (Moderate Severity)**: An isolated 1-day spike exceeding the threshold (commonly caused by weekend laundry, entertaining guests, or garden watering).
- **POSSIBLE LEAKAGE (High Severity)**: Anomaly persists for **2 or more consecutive days** ($streak \ge \text{Consecutive\_Days}$). Consecutive elevated consumption strongly indicates continuous water loss.

### 4. Overnight Flow Audit (Midnight – 6:00 AM)
Because household members are typically asleep overnight, nighttime water consumption should remain consistently low (baseline ~30–50 L). 
If night-time usage spikes beyond $\mu_{night} + 1.8\sigma_{night}$, the system flags a **Night Flow Anomaly**, pointing specifically to continuously running toilet flappers or hidden pipe leaks.

### 5. Academic & Practical Disclaimer
> **Important Note:** Statistical anomalies signify mathematical deviations from historical patterns and do not physically prove a pipe fracture. High usage can also result from legitimate household events such as pool filling, pressure washing, or visiting relatives.

---

## 🖥️ Dashboard Sections

1. **🏠 Home Dashboard**:
   - Hero welcome banner and workflow overview.
   - 4 Summary KPI Cards: *Total Water Consumed*, *Average Daily Usage*, *Highest Recorded Usage*, and *Active Leakage Alerts*.
   - System methodology explanation and latest alert preview.

2. **📋 Water Usage Data**:
   - Upload custom CSV files with automatic format validation.
   - Interactive data table (`DT`) with column search, sorting, and pagination.
   - Interactive **Add Record** form to enter new readings (calculates Total = Morning + Afternoon + Night automatically).
   - "Restore Sample Data" button to reset the view at any time.

3. **📊 Consumption Analysis**:
   - **Figure 1**: Daily usage timeline with baseline mean line, 2-SD upper threshold, and standard variation ribbon.
   - **Figure 2**: Day-of-week comparison bar chart (Monday through Sunday).
   - **Figure 3**: Time-of-day breakdown (Morning vs Afternoon vs Night).
   - **Figure 4**: Histogram and density distribution with Mean and Median markers.
   - **Figure 5**: Proportional summary of Normal vs Warning vs Leakage days.

4. **🔍 Possible Leakage Detection**:
   - Interactive parameter sliders: SD Multiplier ($k$), Min % Increase, Consecutive Days, and Night Flow Audit toggle.
   - Live calculated baseline metrics card.
   - Classified anomaly timeline plot.
   - Overnight flow analysis plot.

5. **🚨 Leakage Alert Panel**:
   - Filterable alert records table showing Alert Date, Recorded Usage, Expected Usage, Excess Volume, Severity, and Recommended Action.
   - Practical step-by-step leak inspection checklist (Overnight meter test, Toilet tank food dye test, Fixture audits).

6. **📄 Reports & Downloads**:
   - Tabular summary of key metrics (Total Volume, Excess Volume, Leakage %).
   - One-click **Download Filtered Data (CSV)** button.
   - One-click **Download Formal Report (PDF)** button.

---

## 🚀 Step-by-Step Installation & Execution Guide

### Step 1: Install R and RStudio
- Download and install **R** (version 4.0 or higher) from [CRAN](https://cran.r-project.org/).
- Download and install **RStudio Desktop** from [Posit](https://posit.co/download/rstudio-desktop/).

### Step 2: Open the Project
1. Extract or clone this project folder to your computer.
2. Launch RStudio.
3. In RStudio, go to **File** → **Open Project...** (or set working directory to this folder via `Session` → `Set Working Directory` → `Choose Directory...`).

### Step 3: Install Required R Packages
Open the RStudio **Console** and run the following command:

```R
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "DT", "gridExtra"))
```

*(Note: All packages are available directly from CRAN and will install automatically.)*

### Step 4: Generate the Sample Dataset (Optional)
The project comes with `water_usage.csv` pre-generated. If you ever wish to re-generate the dataset with fresh reproducible random seeds, execute:

```R
source("generate_dataset.R")
```

### Step 5: Launch the Dashboard
To start the application, run:

```R
shiny::runApp(".")
```

Alternatively, open `app.R` in RStudio and click the green **Run App** button located at the top-right of the editor window.

### Step 6: Test the Features
1. **Home**: Observe the KPI summary cards and baseline figures.
2. **Analysis**: Switch between households (`HH001`, `HH002`, `HH003`) to see different consumption profiles.
3. **Detection**: On `HH001`, note the persistent leakage detected between **February 11 and February 16** (Days 42 to 47), where night usage spiked to ~210 L and total usage exceeded 700 L.
4. **Adjust Sensitivity**: Move the Standard Deviation slider to `1.5` or `2.5` to observe how detection thresholds dynamically adapt.
5. **Add New Record**: Go to the Data tab and enter a reading with 300L morning, 300L afternoon, and 200L night to test real-time alert generation.
6. **Download Report**: Go to Reports tab and click **Download Formal Report (PDF)** to save the 2-page audit document.

---

## 📊 Dataset Format Reference

Custom CSV files uploaded to the dashboard must include the following column headers:

| Column Name | Type | Description |
| :--- | :--- | :--- |
| `Date` | Character / Date | Date of consumption in `YYYY-MM-DD` format |
| `Household_ID` | Character | Identifier for the residence (e.g., `HH001`) |
| `Morning_Usage` | Numeric | Water consumed from 6:00 AM to 12:00 PM (Litres) |
| `Afternoon_Usage`| Numeric | Water consumed from 12:00 PM to 6:00 PM (Litres) |
| `Night_Usage` | Numeric | Water consumed from 6:00 PM to 6:00 AM (Litres) |
| `Total_Daily_Usage` | Numeric *(Optional)* | Will be automatically calculated if omitted |

A downloadable template is provided within the **Water Usage Data** tab.

---

## ❓ Frequently Asked Questions (Academic Evaluation)

**Q: Why use rule-based statistics instead of Machine Learning (e.g. Random Forest, Neural Networks)?**  
**A:** Statistical thresholds ($\mu + 2\sigma$) provide complete explainability and do not require large pre-labeled training datasets. In domestic water management, false alarms cause alert fatigue, so transparent and configurable statistical baselines are industry standard for consumer-facing meters.

**Q: How does the system handle insufficient data?**  
**A:** If a household dataset contains fewer than 7 days, the dashboard automatically displays an informational warning explaining that at least 7 days of historical consumption are required to form a statistically valid baseline.

**Q: Can this project run completely offline?**  
**A:** Yes! All dependencies (`shiny`, `ggplot2`, `dplyr`, `readr`, `DT`, `gridExtra`) run locally on your computer with zero external API calls or internet connection required.

---

## 📜 License & Academic Integrity

This project is prepared for educational and academic submission. Code is documented and structured for easy presentation and code walkthroughs.

