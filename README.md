# High-Dimensional Feature Engineering & Matrix Construction for Swiss Macroeconomic Forecasting

An econometrics project building standardized, balanced high-dimensional design matrices ($N \approx 98, P > 210$) across 2001–2025 to forecast Swiss GDP, CPI Inflation, and 10-Year Government Bond Yields.

---

### 📌 Project Context
* **Institution:** University of Zurich
* **Seminar:** Macroeconomic Forecasting (Co-authored)
* **Targets:** 
  * Swiss GDP Growth (`ch_kof_modelinput_gdpos_pct_3m`)
  * Swiss CPI Inflation (`swconprce_pct_3m`)
  * 10-Year Swiss Government Bond Yield (`swgbond__dif_1y`)

---

### 🛠 My Core Contributions
I designed and implemented the **data preprocessing, frequency harmonization, and feature matrix construction pipelines** in R:

1. **Bridge-Model Frequency Harmonization:** Developed aggregation functions mapping monthly/daily high-frequency indicators down to quarterly observations using end-of-quarter alignment.
2. **Dynamic Key Filtering & Deduplication:** Filtered nominal components, duplicate series, and target-family leakage (e.g., removing all internal CPI/GDP components from predictor sets).
3. **Variance & Missingness Filtering:** Screened out monetary aggregates exhibiting billion-scale raw variances and enforced a strict missingness threshold ($\le 2\%$) within the 2001 Q2 – 2025 Q4 estimation window.
4. **Structural Dynamics:** Engineered autoregressive target lags ($t-1$) and historical dummy structures (COVID-19 crash/rebound indicators).

*(Note: Downstream regularized shrinkage estimation—LASSO, Ridge, and Best Subset selection—was conducted jointly across the seminar team).*

---

### 🧰 Tech Stack
* **Language:** R
* **Libraries:** `dplyr`, `tidyr`, `lubridate`, `jsonlite`, `readxl`, `ggplot2`

---

### 📂 Repository Structure
* `data/swiss_nowcast_data.zip`: Compressed raw JSON nowcast database.
* `data/swiss_nowcast_metadata.xlsx`: Variable definitions and series metadata.
* `src/`: Matrix construction scripts for GDP, CPI, and 10-year yield targets.

---

### 🚀 How to Run
1. Install dependencies in R:
```R
install.packages(c("dplyr", "tidyr", "lubridate", "jsonlite", "readxl", "ggplot2"))
