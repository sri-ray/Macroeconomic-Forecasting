

library(jsonlite)
library(dplyr)
library(lubridate)
library(glmnet)
library(readxl)
library(tidyr)
library(ggplot2)

# =============================================================================
# SECTION 1: LOAD DATA
# =============================================================================

raw         <- fromJSON("swiss_nowcast_data.json")
dates_list  <- raw[["dates"]]
series_list <- raw[names(raw) != "dates"]
meta        <- read_excel("swiss_nowcast_metadata.xlsx")

cat("Loaded JSON:", length(series_list), "series\n")

TARGET_VAR  <- "ch_kof_modelinput_gdpos_pct_3m"     
SUFFIX      <- "_pct_3m"
MAX_NA_PCT  <- 0.20    # drop predictor columns with >20% NAs
MAX_RAW_STD <- 1e9     # drop monetary aggregates in raw CHF (std > 1 billion)

# =============================================================================
# REWORKED SECTION 2 & 3: GDP MATRIX CONSTRUCTION
# =============================================================================

# --- 2a. GDP target series ---
# Ensure dates are parsed correctly
gdp_dates <- dmy(dates_list[[TARGET_VAR]])
gdp_vals  <- unlist(series_list[[TARGET_VAR]])
gdp_ts    <- tibble(date = gdp_dates, gdp = gdp_vals) %>% arrange(date)

cat("GDP target: n =", nrow(gdp_ts), "|",
    format(min(gdp_ts$date)), "to", format(max(gdp_ts$date)), "\n")

# --- 2b. Identify predictor keys ---
# Filter candidate predictors using the correct suffix (_pct_3m) 
# CRITICAL EXCLUSION: Remove all other GDP/gdpos family variables completely
pred_keys <- names(series_list)[
  grepl(paste0(SUFFIX, "$"), names(series_list)) & 
    !grepl("gdp|gdpos", names(series_list), ignore.case = TRUE)
]
cat("Candidate predictors (", SUFFIX, "):", length(pred_keys), "\n")


# --- 2c. Scale Check Note ---
# The billion-scale variance problem only affects the absolute difference transformations (_dif_ly).
# For the GDP matrix, using '_pct_3m' means the series are already well-scaled.
pred_keys_final <- pred_keys


# --- 2d. Remove redundant / duplicate series ---
# Re-using the deduplication base logic from your old script but adapting it 
# to pull dynamically based on the current active SUFFIX [cite: 44, 77]
DROP_EXACT_DUPES     <- c("swky3937f", "swxsfec_", "swxrusd_")
SWAP_SERIES_EXCLUDED <- "swvisgdsa"

# Nominal export base keys (_nom_ pattern)
drop_nom_bases <- unique(sub(paste0(SUFFIX, "$"), "",
                             names(series_list)[grepl("_nom_", names(series_list)) &
                                                  grepl(paste0(SUFFIX, "$"), names(series_list))]))

# KOF nominal component base keys (p-prefix, not pct)
drop_kof_nom <- meta$key[grepl("^ch_kof_modelinput_p", meta$key) &
                           !grepl("^ch_kof_modelinput_pct", meta$key)]

# Combined set of base keys to exclude
all_drop_bases <- c(DROP_EXACT_DUPES, drop_nom_bases, drop_kof_nom, SWAP_SERIES_EXCLUDED)

# Filter out matching keys
get_base_key    <- function(k) sub(paste0(SUFFIX, "$"), "", k)
pred_keys_dedup <- pred_keys_final[
  !sapply(pred_keys_final, get_base_key) %in% all_drop_bases
]


# --- 2e. Aggregate predictors to quarterly (Bridge-Model Rule) ---
# REPLACE SIMPLE MEAN: The bridge-model rule maps monthly series to quarterly 
# data using the final available month of that quarter.
build_quarterly_predictor_bridge <- function(k) {
  d <- dmy(dates_list[[k]])
  v <- unlist(series_list[[k]])
  
  tibble(date = d, value = v) %>%
    arrange(date) %>%
    mutate(
      qdate     = floor_date(date, "quarter"),
      month_num = month(date)
    ) %>%
    group_by(qdate) %>%
    # Bridge-model selection: Extract the latest monthly indicator within the quarter 
    filter(month_num == max(month_num)) %>% 
    summarise(value = first(value), .groups = "drop") %>% 
    rename(date = qdate)
}

cat("Aggregating predictors using the bridge-model rule...\n")
pred_list <- lapply(pred_keys_dedup, function(k) {
  tbl <- build_quarterly_predictor_bridge(k)
  gdp_ts %>%
    select(date) %>%
    left_join(tbl, by = "date") %>%
    pull(value)
})
names(pred_list) <- pred_keys_dedup
X_raw            <- as.data.frame(pred_list)


# --- 2f. Assemble the raw combined dataset to analyze missingness ---
X_raw_bound <- bind_cols(gdp_ts, as_tibble(X_raw)) %>%
  filter(date >= "2001-04-01" & date <= "2025-10-01") # Limit to our target structural window

# --- NEW: Balanced Filter Rule ---
# Calculate NA fractions specifically inside the target window (2001 Q2 - 2025 Q4)
na_fractions_in_window <- colMeans(is.na(X_raw_bound %>% select(-date, -gdp)))

# Only keep columns that are fully populated (or within threshold) across the historical window
keep_cols <- names(na_fractions_in_window)[na_fractions_in_window <= 0.02] # Strict 2% missing limit

cat("Variables causing early truncation dropped:", length(pred_keys_dedup) - length(keep_cols), "\n")
cat("Predictors retained for the full timeline:", length(keep_cols), "\n")

# --- 2g. Re-assemble final clean dataframe using the selected variables ---
base_df <- X_raw_bound %>%
  select(date, gdp, all_of(keep_cols))


# --- 2h. Add GDP lag (GDP at t-1) ---
base_df <- base_df %>%
  mutate(gdp_lag1 = lag(gdp, n = 1))

# --- 2i. Add Two Separate COVID Dummy Variables ---
base_df <- base_df %>%
  mutate(
    covid_crash   = if_else(date == "2020-04-01", 1, 0),
    covid_rebound = if_else(date == "2020-07-01", 1, 0)
  )

# --- 2j. Final Filtering to drop missing cases ---
# Because we filtered out the late-starting columns, this drop_na() will 
# now only eliminate the very first row (due to the t-1 lag requirement).
base_df   <- base_df %>% drop_na()
pred_cols <- keep_cols

cat("\n=========================================================\n")
cat("FINAL GDP DESIGN MATRIX SUMMARY:\n")
cat("Observations (Quarters) :", nrow(base_df), "(Should now be ~98)\n")
cat("External Predictors     :", length(pred_cols), "\n")
cat("=========================================================\n")

#====== EVALUATION =====================
# Check if row count matches expectations
cat("Total quarters:", nrow(base_df), "(Expected: ~98)\n")

# Verify that the sequence is strictly chronological with no missing quarters
print(base_df$date)


# Check if any other GDP variables slipped through the filter
accidental_gdp_vars <- colnames(base_df)[grepl("gdp", colnames(base_df), ignore.case = TRUE)]
print(accidental_gdp_vars)

# Verify all final external predictor suffixes are strictly '_pct_3m'
non_pct_3m_vars <- keep_cols[!grepl("_pct_3m$", keep_cols)]

if(length(non_pct_3m_vars) == 0) {
  cat("SUCCESS: All retained external predictors are strictly '_pct_3m'\n")
} else {
  cat("ERROR: Found variables with incorrect transformations:\n")
  print(non_pct_3m_vars)
}

# Print the rows around 2020 to ensure the dummies fired on the exact quarters
base_df %>% 
  filter(year(date) == 2020) %>% 
  select(date, gdp, covid_crash, covid_rebound)

# Compare your actual dates against a perfect quarterly sequence
perfect_sequence <- seq(from = as.Date("2001-07-01"), to = as.Date("2025-10-01"), by = "quarter")
missing_quarters <- perfect_sequence[!perfect_sequence %in% base_df$date]

print(missing_quarters)
