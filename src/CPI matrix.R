

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

# --- 2a. CPI target series ---
# Ensure dates are parsed correctly
cpi_dates <- dmy(dates_list[[TARGET_VAR]])
cpi_vals  <- unlist(series_list[[TARGET_VAR]])
cpi_ts    <- tibble(date = cpi_dates, cpi = cpi_vals) %>% arrange(date)

cat("CPI target: n =", nrow(cpi_ts), "|",
    format(min(cpi_ts$date)), "to", format(max(cpi_ts$date)), "\n")

# --- 2b. Identify predictor keys ---
# Filter candidate predictors using the correct suffix (_pct_3m)
# CRITICAL EXCLUSION: Remove ALL other CPI family variables completely 
pred_keys <- names(series_list)[
  grepl(paste0(SUFFIX, "$"), names(series_list)) & 
    !grepl("cpi", names(series_list), ignore.case = TRUE)
]
cat("Candidate predictors (", SUFFIX, "):", length(pred_keys), "\n")


# --- 2c. Remove redundant / duplicate series ---
# Re-using your team's core deduplication base logic 
DROP_EXACT_DUPES     <- c("swky3937f", "swxsfec_", "swxrusd_") 
SWAP_SERIES_EXCLUDED <- "swvisgdsa"

drop_nom_bases <- unique(sub(paste0(SUFFIX, "$"), "",
                             names(series_list)[grepl("_nom_", names(series_list)) &
                                                  grepl(paste0(SUFFIX, "$"), names(series_list))])) 

drop_kof_nom <- meta$key[grepl("^ch_kof_modelinput_p", meta$key) &
                           !grepl("^ch_kof_modelinput_pct", meta$key)] 

all_drop_bases <- c(DROP_EXACT_DUPES, drop_nom_bases, drop_kof_nom, SWAP_SERIES_EXCLUDED)

get_base_key    <- function(k) sub(paste0(SUFFIX, "$"), "", k)
pred_keys_dedup <- pred_keys[
  !sapply(pred_keys, get_base_key) %in% all_drop_bases
]


# --- 2d. Aggregate predictors to quarterly (Bridge-Model Rule) ---
# Maps monthly series to quarterly data using the final available month of the quarter 
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
    filter(month_num == max(month_num)) %>% 
    summarise(value = first(value), .groups = "drop") %>% 
    rename(date = qdate)
}

cat("Aggregating predictors using the bridge-model rule...\n")
pred_list <- lapply(pred_keys_dedup, function(k) {
  tbl <- build_quarterly_predictor_bridge(k)
  cpi_ts %>%
    select(date) %>%
    left_join(tbl, by = "date") %>%
    pull(value)
})
names(pred_list) <- pred_keys_dedup
X_raw            <- as.data.frame(pred_list)


# --- 2e. Assemble raw dataset & apply timeline-preservation filter ---
# Maximum usable target window: 2001 Q2 to 2025 Q4 
X_raw_bound <- bind_cols(cpi_ts, as_tibble(X_raw)) %>%
  filter(date >= "2001-04-01" & date <= "2025-10-01") 

# Calculate missingness rates within our historical window 
na_fractions_in_window <- colMeans(is.na(X_raw_bound %>% select(-date, -cpi)))
keep_cols <- names(na_fractions_in_window)[na_fractions_in_window <= 0.02] 

cat("Predictors retained for the full timeline:", length(keep_cols), "\n")

base_df <- X_raw_bound %>%
  select(date, cpi, all_of(keep_cols))


# --- 2f. Add CPI lag (CPI at t-1) ---
# Include the exact autoregressive lag of your target variable 
base_df <- base_df %>%
  mutate(cpi_lag1 = lag(cpi, n = 1))


# --- 2g. Add Single Combined COVID Dummy ---
# For CPI, keep the single combined 2020-2021 indicator 
base_df <- base_df %>%
  mutate(covid_dummy = as.numeric(year(date) %in% c(2020, 2021))) 


# --- 2h. Final Truncation ---
# Clears the first row (NA due to the lag setup) to ensure a perfectly balanced matrix
base_df   <- base_df %>% drop_na()
pred_cols <- keep_cols


cat("\n=========================================================\n")
cat("FINAL CPI DESIGN MATRIX SUMMARY:\n")
cat("Observations (Quarters) :", nrow(base_df), "\n")
cat("External Predictors     :", length(pred_cols), "\n")
cat("=========================================================\n")
