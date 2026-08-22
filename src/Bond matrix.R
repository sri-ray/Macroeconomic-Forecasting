
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


TARGET_VAR  <- "swgbond__dif_1y" 
SUFFIX      <- "_dif_1y" 
MAX_NA_PCT  <- 0.20    # drop predictor columns with >20% NAs
MAX_RAW_STD <- 1e9     # drop monetary aggregates in raw CHF (std > 1 billion)

# --- 2a. Bond Target Series (Forced to Quarterly) ---
bond_dates_raw <- dmy(dates_list[[TARGET_VAR]])
bond_vals_raw  <- unlist(series_list[[TARGET_VAR]])

# Use the bridge-model rule to force the monthly target down to a quarterly timeline
bond_ts <- tibble(date = bond_dates_raw, bond = bond_vals_raw) %>%
  arrange(date) %>%
  mutate(
    qdate     = floor_date(date, "quarter"),
    month_num = month(date)
  ) %>%
  group_by(qdate) %>%
  filter(month_num == max(month_num)) %>% 
  summarise(bond = first(bond), .groups = "drop") %>% 
  rename(date = qdate) %>%
  mutate(date = as.Date(date))

cat("Quarterly Bond target: n =", nrow(bond_ts), "|",
    format(min(bond_ts$date)), "to", format(max(bond_ts$date)), "\n")


# --- 2b. Identify Predictor Keys ---
pred_keys <- names(series_list)[
  grepl(paste0(SUFFIX, "$"), names(series_list)) & 
    !grepl("bond|yield", names(series_list), ignore.case = TRUE)
]


# --- 2c. Remove Redundant / Duplicate Series ---
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


# --- 2d. Aggregate Predictors to Quarterly ---
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
    rename(date = qdate) %>%
    mutate(date = as.Date(date))
}

cat("Aggregating predictors using the bridge-model rule...\n")
pred_list <- lapply(pred_keys_dedup, function(k) {
  tbl <- build_quarterly_predictor_bridge(k)
  bond_ts %>%
    select(date) %>%
    left_join(tbl, by = "date") %>%
    pull(value)
})
names(pred_list) <- pred_keys_dedup
X_raw            <- as.data.frame(pred_list)


# --- 2e. Drop Billion-Scale Monetary Aggregates ---
raw_stds  <- sapply(X_raw, function(col) sd(col, na.rm = TRUE))
scale_safe_cols <- names(raw_stds)[is.na(raw_stds) | raw_stds <= MAX_RAW_STD]
X_scaled_filtered <- X_raw[, scale_safe_cols]

cat("Dropped due to billion-scale variance:", ncol(X_raw) - ncol(X_scaled_filtered), "columns\n")


# --- 2f. Assemble Raw Dataset & Apply Timeline Filter ---
X_raw_bound <- bind_cols(bond_ts, as_tibble(X_scaled_filtered)) %>%
  filter(date >= as.Date("2001-04-01") & date <= as.Date("2025-10-01"))

# Safe position-based selection to avoid duplicate column name bugs
X_predictors_only <- X_raw_bound[, -c(1, 2)]
na_fractions_in_window <- colMeans(is.na(X_predictors_only))

keep_cols <- names(na_fractions_in_window)[na_fractions_in_window <= 0.02]
cat("Predictors retained for the full timeline:", length(keep_cols), "\n")

base_df <- X_raw_bound %>% select(1, 2, all_of(keep_cols))
colnames(base_df)[1] <- "date"
colnames(base_df)[2] <- "bond"


# --- 2g. Add Bond Lag (Bond Yield at t-1) ---
base_df <- base_df %>% mutate(bond_lag1 = lag(bond, n = 1))


# --- 2h. Add Single Combined COVID Dummy ---
base_df <- base_df %>% mutate(covid_dummy = as.numeric(year(date) %in% c(2020, 2021)))


# --- 2i. Final Truncation ---
base_df   <- base_df %>% drop_na()
pred_cols <- keep_cols



cat("\n=========================================================\n")
cat("FINAL BOND DESIGN MATRIX SUMMARY:\n")
cat("Observations (Quarters) :", nrow(base_df), "\n")
cat("External Predictors     :", length(pred_cols), "\n")
cat("=========================================================\n")

