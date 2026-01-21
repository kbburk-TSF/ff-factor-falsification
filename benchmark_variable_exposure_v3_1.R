################################################################################
# BENCHMARK BACKTEST - VARIABLE EXPOSURE STRATEGIES ONLY
# File: benchmark_variable_exposure_v3_1.R
# Version: 3.1
# Date: 2025-01-20
#
# PURPOSE: Benchmark strategies with variable exposure profiles for fair
#          comparison against TSF signals. Calendar-based strategies excluded
#          due to ~100% exposure characteristics.
#
# STRATEGIES: 16 signal-based benchmarks + 2 null hypothesis (random) benchmarks
#
# CHANGES from v3_0:
#   - FIX: Apply BH correction at individual-comparison level (not just aggregate)
#   - FIX: HAC t-test is PRIMARY confirmation test (BH-adjusted)
#   - FIX: R² tests marked as DESCRIPTIVE-ONLY (no inferential test available)
#   - Added _p_bh columns for all TEST hypothesis p-values
#   - Added _tost_p_bh columns for all CONTROL hypothesis TOST p-values
#
# CHANGES from v2_03:
#   - ADD 16 hypothesis tests (H_FF1-8, H_TM1-4, H_HM1-4) from scheduled exposure
#   - ADD HAC standard errors to FF/TM/HM regression outputs
#   - ADD run_aggregate_tests function
#   - ADD FF/HM/TM summary output
#   - NET ONLY processing (removed GROSS)
#   - NO CHANGES to existing code/outputs
#
# CHANGES from v2_02:
#   - ADD ALL 16 HYPOTHESIS METRIC DIFFS (was only 3, now complete)
#   ff_alpha_diff, ff_r_squared_diff, ff_beta_mkt_diff (H_FF1-3)
#   ff_beta_smb_diff, ff_beta_hml_diff, ff_beta_rmw_diff, ff_beta_cma_diff, ff_beta_mom_diff (H_FF4-8)
#   tm_alpha_diff, tm_gamma_diff, tm_r_squared_diff, tm_beta_diff (H_TM1-4)
#   hm_alpha_diff, hm_beta2_diff, hm_r_squared_diff, hm_beta1_diff (H_HM1-4)
#
# CHANGES from v2_01:
#   - COMPLETE Appendix D statistical tests with ALL columns (52 total)
#   - Removed permutation_p (per instruction)
#   - Added: wilcoxon_w_pos, wilcoxon_w_neg, wilcoxon_stat
#   - Added: binomial_k, binomial_n
#   - Added: tost_delta, tost_t_lower, tost_t_upper, tost_p_lower, tost_p_upper
#   - Added: hac_ci_includes_zero, hac_ci_within_bounds
#   - Added: paired_t_stat, welch_t_stat, welch_df
#   - Added: sign_n_pos, sign_n_neg
#   - Added: chisq_stat, fisher_odds_ratio
#   - Added: meta_fe_se, meta_re_tau2
#
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
  library(openxlsx)
  library(sandwich)
  library(lmtest)
})

set.seed(42)

cat("================================================================================\n")
cat("BENCHMARK BACKTEST - VARIABLE EXPOSURE V3.1 (BH @ INDIVIDUAL, HAC PRIMARY)\n")
cat("================================================================================\n\n")



UNIVERSE <- "COMMUNICATION_SERVICES"
INPUT_FILE <- "communication_services_benchmark.csv"
FF_5_FACTOR_FILE <- "F-F_Research_Data_5_Factors_2x3_daily.csv"
FF_MOMENTUM_FILE <- "F-F_Momentum_Factor_daily.csv"
CACHE_DIR <- "communication_services_variable_cache"

INITIAL_CAPITAL <- 100000
TRANSACTION_COST_PCT <- 0.001
RISK_FREE_RATE <- 0.05
NET_MULTIPLIER <- (1 - TRANSACTION_COST_PCT)^2
MAX_HOLD_DAYS <- 180

BOLLINGER_PERIOD <- 20
BOLLINGER_SD <- 2
RSI_PERIOD <- 14
RSI_OVERSOLD <- 30
MA_SHORT <- 50
MA_LONG <- 200
MACD_FAST <- 12
MACD_SLOW <- 26
MACD_SIGNAL <- 9
DONCHIAN_PERIOD <- 20
ROC_PERIOD <- 10
ROC_THRESHOLD <- -0.05
DIP_SMALL <- 0.05
DIP_LARGE <- 0.10
SD_PERIOD <- 20

RANDOM_TSF_PROB <- 0.025
RANDOM_UNIFORM_PROB <- 0.01

PATIENCE_DAYS <- c(0, 60, 90, 120, 180)
PATIENCE_TARGETS <- c(0.15, 0.10, 0.05, 0.02, 0.00)

get_patience_target <- function(days_held) {
  idx <- findInterval(days_held, PATIENCE_DAYS)
  if (idx < 1) idx <- 1
  if (idx > length(PATIENCE_TARGETS)) idx <- length(PATIENCE_TARGETS)
  PATIENCE_TARGETS[idx]
}

POSITION_SIZES <- list(
  list(name = "p05", pct = 0.05),
  list(name = "p10", pct = 0.10),
  list(name = "p15", pct = 0.15),
  list(name = "p20", pct = 0.20)
)

PERIODS <- list(
  list(name = "2006-2015", label = "GFC-Era",      start = as.Date("2006-01-01"), end = as.Date("2015-12-31")),
  list(name = "2016-2025", label = "Post-GFC",     start = as.Date("2016-01-01"), end = as.Date("2025-12-31")),
  list(name = "2006-2025", label = "FULL",         start = as.Date("2006-01-01"), end = as.Date("2025-12-31")),
  list(name = "2023-2025", label = "Present-Bull", start = as.Date("2023-01-01"), end = as.Date("2025-12-31"))
)

# ==============================================================================
# FAMA-FRENCH FACTOR LOADING
# ==============================================================================

load_ff_factors <- function(ff5_file, mom_file) {
  cat("Loading Fama-French factors...\n")
  
  ff5_lines <- readLines(ff5_file, n = 50)
  data_start <- grep("^\\s*\\d{8},", ff5_lines)[1]
  if (is.na(data_start)) data_start <- grep("Mkt-RF", ff5_lines)[1] + 1
  
  ff5 <- fread(ff5_file, skip = data_start - 1, header = FALSE, fill = TRUE)
  ff5 <- ff5[, 1:min(7, ncol(ff5)), with = FALSE]
  if (ncol(ff5) >= 7) {
    setnames(ff5, c("date", "MKT_RF", "SMB", "HML", "RMW", "CMA", "RF"))
  } else if (ncol(ff5) >= 5) {
    setnames(ff5, c("date", "MKT_RF", "SMB", "HML", "RF")[1:ncol(ff5)])
  }
  
  ff5 <- ff5[!is.na(date) & nchar(trimws(as.character(date))) == 8]
  ff5[, date := as.Date(trimws(as.character(date)), format = "%Y%m%d")]
  ff5 <- ff5[!is.na(date)]
  
  num_cols <- setdiff(names(ff5), "date")
  for (col in num_cols) ff5[, (col) := as.numeric(get(col)) / 100]
  
  mom_lines <- readLines(mom_file, n = 50)
  data_start_mom <- grep("^\\s*\\d{8},", mom_lines)[1]
  if (is.na(data_start_mom)) data_start_mom <- grep("Mom", mom_lines)[1] + 1
  
  mom <- fread(mom_file, skip = data_start_mom - 1, header = FALSE, fill = TRUE)
  mom <- mom[, 1:min(2, ncol(mom)), with = FALSE]
  setnames(mom, c("date", "MOM"))
  
  mom <- mom[!is.na(date) & nchar(trimws(as.character(date))) == 8]
  mom[, date := as.Date(trimws(as.character(date)), format = "%Y%m%d")]
  mom <- mom[!is.na(date)]
  mom[, MOM := as.numeric(MOM) / 100]
  
  ff <- merge(ff5, mom, by = "date", all.x = TRUE)
  setkey(ff, date)
  
  cat(sprintf("  Loaded %d days of FF factors (%s to %s)\n", nrow(ff), min(ff$date), max(ff$date)))
  return(ff)
}

# ==============================================================================
# FAMA-FRENCH REGRESSION (WITH HAC STANDARD ERRORS)
# ==============================================================================

run_ff_regression <- function(equity_dt, ff_factors) {
  result <- list(
    ff_alpha = NA_real_, ff_alpha_t = NA_real_, ff_alpha_p = NA_real_,
    ff_r_squared = NA_real_,
    ff_beta_mkt = NA_real_, ff_beta_smb = NA_real_, ff_beta_hml = NA_real_,
    ff_beta_rmw = NA_real_, ff_beta_cma = NA_real_, ff_beta_mom = NA_real_,
    # HAC Standard errors for equivalence testing (Newey-West)
    ff_alpha_se = NA_real_,
    ff_beta_mkt_se = NA_real_, ff_beta_smb_se = NA_real_, ff_beta_hml_se = NA_real_,
    ff_beta_rmw_se = NA_real_, ff_beta_cma_se = NA_real_, ff_beta_mom_se = NA_real_,
    ff_df = NA_integer_  # Degrees of freedom for t-tests
  )
  
  if (is.null(ff_factors) || is.null(equity_dt) || nrow(equity_dt) < 30) return(result)
  
  ret_dt <- data.table(
    date = equity_dt$date[-1],
    ret = diff(equity_dt$portfolio_value) / head(equity_dt$portfolio_value, -1)
  )
  ret_dt <- ret_dt[is.finite(ret)]
  if (nrow(ret_dt) < 30) return(result)
  
  merged <- merge(ret_dt, ff_factors, by = "date")
  merged <- merged[complete.cases(merged[, .(ret, MKT_RF, SMB, HML)])]
  if (nrow(merged) < 30) return(result)
  
  if ("RF" %in% names(merged)) {
    merged[, excess_ret := ret - RF]
  } else {
    merged[, excess_ret := ret - RISK_FREE_RATE/252]
  }
  
  tryCatch({
    has_rmw <- "RMW" %in% names(merged) && sum(!is.na(merged$RMW)) > 30
    has_cma <- "CMA" %in% names(merged) && sum(!is.na(merged$CMA)) > 30
    has_mom <- "MOM" %in% names(merged) && sum(!is.na(merged$MOM)) > 30
    
    if (has_rmw && has_cma && has_mom) {
      fit <- lm(excess_ret ~ MKT_RF + SMB + HML + RMW + CMA + MOM, data = merged)
    } else if (has_mom) {
      fit <- lm(excess_ret ~ MKT_RF + SMB + HML + MOM, data = merged)
    } else {
      fit <- lm(excess_ret ~ MKT_RF + SMB + HML, data = merged)
    }
    
    # Get OLS coefficients
    coefs <- summary(fit)$coefficients
    result$ff_df <- fit$df.residual
    
    # CRITICAL: Use HAC (Newey-West) standard errors as per preregistration
    vcov_hac <- NeweyWest(fit, prewhite = FALSE)
    se_hac <- sqrt(diag(vcov_hac))
    
    result$ff_alpha <- coefs["(Intercept)", "Estimate"] * 252
    result$ff_alpha_se <- se_hac["(Intercept)"] * 252  # HAC SE, annualized
    # Recompute t-stat and p-value using HAC SE
    result$ff_alpha_t <- (coefs["(Intercept)", "Estimate"]) / se_hac["(Intercept)"]
    result$ff_alpha_p <- 2 * pt(abs(result$ff_alpha_t), df = result$ff_df, lower.tail = FALSE)
    result$ff_r_squared <- summary(fit)$r.squared
    
    if ("MKT_RF" %in% rownames(coefs)) {
      result$ff_beta_mkt <- coefs["MKT_RF", "Estimate"]
      result$ff_beta_mkt_se <- se_hac["MKT_RF"]  # HAC SE
    }
    if ("SMB" %in% rownames(coefs)) {
      result$ff_beta_smb <- coefs["SMB", "Estimate"]
      result$ff_beta_smb_se <- se_hac["SMB"]  # HAC SE
    }
    if ("HML" %in% rownames(coefs)) {
      result$ff_beta_hml <- coefs["HML", "Estimate"]
      result$ff_beta_hml_se <- se_hac["HML"]  # HAC SE
    }
    if ("RMW" %in% rownames(coefs)) {
      result$ff_beta_rmw <- coefs["RMW", "Estimate"]
      result$ff_beta_rmw_se <- se_hac["RMW"]  # HAC SE
    }
    if ("CMA" %in% rownames(coefs)) {
      result$ff_beta_cma <- coefs["CMA", "Estimate"]
      result$ff_beta_cma_se <- se_hac["CMA"]  # HAC SE
    }
    if ("MOM" %in% rownames(coefs)) {
      result$ff_beta_mom <- coefs["MOM", "Estimate"]
      result$ff_beta_mom_se <- se_hac["MOM"]  # HAC SE
    }
    
  }, error = function(e) { })
  
  return(result)
}

# ==============================================================================
# HENRIKSSON-MERTON MARKET TIMING TEST (1981) (WITH HAC STANDARD ERRORS)
# ==============================================================================

run_hm_regression <- function(equity_dt, ff_factors) {
  result <- list(
    hm_alpha = NA_real_, hm_alpha_t = NA_real_, hm_alpha_p = NA_real_,
    hm_beta1 = NA_real_, hm_beta2 = NA_real_,
    hm_beta2_t = NA_real_, hm_beta2_p = NA_real_,
    hm_r_squared = NA_real_,
    # HAC Standard errors for equivalence testing (Newey-West)
    hm_alpha_se = NA_real_, hm_beta1_se = NA_real_, hm_beta2_se = NA_real_,
    hm_df = NA_integer_
  )
  
  if (is.null(ff_factors) || is.null(equity_dt) || nrow(equity_dt) < 30) return(result)
  
  ret_dt <- data.table(
    date = equity_dt$date[-1],
    ret = diff(equity_dt$portfolio_value) / head(equity_dt$portfolio_value, -1)
  )
  ret_dt <- ret_dt[is.finite(ret)]
  if (nrow(ret_dt) < 30) return(result)
  
  merged <- merge(ret_dt, ff_factors, by = "date")
  merged <- merged[complete.cases(merged[, .(ret, MKT_RF)])]
  if (nrow(merged) < 30) return(result)
  
  if ("RF" %in% names(merged)) {
    merged[, excess_ret := ret - RF]
  } else {
    merged[, excess_ret := ret - RISK_FREE_RATE/252]
  }
  
  merged[, mkt_up := pmax(0, MKT_RF)]
  
  tryCatch({
    fit <- lm(excess_ret ~ MKT_RF + mkt_up, data = merged)
    coefs <- summary(fit)$coefficients
    result$hm_df <- fit$df.residual
    
    # CRITICAL: Use HAC (Newey-West) standard errors as per preregistration
    vcov_hac <- NeweyWest(fit, prewhite = FALSE)
    se_hac <- sqrt(diag(vcov_hac))
    
    result$hm_alpha <- coefs["(Intercept)", "Estimate"] * 252
    result$hm_alpha_se <- se_hac["(Intercept)"] * 252  # HAC SE, annualized
    result$hm_alpha_t <- (coefs["(Intercept)", "Estimate"]) / se_hac["(Intercept)"]
    result$hm_alpha_p <- 2 * pt(abs(result$hm_alpha_t), df = result$hm_df, lower.tail = FALSE)
    
    result$hm_beta1 <- coefs["MKT_RF", "Estimate"]
    result$hm_beta1_se <- se_hac["MKT_RF"]  # HAC SE
    result$hm_beta2 <- coefs["mkt_up", "Estimate"]
    result$hm_beta2_se <- se_hac["mkt_up"]  # HAC SE
    result$hm_beta2_t <- coefs["mkt_up", "Estimate"] / se_hac["mkt_up"]
    result$hm_beta2_p <- 2 * pt(abs(result$hm_beta2_t), df = result$hm_df, lower.tail = FALSE)
    
    result$hm_r_squared <- summary(fit)$r.squared
  }, error = function(e) { })
  
  return(result)
}

# ==============================================================================
# TREYNOR-MAZUY MARKET TIMING TEST (1966) (WITH HAC STANDARD ERRORS)
# ==============================================================================

run_tm_regression <- function(equity_dt, ff_factors) {
  result <- list(
    tm_alpha = NA_real_, tm_alpha_t = NA_real_, tm_alpha_p = NA_real_,
    tm_beta = NA_real_, tm_gamma = NA_real_,
    tm_gamma_t = NA_real_, tm_gamma_p = NA_real_,
    tm_r_squared = NA_real_,
    # HAC Standard errors for equivalence testing (Newey-West)
    tm_alpha_se = NA_real_, tm_beta_se = NA_real_, tm_gamma_se = NA_real_,
    tm_df = NA_integer_
  )
  
  if (is.null(ff_factors) || is.null(equity_dt) || nrow(equity_dt) < 30) return(result)
  
  ret_dt <- data.table(
    date = equity_dt$date[-1],
    ret = diff(equity_dt$portfolio_value) / head(equity_dt$portfolio_value, -1)
  )
  ret_dt <- ret_dt[is.finite(ret)]
  if (nrow(ret_dt) < 30) return(result)
  
  merged <- merge(ret_dt, ff_factors, by = "date")
  merged <- merged[complete.cases(merged[, .(ret, MKT_RF)])]
  if (nrow(merged) < 30) return(result)
  
  if ("RF" %in% names(merged)) {
    merged[, excess_ret := ret - RF]
  } else {
    merged[, excess_ret := ret - RISK_FREE_RATE/252]
  }
  
  merged[, mkt_sq := MKT_RF^2]
  
  tryCatch({
    fit <- lm(excess_ret ~ MKT_RF + mkt_sq, data = merged)
    coefs <- summary(fit)$coefficients
    result$tm_df <- fit$df.residual
    
    # CRITICAL: Use HAC (Newey-West) standard errors as per preregistration
    vcov_hac <- NeweyWest(fit, prewhite = FALSE)
    se_hac <- sqrt(diag(vcov_hac))
    
    result$tm_alpha <- coefs["(Intercept)", "Estimate"] * 252
    result$tm_alpha_se <- se_hac["(Intercept)"] * 252  # HAC SE, annualized
    result$tm_alpha_t <- (coefs["(Intercept)", "Estimate"]) / se_hac["(Intercept)"]
    result$tm_alpha_p <- 2 * pt(abs(result$tm_alpha_t), df = result$tm_df, lower.tail = FALSE)
    
    result$tm_beta <- coefs["MKT_RF", "Estimate"]
    result$tm_beta_se <- se_hac["MKT_RF"]  # HAC SE
    result$tm_gamma <- coefs["mkt_sq", "Estimate"]
    result$tm_gamma_se <- se_hac["mkt_sq"]  # HAC SE
    result$tm_gamma_t <- coefs["mkt_sq", "Estimate"] / se_hac["mkt_sq"]
    result$tm_gamma_p <- 2 * pt(abs(result$tm_gamma_t), df = result$tm_df, lower.tail = FALSE)
    
    result$tm_r_squared <- summary(fit)$r.squared
  }, error = function(e) { })
  
  return(result)
}

# INDICATOR CALCULATION
# ==============================================================================

calculate_indicators <- function(dt) {
  cat("Calculating indicators...\n")
  
  setorder(dt, forecast_id, date)
  
  dt[, `:=`(
    bb_sma = rollmean(value, BOLLINGER_PERIOD, fill = NA, align = "right"),
    bb_sd = rollapply(value, BOLLINGER_PERIOD, sd, fill = NA, align = "right"),
    price_change = value - shift(value),
    ma_short = rollmean(value, MA_SHORT, fill = NA, align = "right"),
    ma_long = rollmean(value, MA_LONG, fill = NA, align = "right"),
    donchian_high = rollapply(value, DONCHIAN_PERIOD, max, fill = NA, align = "right"),
    donchian_low = rollapply(value, DONCHIAN_PERIOD, min, fill = NA, align = "right"),
    roc = (value - shift(value, ROC_PERIOD)) / shift(value, ROC_PERIOD),
    high_20d = rollapply(value, SD_PERIOD, max, fill = NA, align = "right"),
    mean_20d = rollmean(value, SD_PERIOD, fill = NA, align = "right"),
    sd_20d = rollapply(value, SD_PERIOD, sd, fill = NA, align = "right"),
    high_52w = rollapply(value, 252, max, fill = NA, align = "right"),
    low_52w = rollapply(value, 252, min, fill = NA, align = "right")
  ), by = forecast_id]
  
  dt[, `:=`(
    bb_lower = bb_sma - (BOLLINGER_SD * bb_sd),
    bb_upper = bb_sma + (BOLLINGER_SD * bb_sd),
    pct_from_high = (value - high_20d) / high_20d,
    z_score = (value - mean_20d) / sd_20d,
    gain = fifelse(price_change > 0, price_change, 0),
    loss = fifelse(price_change < 0, abs(price_change), 0),
    down_day = value < shift(value)
  ), by = forecast_id]
  
  dt[, `:=`(
    avg_gain = rollapply(gain, RSI_PERIOD, mean, fill = NA, align = "right"),
    avg_loss = rollapply(loss, RSI_PERIOD, mean, fill = NA, align = "right")
  ), by = forecast_id]
  
  dt[, rs := fifelse(avg_loss == 0, 100, avg_gain / avg_loss)]
  dt[, rsi := 100 - (100 / (1 + rs))]
  
  dt[, `:=`(
    ema_fast = rollmean(value, MACD_FAST, fill = NA, align = "right"),
    ema_slow = rollmean(value, MACD_SLOW, fill = NA, align = "right")
  ), by = forecast_id]
  dt[, macd_line := ema_fast - ema_slow]
  dt[, macd_signal := rollmean(macd_line, MACD_SIGNAL, fill = NA, align = "right"), by = forecast_id]
  
  dt[, consec_down := {
    result <- numeric(.N)
    count <- 0
    for (i in seq_len(.N)) {
      if (is.na(down_day[i])) { count <- 0 }
      else if (down_day[i]) { count <- count + 1 }
      else { count <- 0 }
      result[i] <- count
    }
    result
  }, by = forecast_id]
  
  dt[, `:=`(
    prev_value = shift(value),
    prev_bb_lower = shift(bb_lower),
    prev_rsi = shift(rsi),
    prev_ma_short = shift(ma_short),
    prev_ma_long = shift(ma_long),
    prev_macd_line = shift(macd_line),
    prev_macd_signal = shift(macd_signal),
    prev_donchian_low = shift(donchian_low),
    prev_donchian_high = shift(donchian_high),
    prev_roc = shift(roc),
    prev_pct_from_high = shift(pct_from_high),
    prev_z_score = shift(z_score),
    prev_consec_down = shift(consec_down)
  ), by = forecast_id]
  
  cat("Indicators done.\n")
  return(dt)
}

# ==============================================================================
# GENERATE ENTRY SIGNALS
# ==============================================================================

generate_entry_signals <- function(dt) {
  cat("Generating entry signals...\n")
  
  dt[, day_of_week := weekdays(date)]
  dt[, month := as.integer(format(date, "%m"))]
  dt[, first_of_month := date == min(date), by = .(forecast_id, format(date, "%Y-%m"))]
  dt[, prev_month := shift(month), by = forecast_id]
  
  dt[, `:=`(
    entry_bollinger = (value < bb_lower) & (prev_value >= prev_bb_lower),
    entry_rsi = (rsi < RSI_OVERSOLD) & (prev_rsi >= RSI_OVERSOLD),
    entry_ma_cross = (ma_short > ma_long) & (prev_ma_short <= prev_ma_long),
    entry_macd = (macd_line > macd_signal) & (prev_macd_line <= prev_macd_signal),
    entry_donchian = (value == donchian_low) & (prev_value != prev_donchian_low),
    entry_roc = (roc < ROC_THRESHOLD) & (prev_roc >= ROC_THRESHOLD),
    entry_dip_5 = (pct_from_high < -DIP_SMALL) & (prev_pct_from_high >= -DIP_SMALL),
    entry_dip_10 = (pct_from_high < -DIP_LARGE) & (prev_pct_from_high >= -DIP_LARGE),
    entry_sd_2 = (z_score < -2) & (prev_z_score >= -2),
    entry_sd_3 = (z_score < -3) & (prev_z_score >= -3),
    entry_down_3 = (consec_down >= 3) & (prev_consec_down < 3),
    entry_down_5 = (consec_down >= 5) & (prev_consec_down < 5),
    entry_52w_low = (value == low_52w) & (prev_value != shift(low_52w)),
    entry_52w_high = (value == high_52w) & (prev_value != shift(high_52w)),
    entry_below_ma = (value < ma_long) & (prev_value >= prev_ma_long),
    entry_above_ma = (value > ma_long) & (prev_value <= prev_ma_long),
    entry_monday = (day_of_week == "Monday"),
    entry_first_month = first_of_month,
    entry_january = (month == 1) & (prev_month != 1 | is.na(prev_month)),
    entry_november = (month == 11) & (prev_month != 11 | is.na(prev_month)),
    entry_random_tsf = runif(.N) < RANDOM_TSF_PROB,
    entry_random_uniform = runif(.N) < RANDOM_UNIFORM_PROB
  ), by = forecast_id]
  
  entry_cols <- grep("^entry_", names(dt), value = TRUE)
  for (col in entry_cols) {
    dt[is.na(get(col)), (col) := FALSE]
  }
  
  cat("Entry signals done.\n")
  return(dt)
}

# ==============================================================================
# PRECOMPUTE EXIT SIGNALS (VECTORIZED)
# ==============================================================================

precompute_exit_signals <- function(dt) {
  cat("Precomputing exit signals (vectorized)...\n")
  
  exits <- list()
  
  exits$bollinger <- dt[!is.na(bb_upper) & value > bb_upper, 
                        .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  bollinger: %d signals\n", nrow(exits$bollinger)))
  
  exits$rsi <- dt[!is.na(rsi) & rsi > 70,
                  .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  rsi: %d signals\n", nrow(exits$rsi)))
  
  exits$donchian <- dt[!is.na(donchian_high) & value == donchian_high,
                       .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  donchian: %d signals\n", nrow(exits$donchian)))
  
  exits$dip_recover <- dt[!is.na(pct_from_high) & pct_from_high >= 0,
                          .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  dip_recover: %d signals\n", nrow(exits$dip_recover)))
  
  exits$sd_recover <- dt[!is.na(z_score) & z_score >= 0,
                         .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  sd_recover: %d signals\n", nrow(exits$sd_recover)))
  
  exits$down_recover <- dt[!is.na(down_day) & down_day == FALSE,
                           .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  down_recover: %d signals\n", nrow(exits$down_recover)))
  
  exits$ma_cross <- dt[!is.na(ma_short) & !is.na(ma_long) & 
                       !is.na(prev_ma_short) & !is.na(prev_ma_long) &
                       ma_short < ma_long & prev_ma_short >= prev_ma_long,
                       .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  ma_cross: %d signals\n", nrow(exits$ma_cross)))
  
  exits$macd <- dt[!is.na(macd_line) & !is.na(macd_signal) &
                   !is.na(prev_macd_line) & !is.na(prev_macd_signal) &
                   macd_line < macd_signal & prev_macd_line >= prev_macd_signal,
                   .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  macd: %d signals\n", nrow(exits$macd)))
  
  exits$roc <- dt[!is.na(roc) & !is.na(prev_roc) & roc > 0 & prev_roc <= 0,
                  .(forecast_id, exit_date = date, exit_price = value)]
  cat(sprintf("  roc: %d signals\n", nrow(exits$roc)))
  
  for (nm in names(exits)) {
    setkey(exits[[nm]], forecast_id, exit_date)
  }
  
  cat("Exit signals done.\n")
  return(exits)
}

# ==============================================================================
# JOIN ENTRIES TO EXITS - EXACT PATTERN FROM factor_flip_fund_v6_02.R line 1542
# ==============================================================================

join_entries_to_exits <- function(entries, exit_signals, exit_type) {
  if (nrow(entries) == 0) return(data.table())
  
  exits <- exit_signals[[exit_type]]
  if (is.null(exits) || nrow(exits) == 0) return(data.table())
  
  setkey(exits, forecast_id, exit_date)
  
  # EXACT pattern from factor_flip_fund_v6_02.R line 1542-1545:
  # matched <- band_exits[trades, on = .(forecast_id, exit_date > entry_date),
  #                       .(forecast_id, entry_date = i.entry_date, 
  #                         exit_date = x.exit_date, exit_price = x.exit_price),
  #                       mult = "first"]
  
  matched <- exits[entries, on = .(forecast_id, exit_date > entry_date),
                   .(forecast_id, entry_date = i.entry_date, entry_price = i.entry_price,
                     exit_date = x.exit_date, exit_price = x.exit_price),
                   mult = "first"]
  
  matched <- matched[!is.na(exit_date)]
  matched[, `:=`(exit_reason = exit_type, forced_exit = FALSE)]
  
  matched
}

# ==============================================================================
# RETURN-BASED EXITS (VECTORIZED)
# ==============================================================================

compute_pct_target_exits <- function(entries, dt, target, exit_reason_str) {
  if (nrow(entries) == 0) return(data.table())
  
  entries <- copy(entries)
  entries[, entry_id := .I]
  
  prices <- dt[, .(forecast_id, date, price = value)]
  setkey(prices, forecast_id, date)
  
  # Join entries to all future prices - EXACT pattern
  joined <- prices[entries, on = .(forecast_id, date > entry_date),
                   .(entry_id = i.entry_id, forecast_id = i.forecast_id, 
                     entry_date = i.entry_date, entry_price = i.entry_price,
                     date = x.date, price = x.price),
                   allow.cartesian = TRUE, nomatch = NULL]
  
  if (nrow(joined) == 0) return(data.table())
  
  joined[, ret := (price / entry_price) - 1]
  hits <- joined[ret >= target]
  
  if (nrow(hits) == 0) return(data.table())
  
  setorder(hits, entry_id, date)
  result <- hits[, .SD[1], by = entry_id]
  
  result[, .(forecast_id, entry_date, entry_price, 
             exit_date = date, exit_price = price, 
             exit_reason = exit_reason_str, forced_exit = FALSE)]
}

# ==============================================================================
# HOLD-N-DAYS EXITS (VECTORIZED)
# ==============================================================================

compute_hold_exits <- function(entries, dt, hold_days) {
  if (nrow(entries) == 0) return(data.table())
  
  entries <- copy(entries)
  entries[, target_date := entry_date + hold_days]
  
  prices <- dt[, .(forecast_id, date, price = value)]
  setkey(prices, forecast_id, date)
  
  # Join: find first date >= target_date - EXACT pattern
  result <- prices[entries, on = .(forecast_id, date >= target_date),
                   .(forecast_id = i.forecast_id, entry_date = i.entry_date, 
                     entry_price = i.entry_price, exit_date = x.date, exit_price = x.price),
                   mult = "first", nomatch = NULL]
  
  if (nrow(result) == 0) return(data.table())
  
  result[, `:=`(exit_reason = paste0(hold_days, "day_hold"), forced_exit = FALSE)]
  result
}

# ==============================================================================
# PATIENCE EXITS (VECTORIZED)
# ==============================================================================

compute_patience_exits <- function(entries, dt) {
  if (nrow(entries) == 0) return(data.table())
  
  entries <- copy(entries)
  entries[, entry_id := .I]
  
  prices <- dt[, .(forecast_id, date, price = value)]
  setkey(prices, forecast_id, date)
  
  # Join entries to all future prices - EXACT pattern
  joined <- prices[entries, on = .(forecast_id, date > entry_date),
                   .(entry_id = i.entry_id, forecast_id = i.forecast_id,
                     entry_date = i.entry_date, entry_price = i.entry_price,
                     date = x.date, price = x.price),
                   allow.cartesian = TRUE, nomatch = NULL]
  
  if (nrow(joined) == 0) return(data.table())
  
  joined[, days_held := as.integer(date - entry_date)]
  joined[, ret := (price / entry_price) - 1]
  
  joined[, patience_target := fcase(
    days_held < 60, 0.15,
    days_held < 90, 0.10,
    days_held < 120, 0.05,
    days_held < 180, 0.02,
    default = 0.00
  )]
  
  joined[, should_exit := (days_held >= 180) | (ret >= patience_target)]
  
  hits <- joined[should_exit == TRUE]
  if (nrow(hits) == 0) return(data.table())
  
  setorder(hits, entry_id, date)
  result <- hits[, .SD[1], by = entry_id]
  
  result[, .(forecast_id, entry_date, entry_price,
             exit_date = date, exit_price = price,
             exit_reason = fifelse(days_held >= 180, "forced_180", sprintf("patience_%.0f", patience_target * 100)),
             forced_exit = days_held >= 180)]
}

# ==============================================================================
# RANDOM EXITS (SEQUENTIAL - inherently random)
# ==============================================================================

compute_random_exits <- function(entries, dt) {
  if (nrow(entries) == 0) return(data.table())
  
  setkey(dt, forecast_id, date)
  entries <- copy(entries)
  entries[, entry_id := .I]
  
  results <- entries[, {
    future <- dt[forecast_id == .BY$forecast_id & date > entry_date]
    
    if (nrow(future) == 0) {
      list(exit_date = as.Date(NA), exit_price = NA_real_, exit_reason = "no_exit", forced_exit = FALSE)
    } else {
      exit_idx <- NA_integer_
      for (j in seq_len(nrow(future))) {
        days_held <- as.integer(future$date[j] - entry_date)
        if (days_held >= MAX_HOLD_DAYS) {
          exit_idx <- j
          break
        }
        if (runif(1) < 0.05) {
          exit_idx <- j
          break
        }
      }
      
      if (is.na(exit_idx)) {
        list(exit_date = as.Date(NA), exit_price = NA_real_, exit_reason = "no_exit", forced_exit = FALSE)
      } else {
        days_held <- as.integer(future$date[exit_idx] - entry_date)
        list(
          exit_date = future$date[exit_idx],
          exit_price = future$value[exit_idx],
          exit_reason = fifelse(days_held >= MAX_HOLD_DAYS, "hard_exit", "random"),
          forced_exit = days_held >= MAX_HOLD_DAYS
        )
      }
    }
  }, by = .(forecast_id, entry_date, entry_price)]
  
  results[!is.na(exit_date)]
}

# ==============================================================================
# EXTRACT ENTRIES
# ==============================================================================

extract_entries <- function(dt, entry_col) {
  dt[get(entry_col) == TRUE & !is.na(value),
     .(forecast_id, entry_date = date, entry_price = value)]
}

# ==============================================================================
# APPLY CAPITAL CONSTRAINTS
# ==============================================================================

apply_capital_constraints <- function(trades_dt, position_pct) {
  if (nrow(trades_dt) == 0) return(data.table())
  
  trades <- copy(trades_dt)
  trades <- trades[!is.na(exit_date)]
  setorder(trades, entry_date, forecast_id)
  trades[, trade_id := .I]
  setkey(trades, entry_date)
  
  n_trades <- nrow(trades)
  t_fid <- trades$forecast_id
  t_entry_date <- as.numeric(trades$entry_date)
  t_exit_date <- as.numeric(trades$exit_date)
  t_entry_price <- trades$entry_price
  t_exit_price <- trades$exit_price
  t_exit_reason <- trades$exit_reason
  t_forced_exit <- trades$forced_exit
  
  # Pre-allocate results
  r_fid <- character(n_trades)
  r_entry_date <- numeric(n_trades)
  r_exit_date <- numeric(n_trades)
  r_entry_price <- numeric(n_trades)
  r_exit_price <- numeric(n_trades)
  r_shares <- numeric(n_trades)
  r_position_value <- numeric(n_trades)
  r_exit_reason <- character(n_trades)
  r_forced_exit <- logical(n_trades)
  r_idx <- 0L
  
  # Open positions as list (no fixed limit)
  open_positions <- list()
  
  cash <- INITIAL_CAPITAL
  
  for (i in seq_len(n_trades)) {
    entry_d <- t_entry_date[i]
    fid <- t_fid[i]
    
    # Process exits for positions that have closed by entry_d
    for (pk in names(open_positions)) {
      pos <- open_positions[[pk]]
      if (!is.null(pos) && pos$exit_date <= entry_d) {
        proceeds <- pos$shares * pos$exit_price * (1 - TRANSACTION_COST_PCT)
        cash <- cash + proceeds
        open_positions[[pk]] <- NULL
      }
    }
    
    # Position sizing - percentage of available cash
    position_size <- cash * position_pct
    cost <- position_size * (1 + TRANSACTION_COST_PCT)
    
    if (cost > cash || position_size <= 0) next
    
    shares <- position_size / t_entry_price[i]
    cash <- cash - cost
    
    # Record open position
    pk <- paste0("trade_", i)
    open_positions[[pk]] <- list(
      forecast_id = fid,
      exit_date = t_exit_date[i],
      exit_price = t_exit_price[i],
      shares = shares
    )
    
    # Record realized trade
    r_idx <- r_idx + 1L
    r_fid[r_idx] <- fid
    r_entry_date[r_idx] <- entry_d
    r_exit_date[r_idx] <- t_exit_date[i]
    r_entry_price[r_idx] <- t_entry_price[i]
    r_exit_price[r_idx] <- t_exit_price[i]
    r_shares[r_idx] <- shares
    r_position_value[r_idx] <- position_size
    r_exit_reason[r_idx] <- t_exit_reason[i]
    r_forced_exit[r_idx] <- t_forced_exit[i]
  }
  
  if (r_idx == 0L) return(data.table())
  
  result <- data.table(
    forecast_id = r_fid[1:r_idx],
    entry_date = as.Date(r_entry_date[1:r_idx], origin = "1970-01-01"),
    exit_date = as.Date(r_exit_date[1:r_idx], origin = "1970-01-01"),
    entry_price = r_entry_price[1:r_idx],
    exit_price = r_exit_price[1:r_idx],
    shares = r_shares[1:r_idx],
    position_value = r_position_value[1:r_idx],
    exit_reason = r_exit_reason[1:r_idx],
    forced_exit = r_forced_exit[1:r_idx]
  )
  
  result[, gross_return := (exit_price / entry_price) - 1]
  result[, net_return := (exit_price * (1 - TRANSACTION_COST_PCT)) / 
                         (entry_price * (1 + TRANSACTION_COST_PCT)) - 1]
  result[, duration_days := as.numeric(exit_date - entry_date)]
  
  result
}

# BUILD EQUITY CURVE (VECTORIZED)
# ==============================================================================

build_equity_curve_vectorized <- function(positions_dt, price_dt, all_dates, period) {
  period_dates <- all_dates[all_dates >= period$start & all_dates <= period$end]
  n_dates <- length(period_dates)
  
  if (nrow(positions_dt) == 0) {
    return(data.table(
      date = period_dates,
      portfolio_value = rep(INITIAL_CAPITAL, n_dates),
      cash = rep(INITIAL_CAPITAL, n_dates),
      n_open_positions = rep(0L, n_dates)
    ))
  }
  
  date_lookup <- data.table(date = period_dates, date_idx = seq_along(period_dates))
  setkey(date_lookup, date)
  
  positions_dt <- copy(positions_dt)
  positions_dt[, pos_id := .I]
  positions_dt[date_lookup, entry_idx := i.date_idx, on = .(entry_date = date)]
  positions_dt[date_lookup, exit_idx := i.date_idx, on = .(exit_date = date)]
  
  positions_dt[, exit_in_period := !is.na(exit_idx)]
  positions_dt[is.na(exit_idx), exit_idx := n_dates]
  positions_dt <- positions_dt[!is.na(entry_idx)]
  
  if (nrow(positions_dt) == 0) {
    return(data.table(
      date = period_dates,
      portfolio_value = rep(INITIAL_CAPITAL, n_dates),
      cash = rep(INITIAL_CAPITAL, n_dates),
      n_open_positions = rep(0L, n_dates)
    ))
  }
  
  entry_changes <- positions_dt[, .(date_idx = entry_idx, cash_change = -position_value * (1 + TRANSACTION_COST_PCT)), by = pos_id]
  exit_changes <- positions_dt[exit_in_period == TRUE, .(date_idx = exit_idx, cash_change = shares * exit_price * (1 - TRANSACTION_COST_PCT)), by = pos_id]
  
  all_changes <- rbindlist(list(entry_changes, exit_changes), fill = TRUE)
  cash_by_day <- all_changes[, .(total_change = sum(cash_change)), by = date_idx]
  
  cash_vec <- rep(INITIAL_CAPITAL, n_dates)
  if (nrow(cash_by_day) > 0) {
    change_full <- rep(0, n_dates)
    change_full[cash_by_day$date_idx] <- cash_by_day$total_change
    cash_vec <- INITIAL_CAPITAL + cumsum(change_full)
  }
  
  positions_dt[, end_idx := fifelse(exit_in_period, exit_idx - 1L, exit_idx)]
  pos_expanded <- positions_dt[end_idx >= entry_idx, .(date_idx = entry_idx:end_idx), by = .(pos_id, forecast_id, shares)]
  
  if (nrow(pos_expanded) == 0) {
    return(data.table(
      date = period_dates,
      portfolio_value = cash_vec,
      cash = cash_vec,
      n_open_positions = rep(0L, n_dates)
    ))
  }
  
  pos_expanded[, date := period_dates[date_idx]]
  pos_expanded[price_dt, price := i.value, on = .(forecast_id, date)]
  pos_expanded[, mv := shares * price]
  
  daily_agg <- pos_expanded[, .(market_value = sum(mv, na.rm = TRUE), n_pos = .N), by = date_idx]
  
  market_value_vec <- rep(0, n_dates)
  n_pos_vec <- rep(0L, n_dates)
  
  if (nrow(daily_agg) > 0) {
    market_value_vec[daily_agg$date_idx] <- daily_agg$market_value
    n_pos_vec[daily_agg$date_idx] <- daily_agg$n_pos
  }
  
  data.table(
    date = period_dates,
    portfolio_value = cash_vec + market_value_vec,
    cash = cash_vec,
    n_open_positions = n_pos_vec
  )
}

# ==============================================================================
# METRIC FUNCTIONS
# ==============================================================================

calc_max_drawdown <- function(equity_vec) {
  if (length(equity_vec) < 2) return(NA_real_)
  running_max <- cummax(equity_vec)
  drawdowns <- (equity_vec - running_max) / running_max
  min(drawdowns, na.rm = TRUE)
}

calc_sharpe <- function(equity_vec) {
  if (length(equity_vec) < 10) return(NA_real_)
  rets <- diff(equity_vec) / head(equity_vec, -1)
  rets <- rets[is.finite(rets)]
  if (length(rets) < 10 || sd(rets) == 0) return(NA_real_)
  (mean(rets) * 252 - RISK_FREE_RATE) / (sd(rets) * sqrt(252))
}

calc_sortino <- function(equity_vec) {
  if (length(equity_vec) < 10) return(NA_real_)
  rets <- diff(equity_vec) / head(equity_vec, -1)
  rets <- rets[is.finite(rets)]
  if (length(rets) < 10) return(NA_real_)
  daily_rf <- RISK_FREE_RATE / 252
  excess_rets <- rets - daily_rf
  downside_rets <- pmin(excess_rets, 0)
  downside_dev <- sqrt(mean(downside_rets^2)) * sqrt(252)
  if (downside_dev <= 0) return(NA_real_)
  (mean(rets) * 252 - RISK_FREE_RATE) / downside_dev
}

calc_calmar <- function(equity_vec, n_years) {
  if (length(equity_vec) < 10 || n_years <= 0) return(NA_real_)
  total_ret <- tail(equity_vec, 1) / head(equity_vec, 1) - 1
  cagr <- if (total_ret > -1) (1 + total_ret)^(1/n_years) - 1 else NA_real_
  max_dd <- calc_max_drawdown(equity_vec)
  if (is.na(max_dd) || abs(max_dd) < 0.001) return(NA_real_)
  cagr / abs(max_dd)
}

calc_volatility <- function(equity_vec) {
  if (length(equity_vec) < 10) return(NA_real_)
  rets <- diff(equity_vec) / head(equity_vec, -1)
  rets <- rets[is.finite(rets)]
  if (length(rets) < 10) return(NA_real_)
  sd(rets) * sqrt(252)
}

calc_skewness <- function(equity_vec) {
  if (length(equity_vec) < 10) return(NA_real_)
  rets <- diff(equity_vec) / head(equity_vec, -1)
  rets <- rets[is.finite(rets)]
  n <- length(rets)
  if (n < 10) return(NA_real_)
  m <- mean(rets); s <- sd(rets)
  if (s <= 0) return(NA_real_)
  (n / ((n-1) * (n-2))) * sum(((rets - m) / s)^3)
}

calc_kurtosis <- function(equity_vec) {
  if (length(equity_vec) < 10) return(NA_real_)
  rets <- diff(equity_vec) / head(equity_vec, -1)
  rets <- rets[is.finite(rets)]
  n <- length(rets)
  if (n < 10) return(NA_real_)
  m <- mean(rets); s <- sd(rets)
  if (s <= 0) return(NA_real_)
  ((n * (n+1)) / ((n-1) * (n-2) * (n-3))) * sum(((rets - m) / s)^4) - 
    (3 * (n-1)^2) / ((n-2) * (n-3))
}

calc_avg_exposure <- function(cash_vec, portfolio_vec) {
  if (length(cash_vec) == 0) return(NA_real_)
  valid <- portfolio_vec > 0
  if (sum(valid) == 0) return(NA_real_)
  mean(1 - cash_vec[valid] / portfolio_vec[valid], na.rm = TRUE)
}

calculate_buy_and_hold <- function(dt) {
  stock_endpoints <- dt[, .(
    first_price = value[which.min(date)],
    last_price = value[which.max(date)]
  ), by = forecast_id]
  
  stock_endpoints <- stock_endpoints[!is.na(first_price) & !is.na(last_price) & first_price > 0]
  n_stocks <- nrow(stock_endpoints)
  if (n_stocks == 0) return(list(gross_return = NA_real_, net_return = NA_real_))
  
  per_stock <- INITIAL_CAPITAL / n_stocks
  
  gross_final <- sum((stock_endpoints$last_price / stock_endpoints$first_price) * per_stock, na.rm = TRUE)
  net_final <- sum((stock_endpoints$last_price / stock_endpoints$first_price) * per_stock * NET_MULTIPLIER, na.rm = TRUE)
  
  list(
    gross_return = (gross_final - INITIAL_CAPITAL) / INITIAL_CAPITAL,
    net_return = (net_final - INITIAL_CAPITAL) / INITIAL_CAPITAL
  )
}

# ==============================================================================
# COMPUTE_METRICS WITH FF REGRESSION (WITH HAC SEs)
# ==============================================================================

compute_metrics <- function(strategy_name, refit_name, exit_strategy_name, position_size_name,
                            realized, equity_dt, n_years, bh_gross, bh_net, is_gross, ff_factors = NULL) {
  
  bh_return <- if (is_gross) bh_gross else bh_net
  returns_col <- if (is_gross) "gross_return" else "net_return"
  band_name <- strategy_name
  
  ff_res <- run_ff_regression(equity_dt, ff_factors)
  hm_res <- run_hm_regression(equity_dt, ff_factors)
  tm_res <- run_tm_regression(equity_dt, ff_factors)
  
  if (nrow(realized) == 0 || nrow(equity_dt) == 0) {
    return(data.table(
      strategy = strategy_name, refit = refit_name, exit_strategy = exit_strategy_name,
      position_size = position_size_name, band = band_name,
      total_events = 0L, pct_positive = NA_real_, n_winners = 0L, n_losers = 0L,
      mean_return = NA_real_, median_return = NA_real_, mean_duration = NA_real_,
      max_duration = NA_integer_, n_forced_exits = 0L,
      total_return = NA_real_, cagr = NA_real_, bh_return = bh_return,
      exposure_matched_bh = NA_real_, avg_exposure = NA_real_, excess_return = NA_real_,
      sharpe_ratio = NA_real_, sortino_ratio = NA_real_, calmar_ratio = NA_real_,
      volatility = NA_real_, skewness = NA_real_, kurtosis = NA_real_, max_drawdown = NA_real_,
      p_binom = NA_real_, p_wilcoxon = NA_real_,
      return_per_event = NA_real_, return_per_exposure = NA_real_,
      ff_alpha = NA_real_, ff_alpha_t = NA_real_, ff_alpha_p = NA_real_, ff_r_squared = NA_real_,
      ff_beta_mkt = NA_real_, ff_beta_smb = NA_real_, ff_beta_hml = NA_real_,
      ff_beta_rmw = NA_real_, ff_beta_cma = NA_real_, ff_beta_mom = NA_real_,
      # SEs for equivalence testing
      ff_alpha_se = NA_real_,
      ff_beta_mkt_se = NA_real_, ff_beta_smb_se = NA_real_, ff_beta_hml_se = NA_real_,
      ff_beta_rmw_se = NA_real_, ff_beta_cma_se = NA_real_, ff_beta_mom_se = NA_real_,
      ff_df = NA_integer_,
      hm_alpha = NA_real_, hm_alpha_t = NA_real_, hm_alpha_p = NA_real_,
      hm_beta1 = NA_real_, hm_beta2 = NA_real_, hm_beta2_t = NA_real_, hm_beta2_p = NA_real_,
      hm_r_squared = NA_real_,
      hm_alpha_se = NA_real_, hm_beta1_se = NA_real_, hm_beta2_se = NA_real_,
      hm_df = NA_integer_,
      tm_alpha = NA_real_, tm_alpha_t = NA_real_, tm_alpha_p = NA_real_,
      tm_beta = NA_real_, tm_gamma = NA_real_, tm_gamma_t = NA_real_, tm_gamma_p = NA_real_,
      tm_r_squared = NA_real_,
      tm_alpha_se = NA_real_, tm_beta_se = NA_real_, tm_gamma_se = NA_real_,
      tm_df = NA_integer_
    ))
  }
  
  returns <- realized[[returns_col]]
  equity_vec <- equity_dt$portfolio_value
  cash_vec <- equity_dt$cash
  
  n_events <- nrow(realized)
  total_return <- tail(equity_vec, 1) / INITIAL_CAPITAL - 1
  cagr <- if (total_return > -1) (1 + total_return)^(1/n_years) - 1 else NA_real_
  avg_exposure <- calc_avg_exposure(cash_vec, equity_vec)
  
  rf_period <- RISK_FREE_RATE * n_years
  exposure_matched_bh <- if (!is.na(avg_exposure) && avg_exposure > 0) {
    (avg_exposure * bh_return) + ((1 - avg_exposure) * rf_period)
  } else bh_return
  excess_return <- total_return - exposure_matched_bh
  
  return_per_exposure <- if (!is.na(avg_exposure) && avg_exposure > 0) total_return / avg_exposure else NA_real_
  
  n_pos <- sum(returns > 0)
  p_binom <- tryCatch(binom.test(n_pos, n_events, p = 0.5, alternative = "greater")$p.value, error = function(e) NA_real_)
  p_wilcoxon <- tryCatch(wilcox.test(returns, mu = 0, alternative = "greater", exact = FALSE)$p.value, error = function(e) NA_real_)
  
  data.table(
    strategy = strategy_name, refit = refit_name, exit_strategy = exit_strategy_name,
    position_size = position_size_name, band = band_name,
    total_events = n_events, pct_positive = mean(returns > 0),
    n_winners = sum(returns > 0), n_losers = sum(returns <= 0),
    mean_return = mean(returns), median_return = median(returns),
    mean_duration = mean(realized$duration_days), max_duration = max(realized$duration_days),
    n_forced_exits = sum(realized$forced_exit),
    total_return = total_return, cagr = cagr, bh_return = bh_return,
    exposure_matched_bh = exposure_matched_bh, avg_exposure = avg_exposure, excess_return = excess_return,
    sharpe_ratio = calc_sharpe(equity_vec), sortino_ratio = calc_sortino(equity_vec),
    calmar_ratio = calc_calmar(equity_vec, n_years), volatility = calc_volatility(equity_vec),
    skewness = calc_skewness(equity_vec), kurtosis = calc_kurtosis(equity_vec),
    max_drawdown = calc_max_drawdown(equity_vec),
    p_binom = p_binom, p_wilcoxon = p_wilcoxon,
    return_per_event = if (n_events > 0) excess_return / n_events else NA_real_,
    return_per_exposure = return_per_exposure,
    ff_alpha = ff_res$ff_alpha, ff_alpha_t = ff_res$ff_alpha_t, ff_alpha_p = ff_res$ff_alpha_p,
    ff_r_squared = ff_res$ff_r_squared,
    ff_beta_mkt = ff_res$ff_beta_mkt, ff_beta_smb = ff_res$ff_beta_smb, ff_beta_hml = ff_res$ff_beta_hml,
    ff_beta_rmw = ff_res$ff_beta_rmw, ff_beta_cma = ff_res$ff_beta_cma, ff_beta_mom = ff_res$ff_beta_mom,
    # SEs for equivalence testing
    ff_alpha_se = ff_res$ff_alpha_se,
    ff_beta_mkt_se = ff_res$ff_beta_mkt_se, ff_beta_smb_se = ff_res$ff_beta_smb_se,
    ff_beta_hml_se = ff_res$ff_beta_hml_se, ff_beta_rmw_se = ff_res$ff_beta_rmw_se,
    ff_beta_cma_se = ff_res$ff_beta_cma_se, ff_beta_mom_se = ff_res$ff_beta_mom_se,
    ff_df = ff_res$ff_df,
    hm_alpha = hm_res$hm_alpha, hm_alpha_t = hm_res$hm_alpha_t, hm_alpha_p = hm_res$hm_alpha_p,
    hm_beta1 = hm_res$hm_beta1, hm_beta2 = hm_res$hm_beta2,
    hm_beta2_t = hm_res$hm_beta2_t, hm_beta2_p = hm_res$hm_beta2_p,
    hm_r_squared = hm_res$hm_r_squared,
    hm_alpha_se = hm_res$hm_alpha_se, hm_beta1_se = hm_res$hm_beta1_se, hm_beta2_se = hm_res$hm_beta2_se,
    hm_df = hm_res$hm_df,
    tm_alpha = tm_res$tm_alpha, tm_alpha_t = tm_res$tm_alpha_t, tm_alpha_p = tm_res$tm_alpha_p,
    tm_beta = tm_res$tm_beta, tm_gamma = tm_res$tm_gamma,
    tm_gamma_t = tm_res$tm_gamma_t, tm_gamma_p = tm_res$tm_gamma_p,
    tm_r_squared = tm_res$tm_r_squared,
    tm_alpha_se = tm_res$tm_alpha_se, tm_beta_se = tm_res$tm_beta_se, tm_gamma_se = tm_res$tm_gamma_se,
    tm_df = tm_res$tm_df
  )
}


# ==============================================================================
# EXCEL OUTPUT (with FF + H-M + T-M columns)
# ==============================================================================

write_formatted_excel <- function(results_list, filepath) {
  wb <- createWorkbook()
  
  header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#4472C4", halign = "center", textDecoration = "bold")
  pct_style <- createStyle(numFmt = "0.0%")
  pct4_style <- createStyle(numFmt = "0.0000%")
  sci_style <- createStyle(numFmt = "0.00E+00")
  number_style <- createStyle(numFmt = "#,##0")
  decimal_style <- createStyle(numFmt = "0.000")
  
  pct_cols <- c("pct_positive", "mean_return", "median_return", "total_return", "cagr", "bh_return",
                "exposure_matched_bh", "excess_return", "volatility", "avg_exposure", "return_per_exposure",
                "max_drawdown", "ff_alpha", "ff_r_squared", "hm_alpha", "hm_r_squared", "tm_alpha", "tm_r_squared")
  pct4_cols <- c("return_per_event", "return_per_exposure")
  sci_cols <- c("p_binom", "p_wilcoxon", "ff_alpha_p", "hm_alpha_p", "hm_beta2_p", "tm_alpha_p", "tm_gamma_p")
  int_cols <- c("total_events", "n_winners", "n_losers", "n_forced_exits", "max_duration")
  dec_cols <- c("mean_duration", "sharpe_ratio", "sortino_ratio", "calmar_ratio", "skewness", "kurtosis",
                "ff_alpha_t", "ff_beta_mkt", "ff_beta_smb", "ff_beta_hml", "ff_beta_rmw", "ff_beta_cma", "ff_beta_mom",
                "hm_alpha_t", "hm_beta1", "hm_beta2", "hm_beta2_t",
                "tm_alpha_t", "tm_beta", "tm_gamma", "tm_gamma_t")
  
  for (period_name in names(results_list)) {
    df <- results_list[[period_name]]
    if (nrow(df) == 0) next
    
    addWorksheet(wb, period_name)
    writeData(wb, period_name, df, headerStyle = header_style)
    
    col_names <- names(df)
    n_rows <- nrow(df) + 1
    
    for (col_idx in seq_along(col_names)) {
      col_name <- col_names[col_idx]
      if (col_name %in% pct4_cols) {
        addStyle(wb, period_name, pct4_style, rows = 2:n_rows, cols = col_idx, gridExpand = TRUE)
      } else if (col_name %in% pct_cols) {
        addStyle(wb, period_name, pct_style, rows = 2:n_rows, cols = col_idx, gridExpand = TRUE)
      } else if (col_name %in% sci_cols) {
        addStyle(wb, period_name, sci_style, rows = 2:n_rows, cols = col_idx, gridExpand = TRUE)
      } else if (col_name %in% int_cols) {
        addStyle(wb, period_name, number_style, rows = 2:n_rows, cols = col_idx, gridExpand = TRUE)
      } else if (col_name %in% dec_cols) {
        addStyle(wb, period_name, decimal_style, rows = 2:n_rows, cols = col_idx, gridExpand = TRUE)
      }
    }
    
    setColWidths(wb, period_name, cols = 1:ncol(df), widths = "auto")
  }
  
  saveWorkbook(wb, filepath, overwrite = TRUE)
}

# STRATEGIES CONFIGURATION
# ==============================================================================

STRATEGIES <- list(
  list(name = "bollinger", entry = "entry_bollinger", exit_type = "bollinger", refit = "signal"),
  list(name = "rsi", entry = "entry_rsi", exit_type = "rsi", refit = "signal"),
  list(name = "ma_cross", entry = "entry_ma_cross", exit_type = "ma_cross", refit = "signal"),
  list(name = "macd", entry = "entry_macd", exit_type = "macd", refit = "signal"),
  list(name = "donchian", entry = "entry_donchian", exit_type = "donchian", refit = "signal"),
  list(name = "roc", entry = "entry_roc", exit_type = "roc", refit = "signal"),
  list(name = "dip_5", entry = "entry_dip_5", exit_type = "dip_recover", refit = "signal"),
  list(name = "dip_10", entry = "entry_dip_10", exit_type = "dip_recover", refit = "signal"),
  list(name = "sd_2", entry = "entry_sd_2", exit_type = "sd_recover", refit = "signal"),
  list(name = "sd_3", entry = "entry_sd_3", exit_type = "sd_recover", refit = "signal"),
  list(name = "down_3", entry = "entry_down_3", exit_type = "down_recover", refit = "signal"),
  list(name = "down_5", entry = "entry_down_5", exit_type = "down_recover", refit = "signal"),
  list(name = "52w_low", entry = "entry_52w_low", exit_type = "pct_target_10", refit = "signal"),
  list(name = "52w_high", entry = "entry_52w_high", exit_type = "pct_target_10", refit = "signal"),
  list(name = "below_ma", entry = "entry_below_ma", exit_type = "pct_target_5", refit = "signal"),
  list(name = "above_ma", entry = "entry_above_ma", exit_type = "pct_target_5", refit = "signal"),
  list(name = "random_tsf", entry = "entry_random_tsf", exit_type = "random", refit = "random"),
  list(name = "random_uniform", entry = "entry_random_uniform", exit_type = "random", refit = "random")
)

# MAIN EXECUTION
# ==============================================================================

cat("================================================================================\n")
cat("DATA LOADING\n")
cat("================================================================================\n")

full_data <- fread(INPUT_FILE)
full_data[, date := as.Date(date, format = "%m/%d/%y")]
setorder(full_data, forecast_id, date)
setkey(full_data, forecast_id, date)

cat(sprintf("  %d rows, %d stocks\n", nrow(full_data), uniqueN(full_data$forecast_id)))
cat(sprintf("  Date range: %s to %s\n\n", min(full_data$date), max(full_data$date)))

all_dates <- sort(unique(full_data$date))

full_data <- calculate_indicators(full_data)
full_data <- generate_entry_signals(full_data)

# Load FF factors
ff_factors <- tryCatch({
  load_ff_factors(FF_5_FACTOR_FILE, FF_MOMENTUM_FILE)
}, error = function(e) {
  cat(sprintf("  WARNING: Could not load FF factors: %s\n", e$message))
  cat("  Proceeding without FF analysis.\n")
  NULL
})

# ==============================================================================
# PHASE 1: PRECOMPUTE ALL EXITS
# ==============================================================================

cat("\n================================================================================\n")
cat("PHASE 1: Precompute exit signals (vectorized)\n")
cat("================================================================================\n")

if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

exit_signals <- precompute_exit_signals(full_data)

# ==============================================================================
# PHASE 2: PRECOMPUTE ALL TRADES
# ==============================================================================

cat("\n================================================================================\n")
cat("PHASE 2: Precompute all trades\n")
cat("================================================================================\n")

if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

TRADES_CACHE_FILE <- file.path(CACHE_DIR, paste0(UNIVERSE, "_variable_trades_cache.rds"))

if (file.exists(TRADES_CACHE_FILE)) {
  cat(sprintf("  Loading cached trades from %s\n", TRADES_CACHE_FILE))
  trades_cache <- readRDS(TRADES_CACHE_FILE)
  cat(sprintf("  Loaded %d cached trade sets\n", length(trades_cache)))
} else {
  cat("  No cache found, computing trades...\n")
  trades_cache <- list()

  for (strat in STRATEGIES) {
  strat_name <- strat$name
  entry_col <- strat$entry
  exit_type <- strat$exit_type
  
  t0 <- Sys.time()
  
  entries <- extract_entries(full_data, entry_col)
  
  if (nrow(entries) == 0) {
    trades_cache[[paste0(strat_name, "_native")]] <- data.table()
    trades_cache[[paste0(strat_name, "_patience")]] <- data.table()
    cat(sprintf("  %s: 0 entries\n", strat_name))
    next
  }
  
  if (exit_type %in% names(exit_signals)) {
    native_trades <- join_entries_to_exits(entries, exit_signals, exit_type)
  } else if (exit_type == "pct_target_10") {
    native_trades <- compute_pct_target_exits(entries, full_data, 0.10, "10pct_gain")
  } else if (exit_type == "pct_target_5") {
    native_trades <- compute_pct_target_exits(entries, full_data, 0.05, "5pct_gain")
  } else if (exit_type == "hold_5") {
    native_trades <- compute_hold_exits(entries, full_data, 5)
  } else if (exit_type == "random") {
    native_trades <- compute_random_exits(entries, full_data)
  } else {
    native_trades <- data.table()
  }
  
  trades_cache[[paste0(strat_name, "_native")]] <- native_trades
  
  patience_trades <- compute_patience_exits(entries, full_data)
  trades_cache[[paste0(strat_name, "_patience")]] <- patience_trades
  
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  cat(sprintf("  %s: %d entries -> %d native, %d patience (%.2fs)\n", 
              strat_name, nrow(entries), nrow(native_trades), nrow(patience_trades), elapsed))
}

  # Save cache
  cat(sprintf("  Saving trades cache to %s\n", TRADES_CACHE_FILE))
  saveRDS(trades_cache, TRADES_CACHE_FILE)
  cat(sprintf("  Saved %d trade sets to cache\n", length(trades_cache)))
}

# ==============================================================================
# ALL STATISTICAL TESTS (v2.02 - COMPLETE APPENDIX D COLUMNS)
# ==============================================================================

run_all_20_tests <- function(strat_name, period_name, pos_name,
                             ret_patience, ret_native,
                             ff_patience, ff_native) {
  
  # Align daily returns by date
  common_dates <- intersect(ret_patience$date, ret_native$date)
  n_obs <- length(common_dates)
  
  # NA return template with all columns including 16 metric diffs
  na_result <- data.table(
    strategy = strat_name,
    period = period_name,
    position_size = pos_name,
    n_observations = n_obs,
    # All 16 hypothesis metric diffs
    ff_alpha_diff = NA_real_, ff_r_squared_diff = NA_real_, ff_beta_mkt_diff = NA_real_,
    ff_beta_smb_diff = NA_real_, ff_beta_hml_diff = NA_real_, ff_beta_rmw_diff = NA_real_,
    ff_beta_cma_diff = NA_real_, ff_beta_mom_diff = NA_real_,
    tm_alpha_diff = NA_real_, tm_gamma_diff = NA_real_, tm_r_squared_diff = NA_real_, tm_beta_diff = NA_real_,
    hm_alpha_diff = NA_real_, hm_beta2_diff = NA_real_, hm_r_squared_diff = NA_real_, hm_beta1_diff = NA_real_,
    # HAC t-statistic (D.2.1)
    hac_t_stat = NA_real_, hac_t_p = NA_real_, hac_se = NA_real_,
    # Wilcoxon (D.2.2)
    wilcoxon_w_pos = NA_real_, wilcoxon_w_neg = NA_real_, wilcoxon_stat = NA_real_, wilcoxon_p = NA_real_,
    # Binomial (D.2.3)
    binomial_k = NA_integer_, binomial_n = NA_integer_, binomial_p = NA_real_,
    # Benjamini-Hochberg (D.2.5)
    bh_adj_p = NA_real_,
    # Cohen's d (D.2.6)
    cohens_d = NA_real_,
    # TOST (D.3.1)
    tost_delta = NA_real_, tost_t_lower = NA_real_, tost_t_upper = NA_real_,
    tost_p_lower = NA_real_, tost_p_upper = NA_real_, tost_p = NA_real_,
    # HAC CI (D.3.2)
    hac_ci_lower = NA_real_, hac_ci_upper = NA_real_,
    hac_ci_includes_zero = NA, hac_ci_within_bounds = NA,
    # Paired t-test (D.4.1)
    paired_t_stat = NA_real_, paired_t_p = NA_real_,
    # Welch's t-test (D.4.1)
    welch_t_stat = NA_real_, welch_t_p = NA_real_, welch_df = NA_real_,
    # Sign test (D.4.2)
    sign_n_pos = NA_integer_, sign_n_neg = NA_integer_, sign_test_p = NA_real_,
    # Bonferroni (D.4.3)
    bonferroni_p = NA_real_,
    # Holm-Bonferroni (D.4.3)
    holm_p = NA_real_,
    # Chi-square (D.4.4)
    chisq_stat = NA_real_, chisq_p = NA_real_,
    # Fisher's exact (D.4.4)
    fisher_odds_ratio = NA_real_, fisher_p = NA_real_,
    # Bayes Factor (D.4.5)
    bayes_factor = NA_real_,
    # Meta-analytic (D.4.6)
    meta_fe_effect = NA_real_, meta_fe_se = NA_real_,
    meta_re_effect = NA_real_, meta_re_tau2 = NA_real_,
    i_squared = NA_real_,
    # Summary
    improvement_rate = NA_real_, n_positive = NA_integer_, n_negative = NA_integer_
  )
  
  if (n_obs < 30) {
    return(na_result)
  }
  
  ret_p <- ret_patience[date %in% common_dates][order(date)]$daily_return
  ret_n <- ret_native[date %in% common_dates][order(date)]$daily_return
  diffs <- ret_p - ret_n
  diffs <- diffs[is.finite(diffs)]
  n_diffs <- length(diffs)
  
  if (n_diffs < 30) {
    return(na_result)
  }
  
  # Extract ALL 16 hypothesis metric diffs
  # Fama-French Tests (H_FF1-H_FF3)
  ff_alpha_diff <- as.numeric(ff_patience$ff_alpha) - as.numeric(ff_native$ff_alpha)
  ff_r_squared_diff <- as.numeric(ff_patience$ff_r_squared) - as.numeric(ff_native$ff_r_squared)
  ff_beta_mkt_diff <- as.numeric(ff_patience$ff_beta_mkt) - as.numeric(ff_native$ff_beta_mkt)
  # Fama-French Controls (H_FF4-H_FF8)
  ff_beta_smb_diff <- as.numeric(ff_patience$ff_beta_smb) - as.numeric(ff_native$ff_beta_smb)
  ff_beta_hml_diff <- as.numeric(ff_patience$ff_beta_hml) - as.numeric(ff_native$ff_beta_hml)
  ff_beta_rmw_diff <- as.numeric(ff_patience$ff_beta_rmw) - as.numeric(ff_native$ff_beta_rmw)
  ff_beta_cma_diff <- as.numeric(ff_patience$ff_beta_cma) - as.numeric(ff_native$ff_beta_cma)
  ff_beta_mom_diff <- as.numeric(ff_patience$ff_beta_mom) - as.numeric(ff_native$ff_beta_mom)
  # Treynor-Mazuy (H_TM1-H_TM4)
  tm_alpha_diff <- as.numeric(ff_patience$tm_alpha) - as.numeric(ff_native$tm_alpha)
  tm_gamma_diff <- as.numeric(ff_patience$tm_gamma) - as.numeric(ff_native$tm_gamma)
  tm_r_squared_diff <- as.numeric(ff_patience$tm_r_squared) - as.numeric(ff_native$tm_r_squared)
  tm_beta_diff <- as.numeric(ff_patience$tm_beta) - as.numeric(ff_native$tm_beta)
  # Henriksson-Merton (H_HM1-H_HM4)
  hm_alpha_diff <- as.numeric(ff_patience$hm_alpha) - as.numeric(ff_native$hm_alpha)
  hm_beta2_diff <- as.numeric(ff_patience$hm_beta2) - as.numeric(ff_native$hm_beta2)
  hm_r_squared_diff <- as.numeric(ff_patience$hm_r_squared) - as.numeric(ff_native$hm_r_squared)
  hm_beta1_diff <- as.numeric(ff_patience$hm_beta1) - as.numeric(ff_native$hm_beta1)
  
  # =========================================================================
  # D.2.1 HAC t-statistic (Newey-West)
  # =========================================================================
  hac_t_stat <- NA_real_
  hac_t_p <- NA_real_
  hac_se <- NA_real_
  hac_ci_lower <- NA_real_
  hac_ci_upper <- NA_real_
  
  tryCatch({
    fit <- lm(diffs ~ 1)
    hac_vcov <- sandwich::vcovHAC(fit)
    hac_test <- lmtest::coeftest(fit, vcov = hac_vcov)
    hac_t_stat <- hac_test[1, "t value"]
    hac_t_p <- hac_test[1, "Pr(>|t|)"] / 2  # One-sided
    if (hac_t_stat < 0) hac_t_p <- 1 - hac_t_p
    hac_se <- sqrt(hac_vcov[1,1])
    hac_ci_lower <- coef(fit)[1] - 1.96 * hac_se
    hac_ci_upper <- coef(fit)[1] + 1.96 * hac_se
  }, error = function(e) {})
  
  # D.3.2 HAC CI checks
  hac_ci_includes_zero <- NA
  hac_ci_within_bounds <- NA
  if (!is.na(hac_ci_lower) && !is.na(hac_ci_upper)) {
    hac_ci_includes_zero <- (hac_ci_lower <= 0) & (hac_ci_upper >= 0)
    hac_ci_within_bounds <- (hac_ci_lower >= -0.10) & (hac_ci_upper <= 0.10)
  }
  
  # =========================================================================
  # D.2.2 Wilcoxon Signed-Rank
  # =========================================================================
  wilcoxon_w_pos <- NA_real_
  wilcoxon_w_neg <- NA_real_
  wilcoxon_stat <- NA_real_
  wilcoxon_p <- NA_real_
  
  tryCatch({
    wt <- wilcox.test(diffs, mu = 0, alternative = "greater", exact = FALSE)
    wilcoxon_stat <- as.numeric(wt$statistic)
    wilcoxon_p <- wt$p.value
    
    # Compute W+ and W-
    ranks <- rank(abs(diffs))
    wilcoxon_w_pos <- sum(ranks[diffs > 0])
    wilcoxon_w_neg <- sum(ranks[diffs < 0])
  }, error = function(e) {})
  
  # =========================================================================
  # D.2.3 Binomial
  # =========================================================================
  n_pos <- sum(diffs > 0)
  n_neg <- sum(diffs < 0)
  n_nonzero <- n_pos + n_neg
  
  binomial_k <- n_pos
  binomial_n <- n_nonzero
  binomial_p <- tryCatch({
    binom.test(n_pos, n_nonzero, p = 0.5, alternative = "greater")$p.value
  }, error = function(e) NA_real_)
  
  # =========================================================================
  # D.2.5 Benjamini-Hochberg
  # =========================================================================
  bh_adj_p <- tryCatch({
    p.adjust(hac_t_p, method = "BH")
  }, error = function(e) NA_real_)
  
  # =========================================================================
  # D.2.6 Cohen's d
  # =========================================================================
  cohens_d <- tryCatch({
    mean(diffs) / sd(diffs)
  }, error = function(e) NA_real_)
  
  # =========================================================================
  # D.3.1 TOST (Two One-Sided Tests)
  # =========================================================================
  tost_delta <- 0.10
  tost_t_lower <- NA_real_
  tost_t_upper <- NA_real_
  tost_p_lower <- NA_real_
  tost_p_upper <- NA_real_
  tost_p <- NA_real_
  
  tryCatch({
    mean_diff <- mean(diffs)
    se_diff <- sd(diffs) / sqrt(n_diffs)
    
    # Test H01: difference <= -delta
    tost_t_lower <- (mean_diff - (-tost_delta)) / se_diff
    tost_p_lower <- pt(tost_t_lower, df = n_diffs - 1, lower.tail = FALSE)
    
    # Test H02: difference >= +delta
    tost_t_upper <- (mean_diff - tost_delta) / se_diff
    tost_p_upper <- pt(tost_t_upper, df = n_diffs - 1, lower.tail = TRUE)
    
    # TOST p-value
    tost_p <- max(tost_p_lower, tost_p_upper)
  }, error = function(e) {})
  
  # =========================================================================
  # D.4.1 Paired t-test
  # =========================================================================
  paired_t_stat <- NA_real_
  paired_t_p <- NA_real_
  
  tryCatch({
    pt_result <- t.test(diffs, mu = 0, alternative = "greater")
    paired_t_stat <- as.numeric(pt_result$statistic)
    paired_t_p <- pt_result$p.value
  }, error = function(e) {})
  
  # =========================================================================
  # D.4.1 Welch's t-test
  # =========================================================================
  welch_t_stat <- NA_real_
  welch_t_p <- NA_real_
  welch_df <- NA_real_
  
  tryCatch({
    wt_result <- t.test(ret_p, ret_n, alternative = "greater", paired = FALSE, var.equal = FALSE)
    welch_t_stat <- as.numeric(wt_result$statistic)
    welch_t_p <- wt_result$p.value
    welch_df <- as.numeric(wt_result$parameter)
  }, error = function(e) {})
  
  # =========================================================================
  # D.4.2 Sign test
  # =========================================================================
  sign_n_pos <- n_pos
  sign_n_neg <- n_neg
  sign_test_p <- tryCatch({
    binom.test(n_pos, n_nonzero, p = 0.5, alternative = "greater")$p.value
  }, error = function(e) NA_real_)
  
  # =========================================================================
  # D.4.3 Bonferroni
  # =========================================================================
  bonferroni_p <- tryCatch({
    p.adjust(hac_t_p, method = "bonferroni", n = 20)
  }, error = function(e) NA_real_)
  
  # =========================================================================
  # D.4.3 Holm-Bonferroni
  # =========================================================================
  holm_p <- tryCatch({
    p.adjust(hac_t_p, method = "holm", n = 20)
  }, error = function(e) NA_real_)
  
  # =========================================================================
  # D.4.4 Chi-square
  # =========================================================================
  chisq_stat <- NA_real_
  chisq_p <- NA_real_
  
  tryCatch({
    chi_result <- chisq.test(c(n_pos, n_neg), p = c(0.5, 0.5))
    chisq_stat <- as.numeric(chi_result$statistic)
    chisq_p <- chi_result$p.value
  }, error = function(e) {})
  
  # =========================================================================
  # D.4.4 Fisher's exact
  # =========================================================================
  fisher_odds_ratio <- NA_real_
  fisher_p <- NA_real_
  
  tryCatch({
    expected_half <- n_nonzero / 2
    mat <- matrix(c(n_pos, n_neg, round(expected_half), round(expected_half)), nrow = 2)
    fish_result <- fisher.test(mat, alternative = "greater")
    fisher_odds_ratio <- as.numeric(fish_result$estimate)
    fisher_p <- fish_result$p.value
  }, error = function(e) {})
  
  # =========================================================================
  # D.4.5 Bayes Factor
  # =========================================================================
  bayes_factor <- tryCatch({
    fit_null <- lm(diffs ~ 0)
    fit_alt <- lm(diffs ~ 1)
    bic_null <- BIC(fit_null)
    bic_alt <- BIC(fit_alt)
    exp((bic_null - bic_alt) / 2)
  }, error = function(e) NA_real_)
  
  # =========================================================================
  # D.4.6 Meta-analytic (Fixed Effects)
  # =========================================================================
  meta_fe_effect <- mean(diffs)
  meta_fe_se <- sd(diffs) / sqrt(n_diffs)
  
  # =========================================================================
  # D.4.6 Meta-analytic (Random Effects)
  # =========================================================================
  meta_re_effect <- mean(diffs)
  meta_re_tau2 <- 0  # Single study, no between-study variance
  
  # =========================================================================
  # D.4.6 I-squared
  # =========================================================================
  i_squared <- 0  # Single study, no heterogeneity
  
  # =========================================================================
  # Return complete result
  # =========================================================================
  data.table(
    strategy = strat_name,
    period = period_name,
    position_size = pos_name,
    n_observations = n_obs,
    # All 16 hypothesis metric diffs
    ff_alpha_diff = ff_alpha_diff,
    ff_r_squared_diff = ff_r_squared_diff,
    ff_beta_mkt_diff = ff_beta_mkt_diff,
    ff_beta_smb_diff = ff_beta_smb_diff,
    ff_beta_hml_diff = ff_beta_hml_diff,
    ff_beta_rmw_diff = ff_beta_rmw_diff,
    ff_beta_cma_diff = ff_beta_cma_diff,
    ff_beta_mom_diff = ff_beta_mom_diff,
    tm_alpha_diff = tm_alpha_diff,
    tm_gamma_diff = tm_gamma_diff,
    tm_r_squared_diff = tm_r_squared_diff,
    tm_beta_diff = tm_beta_diff,
    hm_alpha_diff = hm_alpha_diff,
    hm_beta2_diff = hm_beta2_diff,
    hm_r_squared_diff = hm_r_squared_diff,
    hm_beta1_diff = hm_beta1_diff,
    # D.2.1 HAC t-statistic
    hac_t_stat = hac_t_stat,
    hac_t_p = hac_t_p,
    hac_se = hac_se,
    # D.2.2 Wilcoxon
    wilcoxon_w_pos = wilcoxon_w_pos,
    wilcoxon_w_neg = wilcoxon_w_neg,
    wilcoxon_stat = wilcoxon_stat,
    wilcoxon_p = wilcoxon_p,
    # D.2.3 Binomial
    binomial_k = binomial_k,
    binomial_n = binomial_n,
    binomial_p = binomial_p,
    # D.2.5 Benjamini-Hochberg
    bh_adj_p = bh_adj_p,
    # D.2.6 Cohen's d
    cohens_d = cohens_d,
    # D.3.1 TOST
    tost_delta = tost_delta,
    tost_t_lower = tost_t_lower,
    tost_t_upper = tost_t_upper,
    tost_p_lower = tost_p_lower,
    tost_p_upper = tost_p_upper,
    tost_p = tost_p,
    # D.3.2 HAC CI
    hac_ci_lower = hac_ci_lower,
    hac_ci_upper = hac_ci_upper,
    hac_ci_includes_zero = hac_ci_includes_zero,
    hac_ci_within_bounds = hac_ci_within_bounds,
    # D.4.1 Paired t-test
    paired_t_stat = paired_t_stat,
    paired_t_p = paired_t_p,
    # D.4.1 Welch's t-test
    welch_t_stat = welch_t_stat,
    welch_t_p = welch_t_p,
    welch_df = welch_df,
    # D.4.2 Sign test
    sign_n_pos = sign_n_pos,
    sign_n_neg = sign_n_neg,
    sign_test_p = sign_test_p,
    # D.4.3 Bonferroni
    bonferroni_p = bonferroni_p,
    # D.4.3 Holm-Bonferroni
    holm_p = holm_p,
    # D.4.4 Chi-square
    chisq_stat = chisq_stat,
    chisq_p = chisq_p,
    # D.4.4 Fisher's exact
    fisher_odds_ratio = fisher_odds_ratio,
    fisher_p = fisher_p,
    # D.4.5 Bayes Factor
    bayes_factor = bayes_factor,
    # D.4.6 Meta-analytic
    meta_fe_effect = meta_fe_effect,
    meta_fe_se = meta_fe_se,
    meta_re_effect = meta_re_effect,
    meta_re_tau2 = meta_re_tau2,
    i_squared = i_squared,
    # Summary
    improvement_rate = n_pos / n_nonzero,
    n_positive = n_pos,
    n_negative = n_neg
  )
}

# ==============================================================================
# 16 HYPOTHESIS TESTS (FROM SCHEDULED EXPOSURE)
# ==============================================================================

run_16_hypothesis_tests <- function(strat_name, period_name, pos_name,
                                    ret_patience, ret_native,
                                    ff_patience, ff_native) {
  
  # =========================================================================
  # HELPER: Test a single coefficient hypothesis (DIRECTIONAL)
  # =========================================================================
  test_coef_directional <- function(coef_p, coef_n, se_p, se_n, df_p, df_n, direction = "greater") {
    result <- list(
      diff = NA_real_, se = NA_real_, t_stat = NA_real_, p_value = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, improved = NA
    )
    
    safe_is_na <- function(x) {
      if (is.null(x) || length(x) == 0) return(TRUE)
      return(is.na(x))
    }
    
    if (safe_is_na(coef_p) || safe_is_na(coef_n)) return(result)
    
    diff <- as.numeric(coef_p) - as.numeric(coef_n)
    result$diff <- diff
    result$improved <- if (direction == "greater") diff > 0 else diff < 0
    
    se_p_valid <- !safe_is_na(se_p) && as.numeric(se_p) > 0
    se_n_valid <- !safe_is_na(se_n) && as.numeric(se_n) > 0
    
    if (se_p_valid && se_n_valid) {
      se_diff <- sqrt(as.numeric(se_p)^2 + as.numeric(se_n)^2)
      result$se <- se_diff
      
      df_p_valid <- !safe_is_na(df_p) && as.numeric(df_p) > 0
      df_n_valid <- !safe_is_na(df_n) && as.numeric(df_n) > 0
      
      if (df_p_valid && df_n_valid) {
        df_approx <- (se_p^2 + se_n^2)^2 / (se_p^4/df_p + se_n^4/df_n)
      } else {
        df_approx <- 100
      }
      
      t_stat <- diff / se_diff
      result$t_stat <- t_stat
      
      if (direction == "greater") {
        result$p_value <- pt(t_stat, df = df_approx, lower.tail = FALSE)
      } else {
        result$p_value <- pt(t_stat, df = df_approx, lower.tail = TRUE)
      }
      
      t_crit <- qt(0.975, df = df_approx)
      result$ci_lower <- diff - t_crit * se_diff
      result$ci_upper <- diff + t_crit * se_diff
    }
    
    return(result)
  }
  
  # =========================================================================
  # HELPER: Test a single coefficient hypothesis (EQUIVALENCE - TOST)
  # =========================================================================
  test_coef_equivalence <- function(coef_p, coef_n, se_p, se_n, df_p, df_n, delta = 0.10) {
    result <- list(
      diff = NA_real_, se = NA_real_, tost_p = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, ci_includes_zero = NA
    )
    
    safe_is_na <- function(x) {
      if (is.null(x) || length(x) == 0) return(TRUE)
      return(is.na(x))
    }
    
    if (safe_is_na(coef_p) || safe_is_na(coef_n)) return(result)
    
    diff <- as.numeric(coef_p) - as.numeric(coef_n)
    result$diff <- diff
    
    se_p_valid <- !safe_is_na(se_p) && as.numeric(se_p) > 0
    se_n_valid <- !safe_is_na(se_n) && as.numeric(se_n) > 0
    
    if (se_p_valid && se_n_valid) {
      se_diff <- sqrt(as.numeric(se_p)^2 + as.numeric(se_n)^2)
      result$se <- se_diff
      
      df_p_valid <- !safe_is_na(df_p) && as.numeric(df_p) > 0
      df_n_valid <- !safe_is_na(df_n) && as.numeric(df_n) > 0
      
      if (df_p_valid && df_n_valid) {
        df_approx <- (se_p^2 + se_n^2)^2 / (se_p^4/df_p + se_n^4/df_n)
      } else {
        df_approx <- 100
      }
      
      t_lower <- (diff - (-delta)) / se_diff
      t_upper <- (diff - delta) / se_diff
      
      p_lower <- pt(t_lower, df = df_approx, lower.tail = FALSE)
      p_upper <- pt(t_upper, df = df_approx, lower.tail = TRUE)
      
      result$tost_p <- max(p_lower, p_upper)
      
      t_crit <- qt(0.975, df = df_approx)
      result$ci_lower <- diff - t_crit * se_diff
      result$ci_upper <- diff + t_crit * se_diff
      result$ci_includes_zero <- (result$ci_lower <= 0) & (result$ci_upper >= 0)
    }
    
    return(result)
  }
  
  # =========================================================================
  # HELPER: Test R-squared difference (no SE available)
  # =========================================================================
  test_rsq_directional <- function(rsq_p, rsq_n, direction = "less") {
    result <- list(
      diff = NA_real_, improved = NA
    )
    
    if (is.na(rsq_p) || is.na(rsq_n)) return(result)
    
    diff <- as.numeric(rsq_p) - as.numeric(rsq_n)
    result$diff <- diff
    result$improved <- if (direction == "less") diff < 0 else diff > 0
    
    return(result)
  }
  
  # =========================================================================
  # SETUP
  # =========================================================================
  common_dates <- intersect(ret_patience$date, ret_native$date)
  n_obs <- length(common_dates)
  
  df_ff_p <- as.numeric(ff_patience$ff_df)
  df_ff_n <- as.numeric(ff_native$ff_df)
  df_tm_p <- as.numeric(ff_patience$tm_df)
  df_tm_n <- as.numeric(ff_native$tm_df)
  df_hm_p <- as.numeric(ff_patience$hm_df)
  df_hm_n <- as.numeric(ff_native$hm_df)
  
  if (is.na(df_ff_p)) df_ff_p <- n_obs - 7
  if (is.na(df_ff_n)) df_ff_n <- n_obs - 7
  if (is.na(df_tm_p)) df_tm_p <- n_obs - 3
  if (is.na(df_tm_n)) df_tm_n <- n_obs - 3
  if (is.na(df_hm_p)) df_hm_p <- n_obs - 3
  if (is.na(df_hm_n)) df_hm_n <- n_obs - 3
  
  # =========================================================================
  # TEST EACH HYPOTHESIS INDEPENDENTLY
  # =========================================================================
  
  # --- H_FF1: ff_alpha (TSF > Native) ---
  h_ff1 <- test_coef_directional(
    ff_patience$ff_alpha, ff_native$ff_alpha,
    ff_patience$ff_alpha_se, ff_native$ff_alpha_se,
    df_ff_p, df_ff_n, direction = "greater"
  )
  
  # --- H_FF2: ff_r_squared (TSF < Native) ---
  h_ff2 <- test_rsq_directional(
    ff_patience$ff_r_squared, ff_native$ff_r_squared,
    direction = "less"
  )
  
  # --- H_FF3: ff_beta_mkt (TSF < Native) ---
  h_ff3 <- test_coef_directional(
    ff_patience$ff_beta_mkt, ff_native$ff_beta_mkt,
    ff_patience$ff_beta_mkt_se, ff_native$ff_beta_mkt_se,
    df_ff_p, df_ff_n, direction = "less"
  )
  
  # --- H_FF4: ff_beta_smb (TSF ≈ Native) - CONTROL ---
  h_ff4 <- test_coef_equivalence(
    ff_patience$ff_beta_smb, ff_native$ff_beta_smb,
    ff_patience$ff_beta_smb_se, ff_native$ff_beta_smb_se,
    df_ff_p, df_ff_n
  )
  
  # --- H_FF5: ff_beta_hml (TSF ≈ Native) - CONTROL ---
  h_ff5 <- test_coef_equivalence(
    ff_patience$ff_beta_hml, ff_native$ff_beta_hml,
    ff_patience$ff_beta_hml_se, ff_native$ff_beta_hml_se,
    df_ff_p, df_ff_n
  )
  
  # --- H_FF6: ff_beta_rmw (TSF ≈ Native) - CONTROL ---
  h_ff6 <- test_coef_equivalence(
    ff_patience$ff_beta_rmw, ff_native$ff_beta_rmw,
    ff_patience$ff_beta_rmw_se, ff_native$ff_beta_rmw_se,
    df_ff_p, df_ff_n
  )
  
  # --- H_FF7: ff_beta_cma (TSF ≈ Native) - CONTROL ---
  h_ff7 <- test_coef_equivalence(
    ff_patience$ff_beta_cma, ff_native$ff_beta_cma,
    ff_patience$ff_beta_cma_se, ff_native$ff_beta_cma_se,
    df_ff_p, df_ff_n
  )
  
  # --- H_FF8: ff_beta_mom (TSF ≈ Native) - CONTROL ---
  h_ff8 <- test_coef_equivalence(
    ff_patience$ff_beta_mom, ff_native$ff_beta_mom,
    ff_patience$ff_beta_mom_se, ff_native$ff_beta_mom_se,
    df_ff_p, df_ff_n
  )
  
  # --- H_TM1: tm_alpha (TSF > Native) ---
  h_tm1 <- test_coef_directional(
    ff_patience$tm_alpha, ff_native$tm_alpha,
    ff_patience$tm_alpha_se, ff_native$tm_alpha_se,
    df_tm_p, df_tm_n, direction = "greater"
  )
  
  # --- H_TM2: tm_gamma (TSF > Native) ---
  h_tm2 <- test_coef_directional(
    ff_patience$tm_gamma, ff_native$tm_gamma,
    ff_patience$tm_gamma_se, ff_native$tm_gamma_se,
    df_tm_p, df_tm_n, direction = "greater"
  )
  
  # --- H_TM3: tm_r_squared (TSF < Native) ---
  h_tm3 <- test_rsq_directional(
    ff_patience$tm_r_squared, ff_native$tm_r_squared,
    direction = "less"
  )
  
  # --- H_TM4: tm_beta (TSF < Native) ---
  h_tm4 <- test_coef_directional(
    ff_patience$tm_beta, ff_native$tm_beta,
    ff_patience$tm_beta_se, ff_native$tm_beta_se,
    df_tm_p, df_tm_n, direction = "less"
  )
  
  # --- H_HM1: hm_alpha (TSF > Native) ---
  h_hm1 <- test_coef_directional(
    ff_patience$hm_alpha, ff_native$hm_alpha,
    ff_patience$hm_alpha_se, ff_native$hm_alpha_se,
    df_hm_p, df_hm_n, direction = "greater"
  )
  
  # --- H_HM2: hm_beta2 (TSF > Native) ---
  h_hm2 <- test_coef_directional(
    ff_patience$hm_beta2, ff_native$hm_beta2,
    ff_patience$hm_beta2_se, ff_native$hm_beta2_se,
    df_hm_p, df_hm_n, direction = "greater"
  )
  
  # --- H_HM3: hm_r_squared (TSF < Native) ---
  h_hm3 <- test_rsq_directional(
    ff_patience$hm_r_squared, ff_native$hm_r_squared,
    direction = "less"
  )
  
  # --- H_HM4: hm_beta1 (TSF < Native) ---
  h_hm4 <- test_coef_directional(
    ff_patience$hm_beta1, ff_native$hm_beta1,
    ff_patience$hm_beta1_se, ff_native$hm_beta1_se,
    df_hm_p, df_hm_n, direction = "less"
  )
  
  # =========================================================================
  # APPLY BENJAMINI-HOCHBERG CORRECTION AT INDIVIDUAL-COMPARISON LEVEL
  # Per preregistration: BH is part of confirmation criteria
  # =========================================================================
  
  # Collect TEST hypothesis p-values (8 tests with inferential statistics)
  test_p_raw <- c(
    H_FF1 = h_ff1$p_value,
    H_FF3 = h_ff3$p_value,
    H_TM1 = h_tm1$p_value,
    H_TM2 = h_tm2$p_value,
    H_TM4 = h_tm4$p_value,
    H_HM1 = h_hm1$p_value,
    H_HM2 = h_hm2$p_value,
    H_HM4 = h_hm4$p_value
  )
  
  # Apply BH correction to TEST p-values
  test_p_bh <- p.adjust(test_p_raw, method = "BH")
  
  # Collect CONTROL hypothesis TOST p-values (5 equivalence tests)
  control_tost_raw <- c(
    H_FF4 = h_ff4$tost_p,
    H_FF5 = h_ff5$tost_p,
    H_FF6 = h_ff6$tost_p,
    H_FF7 = h_ff7$tost_p,
    H_FF8 = h_ff8$tost_p
  )
  
  # Apply BH correction to CONTROL TOST p-values
  control_tost_bh <- p.adjust(control_tost_raw, method = "BH")
  
  # =========================================================================
  # RETURN RESULTS
  # PRIMARY: HAC t-test with BH-adjusted p-values
  # SECONDARY: Binomial, Wilcoxon (computed at aggregate level)
  # DESCRIPTIVE: R² differences (no SE available for inference)
  # =========================================================================
  
  data.table(
    strategy = strat_name,
    period = period_name,
    position_size = pos_name,
    n_observations = n_obs,
    
    # H_FF1: ff_alpha (TSF > Native) - TEST (HAC PRIMARY)
    H_FF1_diff = h_ff1$diff,
    H_FF1_se = h_ff1$se,
    H_FF1_t = h_ff1$t_stat,
    H_FF1_p = h_ff1$p_value,
    H_FF1_p_bh = test_p_bh["H_FF1"],
    H_FF1_ci_lo = h_ff1$ci_lower,
    H_FF1_ci_hi = h_ff1$ci_upper,
    H_FF1_improved = h_ff1$improved,
    
    # H_FF2: ff_r_squared (TSF < Native) - DESCRIPTIVE ONLY (no SE)
    H_FF2_diff_desc = h_ff2$diff,
    H_FF2_improved_desc = h_ff2$improved,
    
    # H_FF3: ff_beta_mkt (TSF < Native) - TEST (HAC PRIMARY)
    H_FF3_diff = h_ff3$diff,
    H_FF3_se = h_ff3$se,
    H_FF3_t = h_ff3$t_stat,
    H_FF3_p = h_ff3$p_value,
    H_FF3_p_bh = test_p_bh["H_FF3"],
    H_FF3_ci_lo = h_ff3$ci_lower,
    H_FF3_ci_hi = h_ff3$ci_upper,
    H_FF3_improved = h_ff3$improved,
    
    # H_FF4: ff_beta_smb (TSF ≈ Native) - CONTROL (TOST)
    H_FF4_diff = h_ff4$diff,
    H_FF4_se = h_ff4$se,
    H_FF4_tost_p = h_ff4$tost_p,
    H_FF4_tost_p_bh = control_tost_bh["H_FF4"],
    H_FF4_ci_lo = h_ff4$ci_lower,
    H_FF4_ci_hi = h_ff4$ci_upper,
    H_FF4_ci_inc_zero = h_ff4$ci_includes_zero,
    
    # H_FF5: ff_beta_hml (TSF ≈ Native) - CONTROL (TOST)
    H_FF5_diff = h_ff5$diff,
    H_FF5_se = h_ff5$se,
    H_FF5_tost_p = h_ff5$tost_p,
    H_FF5_tost_p_bh = control_tost_bh["H_FF5"],
    H_FF5_ci_lo = h_ff5$ci_lower,
    H_FF5_ci_hi = h_ff5$ci_upper,
    H_FF5_ci_inc_zero = h_ff5$ci_includes_zero,
    
    # H_FF6: ff_beta_rmw (TSF ≈ Native) - CONTROL (TOST)
    H_FF6_diff = h_ff6$diff,
    H_FF6_se = h_ff6$se,
    H_FF6_tost_p = h_ff6$tost_p,
    H_FF6_tost_p_bh = control_tost_bh["H_FF6"],
    H_FF6_ci_lo = h_ff6$ci_lower,
    H_FF6_ci_hi = h_ff6$ci_upper,
    H_FF6_ci_inc_zero = h_ff6$ci_includes_zero,
    
    # H_FF7: ff_beta_cma (TSF ≈ Native) - CONTROL (TOST)
    H_FF7_diff = h_ff7$diff,
    H_FF7_se = h_ff7$se,
    H_FF7_tost_p = h_ff7$tost_p,
    H_FF7_tost_p_bh = control_tost_bh["H_FF7"],
    H_FF7_ci_lo = h_ff7$ci_lower,
    H_FF7_ci_hi = h_ff7$ci_upper,
    H_FF7_ci_inc_zero = h_ff7$ci_includes_zero,
    
    # H_FF8: ff_beta_mom (TSF ≈ Native) - CONTROL (TOST)
    H_FF8_diff = h_ff8$diff,
    H_FF8_se = h_ff8$se,
    H_FF8_tost_p = h_ff8$tost_p,
    H_FF8_tost_p_bh = control_tost_bh["H_FF8"],
    H_FF8_ci_lo = h_ff8$ci_lower,
    H_FF8_ci_hi = h_ff8$ci_upper,
    H_FF8_ci_inc_zero = h_ff8$ci_includes_zero,
    
    # H_TM1: tm_alpha (TSF > Native) - TEST (HAC PRIMARY)
    H_TM1_diff = h_tm1$diff,
    H_TM1_se = h_tm1$se,
    H_TM1_t = h_tm1$t_stat,
    H_TM1_p = h_tm1$p_value,
    H_TM1_p_bh = test_p_bh["H_TM1"],
    H_TM1_ci_lo = h_tm1$ci_lower,
    H_TM1_ci_hi = h_tm1$ci_upper,
    H_TM1_improved = h_tm1$improved,
    
    # H_TM2: tm_gamma (TSF > Native) - TEST (HAC PRIMARY)
    H_TM2_diff = h_tm2$diff,
    H_TM2_se = h_tm2$se,
    H_TM2_t = h_tm2$t_stat,
    H_TM2_p = h_tm2$p_value,
    H_TM2_p_bh = test_p_bh["H_TM2"],
    H_TM2_ci_lo = h_tm2$ci_lower,
    H_TM2_ci_hi = h_tm2$ci_upper,
    H_TM2_improved = h_tm2$improved,
    
    # H_TM3: tm_r_squared (TSF < Native) - DESCRIPTIVE ONLY (no SE)
    H_TM3_diff_desc = h_tm3$diff,
    H_TM3_improved_desc = h_tm3$improved,
    
    # H_TM4: tm_beta (TSF < Native) - TEST (HAC PRIMARY)
    H_TM4_diff = h_tm4$diff,
    H_TM4_se = h_tm4$se,
    H_TM4_t = h_tm4$t_stat,
    H_TM4_p = h_tm4$p_value,
    H_TM4_p_bh = test_p_bh["H_TM4"],
    H_TM4_ci_lo = h_tm4$ci_lower,
    H_TM4_ci_hi = h_tm4$ci_upper,
    H_TM4_improved = h_tm4$improved,
    
    # H_HM1: hm_alpha (TSF > Native) - TEST (HAC PRIMARY)
    H_HM1_diff = h_hm1$diff,
    H_HM1_se = h_hm1$se,
    H_HM1_t = h_hm1$t_stat,
    H_HM1_p = h_hm1$p_value,
    H_HM1_p_bh = test_p_bh["H_HM1"],
    H_HM1_ci_lo = h_hm1$ci_lower,
    H_HM1_ci_hi = h_hm1$ci_upper,
    H_HM1_improved = h_hm1$improved,
    
    # H_HM2: hm_beta2 (TSF > Native) - TEST (HAC PRIMARY)
    H_HM2_diff = h_hm2$diff,
    H_HM2_se = h_hm2$se,
    H_HM2_t = h_hm2$t_stat,
    H_HM2_p = h_hm2$p_value,
    H_HM2_p_bh = test_p_bh["H_HM2"],
    H_HM2_ci_lo = h_hm2$ci_lower,
    H_HM2_ci_hi = h_hm2$ci_upper,
    H_HM2_improved = h_hm2$improved,
    
    # H_HM3: hm_r_squared (TSF < Native) - DESCRIPTIVE ONLY (no SE)
    H_HM3_diff_desc = h_hm3$diff,
    H_HM3_improved_desc = h_hm3$improved,
    
    # H_HM4: hm_beta1 (TSF < Native) - TEST (HAC PRIMARY)
    H_HM4_diff = h_hm4$diff,
    H_HM4_se = h_hm4$se,
    H_HM4_t = h_hm4$t_stat,
    H_HM4_p = h_hm4$p_value,
    H_HM4_p_bh = test_p_bh["H_HM4"],
    H_HM4_ci_lo = h_hm4$ci_lower,
    H_HM4_ci_hi = h_hm4$ci_upper,
    H_HM4_improved = h_hm4$improved
  )
}

# ==============================================================================
# AGGREGATE TESTS ACROSS COMPARISONS (Wilcoxon, Binomial, Cohen's d, BH)
# ==============================================================================

run_aggregate_tests <- function(all_comparisons, hypothesis_col, improved_col = NULL) {
  result <- list(
    n = NA_integer_,
    mean_diff = NA_real_,
    wilcoxon_p = NA_real_,
    binomial_k = NA_integer_,
    binomial_n = NA_integer_,
    binomial_p = NA_real_,
    cohens_d = NA_real_,
    improvement_rate = NA_real_
  )
  
  diffs <- all_comparisons[[hypothesis_col]]
  diffs <- diffs[is.finite(diffs)]
  n <- length(diffs)
  
  if (n < 2) return(result)
  
  result$n <- n
  result$mean_diff <- mean(diffs)
  
  tryCatch({
    wt <- wilcox.test(diffs, mu = 0, alternative = "two.sided", exact = FALSE)
    result$wilcoxon_p <- wt$p.value
  }, error = function(e) {})
  
  tryCatch({
    result$cohens_d <- mean(diffs) / sd(diffs)
  }, error = function(e) {})
  
  if (!is.null(improved_col) && improved_col %in% names(all_comparisons)) {
    improved <- all_comparisons[[improved_col]]
    improved <- improved[!is.na(improved)]
    n_improved <- sum(improved == TRUE)
    n_total <- length(improved)
    
    if (n_total > 0) {
      result$binomial_k <- n_improved
      result$binomial_n <- n_total
      result$improvement_rate <- n_improved / n_total
      
      tryCatch({
        result$binomial_p <- binom.test(n_improved, n_total, p = 0.5, 
                                        alternative = "greater")$p.value
      }, error = function(e) {})
    }
  }
  
  return(result)
}

# ==============================================================================
# PHASE 3: PROCESS ALL CONFIGURATIONS (NET ONLY)
# ==============================================================================

cat("\n================================================================================\n")
cat("PHASE 3: Process all configurations (NET ONLY)\n")
cat("================================================================================\n")

OUTPUT_DIR <- paste0(UNIVERSE, "_variable_benchmark_results")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

NET_DIR <- file.path(OUTPUT_DIR, "NET")
DAILY_RETURNS_DIR <- file.path(OUTPUT_DIR, "daily_returns")
COMPARISONS_DIR <- file.path(OUTPUT_DIR, "comparisons")
HYPOTHESIS_DIR <- file.path(OUTPUT_DIR, "hypothesis_tests")

if (!dir.exists(NET_DIR)) dir.create(NET_DIR, recursive = TRUE)
if (!dir.exists(DAILY_RETURNS_DIR)) dir.create(DAILY_RETURNS_DIR, recursive = TRUE)
if (!dir.exists(COMPARISONS_DIR)) dir.create(COMPARISONS_DIR, recursive = TRUE)
if (!dir.exists(HYPOTHESIS_DIR)) dir.create(HYPOTHESIS_DIR, recursive = TRUE)

all_comparisons <- list()
all_hypothesis_tests <- list()

for (pos_cfg in POSITION_SIZES) {
  pos_name <- pos_cfg$name
  pos_pct <- pos_cfg$pct
  
  cat(sprintf("\n=== POSITION SIZE: %s (%.0f%%) ===\n", pos_name, pos_pct * 100))
  
  results_net_by_period <- list()
  comparisons_by_period <- list()
  hypothesis_by_period <- list()
  
  for (period in PERIODS) {
    per_name <- period$name
    per_label <- period$label
    
    cat(sprintf("\n  PERIOD: %s (%s)\n", per_name, per_label))
    
    period_data <- full_data[date >= period$start & date <= period$end]
    period_dates <- all_dates[all_dates >= period$start & all_dates <= period$end]
    n_years <- length(period_dates) / 252
    
    bh <- calculate_buy_and_hold(period_data)
    cat(sprintf("    B&H: Net=%.2f%%\n", bh$net_return * 100))
    
    period_results_net <- list()
    equity_curves_native <- list()
    equity_curves_patience <- list()
    ff_results_native <- list()
    ff_results_patience <- list()
    
    for (strat in STRATEGIES) {
      strat_name <- strat$name
      refit_type <- strat$refit
      
      for (exit_type in c("native", "patience")) {
        cache_key <- paste0(strat_name, "_", exit_type)
        all_trades <- trades_cache[[cache_key]]
        
        if (is.null(all_trades) || nrow(all_trades) == 0) next
        
        period_trades <- all_trades[entry_date >= period$start & entry_date <= period$end]
        if (nrow(period_trades) == 0) next
        
        realized <- apply_capital_constraints(period_trades, pos_pct)
        if (nrow(realized) == 0) next
        
        equity_dt <- build_equity_curve_vectorized(realized, full_data, all_dates, period)
        
        # Save daily returns
        if (nrow(equity_dt) > 1) {
          daily_ret <- data.table(
            date = equity_dt$date[-1],
            portfolio_value = equity_dt$portfolio_value[-1],
            daily_return = diff(equity_dt$portfolio_value) / head(equity_dt$portfolio_value, -1)
          )
          fwrite(daily_ret, file.path(DAILY_RETURNS_DIR, 
                 sprintf("%s_%s_%s_%s_%s.csv", UNIVERSE, strat_name, exit_type, pos_name, per_name)))
          
          if (exit_type == "native") {
            equity_curves_native[[strat_name]] <- daily_ret
          } else {
            equity_curves_patience[[strat_name]] <- daily_ret
          }
        }
        
        # NET ONLY
        metrics_n <- compute_metrics(strat_name, refit_type, exit_type, pos_name, realized, equity_dt, n_years, bh$gross_return, bh$net_return, FALSE, ff_factors)
        
        period_results_net[[cache_key]] <- metrics_n
        
        if (exit_type == "native") {
          ff_results_native[[strat_name]] <- metrics_n
        } else {
          ff_results_patience[[strat_name]] <- metrics_n
        }
      }
    }
    
    results_net_by_period[[per_name]] <- rbindlist(period_results_net, fill = TRUE)
    
    # Run comparisons (20 Appendix D tests) for NATIVE vs PATIENCE
    cat("  Running statistical comparisons (20 Appendix D tests)...\n")
    period_comparisons <- list()
    
    for (strat_name in names(equity_curves_native)) {
      if (strat_name %in% names(equity_curves_patience)) {
        ret_native <- equity_curves_native[[strat_name]]
        ret_patience <- equity_curves_patience[[strat_name]]
        ff_native <- ff_results_native[[strat_name]]
        ff_patience <- ff_results_patience[[strat_name]]
        
        comp_row <- run_all_20_tests(strat_name, per_name, pos_name,
                                     ret_patience, ret_native,
                                     ff_patience, ff_native)
        period_comparisons[[strat_name]] <- comp_row
      }
    }
    
    if (length(period_comparisons) > 0) {
      comparisons_by_period[[per_name]] <- rbindlist(period_comparisons, fill = TRUE)
    }
    
    # Run 16 hypothesis tests
    cat("  Running 16 hypothesis tests (H_FF1-8, H_TM1-4, H_HM1-4)...\n")
    period_hypothesis <- list()
    
    for (strat_name in names(equity_curves_native)) {
      if (strat_name %in% names(equity_curves_patience)) {
        ret_native <- equity_curves_native[[strat_name]]
        ret_patience <- equity_curves_patience[[strat_name]]
        ff_native <- ff_results_native[[strat_name]]
        ff_patience <- ff_results_patience[[strat_name]]
        
        hyp_row <- run_16_hypothesis_tests(strat_name, per_name, pos_name,
                                           ret_patience, ret_native,
                                           ff_patience, ff_native)
        period_hypothesis[[strat_name]] <- hyp_row
      }
    }
    
    if (length(period_hypothesis) > 0) {
      hypothesis_by_period[[per_name]] <- rbindlist(period_hypothesis, fill = TRUE)
    }
  }
  
  net_file <- file.path(NET_DIR, paste0(UNIVERSE, "_variable_benchmark_", pos_name, "_NET.xlsx"))
  write_formatted_excel(results_net_by_period, net_file)
  
  comp_file <- file.path(COMPARISONS_DIR, paste0(UNIVERSE, "_variable_benchmark_", pos_name, "_comparisons.xlsx"))
  write_formatted_excel(comparisons_by_period, comp_file)
  
  hyp_file <- file.path(HYPOTHESIS_DIR, paste0(UNIVERSE, "_variable_benchmark_", pos_name, "_hypothesis_tests.xlsx"))
  write_formatted_excel(hypothesis_by_period, hyp_file)
  
  cat(sprintf("\n  Saved: %s\n", net_file))
  cat(sprintf("  Saved: %s\n", comp_file))
  cat(sprintf("  Saved: %s\n", hyp_file))
  
  for (per_name in names(comparisons_by_period)) {
    key <- paste0(pos_name, "_", per_name)
    all_comparisons[[key]] <- comparisons_by_period[[per_name]]
    all_hypothesis_tests[[key]] <- hypothesis_by_period[[per_name]]
  }
}

# Save consolidated comparisons
if (length(all_comparisons) > 0) {
  consolidated_comp_file <- file.path(OUTPUT_DIR, paste0(UNIVERSE, "_variable_benchmark_ALL_comparisons.xlsx"))
  write_formatted_excel(all_comparisons, consolidated_comp_file)
  cat(sprintf("\nSaved consolidated comparisons: %s\n", consolidated_comp_file))
}

# Save consolidated hypothesis tests
if (length(all_hypothesis_tests) > 0) {
  consolidated_hyp_file <- file.path(OUTPUT_DIR, paste0(UNIVERSE, "_variable_benchmark_ALL_hypothesis_tests.xlsx"))
  write_formatted_excel(all_hypothesis_tests, consolidated_hyp_file)
  cat(sprintf("Saved consolidated hypothesis tests: %s\n", consolidated_hyp_file))
}

# ==============================================================================
# AGGREGATE HYPOTHESIS TESTS 
# PRIMARY: HAC t-test with BH-adjusted p-values
# SECONDARY: Binomial, Wilcoxon, Cohen's d
# DESCRIPTIVE: R² differences (H_FF2, H_TM3, H_HM3)
# ==============================================================================

cat("\n================================================================================\n")
cat("Running Aggregate Hypothesis Tests (HAC PRIMARY)...\n")
cat("================================================================================\n")

all_hyp_combined <- rbindlist(all_hypothesis_tests, fill = TRUE)

if (nrow(all_hyp_combined) > 0) {
  
  # TEST hypotheses with HAC t-test (PRIMARY)
  # Note: H_FF2, H_TM3, H_HM3 are R² (descriptive only - no inferential test)
  test_hypotheses <- list(
    H_FF1 = list(diff = "H_FF1_diff", p = "H_FF1_p", p_bh = "H_FF1_p_bh", improved = "H_FF1_improved", direction = "greater", has_hac = TRUE),
    H_FF2 = list(diff = "H_FF2_diff_desc", improved = "H_FF2_improved_desc", direction = "less", has_hac = FALSE),
    H_FF3 = list(diff = "H_FF3_diff", p = "H_FF3_p", p_bh = "H_FF3_p_bh", improved = "H_FF3_improved", direction = "less", has_hac = TRUE),
    H_TM1 = list(diff = "H_TM1_diff", p = "H_TM1_p", p_bh = "H_TM1_p_bh", improved = "H_TM1_improved", direction = "greater", has_hac = TRUE),
    H_TM2 = list(diff = "H_TM2_diff", p = "H_TM2_p", p_bh = "H_TM2_p_bh", improved = "H_TM2_improved", direction = "greater", has_hac = TRUE),
    H_TM3 = list(diff = "H_TM3_diff_desc", improved = "H_TM3_improved_desc", direction = "less", has_hac = FALSE),
    H_TM4 = list(diff = "H_TM4_diff", p = "H_TM4_p", p_bh = "H_TM4_p_bh", improved = "H_TM4_improved", direction = "less", has_hac = TRUE),
    H_HM1 = list(diff = "H_HM1_diff", p = "H_HM1_p", p_bh = "H_HM1_p_bh", improved = "H_HM1_improved", direction = "greater", has_hac = TRUE),
    H_HM2 = list(diff = "H_HM2_diff", p = "H_HM2_p", p_bh = "H_HM2_p_bh", improved = "H_HM2_improved", direction = "greater", has_hac = TRUE),
    H_HM3 = list(diff = "H_HM3_diff_desc", improved = "H_HM3_improved_desc", direction = "less", has_hac = FALSE),
    H_HM4 = list(diff = "H_HM4_diff", p = "H_HM4_p", p_bh = "H_HM4_p_bh", improved = "H_HM4_improved", direction = "less", has_hac = TRUE)
  )
  
  # CONTROL hypotheses (TOST equivalence)
  control_hypotheses <- list(
    H_FF4 = list(diff = "H_FF4_diff", tost_p = "H_FF4_tost_p", tost_p_bh = "H_FF4_tost_p_bh", ci_inc_zero = "H_FF4_ci_inc_zero"),
    H_FF5 = list(diff = "H_FF5_diff", tost_p = "H_FF5_tost_p", tost_p_bh = "H_FF5_tost_p_bh", ci_inc_zero = "H_FF5_ci_inc_zero"),
    H_FF6 = list(diff = "H_FF6_diff", tost_p = "H_FF6_tost_p", tost_p_bh = "H_FF6_tost_p_bh", ci_inc_zero = "H_FF6_ci_inc_zero"),
    H_FF7 = list(diff = "H_FF7_diff", tost_p = "H_FF7_tost_p", tost_p_bh = "H_FF7_tost_p_bh", ci_inc_zero = "H_FF7_ci_inc_zero"),
    H_FF8 = list(diff = "H_FF8_diff", tost_p = "H_FF8_tost_p", tost_p_bh = "H_FF8_tost_p_bh", ci_inc_zero = "H_FF8_ci_inc_zero")
  )
  
  agg_test_results <- list()
  
  cat("\n--- TEST HYPOTHESES (HAC t-test PRIMARY) ---\n")
  
  for (hyp_name in names(test_hypotheses)) {
    hyp <- test_hypotheses[[hyp_name]]
    
    diffs <- all_hyp_combined[[hyp$diff]]
    diffs <- diffs[is.finite(diffs)]
    n <- length(diffs)
    
    # PRIMARY: HAC t-test results (BH-adjusted)
    hac_sig_raw <- NA_integer_
    hac_sig_bh <- NA_integer_
    median_p_raw <- NA_real_
    median_p_bh <- NA_real_
    
    if (hyp$has_hac) {
      p_raw <- all_hyp_combined[[hyp$p]]
      p_bh <- all_hyp_combined[[hyp$p_bh]]
      p_raw <- p_raw[is.finite(p_raw)]
      p_bh <- p_bh[is.finite(p_bh)]
      
      hac_sig_raw <- sum(p_raw < 0.05, na.rm = TRUE)
      hac_sig_bh <- sum(p_bh < 0.05, na.rm = TRUE)
      median_p_raw <- median(p_raw, na.rm = TRUE)
      median_p_bh <- median(p_bh, na.rm = TRUE)
    }
    
    # SECONDARY: Aggregate statistics
    agg <- run_aggregate_tests(all_hyp_combined, hyp$diff, hyp$improved)
    
    wilcox_directional_p <- NA_real_
    tryCatch({
      alt <- if (hyp$direction == "greater") "greater" else "less"
      wt <- wilcox.test(diffs, mu = 0, alternative = alt, exact = FALSE)
      wilcox_directional_p <- wt$p.value
    }, error = function(e) {})
    
    agg_test_results[[hyp_name]] <- data.table(
      hypothesis = hyp_name,
      type = if (hyp$has_hac) "TEST" else "DESCRIPTIVE",
      n_comparisons = n,
      mean_diff = agg$mean_diff,
      # PRIMARY: HAC results
      hac_sig_raw = hac_sig_raw,
      hac_sig_bh = hac_sig_bh,
      median_p_raw = median_p_raw,
      median_p_bh = median_p_bh,
      # SECONDARY: Aggregate
      wilcoxon_p = wilcox_directional_p,
      binomial_k = agg$binomial_k,
      binomial_n = agg$binomial_n,
      binomial_p = agg$binomial_p,
      cohens_d = agg$cohens_d,
      improvement_rate = agg$improvement_rate
    )
    
    if (hyp$has_hac) {
      cat(sprintf("  %s: n=%d, HAC_sig_BH=%d/%d, median_p_BH=%.4g, improvement=%.1f%%\n",
                  hyp_name, n, hac_sig_bh, n, median_p_bh, agg$improvement_rate * 100))
    } else {
      cat(sprintf("  %s (DESCRIPTIVE): n=%d, improvement=%.1f%% (no inferential test)\n",
                  hyp_name, n, agg$improvement_rate * 100))
    }
  }
  
  cat("\n--- CONTROL HYPOTHESES (TOST equivalence) ---\n")
  
  agg_control_results <- list()
  
  for (hyp_name in names(control_hypotheses)) {
    hyp <- control_hypotheses[[hyp_name]]
    
    diffs <- all_hyp_combined[[hyp$diff]]
    diffs <- diffs[is.finite(diffs)]
    n <- length(diffs)
    
    tost_ps <- all_hyp_combined[[hyp$tost_p]]
    tost_ps_bh <- all_hyp_combined[[hyp$tost_p_bh]]
    ci_zeros <- all_hyp_combined[[hyp$ci_inc_zero]]
    
    # Count how many pass equivalence (TOST p < 0.05 AND CI includes zero)
    n_equiv_raw <- sum(tost_ps < 0.05 & ci_zeros == TRUE, na.rm = TRUE)
    n_equiv_bh <- sum(tost_ps_bh < 0.05 & ci_zeros == TRUE, na.rm = TRUE)
    
    agg_control_results[[hyp_name]] <- data.table(
      hypothesis = hyp_name,
      type = "CONTROL",
      n_comparisons = n,
      mean_diff = mean(diffs, na.rm = TRUE),
      n_equivalent_raw = n_equiv_raw,
      n_equivalent_bh = n_equiv_bh,
      pct_equivalent_raw = n_equiv_raw / n,
      pct_equivalent_bh = n_equiv_bh / n,
      median_tost_p_raw = median(tost_ps, na.rm = TRUE),
      median_tost_p_bh = median(tost_ps_bh, na.rm = TRUE)
    )
    
    cat(sprintf("  %s: n=%d, mean_diff=%.4f, equiv_BH=%d/%d (%.1f%%)\n",
                hyp_name, n, mean(diffs, na.rm = TRUE), n_equiv_bh, n, (n_equiv_bh / n) * 100))
  }
  
  agg_test_dt <- rbindlist(agg_test_results, fill = TRUE)
  agg_control_dt <- rbindlist(agg_control_results, fill = TRUE)
  
  agg_file <- file.path(OUTPUT_DIR, paste0(UNIVERSE, "_AGGREGATE_HYPOTHESIS_TESTS.xlsx"))
  wb_agg <- createWorkbook()
  addWorksheet(wb_agg, "TEST_Hypotheses")
  writeData(wb_agg, "TEST_Hypotheses", agg_test_dt)
  addWorksheet(wb_agg, "CONTROL_Hypotheses")
  writeData(wb_agg, "CONTROL_Hypotheses", agg_control_dt)
  saveWorkbook(wb_agg, agg_file, overwrite = TRUE)
  
  cat(sprintf("\nSaved aggregate tests: %s\n", agg_file))
}

# FF/HM/TM summary
ff_hm_tm_cols <- c("strategy", "exit_strategy", 
                "ff_alpha", "ff_alpha_t", "ff_alpha_p", "ff_r_squared", 
                "ff_beta_mkt", "ff_beta_smb", "ff_beta_hml", "ff_beta_rmw", "ff_beta_cma", "ff_beta_mom",
                "hm_alpha", "hm_alpha_t", "hm_alpha_p", "hm_beta1", "hm_beta2", "hm_beta2_t", "hm_beta2_p", "hm_r_squared",
                "tm_alpha", "tm_alpha_t", "tm_alpha_p", "tm_beta", "tm_gamma", "tm_gamma_t", "tm_gamma_p", "tm_r_squared")

# Collect all NET results for FF/HM/TM summary
ff_hm_tm_summary <- list()
for (pos_cfg in POSITION_SIZES) {
  pos_name <- pos_cfg$name
  net_file <- file.path(NET_DIR, paste0(UNIVERSE, "_variable_benchmark_", pos_name, "_NET.xlsx"))
  if (file.exists(net_file)) {
    wb <- loadWorkbook(net_file)
    for (sheet_name in names(wb)) {
      dt <- as.data.table(read.xlsx(net_file, sheet = sheet_name))
      if (nrow(dt) > 0) {
        cols_present <- intersect(ff_hm_tm_cols, names(dt))
        key <- paste0(pos_name, "_", sheet_name)
        ff_hm_tm_summary[[key]] <- dt[, ..cols_present]
      }
    }
  }
}

if (length(ff_hm_tm_summary) > 0) {
  ff_hm_tm_file <- file.path(OUTPUT_DIR, paste0(UNIVERSE, "_variable_FF_HM_TM_SUMMARY.xlsx"))
  write_formatted_excel(ff_hm_tm_summary, ff_hm_tm_file)
  cat(sprintf("\nSaved FF/HM/TM summary: %s\n", ff_hm_tm_file))
}

cat(sprintf("\nDaily returns saved to: %s/\n", DAILY_RETURNS_DIR))

cat("\n================================================================================\n")
cat("COMPLETE\n")
cat("================================================================================\n")
