################################################################################
# BENCHMARK BACKTEST - SCHEDULED EXPOSURE STRATEGIES
# File: benchmark_scheduled_exposure_v3_1.R
# Version: 3.1
# Date: 2025-01-20
#
# PURPOSE: Benchmark strategies with scheduled/full exposure profiles for
#          EXIT TIMING validation (Study 3a). Calendar-based strategies with
#          full capital deployment during scheduled windows.
#
# STRATEGIES: 7 calendar-based benchmarks + 3 fixed-hold random benchmarks
#
# EXPOSURE: FULL - entire available capital deployed at entry across universe
#
# CHANGES from v2_08:
#   - FIX: Apply BH correction at individual-comparison level (not just aggregate)
#   - FIX: HAC t-test is PRIMARY confirmation test (BH-adjusted)
#   - FIX: R² tests marked as DESCRIPTIVE-ONLY (no inferential test available)
#   - Added _p_bh columns for all TEST hypothesis p-values
#   - Added _tost_p_bh columns for all CONTROL hypothesis TOST p-values
#
# CHANGES from v2_07:
#   - CRITICAL: All regressions now use HAC (Newey-West) standard errors
#     as specified in preregistration. Previous versions used OLS SEs which
#     were too small, making TOST tests too strict, causing controls to fail.
#   - CRITICAL: Calendar strategies now use MATCHED entries for patience
#   - CRITICAL: Random strategies use entries that survived native filtering
#   - CRITICAL: NULL handling in test_coef_directional/equivalence fixed
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
cat("BENCHMARK BACKTEST - SCHEDULED EXPOSURE V3.1 (BH @ INDIVIDUAL, HAC PRIMARY)\n")
cat("================================================================================\n\n")


# CONFIGURATION
# ==============================================================================

UNIVERSE <- "COMMUNICATION_SERVICES"
INPUT_FILE <- "communication_services_benchmark.csv"
FF_5_FACTOR_FILE <- "F-F_Research_Data_5_Factors_2x3_daily.csv"
FF_MOMENTUM_FILE <- "F-F_Momentum_Factor_daily.csv"
CACHE_DIR <- "communication_services_scheduled_cache"

INITIAL_CAPITAL <- 100000
TRANSACTION_COST_PCT <- 0.001
RISK_FREE_RATE <- 0.05
NET_MULTIPLIER <- (1 - TRANSACTION_COST_PCT)^2
MAX_HOLD_DAYS <- 365

PATIENCE_DAYS <- c(0, 60, 90, 120, 180)
PATIENCE_TARGETS <- c(0.15, 0.10, 0.05, 0.02, 0.00)

get_patience_target <- function(days_held) {
  idx <- findInterval(days_held, PATIENCE_DAYS)
  if (idx < 1) idx <- 1
  if (idx > length(PATIENCE_TARGETS)) idx <- length(PATIENCE_TARGETS)
  PATIENCE_TARGETS[idx]
}

RANDOM_HOLD_21 <- 21
RANDOM_HOLD_63 <- 63
RANDOM_HOLD_126 <- 126
RANDOM_ITERATIONS <- 100

PERIODS <- list(
  list(name = "2006-2015", label = "GFC-Era",      start = as.Date("2006-01-01"), end = as.Date("2015-12-31")),
  list(name = "2016-2025", label = "Post-GFC",     start = as.Date("2016-01-01"), end = as.Date("2025-12-31")),
  list(name = "2006-2025", label = "FULL",         start = as.Date("2006-01-01"), end = as.Date("2025-12-31")),
  list(name = "2023-2025", label = "Present-Bull", start = as.Date("2023-01-01"), end = as.Date("2025-12-31"))
)

CALENDAR_STRATEGIES <- list(
  # Calendar-based benchmarks (7) - scheduled exposure
  list(name = "sell_in_may", entry = "entry_sell_in_may", exit_type = "sell_in_may", refit = "calendar"),
  list(name = "january", entry = "entry_january", exit_type = "january", refit = "calendar"),
  list(name = "turn_of_month", entry = "entry_turn_of_month", exit_type = "turn_of_month", refit = "calendar"),
  list(name = "sept_avoid", entry = "entry_sept_avoid", exit_type = "sept_avoid", refit = "calendar"),
  list(name = "end_of_quarter", entry = "entry_end_of_quarter", exit_type = "end_of_quarter", refit = "calendar"),
  list(name = "weekend", entry = "entry_weekend", exit_type = "weekend", refit = "calendar"),
  list(name = "friday", entry = "entry_friday", exit_type = "friday", refit = "calendar")
)

RANDOM_STRATEGIES <- list(
  # Fixed-hold random benchmarks (3) - scheduled exposure null
  list(name = "random_21d", hold_days = 21, refit = "random"),
  list(name = "random_63d", hold_days = 63, refit = "random"),
  list(name = "random_126d", hold_days = 126, refit = "random")
)

# ==============================================================================
# FAMA-FRENCH FACTOR LOADING
# ==============================================================================

load_ff_factors <- function(ff5_file, mom_file) {
  cat("Loading Fama-French factors...\n")
  
  # Read 5-factor file - find data start
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
  
  # Filter valid dates and convert
  ff5 <- ff5[!is.na(date) & nchar(trimws(as.character(date))) == 8]
  ff5[, date := as.Date(trimws(as.character(date)), format = "%Y%m%d")]
  ff5 <- ff5[!is.na(date)]
  
  # Convert to decimals (data is in percentages)
  num_cols <- setdiff(names(ff5), "date")
  for (col in num_cols) {
    ff5[, (col) := as.numeric(get(col)) / 100]
  }
  
  # Read momentum file
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
  
  # Merge
  ff <- merge(ff5, mom, by = "date", all.x = TRUE)
  setkey(ff, date)
  
  cat(sprintf("  Loaded %d days of FF factors (%s to %s)\n", 
              nrow(ff), min(ff$date), max(ff$date)))
  return(ff)
}

# ==============================================================================
# FAMA-FRENCH REGRESSION
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
# HENRIKSSON-MERTON MARKET TIMING TEST (1981)
# ==============================================================================
# Model: R_strategy - R_f = α + β₁(R_m - R_f) + β₂(R_m - R_f)⁺ + ε
# Where (R_m - R_f)⁺ = max(0, market excess return)
#
# Interpretation:
#   β₁ = exposure in DOWN markets
#   β₁ + β₂ = exposure in UP markets
#   β₂ > 0 and significant = TIMING SKILL
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
  
  # Asymmetric market term: max(0, MKT_RF)
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
# TREYNOR-MAZUY MARKET TIMING TEST (1966)
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

# CALENDAR DATE GENERATION
# ==============================================================================

generate_calendar_dates <- function(all_dates, start_date, end_date) {
  period_dates <- all_dates[all_dates >= start_date & all_dates <= end_date]
  dt <- data.table(date = period_dates)
  
  dt[, `:=`(
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    day = as.integer(format(date, "%d")),
    dow = weekdays(date)
  )]
  
  dt[, trading_day_of_month := seq_len(.N), by = .(year, month)]
  dt[, is_last_of_month := date == max(date), by = .(year, month)]
  dt[, quarter := ceiling(month / 3)]
  dt[, is_last_of_quarter := date == max(date), by = .(year, quarter)]
  
  dt[, days_to_quarter_end := {
    last_day <- max(date)
    idx <- which(date == last_day)
    result <- rep(NA_integer_, .N)
    for (i in seq_len(.N)) result[i] <- idx - i
    result
  }, by = .(year, quarter)]
  
  return(dt)
}

generate_calendar_entries <- function(calendar_dt, all_dates) {
  dt <- copy(calendar_dt)
  dt[, entry_sell_in_may := (month == 11 & trading_day_of_month == 1)]
  dt[, entry_january := (month == 1 & trading_day_of_month == 1)]
  dt[, entry_turn_of_month := is_last_of_month]
  dt[, entry_sept_avoid := (month == 10 & trading_day_of_month == 1)]
  dt[, entry_end_of_quarter := (days_to_quarter_end == 5)]
  dt[, entry_weekend := (dow == "Monday")]
  dt[, entry_friday := (dow == "Thursday")]
  return(dt)
}

precompute_calendar_exits <- function(calendar_dt, all_dates) {
  cat("Precomputing calendar exit signals...\n")
  exits <- list()
  dt <- copy(calendar_dt)
  
  exits$sell_in_may <- dt[month == 4 & is_last_of_month == TRUE, .(exit_date = date)]
  exits$january <- dt[month == 1 & is_last_of_month == TRUE, .(exit_date = date)]
  exits$turn_of_month <- dt[trading_day_of_month == 4, .(exit_date = date)]
  exits$sept_avoid <- dt[month == 8 & is_last_of_month == TRUE, .(exit_date = date)]
  exits$end_of_quarter <- dt[is_last_of_quarter == TRUE, .(exit_date = date)]
  exits$weekend <- dt[dow == "Friday", .(exit_date = date)]
  exits$friday <- dt[dow == "Friday", .(exit_date = date)]
  
  for (nm in names(exits)) cat(sprintf("  %s exits: %d\n", nm, nrow(exits[[nm]])))
  return(exits)
}

# ==============================================================================
# ENTRY/EXIT FUNCTIONS
# ==============================================================================

extract_calendar_entries <- function(calendar_dt, price_dt, entry_col) {
  entry_dates <- calendar_dt[get(entry_col) == TRUE, .(entry_date = date)]
  if (nrow(entry_dates) == 0) return(data.table())
  
  entries <- price_dt[entry_dates, on = .(date = entry_date), nomatch = 0]
  setnames(entries, "date", "entry_date")
  setnames(entries, "value", "entry_price")
  entries <- entries[!is.na(entry_price) & entry_price > 0]
  entries[, trade_id := .I]
  return(entries)
}

join_calendar_entries_to_exits <- function(entries, exit_dates, exit_type) {
  if (nrow(entries) == 0 || nrow(exit_dates) == 0) return(data.table())
  
  setorder(exit_dates, exit_date)
  exit_vec <- exit_dates$exit_date
  
  entries <- copy(entries)
  entries[, exit_date := {
    idx <- findInterval(entry_date, exit_vec) + 1
    fifelse(idx > length(exit_vec), as.Date(NA), exit_vec[idx])
  }]
  
  entries <- entries[!is.na(exit_date)]
  entries[, duration_days := as.numeric(exit_date - entry_date)]
  entries <- entries[duration_days > 0]
  return(entries)
}

compute_patience_exits <- function(entries, price_dt) {
  if (nrow(entries) == 0) return(data.table())
  
  entries <- copy(entries)
  all_dates <- sort(unique(price_dt$date))
  results <- vector("list", nrow(entries))
  
  for (i in seq_len(nrow(entries))) {
    entry <- entries[i]
    fid <- entry$forecast_id
    entry_date <- entry$entry_date
    entry_price <- entry$entry_price
    
    future_dates <- all_dates[all_dates > entry_date]
    if (length(future_dates) == 0) next
    
    max_exit_date <- entry_date + MAX_HOLD_DAYS
    future_dates <- future_dates[future_dates <= max_exit_date]
    if (length(future_dates) == 0) future_dates <- all_dates[all_dates > entry_date][1]
    
    future_prices <- price_dt[.(fid, future_dates), nomatch = 0]
    if (nrow(future_prices) == 0) next
    
    setorder(future_prices, date)
    future_prices[, days_held := as.numeric(date - entry_date)]
    future_prices[, pct_return := (value / entry_price) - 1]
    future_prices[, patience_target := sapply(days_held, get_patience_target)]
    future_prices[, exit_triggered := pct_return >= patience_target]
    
    exit_row <- future_prices[exit_triggered == TRUE][1]
    
    if (is.na(exit_row$date)) {
      exit_row <- future_prices[.N]
      forced <- TRUE
      exit_reason <- "max_hold"
    } else {
      forced <- FALSE
      exit_reason <- "patience"
    }
    
    results[[i]] <- data.table(
      forecast_id = fid, entry_date = entry_date, entry_price = entry_price,
      exit_date = exit_row$date, exit_price = exit_row$value,
      exit_reason = exit_reason, forced_exit = forced
    )
  }
  
  result <- rbindlist(results)
  if (nrow(result) > 0) result[, duration_days := as.numeric(exit_date - entry_date)]
  return(result)
}

generate_random_entries <- function(all_dates, price_dt, n_iter, start_date, end_date) {
  period_dates <- all_dates[all_dates >= start_date & all_dates <= end_date]
  period_dates <- period_dates[period_dates <= max(period_dates) - 30]
  if (length(period_dates) == 0) return(data.table())
  
  stocks <- unique(price_dt$forecast_id)
  entries_list <- lapply(stocks, function(fid) {
    sampled_dates <- sample(period_dates, min(n_iter, length(period_dates)), replace = FALSE)
    data.table(forecast_id = fid, entry_date = sampled_dates)
  })
  
  entries <- rbindlist(entries_list)
  entries[price_dt, entry_price := i.value, on = .(forecast_id, entry_date = date)]
  entries <- entries[!is.na(entry_price) & entry_price > 0]
  entries[, trade_id := .I]
  return(entries)
}

compute_hold_exits <- function(entries, price_dt, hold_days) {
  if (nrow(entries) == 0) return(data.table())
  
  entries <- copy(entries)
  all_dates <- sort(unique(price_dt$date))
  
  entries[, target_exit_date := entry_date + hold_days]
  entries[, exit_date := sapply(target_exit_date, function(td) {
    candidates <- all_dates[all_dates >= td]
    if (length(candidates) > 0) candidates[1] else NA
  })]
  entries[, exit_date := as.Date(exit_date, origin = "1970-01-01")]
  
  entries <- entries[!is.na(exit_date)]
  entries[price_dt, exit_price := i.value, on = .(forecast_id, exit_date = date)]
  entries <- entries[!is.na(exit_price)]
  entries[, `:=`(exit_reason = "hold_period", forced_exit = FALSE)]
  entries[, duration_days := as.numeric(exit_date - entry_date)]
  entries[, target_exit_date := NULL]
  return(entries)
}

# ==============================================================================
# CAPITAL MANAGEMENT
# ==============================================================================

apply_full_exposure_constraints <- function(trades_dt, price_dt, all_dates) {
  if (nrow(trades_dt) == 0) return(data.table())
  
  trades <- copy(trades_dt)
  setorder(trades, entry_date, forecast_id)
  
  n_trades <- nrow(trades)
  max_trades <- n_trades * 2
  
  r_fid <- character(max_trades)
  r_entry_date <- integer(max_trades)
  r_exit_date <- integer(max_trades)
  r_entry_price <- numeric(max_trades)
  r_exit_price <- numeric(max_trades)
  r_shares <- numeric(max_trades)
  r_position_value <- numeric(max_trades)
  r_exit_reason <- character(max_trades)
  r_forced_exit <- logical(max_trades)
  r_idx <- 0L
  
  cash <- INITIAL_CAPITAL
  open_exit_dates <- integer(0)
  open_shares <- numeric(0)
  open_exit_prices <- numeric(0)
  open_ids <- integer(0)
  
  t_fid <- trades$forecast_id
  t_entry_date <- as.integer(trades$entry_date)
  t_exit_date <- as.integer(trades$exit_date)
  t_entry_price <- trades$entry_price
  t_exit_price <- trades$exit_price
  t_exit_reason <- trades$exit_reason
  t_forced_exit <- trades$forced_exit
  
  unique_entry_dates <- sort(unique(t_entry_date))
  
  for (entry_d in unique_entry_dates) {
    if (length(open_exit_dates) > 0) {
      closed_mask <- open_exit_dates <= entry_d
      if (any(closed_mask)) {
        proceeds <- sum(open_shares[closed_mask] * open_exit_prices[closed_mask] * (1 - TRANSACTION_COST_PCT))
        cash <- cash + proceeds
        open_exit_dates <- open_exit_dates[!closed_mask]
        open_shares <- open_shares[!closed_mask]
        open_exit_prices <- open_exit_prices[!closed_mask]
        open_ids <- open_ids[!closed_mask]
      }
    }
    
    entry_mask <- t_entry_date == entry_d
    entry_indices <- which(entry_mask)
    n_stocks <- length(entry_indices)
    
    if (n_stocks == 0 || cash <= 0) next
    
    per_stock <- cash / n_stocks
    total_cost <- per_stock * n_stocks * (1 + TRANSACTION_COST_PCT)
    if (total_cost > cash) {
      scale_factor <- cash / total_cost
      per_stock <- per_stock * scale_factor
    }
    
    for (i in entry_indices) {
      shares <- per_stock / t_entry_price[i]
      r_idx <- r_idx + 1L
      r_fid[r_idx] <- t_fid[i]
      r_entry_date[r_idx] <- t_entry_date[i]
      r_exit_date[r_idx] <- t_exit_date[i]
      r_entry_price[r_idx] <- t_entry_price[i]
      r_exit_price[r_idx] <- t_exit_price[i]
      r_shares[r_idx] <- shares
      r_position_value[r_idx] <- per_stock
      r_exit_reason[r_idx] <- t_exit_reason[i]
      r_forced_exit[r_idx] <- t_forced_exit[i]
      
      open_exit_dates <- c(open_exit_dates, t_exit_date[i])
      open_shares <- c(open_shares, shares)
      open_exit_prices <- c(open_exit_prices, t_exit_price[i])
      open_ids <- c(open_ids, r_idx)
    }
    
    cash <- cash - (per_stock * n_stocks * (1 + TRANSACTION_COST_PCT))
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
  result[, net_return := (exit_price * (1 - TRANSACTION_COST_PCT)) / (entry_price * (1 + TRANSACTION_COST_PCT)) - 1]
  result[, duration_days := as.numeric(exit_date - entry_date)]
  result
}

# ==============================================================================
# EQUITY CURVE
# ==============================================================================

build_equity_curve_vectorized <- function(positions_dt, price_dt, all_dates, period) {
  period_dates <- all_dates[all_dates >= period$start & all_dates <= period$end]
  n_dates <- length(period_dates)
  
  if (nrow(positions_dt) == 0) {
    return(data.table(date = period_dates, portfolio_value = rep(INITIAL_CAPITAL, n_dates),
                      cash = rep(INITIAL_CAPITAL, n_dates), n_open_positions = rep(0L, n_dates)))
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
    return(data.table(date = period_dates, portfolio_value = rep(INITIAL_CAPITAL, n_dates),
                      cash = rep(INITIAL_CAPITAL, n_dates), n_open_positions = rep(0L, n_dates)))
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
    return(data.table(date = period_dates, portfolio_value = cash_vec, cash = cash_vec, n_open_positions = rep(0L, n_dates)))
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
  
  data.table(date = period_dates, portfolio_value = cash_vec + market_value_vec, cash = cash_vec, n_open_positions = n_pos_vec)
}

# ==============================================================================
# METRICS
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
  ((n * (n+1)) / ((n-1) * (n-2) * (n-3))) * sum(((rets - m) / s)^4) - (3 * (n-1)^2) / ((n-2) * (n-3))
}

calc_avg_exposure <- function(cash_vec, portfolio_vec) {
  if (length(cash_vec) == 0) return(NA_real_)
  valid <- portfolio_vec > 0
  if (sum(valid) == 0) return(NA_real_)
  mean(1 - cash_vec[valid] / portfolio_vec[valid], na.rm = TRUE)
}

calculate_buy_and_hold <- function(dt) {
  stock_endpoints <- dt[, .(first_price = value[which.min(date)], last_price = value[which.max(date)]), by = forecast_id]
  stock_endpoints <- stock_endpoints[!is.na(first_price) & !is.na(last_price) & first_price > 0]
  n_stocks <- nrow(stock_endpoints)
  if (n_stocks == 0) return(list(gross_return = NA_real_, net_return = NA_real_))
  
  per_stock <- INITIAL_CAPITAL / n_stocks
  gross_final <- sum((stock_endpoints$last_price / stock_endpoints$first_price) * per_stock, na.rm = TRUE)
  net_final <- sum((stock_endpoints$last_price / stock_endpoints$first_price) * per_stock * NET_MULTIPLIER, na.rm = TRUE)
  
  list(gross_return = (gross_final - INITIAL_CAPITAL) / INITIAL_CAPITAL,
       net_return = (net_final - INITIAL_CAPITAL) / INITIAL_CAPITAL)
}



compute_metrics <- function(strategy_name, refit_name, exit_strategy_name,
                            realized, equity_dt, n_years, bh_gross, bh_net, is_gross, ff_factors = NULL) {
  
  bh_return <- if (is_gross) bh_gross else bh_net
  returns_col <- if (is_gross) "gross_return" else "net_return"
  
  ff_res <- run_ff_regression(equity_dt, ff_factors)
  hm_res <- run_hm_regression(equity_dt, ff_factors)
  tm_res <- run_tm_regression(equity_dt, ff_factors)
  
  if (nrow(realized) == 0 || nrow(equity_dt) == 0) {
    return(data.table(
      strategy = strategy_name, refit = refit_name, exit_strategy = exit_strategy_name, band = strategy_name,
      total_events = 0L, pct_positive = NA_real_, n_winners = 0L, n_losers = 0L,
      mean_return = NA_real_, median_return = NA_real_, mean_duration = NA_real_,
      max_duration = NA_integer_, n_forced_exits = 0L,
      total_return = NA_real_, cagr = NA_real_, bh_return = bh_return,
      exposure_matched_bh = NA_real_, avg_exposure = NA_real_, excess_return = NA_real_,
      sharpe_ratio = NA_real_, sortino_ratio = NA_real_, calmar_ratio = NA_real_,
      volatility = NA_real_, skewness = NA_real_, kurtosis = NA_real_, max_drawdown = NA_real_,
      p_binom = NA_real_, p_wilcoxon = NA_real_, return_per_event = NA_real_, return_per_exposure = NA_real_,
      ff_alpha = NA_real_, ff_alpha_t = NA_real_, ff_alpha_p = NA_real_, ff_r_squared = NA_real_,
      ff_beta_mkt = NA_real_, ff_beta_smb = NA_real_, ff_beta_hml = NA_real_,
      ff_beta_rmw = NA_real_, ff_beta_cma = NA_real_, ff_beta_mom = NA_real_,
      ff_alpha_se = NA_real_, ff_beta_mkt_se = NA_real_, ff_beta_smb_se = NA_real_,
      ff_beta_hml_se = NA_real_, ff_beta_rmw_se = NA_real_, ff_beta_cma_se = NA_real_, ff_beta_mom_se = NA_real_,
      ff_df = NA_integer_,
      hm_alpha = NA_real_, hm_alpha_t = NA_real_, hm_alpha_p = NA_real_,
      hm_beta1 = NA_real_, hm_beta2 = NA_real_, hm_beta2_t = NA_real_, hm_beta2_p = NA_real_,
      hm_r_squared = NA_real_,
      hm_alpha_se = NA_real_, hm_beta1_se = NA_real_, hm_beta2_se = NA_real_, hm_df = NA_integer_,
      tm_alpha = NA_real_, tm_alpha_t = NA_real_, tm_alpha_p = NA_real_,
      tm_beta = NA_real_, tm_gamma = NA_real_, tm_gamma_t = NA_real_, tm_gamma_p = NA_real_,
      tm_r_squared = NA_real_,
      tm_alpha_se = NA_real_, tm_beta_se = NA_real_, tm_gamma_se = NA_real_, tm_df = NA_integer_
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
    strategy = strategy_name, refit = refit_name, exit_strategy = exit_strategy_name, band = strategy_name,
    total_events = n_events, pct_positive = mean(returns > 0),
    n_winners = sum(returns > 0), n_losers = sum(returns <= 0),
    mean_return = mean(returns), median_return = median(returns),
    mean_duration = mean(realized$duration_days), max_duration = max(realized$duration_days),
    n_forced_exits = sum(realized$forced_exit, na.rm = TRUE),
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
# EXCEL OUTPUT (WITH FF + H-M + T-M COLUMNS)
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

ff_factors <- tryCatch({
  load_ff_factors(FF_5_FACTOR_FILE, FF_MOMENTUM_FILE)
}, error = function(e) {
  cat(sprintf("  WARNING: Could not load FF factors: %s\n", e$message))
  cat("  Proceeding without FF analysis.\n")
  NULL
})

# ==============================================================================
# PHASE 1: GENERATE CALENDAR DATES AND ENTRIES
# ==============================================================================

cat("\n================================================================================\n")
cat("PHASE 1: Generate calendar dates and entry/exit signals\n")
cat("================================================================================\n")

calendar_dt <- generate_calendar_dates(all_dates, min(all_dates), max(all_dates))
calendar_dt <- generate_calendar_entries(calendar_dt, all_dates)
calendar_exits <- precompute_calendar_exits(calendar_dt, all_dates)

# ==============================================================================
# PHASE 2: PRECOMPUTE ALL TRADES
# ==============================================================================

cat("\n================================================================================\n")
cat("PHASE 2: Precompute all trades\n")
cat("================================================================================\n")

if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

TRADES_CACHE_FILE <- file.path(CACHE_DIR, paste0(UNIVERSE, "_scheduled_trades_cache.rds"))

if (file.exists(TRADES_CACHE_FILE)) {
  cat(sprintf("  Loading cached trades from %s\n", TRADES_CACHE_FILE))
  trades_cache <- readRDS(TRADES_CACHE_FILE)
  cat(sprintf("  Loaded %d cached trade sets\n", length(trades_cache)))
} else {
  cat("  No cache found, computing trades...\n")
  trades_cache <- list()

  for (strat in CALENDAR_STRATEGIES) {
  strat_name <- strat$name
  entry_col <- strat$entry
  exit_type <- strat$exit_type
  
  t0 <- Sys.time()
  entries <- extract_calendar_entries(calendar_dt, full_data, entry_col)
  
  if (nrow(entries) == 0) {
    trades_cache[[paste0(strat_name, "_native")]] <- data.table()
    trades_cache[[paste0(strat_name, "_patience")]] <- data.table()
    cat(sprintf("  %s: 0 entries\n", strat_name))
    next
  }
  
  exit_dates <- calendar_exits[[exit_type]]
  if (!is.null(exit_dates) && nrow(exit_dates) > 0) {
    matched <- join_calendar_entries_to_exits(entries, exit_dates, exit_type)
    if (nrow(matched) > 0) {
      matched[full_data, exit_price := i.value, on = .(forecast_id, exit_date = date)]
      matched <- matched[!is.na(exit_price)]
      matched[, `:=`(exit_reason = exit_type, forced_exit = FALSE)]
      trades_cache[[paste0(strat_name, "_native")]] <- matched
      
      # CRITICAL FIX v2.07: Use MATCHED entries for patience (same entries as native)
      matched_entries <- unique(matched[, .(forecast_id, entry_date, entry_price, trade_id)])
      patience_trades <- compute_patience_exits(matched_entries, full_data)
      trades_cache[[paste0(strat_name, "_patience")]] <- patience_trades
    } else {
      trades_cache[[paste0(strat_name, "_native")]] <- data.table()
      trades_cache[[paste0(strat_name, "_patience")]] <- data.table()
    }
  } else {
    trades_cache[[paste0(strat_name, "_native")]] <- data.table()
    trades_cache[[paste0(strat_name, "_patience")]] <- data.table()
  }
  
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  n_native <- nrow(trades_cache[[paste0(strat_name, "_native")]])
  n_patience <- nrow(trades_cache[[paste0(strat_name, "_patience")]])
  cat(sprintf("  %s: %d entries -> %d native, %d patience (%.2fs)\n", 
              strat_name, nrow(entries), n_native, n_patience, elapsed))
}

for (strat in RANDOM_STRATEGIES) {
  strat_name <- strat$name
  hold_days <- strat$hold_days
  
  t0 <- Sys.time()
  entries <- generate_random_entries(all_dates, full_data, RANDOM_ITERATIONS, min(all_dates), max(all_dates))
  
  if (nrow(entries) == 0) {
    trades_cache[[paste0(strat_name, "_native")]] <- data.table()
    trades_cache[[paste0(strat_name, "_patience")]] <- data.table()
    cat(sprintf("  %s: 0 entries\n", strat_name))
    next
  }
  
  native_trades <- compute_hold_exits(entries, full_data, hold_days)
  trades_cache[[paste0(strat_name, "_native")]] <- native_trades
  
  # CRITICAL FIX v2.07: Use entries that survived native filtering for patience
  if (nrow(native_trades) > 0) {
    native_entries <- unique(native_trades[, .(forecast_id, entry_date, entry_price, trade_id)])
    patience_trades <- compute_patience_exits(native_entries, full_data)
  } else {
    patience_trades <- data.table()
  }
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

run_all_20_tests <- function(strat_name, period_name,
                             ret_patience, ret_native,
                             ff_patience, ff_native) {
  
  # =========================================================================
  # HELPER: Test a single coefficient hypothesis (DIRECTIONAL)
  # =========================================================================
  test_coef_directional <- function(coef_p, coef_n, se_p, se_n, df_p, df_n, direction = "greater") {
    # direction: "greater" means TSF > Native expected (positive diff)
    #            "less" means TSF < Native expected (negative diff)
    result <- list(
      diff = NA_real_, se = NA_real_, t_stat = NA_real_, p_value = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, improved = NA
    )
    
    # CRITICAL FIX v2.07: Safe NA check that handles NULL/empty values
    safe_is_na <- function(x) {
      if (is.null(x) || length(x) == 0) return(TRUE)
      return(is.na(x))
    }
    
    if (safe_is_na(coef_p) || safe_is_na(coef_n)) return(result)
    
    diff <- as.numeric(coef_p) - as.numeric(coef_n)
    result$diff <- diff
    
    # Determine if improved in expected direction
    result$improved <- if (direction == "greater") diff > 0 else diff < 0
    
    # If SEs available, calculate t-test
    se_p_valid <- !safe_is_na(se_p) && as.numeric(se_p) > 0
    se_n_valid <- !safe_is_na(se_n) && as.numeric(se_n) > 0
    
    if (se_p_valid && se_n_valid) {
      se_diff <- sqrt(as.numeric(se_p)^2 + as.numeric(se_n)^2)
      result$se <- se_diff
      
      # Welch-Satterthwaite df approximation
      df_p_valid <- !safe_is_na(df_p) && as.numeric(df_p) > 0
      df_n_valid <- !safe_is_na(df_n) && as.numeric(df_n) > 0
      
      if (df_p_valid && df_n_valid) {
        df_approx <- (se_p^2 + se_n^2)^2 / (se_p^4/df_p + se_n^4/df_n)
      } else {
        df_approx <- 100  # Default if df not available
      }
      
      t_stat <- diff / se_diff
      result$t_stat <- t_stat
      
      # One-sided p-value based on expected direction
      if (direction == "greater") {
        result$p_value <- pt(t_stat, df = df_approx, lower.tail = FALSE)
      } else {
        result$p_value <- pt(t_stat, df = df_approx, lower.tail = TRUE)
      }
      
      # 95% CI
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
    
    # CRITICAL FIX v2.07: Safe NA check that handles NULL/empty values
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
      
      # Welch-Satterthwaite df approximation
      df_p_valid <- !safe_is_na(df_p) && as.numeric(df_p) > 0
      df_n_valid <- !safe_is_na(df_n) && as.numeric(df_n) > 0
      
      if (df_p_valid && df_n_valid) {
        df_approx <- (se_p^2 + se_n^2)^2 / (se_p^4/df_p + se_n^4/df_n)
      } else {
        df_approx <- 100
      }
      
      # TOST: Test if |diff| < delta
      t_lower <- (diff - (-delta)) / se_diff
      t_upper <- (diff - delta) / se_diff
      
      p_lower <- pt(t_lower, df = df_approx, lower.tail = FALSE)
      p_upper <- pt(t_upper, df = df_approx, lower.tail = TRUE)
      
      result$tost_p <- max(p_lower, p_upper)
      
      # 95% CI
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
  
  # Get degrees of freedom
  df_ff_p <- as.numeric(ff_patience$ff_df)
  df_ff_n <- as.numeric(ff_native$ff_df)
  df_tm_p <- as.numeric(ff_patience$tm_df)
  df_tm_n <- as.numeric(ff_native$tm_df)
  df_hm_p <- as.numeric(ff_patience$hm_df)
  df_hm_n <- as.numeric(ff_native$hm_df)
  
  # Default if missing
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
  # RETURN RESULTS - One row per comparison with all hypothesis tests
  # PRIMARY: HAC t-test with BH-adjusted p-values
  # SECONDARY: Binomial, Wilcoxon (computed at aggregate level)
  # DESCRIPTIVE: R² differences (no SE available for inference)
  # =========================================================================
  
  data.table(
    strategy = strat_name,
    period = period_name,
    n_observations = n_obs,
    
    # =====================================================================
    # H_FF1: ff_alpha (TSF > Native) - TEST (HAC PRIMARY)
    # =====================================================================
    H_FF1_diff = h_ff1$diff,
    H_FF1_se = h_ff1$se,
    H_FF1_t = h_ff1$t_stat,
    H_FF1_p = h_ff1$p_value,
    H_FF1_p_bh = test_p_bh["H_FF1"],
    H_FF1_ci_lo = h_ff1$ci_lower,
    H_FF1_ci_hi = h_ff1$ci_upper,
    H_FF1_improved = h_ff1$improved,
    
    # =====================================================================
    # H_FF2: ff_r_squared (TSF < Native) - DESCRIPTIVE ONLY (no SE)
    # =====================================================================
    H_FF2_diff_desc = h_ff2$diff,
    H_FF2_improved_desc = h_ff2$improved,
    
    # =====================================================================
    # H_FF3: ff_beta_mkt (TSF < Native) - TEST (HAC PRIMARY)
    # =====================================================================
    H_FF3_diff = h_ff3$diff,
    H_FF3_se = h_ff3$se,
    H_FF3_t = h_ff3$t_stat,
    H_FF3_p = h_ff3$p_value,
    H_FF3_p_bh = test_p_bh["H_FF3"],
    H_FF3_ci_lo = h_ff3$ci_lower,
    H_FF3_ci_hi = h_ff3$ci_upper,
    H_FF3_improved = h_ff3$improved,
    
    # =====================================================================
    # H_FF4: ff_beta_smb (TSF ≈ Native) - CONTROL (TOST)
    # =====================================================================
    H_FF4_diff = h_ff4$diff,
    H_FF4_se = h_ff4$se,
    H_FF4_tost_p = h_ff4$tost_p,
    H_FF4_tost_p_bh = control_tost_bh["H_FF4"],
    H_FF4_ci_lo = h_ff4$ci_lower,
    H_FF4_ci_hi = h_ff4$ci_upper,
    H_FF4_ci_inc_zero = h_ff4$ci_includes_zero,
    
    # =====================================================================
    # H_FF5: ff_beta_hml (TSF ≈ Native) - CONTROL (TOST)
    # =====================================================================
    H_FF5_diff = h_ff5$diff,
    H_FF5_se = h_ff5$se,
    H_FF5_tost_p = h_ff5$tost_p,
    H_FF5_tost_p_bh = control_tost_bh["H_FF5"],
    H_FF5_ci_lo = h_ff5$ci_lower,
    H_FF5_ci_hi = h_ff5$ci_upper,
    H_FF5_ci_inc_zero = h_ff5$ci_includes_zero,
    
    # =====================================================================
    # H_FF6: ff_beta_rmw (TSF ≈ Native) - CONTROL (TOST)
    # =====================================================================
    H_FF6_diff = h_ff6$diff,
    H_FF6_se = h_ff6$se,
    H_FF6_tost_p = h_ff6$tost_p,
    H_FF6_tost_p_bh = control_tost_bh["H_FF6"],
    H_FF6_ci_lo = h_ff6$ci_lower,
    H_FF6_ci_hi = h_ff6$ci_upper,
    H_FF6_ci_inc_zero = h_ff6$ci_includes_zero,
    
    # =====================================================================
    # H_FF7: ff_beta_cma (TSF ≈ Native) - CONTROL (TOST)
    # =====================================================================
    H_FF7_diff = h_ff7$diff,
    H_FF7_se = h_ff7$se,
    H_FF7_tost_p = h_ff7$tost_p,
    H_FF7_tost_p_bh = control_tost_bh["H_FF7"],
    H_FF7_ci_lo = h_ff7$ci_lower,
    H_FF7_ci_hi = h_ff7$ci_upper,
    H_FF7_ci_inc_zero = h_ff7$ci_includes_zero,
    
    # =====================================================================
    # H_FF8: ff_beta_mom (TSF ≈ Native) - CONTROL (TOST)
    # =====================================================================
    H_FF8_diff = h_ff8$diff,
    H_FF8_se = h_ff8$se,
    H_FF8_tost_p = h_ff8$tost_p,
    H_FF8_tost_p_bh = control_tost_bh["H_FF8"],
    H_FF8_ci_lo = h_ff8$ci_lower,
    H_FF8_ci_hi = h_ff8$ci_upper,
    H_FF8_ci_inc_zero = h_ff8$ci_includes_zero,
    
    # =====================================================================
    # H_TM1: tm_alpha (TSF > Native) - TEST (HAC PRIMARY)
    # =====================================================================
    H_TM1_diff = h_tm1$diff,
    H_TM1_se = h_tm1$se,
    H_TM1_t = h_tm1$t_stat,
    H_TM1_p = h_tm1$p_value,
    H_TM1_p_bh = test_p_bh["H_TM1"],
    H_TM1_ci_lo = h_tm1$ci_lower,
    H_TM1_ci_hi = h_tm1$ci_upper,
    H_TM1_improved = h_tm1$improved,
    
    # =====================================================================
    # H_TM2: tm_gamma (TSF > Native) - TEST (HAC PRIMARY)
    # =====================================================================
    H_TM2_diff = h_tm2$diff,
    H_TM2_se = h_tm2$se,
    H_TM2_t = h_tm2$t_stat,
    H_TM2_p = h_tm2$p_value,
    H_TM2_p_bh = test_p_bh["H_TM2"],
    H_TM2_ci_lo = h_tm2$ci_lower,
    H_TM2_ci_hi = h_tm2$ci_upper,
    H_TM2_improved = h_tm2$improved,
    
    # =====================================================================
    # H_TM3: tm_r_squared (TSF < Native) - DESCRIPTIVE ONLY (no SE)
    # =====================================================================
    H_TM3_diff_desc = h_tm3$diff,
    H_TM3_improved_desc = h_tm3$improved,
    
    # =====================================================================
    # H_TM4: tm_beta (TSF < Native) - TEST (HAC PRIMARY)
    # =====================================================================
    H_TM4_diff = h_tm4$diff,
    H_TM4_se = h_tm4$se,
    H_TM4_t = h_tm4$t_stat,
    H_TM4_p = h_tm4$p_value,
    H_TM4_p_bh = test_p_bh["H_TM4"],
    H_TM4_ci_lo = h_tm4$ci_lower,
    H_TM4_ci_hi = h_tm4$ci_upper,
    H_TM4_improved = h_tm4$improved,
    
    # =====================================================================
    # H_HM1: hm_alpha (TSF > Native) - TEST (HAC PRIMARY)
    # =====================================================================
    H_HM1_diff = h_hm1$diff,
    H_HM1_se = h_hm1$se,
    H_HM1_t = h_hm1$t_stat,
    H_HM1_p = h_hm1$p_value,
    H_HM1_p_bh = test_p_bh["H_HM1"],
    H_HM1_ci_lo = h_hm1$ci_lower,
    H_HM1_ci_hi = h_hm1$ci_upper,
    H_HM1_improved = h_hm1$improved,
    
    # =====================================================================
    # H_HM2: hm_beta2 (TSF > Native) - TEST (HAC PRIMARY)
    # =====================================================================
    H_HM2_diff = h_hm2$diff,
    H_HM2_se = h_hm2$se,
    H_HM2_t = h_hm2$t_stat,
    H_HM2_p = h_hm2$p_value,
    H_HM2_p_bh = test_p_bh["H_HM2"],
    H_HM2_ci_lo = h_hm2$ci_lower,
    H_HM2_ci_hi = h_hm2$ci_upper,
    H_HM2_improved = h_hm2$improved,
    
    # =====================================================================
    # H_HM3: hm_r_squared (TSF < Native) - DESCRIPTIVE ONLY (no SE)
    # =====================================================================
    H_HM3_diff_desc = h_hm3$diff,
    H_HM3_improved_desc = h_hm3$improved,
    
    # =====================================================================
    # H_HM4: hm_beta1 (TSF < Native) - TEST (HAC PRIMARY)
    # =====================================================================
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
# Called AFTER all per-comparison results are collected
# ==============================================================================

run_aggregate_tests <- function(all_comparisons, hypothesis_col, improved_col = NULL) {
  # all_comparisons: data.table with all comparison results
  # hypothesis_col: column name for the metric differences (e.g., "H_FF1_diff")
  # improved_col: column name for improved flag (e.g., "H_FF1_improved") - optional
  
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
  
  # Wilcoxon signed-rank test (two-sided for general use)
  tryCatch({
    wt <- wilcox.test(diffs, mu = 0, alternative = "two.sided", exact = FALSE)
    result$wilcoxon_p <- wt$p.value
  }, error = function(e) {})
  
  # Cohen's d
  tryCatch({
    result$cohens_d <- mean(diffs) / sd(diffs)
  }, error = function(e) {})
  
  # Binomial test on improvement rate
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
# PHASE 3: PROCESS ALL CONFIGURATIONS
# ==============================================================================

cat("\n================================================================================\n")
cat("PHASE 3: Process all configurations\n")
cat("================================================================================\n")

OUTPUT_DIR <- paste0(UNIVERSE, "_scheduled_benchmark_results")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

GROSS_DIR <- file.path(OUTPUT_DIR, "GROSS")
NET_DIR <- file.path(OUTPUT_DIR, "NET")
DAILY_RETURNS_DIR <- file.path(OUTPUT_DIR, "daily_returns")
COMPARISONS_DIR <- file.path(OUTPUT_DIR, "comparisons")

if (!dir.exists(GROSS_DIR)) dir.create(GROSS_DIR, recursive = TRUE)
if (!dir.exists(NET_DIR)) dir.create(NET_DIR, recursive = TRUE)
if (!dir.exists(DAILY_RETURNS_DIR)) dir.create(DAILY_RETURNS_DIR, recursive = TRUE)
if (!dir.exists(COMPARISONS_DIR)) dir.create(COMPARISONS_DIR, recursive = TRUE)

ALL_STRATEGIES <- c(
  lapply(CALENDAR_STRATEGIES, function(x) list(name = x$name, refit = x$refit)),
  lapply(RANDOM_STRATEGIES, function(x) list(name = x$name, refit = x$refit))
)

results_gross_by_period <- list()
results_net_by_period <- list()
all_comparisons <- list()

for (period in PERIODS) {
  per_name <- period$name
  per_label <- period$label
  
  cat(sprintf("\n=== PERIOD: %s (%s) ===\n", per_name, per_label))
  
  period_data <- full_data[date >= period$start & date <= period$end]
  period_dates <- all_dates[all_dates >= period$start & all_dates <= period$end]
  n_years <- length(period_dates) / 252
  
  bh <- calculate_buy_and_hold(period_data)
  cat(sprintf("  B&H: Gross=%.2f%%, Net=%.2f%%\n", bh$gross_return * 100, bh$net_return * 100))
  
  period_results_gross <- list()
  period_results_net <- list()
  equity_curves_native <- list()
  equity_curves_patience <- list()
  ff_results_native <- list()
  ff_results_patience <- list()
  
  for (strat in ALL_STRATEGIES) {
    strat_name <- strat$name
    refit_type <- strat$refit
    
    for (exit_type in c("native", "patience")) {
      cache_key <- paste0(strat_name, "_", exit_type)
      all_trades <- trades_cache[[cache_key]]
      
      if (is.null(all_trades) || nrow(all_trades) == 0) next
      
      period_trades <- all_trades[entry_date >= period$start & entry_date <= period$end]
      if (nrow(period_trades) == 0) next
      
      realized <- apply_full_exposure_constraints(period_trades, full_data, all_dates)
      if (nrow(realized) == 0) next
      
      equity_dt <- build_equity_curve_vectorized(realized, full_data, all_dates, period)
      
      # Save daily returns
      if (nrow(equity_dt) > 1) {
        daily_ret <- data.table(
          date = equity_dt$date[-1],
          portfolio_value = equity_dt$portfolio_value[-1],
          daily_return = diff(equity_dt$portfolio_value) / head(equity_dt$portfolio_value, -1)
        )
        fwrite(daily_ret, file.path(DAILY_RETURNS_DIR, sprintf("%s_%s_%s_%s.csv", UNIVERSE, strat_name, exit_type, per_name)))
        
        if (exit_type == "native") {
          equity_curves_native[[strat_name]] <- daily_ret
        } else {
          equity_curves_patience[[strat_name]] <- daily_ret
        }
      }
      
      metrics_g <- compute_metrics(strat_name, refit_type, exit_type, realized, equity_dt, n_years, bh$gross_return, bh$net_return, TRUE, ff_factors)
      metrics_n <- compute_metrics(strat_name, refit_type, exit_type, realized, equity_dt, n_years, bh$gross_return, bh$net_return, FALSE, ff_factors)
      
      period_results_gross[[cache_key]] <- metrics_g
      period_results_net[[cache_key]] <- metrics_n
      
      if (exit_type == "native") {
        ff_results_native[[strat_name]] <- metrics_n
      } else {
        ff_results_patience[[strat_name]] <- metrics_n
      }
    }
  }
  
  results_gross_by_period[[per_name]] <- rbindlist(period_results_gross, fill = TRUE)
  results_net_by_period[[per_name]] <- rbindlist(period_results_net, fill = TRUE)
  
  # Run comparisons (16 hypotheses tested independently)
  cat("  Running statistical comparisons (16 independent hypothesis tests)...\n")
  period_comparisons <- list()
  
  for (strat_name in names(equity_curves_native)) {
    if (strat_name %in% names(equity_curves_patience)) {
      ret_native <- equity_curves_native[[strat_name]]
      ret_patience <- equity_curves_patience[[strat_name]]
      ff_native <- ff_results_native[[strat_name]]
      ff_patience <- ff_results_patience[[strat_name]]
      
      comp_row <- run_all_20_tests(strat_name, per_name, ret_patience, ret_native, ff_patience, ff_native)
      period_comparisons[[strat_name]] <- comp_row
    }
  }
  
  if (length(period_comparisons) > 0) {
    all_comparisons[[per_name]] <- rbindlist(period_comparisons, fill = TRUE)
  }
}

gross_file <- file.path(GROSS_DIR, paste0(UNIVERSE, "_scheduled_benchmark_GROSS.xlsx"))
net_file <- file.path(NET_DIR, paste0(UNIVERSE, "_scheduled_benchmark_NET.xlsx"))

write_formatted_excel(results_gross_by_period, gross_file)
write_formatted_excel(results_net_by_period, net_file)

# Save comparisons
comp_file <- file.path(COMPARISONS_DIR, paste0(UNIVERSE, "_scheduled_benchmark_comparisons.xlsx"))
write_formatted_excel(all_comparisons, comp_file)

# Save consolidated comparisons
consolidated_comp_file <- file.path(OUTPUT_DIR, paste0(UNIVERSE, "_scheduled_benchmark_ALL_comparisons.xlsx"))
write_formatted_excel(all_comparisons, consolidated_comp_file)

# ==============================================================================
# AGGREGATE HYPOTHESIS TESTS (Wilcoxon, Binomial, Cohen's d across comparisons)
# ==============================================================================

cat("\n================================================================================\n")
cat("Running Aggregate Hypothesis Tests...\n")
cat("================================================================================\n")

# Combine all comparisons across periods
all_comp_combined <- rbindlist(all_comparisons, fill = TRUE)

if (nrow(all_comp_combined) > 0) {
  
  # ===========================================================================
  # AGGREGATE HYPOTHESIS TESTS 
  # PRIMARY: HAC t-test with BH-adjusted p-values
  # SECONDARY: Binomial, Wilcoxon, Cohen's d
  # DESCRIPTIVE: R² differences (H_FF2, H_TM3, H_HM3)
  # ===========================================================================
  
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
  
  # Aggregate tests for TEST hypotheses
  agg_test_results <- list()
  
  cat("\n--- TEST HYPOTHESES (HAC t-test PRIMARY) ---\n")
  
  for (hyp_name in names(test_hypotheses)) {
    hyp <- test_hypotheses[[hyp_name]]
    
    diffs <- all_comp_combined[[hyp$diff]]
    diffs <- diffs[is.finite(diffs)]
    n <- length(diffs)
    
    # PRIMARY: HAC t-test results (BH-adjusted)
    hac_sig_raw <- NA_integer_
    hac_sig_bh <- NA_integer_
    median_p_raw <- NA_real_
    median_p_bh <- NA_real_
    
    if (hyp$has_hac) {
      p_raw <- all_comp_combined[[hyp$p]]
      p_bh <- all_comp_combined[[hyp$p_bh]]
      p_raw <- p_raw[is.finite(p_raw)]
      p_bh <- p_bh[is.finite(p_bh)]
      
      hac_sig_raw <- sum(p_raw < 0.05, na.rm = TRUE)
      hac_sig_bh <- sum(p_bh < 0.05, na.rm = TRUE)
      median_p_raw <- median(p_raw, na.rm = TRUE)
      median_p_bh <- median(p_bh, na.rm = TRUE)
    }
    
    # SECONDARY: Aggregate statistics
    agg <- run_aggregate_tests(all_comp_combined, hyp$diff, hyp$improved)
    
    # Direction-appropriate Wilcoxon
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
  
  # Aggregate tests for CONTROL hypotheses
  agg_control_results <- list()
  
  for (hyp_name in names(control_hypotheses)) {
    hyp <- control_hypotheses[[hyp_name]]
    
    diffs <- all_comp_combined[[hyp$diff]]
    diffs <- diffs[is.finite(diffs)]
    n <- length(diffs)
    
    tost_ps <- all_comp_combined[[hyp$tost_p]]
    tost_ps_bh <- all_comp_combined[[hyp$tost_p_bh]]
    ci_zeros <- all_comp_combined[[hyp$ci_inc_zero]]
    
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
  
  # Save aggregate results
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

ff_hm_tm_summary_by_period <- lapply(results_net_by_period, function(dt) {
  if (nrow(dt) == 0) return(data.table())
  cols_present <- intersect(ff_hm_tm_cols, names(dt))
  dt[, ..cols_present]
})

ff_hm_tm_file <- file.path(OUTPUT_DIR, paste0(UNIVERSE, "_scheduled_FF_HM_TM_SUMMARY.xlsx"))
write_formatted_excel(ff_hm_tm_summary_by_period, ff_hm_tm_file)

cat(sprintf("\nSaved: %s\n", gross_file))
cat(sprintf("Saved: %s\n", net_file))
cat(sprintf("Saved: %s\n", comp_file))
cat(sprintf("Saved: %s\n", consolidated_comp_file))
cat(sprintf("Saved: %s\n", ff_hm_tm_file))
cat(sprintf("Daily returns saved to: %s/\n", DAILY_RETURNS_DIR))

cat("\n================================================================================\n")
cat("COMPLETE\n")
cat("================================================================================\n")
