suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch10.R
# ------------------------------------------------------------
# FX derivatives / Garman-Kohlhagen / FX forward / FX option
# Python notebook ch10.ipynb を R + QuantLib(SWIG) 向けに
# tidyverse 風に書き直した版。
#
# 内容:
# 1. Domestic / foreign yield curves
# 2. FX forward points / implied forward
# 3. Garman-Kohlhagen process
# 4. European FX option valuation
# 5. Greeks table
# 6. Spot / vol / rate scenario table
#
# 方針:
# - Date は DateParser_parseISO() を使う
# - FX は foreign dividend yield として表現
# - engine 差は tryCatch で吸収
# - まず end-to-end の骨格を優先
# ============================================================

# ------------------------------------------------------------
# 0. Utility helpers
# ------------------------------------------------------------

ql_date <- function(x) {
  if (inherits(x, "POSIXt")) x <- format(as.Date(x), "%Y-%m-%d")
  if (inherits(x, "Date"))   x <- format(x, "%Y-%m-%d")
  if (is.character(x) && length(x) == 1L) return(DateParser_parseISO(x))
  x
}

ql_iso <- function(x) {
  tryCatch(Date_ISO(x), error = function(e) as.character(x))
}

show_tbl <- function(tbl, title = NULL, n = 10) {
  if (!is.null(title)) {
    cat("\n", strrep("=", 72), "\n", title, "\n", strrep("=", 72), "\n", sep = "")
  }
  print(dplyr::slice_head(tbl, n = n))
  invisible(tbl)
}

set_eval_date <- function(eval_date) {
  Settings_instance()$setEvaluationDate(ql_date(eval_date))
}

fmt_num <- function(x, digits = 6) sprintf(paste0("%.", digits, "f"), x)
fmt_pct <- function(x, digits = 4) sprintf(paste0("%.", digits, "f%%"), 100 * x)

safe_npv <- function(obj) {
  tryCatch(obj$NPV(), error = function(e) NA_real_)
}

safe_engine_set <- function(obj, engine) {
  tryCatch({
    Instrument_setPricingEngine(obj, engine)
    TRUE
  }, error = function(e) FALSE)
}

safe_greek <- function(obj, greek_name) {
  fn <- tryCatch(obj[[greek_name]], error = function(e) NULL)
  if (is.null(fn)) return(NA_real_)
  tryCatch(fn(), error = function(e) NA_real_)
}

curve_tbl <- function(curve_obj, n = 200, extrapolate = TRUE) {
  if (extrapolate) {
    TermStructure_enableExtrapolation(curve_obj)
  }
  
  ref_date_r <- as.Date(ql_iso(curve_obj$referenceDate()))
  max_t <- curve_obj$maxTime()
  times <- seq(0, max_t, length.out = n)
  
  tibble(time = times) %>%
    mutate(
      discount = purrr::map_dbl(time, ~ curve_obj$discount(.x)),
      zero = if_else(time > 0, -log(discount) / time, 0),
      curve_date = ref_date_r + round(time * 365)
    )
}

# ------------------------------------------------------------
# 1. Market inputs
# ------------------------------------------------------------

eval_date <- ql_date("2023-01-03")
set_eval_date(eval_date)

calendar_obj <- TARGET()
day_counter_obj <- Actual365Fixed()

spot_fx <- 130.00
foreign_rate <- 0.01
# JPY domestic side for USDJPY-like quote
domestic_rate <- 0.03
volatility_value <- 0.12
maturity_date <- ql_date("2024-01-03")
strike_value <- 130.00

spot_quote <- SimpleQuote(spot_fx)
spot_handle <- QuoteHandle(spot_quote)

foreign_curve <- FlatForward(eval_date, foreign_rate, day_counter_obj)
domestic_curve <- FlatForward(eval_date, domestic_rate, day_counter_obj)
fx_vol_curve <- BlackConstantVol(eval_date, calendar_obj, volatility_value, day_counter_obj)

TermStructure_enableExtrapolation(foreign_curve)
TermStructure_enableExtrapolation(domestic_curve)

foreign_handle <- YieldTermStructureHandle(foreign_curve)
domestic_handle <- YieldTermStructureHandle(domestic_curve)
fx_vol_handle <- BlackVolTermStructureHandle(fx_vol_curve)

show_tbl(
  tibble(
    item = c("spot_fx", "foreign_rate", "domestic_rate", "volatility", "strike"),
    value = c(spot_fx, foreign_rate, domestic_rate, volatility_value, strike_value)
  ),
  "FX market inputs",
  n = 20
)

show_tbl(curve_tbl(foreign_curve), "Foreign curve", n = 12)
show_tbl(curve_tbl(domestic_curve), "Domestic curve", n = 12)

# ------------------------------------------------------------
# 2. FX forward table
# ------------------------------------------------------------

fx_forward_tbl <- tibble(time = c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3)) %>%
  mutate(
    foreign_df = purrr::map_dbl(time, ~ foreign_curve$discount(.x)),
    domestic_df = purrr::map_dbl(time, ~ domestic_curve$discount(.x)),
    implied_forward = spot_fx * domestic_df / foreign_df,
    forward_points = implied_forward - spot_fx
  )

show_tbl(fx_forward_tbl, "FX forward table", n = 20)

# ------------------------------------------------------------
# 3. Garman-Kohlhagen / BSM-style process
# ------------------------------------------------------------

fx_process <- BlackScholesMertonProcess(
  spot_handle,
  foreign_handle,
  domestic_handle,
  fx_vol_handle
)

show_tbl(
  tibble(
    evaluation_date = ql_iso(eval_date),
    maturity_date = ql_iso(maturity_date),
    process_created = !is.null(fx_process)
  ),
  "FX process summary",
  n = 20
)

# ------------------------------------------------------------
# 4. European FX option valuation
# ------------------------------------------------------------

payoff_call <- PlainVanillaPayoff("Call", strike_value)
payoff_put <- PlainVanillaPayoff("Put", strike_value)
exercise_obj <- EuropeanExercise(maturity_date)

fx_call_obj <- VanillaOption(payoff_call, exercise_obj)
fx_put_obj <- VanillaOption(payoff_put, exercise_obj)

analytic_engine <- tryCatch(
  AnalyticEuropeanEngine(fx_process),
  error = function(e) NULL
)

if (!is.null(analytic_engine)) {
  safe_engine_set(fx_call_obj, analytic_engine)
  safe_engine_set(fx_put_obj, analytic_engine)
}

fx_option_tbl <- tibble(
  option_type = c("Call", "Put"),
  npv = c(safe_npv(fx_call_obj), safe_npv(fx_put_obj)),
  delta = c(safe_greek(fx_call_obj, "delta"), safe_greek(fx_put_obj, "delta")),
  gamma = c(safe_greek(fx_call_obj, "gamma"), safe_greek(fx_put_obj, "gamma")),
  vega = c(safe_greek(fx_call_obj, "vega"), safe_greek(fx_put_obj, "vega")),
  theta = c(safe_greek(fx_call_obj, "theta"), safe_greek(fx_put_obj, "theta")),
  rho = c(safe_greek(fx_call_obj, "rho"), safe_greek(fx_put_obj, "rho")),
  dividend_rho = c(safe_greek(fx_call_obj, "dividendRho"), safe_greek(fx_put_obj, "dividendRho"))
)

show_tbl(fx_option_tbl, "FX option valuation", n = 20)

# ------------------------------------------------------------
# 5. Put-call parity check in FX form
# ------------------------------------------------------------

time_to_maturity <- day_counter_obj$yearFraction(eval_date, maturity_date)
foreign_df_t <- foreign_curve$discount(time_to_maturity)
domestic_df_t <- domestic_curve$discount(time_to_maturity)

parity_tbl <- tibble(
  lhs_call_minus_put = safe_npv(fx_call_obj) - safe_npv(fx_put_obj),
  rhs_spot_foreign_minus_strike_domestic = spot_fx * foreign_df_t - strike_value * domestic_df_t,
  difference = lhs_call_minus_put - rhs_spot_foreign_minus_strike_domestic
)

show_tbl(parity_tbl, "FX put-call parity check", n = 20)

# ------------------------------------------------------------
# 6. Scenario table: spot / vol / domestic rate
# ------------------------------------------------------------

price_fx_call_scenario <- function(spot, volatility, domestic_rate_local) {
  local_spot_quote <- SimpleQuote(spot)
  local_spot_handle <- QuoteHandle(local_spot_quote)
  
  local_domestic_curve <- FlatForward(eval_date, domestic_rate_local, day_counter_obj)
  local_foreign_curve <- FlatForward(eval_date, foreign_rate, day_counter_obj)
  local_vol_curve <- BlackConstantVol(eval_date, calendar_obj, volatility, day_counter_obj)
  
  TermStructure_enableExtrapolation(local_domestic_curve)
  TermStructure_enableExtrapolation(local_foreign_curve)
  
  local_domestic_handle <- YieldTermStructureHandle(local_domestic_curve)
  local_foreign_handle <- YieldTermStructureHandle(local_foreign_curve)
  local_vol_handle <- BlackVolTermStructureHandle(local_vol_curve)
  
  local_process <- BlackScholesMertonProcess(
    local_spot_handle,
    local_foreign_handle,
    local_domestic_handle,
    local_vol_handle
  )
  
  local_engine <- tryCatch(AnalyticEuropeanEngine(local_process), error = function(e) NULL)
  if (is.null(local_engine)) return(NA_real_)
  
  local_option <- VanillaOption(payoff_call, exercise_obj)
  ok <- safe_engine_set(local_option, local_engine)
  if (!ok) return(NA_real_)
  
  safe_npv(local_option)
}

scenario_tbl <- tidyr::crossing(
  spot = c(120, 125, 130, 135, 140),
  volatility = c(0.08, 0.12, 0.16),
  domestic_rate = c(0.02, 0.03, 0.04)
) %>%
  mutate(
    call_npv = purrr::pmap_dbl(
      list(spot, volatility, domestic_rate),
      ~ price_fx_call_scenario(..1, ..2, ..3)
    )
  )

show_tbl(scenario_tbl, "FX option scenario table", n = 20)

# ------------------------------------------------------------
# 7. Long-format comparison tables
# ------------------------------------------------------------

fx_option_long_tbl <- fx_option_tbl %>%
  pivot_longer(
    cols = c(npv, delta, gamma, vega, theta, rho, dividend_rho),
    names_to = "metric",
    values_to = "value"
  )

fx_forward_long_tbl <- fx_forward_tbl %>%
  pivot_longer(
    cols = c(foreign_df, domestic_df, implied_forward, forward_points),
    names_to = "metric",
    values_to = "value"
  )

show_tbl(fx_option_long_tbl, "FX option long table", n = 20)
show_tbl(fx_forward_long_tbl, "FX forward long table", n = 20)

cat("\nch10 FX derivatives rewrite completed successfully.\n")
