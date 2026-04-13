suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch09.R
# ------------------------------------------------------------
# Equity derivatives / Black-Scholes / American option / Greeks
# Python notebook ch09.ipynb を R + QuantLib(SWIG) 向けに
# tidyverse 風に書き直した版。
#
# 内容:
# 1. Market inputs
# 2. Black-Scholes-Merton process
# 3. European option valuation
# 4. American option valuation
# 5. Greeks table
# 6. Volatility / spot scenario table
#
# 方針:
# - Date は DateParser_parseISO() を使う
# - quote / curve / vol を handle 化
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

# ------------------------------------------------------------
# 1. Market setup
# ------------------------------------------------------------

eval_date <- ql_date("2023-01-03")
set_eval_date(eval_date)

calendar_obj <- TARGET()
day_counter_obj <- Actual365Fixed()

spot_value <- 100
dividend_yield <- 0.01
risk_free_rate <- 0.03
volatility_value <- 0.20
strike_value <- 100
maturity_date <- ql_date("2024-01-03")

spot_quote <- SimpleQuote(spot_value)
spot_handle <- QuoteHandle(spot_quote)

dividend_curve <- FlatForward(eval_date, dividend_yield, day_counter_obj)
risk_free_curve <- FlatForward(eval_date, risk_free_rate, day_counter_obj)
black_vol_curve <- BlackConstantVol(eval_date, calendar_obj, volatility_value, day_counter_obj)

black_vol_curve <- BlackConstantVol(eval_date, calendar_obj, volatility_value, day_counter_obj)

TermStructure_enableExtrapolation(dividend_curve)
TermStructure_enableExtrapolation(risk_free_curve)

dividend_handle <- YieldTermStructureHandle(dividend_curve)
risk_free_handle <- YieldTermStructureHandle(risk_free_curve)
black_vol_handle <- BlackVolTermStructureHandle(black_vol_curve)
TermStructure_enableExtrapolation(dividend_curve)
TermStructure_enableExtrapolation(risk_free_curve)
BlackVolTermStructure_enableExtrapolation(black_vol_curve)

dividend_handle <- YieldTermStructureHandle(dividend_curve)
risk_free_handle <- YieldTermStructureHandle(risk_free_curve)
#Sblack_vol_handle <- BlackVolTermStructureHandle(black_vol_curve)

show_tbl(
  tibble(
    item = c("spot", "dividend_yield", "risk_free_rate", "volatility", "strike"),
    value = c(spot_value, dividend_yield, risk_free_rate, volatility_value, strike_value)
  ),
  "Market inputs",
  n = 20
)

# ------------------------------------------------------------
# 2. BSM process
# ------------------------------------------------------------

bsm_process <- BlackScholesMertonProcess(
  spot_handle,
  dividend_handle,
  risk_free_handle,
  black_vol_handle
)

show_tbl(
  tibble(
    evaluation_date = ql_iso(eval_date),
    maturity_date = ql_iso(maturity_date),
    process_created = !is.null(bsm_process)
  ),
  "BSM process summary",
  n = 20
)

# ------------------------------------------------------------
# 3. European option
# ------------------------------------------------------------

payoff_call <- PlainVanillaPayoff("Call", strike_value)
payoff_put  <- PlainVanillaPayoff("Put", strike_value)

european_exercise <- EuropeanExercise(maturity_date)

european_call <- VanillaOption(payoff_call, european_exercise)
european_put  <- VanillaOption(payoff_put, european_exercise)

analytic_engine <- tryCatch(
  AnalyticEuropeanEngine(bsm_process),
  error = function(e) NULL
)

if (!is.null(analytic_engine)) {
  safe_engine_set(european_call, analytic_engine)
  safe_engine_set(european_put, analytic_engine)
}

european_tbl <- tibble(
  option_type = c("Call", "Put"),
  npv = c(safe_npv(european_call), safe_npv(european_put)),
  delta = c(safe_greek(european_call, "delta"), safe_greek(european_put, "delta")),
  gamma = c(safe_greek(european_call, "gamma"), safe_greek(european_put, "gamma")),
  vega = c(safe_greek(european_call, "vega"), safe_greek(european_put, "vega")),
  theta = c(safe_greek(european_call, "theta"), safe_greek(european_put, "theta")),
  rho = c(safe_greek(european_call, "rho"), safe_greek(european_put, "rho"))
)

show_tbl(european_tbl, "European option valuation", n = 20)

# ------------------------------------------------------------
# 4. American option
# ------------------------------------------------------------

american_exercise <- AmericanExercise(eval_date, maturity_date)

american_call <- VanillaOption(payoff_call, american_exercise)
american_put  <- VanillaOption(payoff_put, american_exercise)

fd_engine <- tryCatch(
  FdBlackScholesVanillaEngine(bsm_process),
  error = function(e) NULL
)
if (is.null(fd_engine)) {
  fd_engine <- tryCatch(
    FdBlackScholesVanillaEngine(bsm_process, 200, 200),
    error = function(e) NULL
  )
}

barone_engine <- tryCatch(
  BaroneAdesiWhaleyApproximationEngine(bsm_process),
  error = function(e) NULL
)

american_result_tbl <- tibble(
  option_type = c("Call", "Put"),
  fd_npv = c(NA_real_, NA_real_),
  baw_npv = c(NA_real_, NA_real_)
)

if (!is.null(fd_engine)) {
  american_call_fd <- VanillaOption(payoff_call, american_exercise)
  american_put_fd  <- VanillaOption(payoff_put, american_exercise)
  safe_engine_set(american_call_fd, fd_engine)
  safe_engine_set(american_put_fd, fd_engine)
  american_result_tbl$fd_npv <- c(safe_npv(american_call_fd), safe_npv(american_put_fd))
}

if (!is.null(barone_engine)) {
  american_call_baw <- VanillaOption(payoff_call, american_exercise)
  american_put_baw  <- VanillaOption(payoff_put, american_exercise)
  safe_engine_set(american_call_baw, barone_engine)
  safe_engine_set(american_put_baw, barone_engine)
  american_result_tbl$baw_npv <- c(safe_npv(american_call_baw), safe_npv(american_put_baw))
}

show_tbl(american_result_tbl, "American option valuation", n = 20)

# ------------------------------------------------------------
# 5. Greeks table (European call focus)
# ------------------------------------------------------------

greeks_tbl <- tibble(
  greek = c("delta", "gamma", "vega", "theta", "rho", "dividendRho"),
  value = c(
    safe_greek(european_call, "delta"),
    safe_greek(european_call, "gamma"),
    safe_greek(european_call, "vega"),
    safe_greek(european_call, "theta"),
    safe_greek(european_call, "rho"),
    safe_greek(european_call, "dividendRho")
  )
)

show_tbl(greeks_tbl, "European call Greeks", n = 20)

# ------------------------------------------------------------
# 6. Spot / volatility scenario table
# ------------------------------------------------------------

scenario_input_tbl <- tidyr::crossing(
  spot = c(80, 90, 100, 110, 120),
  volatility = c(0.10, 0.20, 0.30)
)

price_european_call_scenario <- function(spot, volatility) {
  local_spot_quote <- SimpleQuote(spot)
  local_spot_handle <- QuoteHandle(local_spot_quote)
  
  local_vol_curve <- BlackConstantVol(eval_date, calendar_obj, volatility, day_counter_obj)
  BlackVolTermStructure_enableExtrapolation(local_vol_curve)
  local_vol_handle <- BlackVolTermStructureHandle(local_vol_curve)
  
  local_process <- BlackScholesMertonProcess(
    local_spot_handle,
    dividend_handle,
    risk_free_handle,
    local_vol_handle
  )
  
  local_engine <- tryCatch(AnalyticEuropeanEngine(local_process), error = function(e) NULL)
  if (is.null(local_engine)) return(NA_real_)
  
  local_option <- VanillaOption(payoff_call, european_exercise)
  ok <- safe_engine_set(local_option, local_engine)
  if (!ok) return(NA_real_)
  
  safe_npv(local_option)
}

scenario_result_tbl <- scenario_input_tbl %>%
  mutate(
    call_npv = purrr::pmap_dbl(
      list(spot, volatility),
      ~ price_european_call_scenario(..1, ..2)
    )
  )

show_tbl(scenario_result_tbl, "European call scenario table", n = 20)

# ------------------------------------------------------------
# 7. Long-format comparison tables
# ------------------------------------------------------------

european_long_tbl <- european_tbl %>%
  pivot_longer(
    cols = c(npv, delta, gamma, vega, theta, rho),
    names_to = "metric",
    values_to = "value"
  )

american_long_tbl <- american_result_tbl %>%
  pivot_longer(
    cols = c(fd_npv, baw_npv),
    names_to = "engine",
    values_to = "value"
  )

show_tbl(european_long_tbl, "European option long table", n = 20)
show_tbl(american_long_tbl, "American option long table", n = 20)

cat("\nch09 equity derivatives rewrite completed successfully.\n")

