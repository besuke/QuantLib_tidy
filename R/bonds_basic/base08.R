suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch08.R
# ------------------------------------------------------------
# Credit derivatives / Hazard rate / CDS
# Python notebook ch08.ipynb を R + QuantLib(SWIG) 向けに
# tidyverse 風に書き直した版。
#
# 内容:
# 1. Discount curve
# 2. CDS market spreads から hazard curve の骨格
# 3. Flat hazard / survival probability
# 4. CreditDefaultSwap valuation
# 5. Spread / upfront / risky annuity の確認
#
# 方針:
# - Date は DateParser_parseISO() を使う
# - survival / discount は table 化
# - build 差の大きい CDS engine / helper は tryCatch 多用
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

period_months <- function(n) Period(as.integer(n), "Months")
period_years  <- function(n) Period(as.integer(n), "Years")

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

survival_tbl <- function(default_curve_obj, n = 200) {
  max_t <- tryCatch(default_curve_obj$maxTime(), error = function(e) 10)
  ref_date_r <- tryCatch(as.Date(ql_iso(default_curve_obj$referenceDate())), error = function(e) as.Date("2023-01-03"))
  times <- seq(0, max_t, length.out = n)
  
  tibble(time = times) %>%
    mutate(
      survival_probability = purrr::map_dbl(
        time,
        ~ tryCatch(default_curve_obj$survivalProbability(.x), error = function(e) NA_real_)
      ),
      hazard_rate_proxy = if_else(time > 0 & !is.na(survival_probability), -log(survival_probability) / time, 0),
      curve_date = ref_date_r + round(time * 365)
    )
}

schedule_date_at <- function(schedule_obj, i_one_based) {
  idx0 <- as.integer(i_one_based - 1)
  
  out <- tryCatch(schedule_obj$date(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(schedule_obj$get(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(schedule_obj[[i_one_based]][[1]], error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  stop("Unable to access schedule date at index ", i_one_based)
}

make_schedule_tbl <- function(schedule_obj) {
  tibble(
    schedule_date = purrr::map_chr(seq_len(schedule_obj$size()), ~ ql_iso(schedule_date_at(schedule_obj, .x)))
  )
}

safe_npv <- function(obj) {
  tryCatch(obj$NPV(), error = function(e) NA_real_)
}

safe_set_engine <- function(obj, engine) {
  tryCatch({
    Instrument_setPricingEngine(obj, engine)
    TRUE
  }, error = function(e) FALSE)
}

# ------------------------------------------------------------
# 1. Base setup
# ------------------------------------------------------------

trade_date <- ql_date("2023-01-03")
set_eval_date(trade_date)

cal_target <- TARGET()
dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()

flat_rate <- 0.03
recovery_rate <- 0.40
notional <- 1e7
running_spread <- 100 / 10000

# discount curve
flat_curve <- FlatForward(trade_date, flat_rate, dc_a365)
TermStructure_enableExtrapolation(flat_curve)
flat_curve_handle <- YieldTermStructureHandle(flat_curve)

show_tbl(curve_tbl(flat_curve), "Discount curve", n = 12)

# ------------------------------------------------------------
# 2. Flat hazard curve
# ------------------------------------------------------------

flat_hazard_rate <- 0.02
flat_default_curve <- FlatHazardRate(
  trade_date,
  QuoteHandle(SimpleQuote(flat_hazard_rate)),
  dc_a365
)

TermStructure_enableExtrapolation(flat_default_curve)
default_curve_handle <- DefaultProbabilityTermStructureHandle(flat_default_curve)

show_tbl(survival_tbl(flat_default_curve), "Flat hazard survival table", n = 12)

show_tbl(
  tibble(
    query_time = c(0.5, 1, 3, 5),
    survival_probability = purrr::map_dbl(
      c(0.5, 1, 3, 5),
      ~ flat_default_curve$survivalProbability(.x)
    ),
    default_probability = 1 - survival_probability
  ),
  "Survival probability checkpoints",
  n = 20
)

# ------------------------------------------------------------
# 3. CDS schedule and object
# ------------------------------------------------------------

cds_start_date <- ql_date("2023-01-05")
cds_maturity_date <- ql_date("2028-01-05")

cds_schedule <- Schedule(
  cds_start_date,
  cds_maturity_date,
  period_months(3),
  cal_target,
  "Following",
  "Unadjusted",
  "Forward",
  FALSE
)

show_tbl(make_schedule_tbl(cds_schedule), "CDS schedule", n = 20)

cds_obj <- tryCatch(
  CreditDefaultSwap(
    Protection_Buyer_get(),
    notional,
    running_spread,
    cds_schedule,
    "Following",
    dc_a360,
    TRUE,
    TRUE
  ),
  error = function(e) NULL
)

show_tbl(
  tibble(
    cds_available = !is.null(cds_obj),
    notional = notional,
    running_spread = running_spread,
    recovery_rate = recovery_rate
  ),
  "CDS object summary",
  n = 20
)

# ------------------------------------------------------------
# 4. CDS pricing engine
# ------------------------------------------------------------

cds_engine_midpoint <- tryCatch(
  MidPointCdsEngine(
    default_curve_handle,
    recovery_rate,
    flat_curve_handle
  ),
  error = function(e) NULL
)

cds_engine_isda <- tryCatch(
  IsdaCdsEngine(
    default_curve_handle,
    recovery_rate,
    flat_curve_handle
  ),
  error = function(e) NULL
)

cds_result_tbl <- tibble(
  engine = c("MidPoint", "ISDA"),
  available = c(!is.null(cds_engine_midpoint), !is.null(cds_engine_isda)),
  npv = c(NA_real_, NA_real_),
  fair_spread = c(NA_real_, NA_real_),
  upfront_npv = c(NA_real_, NA_real_)
)

if (!is.null(cds_obj) && !is.null(cds_engine_midpoint)) {
  cds_mid_obj <- tryCatch(
    CreditDefaultSwap(
      Protection_Buyer_get(),
      notional,
      running_spread,
      cds_schedule,
      "Following",
      dc_a360,
      TRUE,
      TRUE
    ),
    error = function(e) NULL
  )
  
  if (!is.null(cds_mid_obj)) {
    safe_set_engine(cds_mid_obj, cds_engine_midpoint)
    cds_result_tbl$npv[cds_result_tbl$engine == "MidPoint"] <- safe_npv(cds_mid_obj)
    cds_result_tbl$fair_spread[cds_result_tbl$engine == "MidPoint"] <- tryCatch(cds_mid_obj$fairSpread(), error = function(e) NA_real_)
    cds_result_tbl$upfront_npv[cds_result_tbl$engine == "MidPoint"] <- tryCatch(cds_mid_obj$couponLegNPV(), error = function(e) NA_real_)
  }
}

if (!is.null(cds_obj) && !is.null(cds_engine_isda)) {
  cds_isda_obj <- tryCatch(
    CreditDefaultSwap(
      Protection_Buyer_get(),
      notional,
      running_spread,
      cds_schedule,
      "Following",
      dc_a360,
      TRUE,
      TRUE
    ),
    error = function(e) NULL
  )
  
  if (!is.null(cds_isda_obj)) {
    safe_set_engine(cds_isda_obj, cds_engine_isda)
    cds_result_tbl$npv[cds_result_tbl$engine == "ISDA"] <- safe_npv(cds_isda_obj)
    cds_result_tbl$fair_spread[cds_result_tbl$engine == "ISDA"] <- tryCatch(cds_isda_obj$fairSpread(), error = function(e) NA_real_)
    cds_result_tbl$upfront_npv[cds_result_tbl$engine == "ISDA"] <- tryCatch(cds_isda_obj$couponLegNPV(), error = function(e) NA_real_)
  }
}

show_tbl(cds_result_tbl, "CDS valuation", n = 20)

# ------------------------------------------------------------
# 5. CDS market spread helpers (bootstrap skeleton)
# ------------------------------------------------------------

cds_market_tbl <- tibble::tribble(
  ~tenor, ~market_spread_bp,
  "6M",  60,
  "1Y",  75,
  "3Y",  95,
  "5Y",  110,
  "7Y",  125
)

parse_cds_tenor <- function(x) {
  unit <- substring(x, nchar(x), nchar(x))
  n <- as.integer(substring(x, 1, nchar(x) - 1))
  switch(
    unit,
    M = period_months(n),
    Y = period_years(n),
    stop("Unsupported tenor: ", x)
  )
}

cds_helper_list <- purrr::map(
  seq_len(nrow(cds_market_tbl)),
  function(i) {
    tenor_i <- cds_market_tbl$tenor[i]
    spread_bp_i <- cds_market_tbl$market_spread_bp[i]
    
    tryCatch(
      SpreadCdsHelper(
        spread_bp_i / 10000,
        parse_cds_tenor(tenor_i),
        0,
        cal_target,
        "Following",
        Quarterly_get(),
        "OldCDS",
        dc_a360,
        recovery_rate,
        flat_curve_handle
      ),
      error = function(e) NULL
    )
  }
)

cds_helper_tbl <- tibble(
  tenor = cds_market_tbl$tenor,
  market_spread_bp = cds_market_tbl$market_spread_bp,
  helper_available = !purrr::map_lgl(cds_helper_list, is.null)
)

show_tbl(cds_helper_tbl, "CDS helper availability", n = 20)

available_helpers <- cds_helper_list[!purrr::map_lgl(cds_helper_list, is.null)]

boot_curve_obj <- NULL
if (length(available_helpers) > 0) {
  helper_vec <- RateHelperVector()
  purrr::walk(available_helpers, ~ RateHelperVector_append(helper_vec, .x))
  
  boot_curve_obj <- tryCatch(
    PiecewiseFlatHazardRate(
      trade_date,
      helper_vec,
      dc_a365
    ),
    error = function(e) NULL
  )
}

if (is.null(boot_curve_obj)) {
  show_tbl(
    tibble(note = "Bootstrapped hazard curve depends on SWIG helper availability; skipped in this build"),
    "Bootstrapped hazard curve"
  )
} else {
  TermStructure_enableExtrapolation(boot_curve_obj)
  show_tbl(survival_tbl(boot_curve_obj), "Bootstrapped hazard curve", n = 20)
}

# ------------------------------------------------------------
# 6. Long-format comparison tables
# ------------------------------------------------------------

survival_compare_tbl <- bind_rows(
  survival_tbl(flat_default_curve) %>% mutate(curve_name = "flat_hazard"),
  if (!is.null(boot_curve_obj)) survival_tbl(boot_curve_obj) %>% mutate(curve_name = "bootstrapped_hazard") else tibble()
)

cds_result_long_tbl <- cds_result_tbl %>%
  pivot_longer(
    cols = c(npv, fair_spread, upfront_npv),
    names_to = "metric",
    values_to = "value"
  )

show_tbl(survival_compare_tbl, "Survival comparison (long table)", n = 20)
show_tbl(cds_result_long_tbl, "CDS results (long table)", n = 20)

cat("\nch08 credit derivatives rewrite completed successfully.\n")
