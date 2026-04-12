suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch03.R
# ------------------------------------------------------------
# RFR スワップ / OIS / マルチカーブのうち、まず OIS 部分を
# R + QuantLib(SWIG) 向けに tidyverse 風で書き直した版。
#
# この版で扱う内容:
# 1. TONA OIS curve
# 2. TONA OIS valuation
# 3. daily OIS forward check
# 4. fixing 登録後の再評価
# 5. SOFR OIS curve
# 6. SOFR OIS valuation
# 7. optional: Term SOFR basis bootstrap
#
# 方針:
# - Date は DateParser_parseISO() を使う
# - curve の zero は discount から自前計算する
# - nodes()/dates() への依存を減らす
# - fixing は valuation 前後で比較できるようにする
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

ql_chr <- function(x) {
  tryCatch(
    x$`__str__`(),
    error = function(e1) {
      tryCatch(as.character(x), error = function(e2) "<unprintable>")
    }
  )
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

advance_days <- function(calendar_obj, date_obj, n_days, convention = "Days") {
  Calendar_advance(calendar_obj, ql_date(date_obj), as.integer(n_days), convention)
}

period_days   <- function(n) Period(as.integer(n), "Days")
period_weeks  <- function(n) Period(as.integer(n), "Weeks")
period_months <- function(n) Period(as.integer(n), "Months")
period_years  <- function(n) Period(as.integer(n), "Years")

parse_tenor <- function(x) {
  unit <- substring(x, nchar(x), nchar(x))
  n <- as.integer(substring(x, 1, nchar(x) - 1))
  
  switch(
    unit,
    d = period_days(n),
    w = period_weeks(n),
    m = period_months(n),
    y = period_years(n),
    stop("Unsupported tenor: ", x)
  )
}

simple_quote_handle <- function(value) {
  QuoteHandle(SimpleQuote(value))
}

build_rate_helper_vector <- function(helper_list) {
  helper_vec <- RateHelperVector()
  purrr::walk(helper_list, ~ RateHelperVector_append(helper_vec, .x))
  helper_vec
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

leg_cashflow_at <- function(leg_obj, i_one_based) {
  idx0 <- as.integer(i_one_based - 1)
  
  out <- tryCatch(leg_obj$get(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(Leg___getitem__(leg_obj, idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(leg_obj[[i_one_based]][[1]], error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  stop("Unable to access leg cashflow at index ", i_one_based)
}

make_schedule_tbl_from_dates <- function(effective_date, maturity_date, tenor_months = 12) {
  tibble(
    schedule_date = seq.Date(
      from = as.Date(effective_date),
      to   = as.Date(maturity_date),
      by   = paste0(tenor_months, " months")
    )
  ) %>%
    mutate(schedule_date = as.character(schedule_date))
}

# calendars / counters
jp_calendar <- Japan()
us_fixing_calendar <- Sofr()$fixingCalendar()
dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()

# ============================================================
# 1. TONA OIS curve
# ============================================================

make_tona_curve <- function(curve_data, trade_date = "2022-08-19") {
  trade_date_ql <- ql_date(trade_date)
  set_eval_date(trade_date_ql)
  
  tona_curve_handle <- RelinkableYieldTermStructureHandle()
  tona_index <- OvernightIndex(
    "TONA",
    0,
    JPYCurrency(),
    jp_calendar,
    dc_a365,
    tona_curve_handle
  )
  
  helper_list <- purrr::map(curve_data, function(x) {
    kind <- as.character(x[[1]])
    tenor <- as.character(x[[2]])
    rate_pct <- as.numeric(x[[3]])
    
    if (kind == "depo") {
      return(
        DepositRateHelper(
          simple_quote_handle(rate_pct / 100),
          tona_index
        )
      )
    }
    
    if (kind == "swap") {
      return(
        OISRateHelper(
          2,
          parse_tenor(tenor),
          simple_quote_handle(rate_pct / 100),
          tona_index
        )
      )
    }
    
    stop("Unsupported helper type: ", kind)
  })
  
  helper_vec <- build_rate_helper_vector(helper_list)
  
  tona_curve_obj <- PiecewiseLogLinearDiscount(
    0,
    jp_calendar,
    helper_vec,
    dc_a365
  )
  TermStructure_enableExtrapolation(tona_curve_obj)
  RelinkableYieldTermStructureHandle_linkTo(tona_curve_handle, tona_curve_obj)
  
  list(
    trade_date = trade_date_ql,
    settle_date = tona_curve_obj$referenceDate(),
    index = tona_index,
    curve = tona_curve_obj,
    curve_handle = tona_curve_handle,
    quote_tbl = tibble(
      kind = purrr::map_chr(curve_data, 1),
      tenor = purrr::map_chr(curve_data, 2),
      rate_pct = purrr::map_dbl(curve_data, ~ as.numeric(.x[[3]]))
    )
  )
}

tona_curve_data <- list(
  c("depo", "1d", -0.00900),
  c("swap", "1w", -0.01261),
  c("swap", "2w", -0.01469),
  c("swap", "1m", -0.01807),
  c("swap", "3m", -0.01919),
  c("swap", "6m", -0.01043),
  c("swap", "9m",  0.00022),
  c("swap", "12m", 0.01250),
  c("swap", "18m", 0.03125),
  c("swap", "2y",  0.04875),
  c("swap", "3y",  0.07375),
  c("swap", "4y",  0.09479),
  c("swap", "5y",  0.11854),
  c("swap", "7y",  0.19146)
)

tona_bundle <- make_tona_curve(tona_curve_data, trade_date = "2022-08-19")

cat("TONA trade date :", ql_iso(tona_bundle$trade_date), "\n")
cat("TONA settle date:", ql_iso(tona_bundle$settle_date), "\n")
show_tbl(tona_bundle$quote_tbl, "TONA input quotes")
show_tbl(curve_tbl(tona_bundle$curve), "TONA curve table")

show_tbl(
  tibble(
    query_date = "2022-08-23",
    discount_factor = tona_bundle$curve$discount(ql_date("2022-08-23"))
  ),
  "TONA DF at 2022-08-23"
)

# optional: shifted curve
make_zero_spreaded_curve <- function(base_curve_handle, spread_rate) {
  spread_curve_obj <- ZeroSpreadedTermStructure(
    base_curve_handle,
    simple_quote_handle(spread_rate),
    compounding = comp_simple,
    frequency = Frequency_Annual_get(),
    dayCounter = dc_a365
  )
  TermStructure_enableExtrapolation(spread_curve_obj)
  spread_curve_obj
}

# ============================================================
# 2. TONA OIS setup
# ============================================================

make_ois_swap <- function(index_obj,
                          curve_handle,
                          effective_date,
                          maturity_date,
                          nominal = 1e7,
                          fixed_rate = 0.05,
                          spread = 0.0,
                          fixed_day_count = dc_a365,
                          schedule_tenor = period_years(1),
                          pay_lag = 2,
                          calendar_obj = jp_calendar,
                          convention = "ModifiedFollowing") {
  fixed_schedule <- Schedule(
    ql_date(effective_date),
    ql_date(maturity_date),
    schedule_tenor,
    calendar_obj,
    convention,
    convention,
    "Backward",
    FALSE
  )
  
  swap_obj <- OvernightIndexedSwap(
    Swap_Payer_get(),
    nominal,
    fixed_schedule,
    fixed_rate,
    fixed_day_count,
    index_obj,
    spread,
    pay_lag
  )
  
  engine_obj <- DiscountingSwapEngine(curve_handle)
  Instrument_setPricingEngine(swap_obj, engine_obj)
  
  list(
    swap = swap_obj,
    fixed_schedule = fixed_schedule,
    effective_date = effective_date,
    maturity_date = maturity_date,
    pay_lag = pay_lag,
    spread = spread,
    fixed_rate = fixed_rate,
    index = index_obj,
    engine = engine_obj
  )
}

tona_swap_bundle <- make_ois_swap(
  index_obj = tona_bundle$index,
  curve_handle = tona_bundle$curve_handle,
  effective_date = "2022-08-23",
  maturity_date = "2022-08-30",
  nominal = 10000000,
  fixed_rate = 5.0 / 100,
  spread = 0.0,
  fixed_day_count = dc_a365,
  schedule_tenor = period_years(1),
  pay_lag = 2,
  calendar_obj = jp_calendar
)

show_tbl(
  make_schedule_tbl_from_dates(
    effective_date = tona_swap_bundle$effective_date,
    maturity_date = tona_swap_bundle$maturity_date,
    tenor_months = 12
  ),
  "TONA OIS schedule"
)

cat("\nTONA OIS valuation\n")
cat("Fixed leg NPV    :", tona_swap_bundle$swap$legNPV(0), "\n")
cat("Floating leg NPV :", tona_swap_bundle$swap$legNPV(1), "\n")
cat("Swap NPV         :", tona_swap_bundle$swap$NPV(), "\n")
cat("Fair rate        :", tona_swap_bundle$swap$fairRate(), "\n")
cat("Fair spread      :", tona_swap_bundle$swap$fairSpread(), "\n")

# ============================================================
# 3. OIS cashflow / forward diagnostics
# ============================================================

ois_cashflow_tbl <- function(swap_obj, curve_obj, leg = 1, day_count_obj = dc_a365) {
  target_leg <- swap_obj$leg(leg)
  
  purrr::map_dfr(seq_len(target_leg$size()), function(i) {
    cashflow_obj <- leg_cashflow_at(target_leg, i)
    pay_date <- CashFlow_date(cashflow_obj)
    pay_date_chr <- ql_iso(pay_date)
    pay_date_r <- as.Date(pay_date_chr)
    ref_date_r <- as.Date(ql_iso(curve_obj$referenceDate()))
    t <- as.numeric(pay_date_r - ref_date_r) / 365
    df <- if (t >= 0) curve_obj$discount(pay_date) else NA_real_
    
    if (leg == 1) {
      coupon_obj <- as_floating_rate_coupon(cashflow_obj)
      tibble(
        fixing_date = ql_iso(FloatingRateCoupon_fixingDate(coupon_obj)),
        accrual_start = ql_iso(Coupon_accrualStartDate(coupon_obj)),
        accrual_end = ql_iso(Coupon_accrualEndDate(coupon_obj)),
        pay_date = pay_date_chr,
        days = day_count_obj$dayCount(Coupon_accrualStartDate(coupon_obj), Coupon_accrualEndDate(coupon_obj)),
        rate = coupon_obj$rate(),
        spread = FloatingRateCoupon_spread(coupon_obj),
        amount = CashFlow_amount(cashflow_obj),
        df = df,
        pv = CashFlow_amount(cashflow_obj) * df,
        leg = "floating"
      )
    } else {
      coupon_obj <- as_fixed_rate_coupon(cashflow_obj)
      tibble(
        fixing_date = NA_character_,
        accrual_start = ql_iso(Coupon_accrualStartDate(coupon_obj)),
        accrual_end = ql_iso(Coupon_accrualEndDate(coupon_obj)),
        pay_date = pay_date_chr,
        days = day_count_obj$dayCount(Coupon_accrualStartDate(coupon_obj), Coupon_accrualEndDate(coupon_obj)),
        rate = coupon_obj$rate(),
        spread = NA_real_,
        amount = CashFlow_amount(cashflow_obj),
        df = df,
        pv = CashFlow_amount(cashflow_obj) * df,
        leg = "fixed"
      )
    }
  })
}

tona_fixed_cf_tbl <- ois_cashflow_tbl(tona_swap_bundle$swap, tona_bundle$curve, leg = 0, day_count_obj = dc_a365)
tona_float_cf_tbl <- ois_cashflow_tbl(tona_swap_bundle$swap, tona_bundle$curve, leg = 1, day_count_obj = dc_a365)

show_tbl(tona_fixed_cf_tbl, "TONA fixed leg cashflows", n = 20)
show_tbl(tona_float_cf_tbl, "TONA floating leg cashflows", n = 20)

cat("\nFloating leg PV (manual):", sum(tona_float_cf_tbl$pv, na.rm = TRUE), "\n")
cat("Fixed leg PV (manual)   :", sum(tona_fixed_cf_tbl$pv, na.rm = TRUE), "\n")

# daily forward OIS view

tona_swap_daily <- make_ois_swap(
  index_obj = tona_bundle$index,
  curve_handle = tona_bundle$curve_handle,
  effective_date = "2022-08-23",
  maturity_date = "2022-08-30",
  nominal = 10000000,
  fixed_rate = 5.0 / 100,
  spread = 0.0,
  fixed_day_count = dc_a365,
  schedule_tenor = period_days(1),
  pay_lag = 0,
  calendar_obj = jp_calendar
)

tona_daily_cf_tbl <- ois_cashflow_tbl(tona_swap_daily$swap, tona_bundle$curve, leg = 1, day_count_obj = dc_a365)
show_tbl(tona_daily_cf_tbl, "TONA daily forward leg", n = 20)

# Aug 29 implied forward from DF table, roughly mirroring the notebook
if (nrow(tona_daily_cf_tbl) >= 6) {
  aug29_fwd <- (tona_daily_cf_tbl$df[5] / tona_daily_cf_tbl$df[6] - 1) * 365
  cat("Implied TONA forward (around Aug 29):", sprintf("%.6f%%", 100 * aug29_fwd), "\n")
}

if (nrow(tona_daily_cf_tbl) >= 2) {
  daily_cmp <- (1 + tona_daily_cf_tbl$days[-1] / 365 * tona_daily_cf_tbl$rate[-1]) |> cumprod()
  daily_cmp_last <- daily_cmp[length(daily_cmp)] - 1
  cat("Compounded future value:", sprintf("%0.4f", daily_cmp_last * 10000000), "\n")
  cat("Implied fair rate     :", sprintf("%.6f%%", 100 * daily_cmp_last * 365 / 7), "\n")
}

# ============================================================
# 4. TONA fixing before / after registration
# ============================================================
# ============================================================
# 4. TONA fixing diagnostics and valuation flow
# ============================================================

make_date_vector <- function(x) {
  dv <- DateVector()
  purrr::walk(x, ~ DateVector_append(dv, ql_date(.x)))
  dv
}

make_business_dates_tbl <- function(start_date, end_date, calendar_obj) {
  start_r <- as.Date(start_date)
  end_r   <- as.Date(end_date)
  
  raw_dates <- seq.Date(start_r, end_r - 1, by = "day")
  
  tibble(accrual_date = raw_dates) %>%
    mutate(
      accrual_date_chr = as.character(accrual_date),
      is_business_day = purrr::map_lgl(
        accrual_date_chr,
        ~ !Calendar_isHoliday(calendar_obj, ql_date(.x))
      )
    ) %>%
    filter(is_business_day) %>%
    select(accrual_date = accrual_date_chr)
}

tona_daily_fixing_tbl <- function(
    accrual_start,
    accrual_end,
    index_obj,
    eval_date,
    calendar_obj = jp_calendar
) {
  eval_date_r <- as.Date(eval_date)
  
  make_business_dates_tbl(
    start_date = accrual_start,
    end_date   = accrual_end,
    calendar_obj = calendar_obj
  ) %>%
    mutate(
      fixing_date = accrual_date,
      fixing_date_r = as.Date(fixing_date),
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval = fixing_date_r == eval_date_r,
      fixing_value = purrr::map_dbl(
        fixing_date,
        ~ tryCatch(index_obj$fixing(ql_date(.x)), error = function(e) NA_real_)
      ),
      fixing_source = dplyr::case_when(
        fixing_date_r < eval_date_r ~ "historical fixing expected",
        fixing_date_r == eval_date_r ~ "today fixing boundary",
        TRUE ~ "forward projection expected"
      )
    ) %>%
    select(
      accrual_date,
      fixing_date,
      fixing_before_eval,
      fixing_on_eval,
      fixing_value,
      fixing_source
    )
}

ois_fixing_status_tbl <- function(
    swap_obj,
    curve_obj,
    index_obj,
    leg = 1,
    eval_date = NULL,
    day_count_obj = dc_a365
) {
  if (is.null(eval_date)) {
    eval_date <- Settings_instance()$evaluationDate()
  } else {
    eval_date <- ql_date(eval_date)
  }
  
  target_leg <- swap_obj$leg(leg)
  
  purrr::map_dfr(seq_len(target_leg$size()), function(i) {
    cashflow_obj <- leg_cashflow_at(target_leg, i)
    coupon_obj <- as_floating_rate_coupon(cashflow_obj)
    
    fixing_date_obj <- FloatingRateCoupon_fixingDate(coupon_obj)
    fixing_date_chr <- ql_iso(fixing_date_obj)
    fixing_date_r <- as.Date(fixing_date_chr)
    eval_date_r <- as.Date(ql_iso(eval_date))
    
    accrual_start_obj <- Coupon_accrualStartDate(coupon_obj)
    accrual_end_obj <- Coupon_accrualEndDate(coupon_obj)
    pay_date_obj <- CashFlow_date(cashflow_obj)
    
    pay_date_r <- as.Date(ql_iso(pay_date_obj))
    ref_date_r <- as.Date(ql_iso(curve_obj$referenceDate()))
    t <- as.numeric(pay_date_r - ref_date_r) / 365
    df <- if (t >= 0) curve_obj$discount(pay_date_obj) else NA_real_
    
    coupon_rate <- coupon_obj$rate()
    coupon_spread <- FloatingRateCoupon_spread(coupon_obj)
    
    fixing_value <- tryCatch(
      index_obj$fixing(fixing_date_obj),
      error = function(e) NA_real_
    )
    
    tibble(
      fixing_date = fixing_date_chr,
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval = fixing_date_r == eval_date_r,
      accrual_start = ql_iso(accrual_start_obj),
      accrual_end = ql_iso(accrual_end_obj),
      pay_date = ql_iso(pay_date_obj),
      amount = CashFlow_amount(cashflow_obj),
      coupon_rate = coupon_rate,
      spread = coupon_spread,
      fixing_value = fixing_value,
      implied_margin = coupon_rate - fixing_value,
      df = df,
      pv = CashFlow_amount(cashflow_obj) * df,
      fixing_source = dplyr::case_when(
        fixing_date_r < eval_date_r ~ "historical fixing expected",
        fixing_date_r == eval_date_r ~ "today fixing boundary",
        TRUE ~ "forward projection expected"
      )
    )
  })
}
ois_fixing_status_tbl <- function(
    swap_obj,
    curve_obj,
    index_obj,
    leg = 1,
    eval_date = NULL,
    day_count_obj = dc_a365
) {
  if (is.null(eval_date)) {
    eval_date <- Settings_instance()$evaluationDate()
  } else {
    eval_date <- ql_date(eval_date)
  }
  
  target_leg <- swap_obj$leg(leg)
  
  purrr::map_dfr(seq_len(target_leg$size()), function(i) {
    cashflow_obj <- leg_cashflow_at(target_leg, i)
    coupon_obj <- as_floating_rate_coupon(cashflow_obj)
    
    fixing_date_obj <- FloatingRateCoupon_fixingDate(coupon_obj)
    fixing_date_chr <- ql_iso(fixing_date_obj)
    fixing_date_r <- as.Date(fixing_date_chr)
    eval_date_r <- as.Date(ql_iso(eval_date))
    
    accrual_start_obj <- Coupon_accrualStartDate(coupon_obj)
    accrual_end_obj <- Coupon_accrualEndDate(coupon_obj)
    pay_date_obj <- CashFlow_date(cashflow_obj)
    pay_date_chr <- ql_iso(pay_date_obj)
    
    pay_date_r <- as.Date(pay_date_chr)
    ref_date_r <- as.Date(ql_iso(curve_obj$referenceDate()))
    t <- as.numeric(pay_date_r - ref_date_r) / 365
    df <- if (t >= 0) curve_obj$discount(pay_date_obj) else NA_real_
    
    fixing_value <- tryCatch(
      index_obj$fixing(fixing_date_obj),
      error = function(e) NA_real_
    )
    
    coupon_spread <- tryCatch(
      FloatingRateCoupon_spread(coupon_obj),
      error = function(e) NA_real_
    )
    
    coupon_rate <- tryCatch(
      coupon_obj$rate(),
      error = function(e) NA_real_
    )
    
    amount_value <- tryCatch(
      CashFlow_amount(cashflow_obj),
      error = function(e) NA_real_
    )
    
    tibble(
      fixing_date = fixing_date_chr,
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval = fixing_date_r == eval_date_r,
      accrual_start = ql_iso(accrual_start_obj),
      accrual_end = ql_iso(accrual_end_obj),
      pay_date = pay_date_chr,
      amount = amount_value,
      coupon_rate = coupon_rate,
      spread = coupon_spread,
      fixing_value = fixing_value,
      implied_margin = coupon_rate - fixing_value,
      df = df,
      pv = amount_value * df,
      fixing_source = dplyr::case_when(
        fixing_date_r < eval_date_r ~ "historical fixing expected",
        fixing_date_r == eval_date_r ~ "today fixing boundary",
        TRUE ~ "forward projection expected"
      )
    )
  })
}
# ------------------------------------------------------------
# 4-1. BEFORE addFixings
# ------------------------------------------------------------

set_eval_date("2022-08-25")

tona_daily_fixing_before <- tona_daily_fixing_tbl(
  accrual_start = "2022-08-23",
  accrual_end   = "2030-08-30",
  index_obj     = tona_swap_bundle$index,
  eval_date     = "2022-08-25",
  calendar_obj  = jp_calendar
)

show_tbl(
  tona_daily_fixing_before,
  "TONA daily fixing decomposition: BEFORE addFixings",
  n = 20
)
# ------------------------------------------------------------
# 4-2. Register historical fixings
# 実務ではここに実データを入れる
# ------------------------------------------------------------


tona_fixing_tbl <- tibble::tribble(
  ~fixing_date,  ~fixing_rate,
  "2022-08-23",  0.00052,
  "2022-08-24",  0.00049
)

tona_swap_bundle$index$addFixings(
  make_date_vector(tona_fixing_tbl$fixing_date),
  tona_fixing_tbl$fixing_rate,
  TRUE
)

show_tbl(
  tona_fixing_tbl,
  "TONA fixings to register"
)

stopifnot(!any(is.na(tona_fixing_tbl$fixing_rate)))

tona_swap_bundle$index$addFixings(
  make_date_vector(tona_fixing_tbl$fixing_date),
  tona_fixing_tbl$fixing_rate,
  TRUE
)
set_eval_date("2022-08-25")

tona_coupon_fixing_after <- ois_fixing_status_tbl(
  swap_obj = tona_swap_bundle$swap,
  curve_obj = tona_bundle$curve,
  index_obj = tona_swap_bundle$index,
  leg = 1,
  eval_date = "2022-08-25",
  day_count_obj = dc_a365
)

show_tbl(
  tona_coupon_fixing_after,
  "TONA coupon-level fixing check: AFTER addFixings",
  n = 20
)
# ------------------------------------------------------------
# 4-3. AFTER addFixings
# ------------------------------------------------------------

tona_coupon_fixing_after <- ois_fixing_status_tbl(
  swap_obj = tona_swap_bundle$swap,
  curve_obj = tona_bundle$curve,
  index_obj = tona_swap_bundle$index,
  leg = 1,
  eval_date = "2022-08-25",
  day_count_obj = dc_a365
)

show_tbl(
  tona_coupon_fixing_after,
  "TONA coupon-level fixing check: AFTER addFixings",
  n = 20
)

tona_daily_fixing_after <- tona_daily_fixing_tbl(
  accrual_start = "2022-08-23",
  accrual_end   = "2022-08-30",
  index_obj     = tona_swap_bundle$index,
  eval_date     = "2022-08-25",
  calendar_obj  = jp_calendar
)

show_tbl(
  tona_daily_fixing_after,
  "TONA daily fixing decomposition: AFTER addFixings",
  n = 20
)

# ------------------------------------------------------------
# 4-4. Valuation after historical fixings
# ------------------------------------------------------------

cat("\nTONA OIS valuation after fixings (2022-08-25 eval)\n")
cat("Fixed leg NPV    :", tona_swap_bundle$swap$legNPV(0), "\n")
cat("Floating leg NPV :", tona_swap_bundle$swap$legNPV(1), "\n")
cat("Swap NPV         :", tona_swap_bundle$swap$NPV(), "\n")
cat("Fair rate        :", tona_swap_bundle$swap$fairRate(), "\n")
cat("Fair spread      :", tona_swap_bundle$swap$fairSpread(), "\n")
# ============================================================
# 5. SOFR OIS curve and valuation
# ============================================================

make_sofr_curve <- function(curve_data, trade_date = "2023-09-26") {
  trade_date_ql <- ql_date(trade_date)
  set_eval_date(trade_date_ql)
  
  sofr_curve_handle <- RelinkableYieldTermStructureHandle()
  sofr_index <- Sofr(sofr_curve_handle)
  
  helper_list <- purrr::map(curve_data, function(x) {
    kind <- as.character(x[[1]])
    tenor <- as.character(x[[2]])
    rate_pct <- as.numeric(x[[3]])
    
    if (kind == "depo") {
      return(
        DepositRateHelper(
          simple_quote_handle(rate_pct / 100),
          sofr_index
        )
      )
    }
    
    if (kind == "swap") {
      return(
        OISRateHelper(
          2,
          parse_tenor(tenor),
          simple_quote_handle(rate_pct / 100),
          sofr_index
        )
      )
    }
    
    stop("Unsupported helper type: ", kind)
  })
  
  helper_vec <- build_rate_helper_vector(helper_list)
  
  sofr_curve_obj <- PiecewiseLogLinearDiscount(
    0,
    sofr_index$fixingCalendar(),
    helper_vec,
    dc_a360
  )
  TermStructure_enableExtrapolation(sofr_curve_obj)
  RelinkableYieldTermStructureHandle_linkTo(sofr_curve_handle, sofr_curve_obj)
  
  list(
    trade_date = trade_date_ql,
    settle_date = sofr_curve_obj$referenceDate(),
    index = sofr_index,
    curve = sofr_curve_obj,
    curve_handle = sofr_curve_handle,
    quote_tbl = tibble(
      kind = purrr::map_chr(curve_data, 1),
      tenor = purrr::map_chr(curve_data, 2),
      rate_pct = purrr::map_dbl(curve_data, ~ as.numeric(.x[[3]]))
    )
  )
}

sofr_curve_data <- list(
  c("depo", "1d", 5.31),
  c("swap", "1m", 5.32),
  c("swap", "3m", 5.38),
  c("swap", "6m", 5.46),
  c("swap", "1y", 5.45),
  c("swap", "2y", 5.01),
  c("swap", "3y", 4.67)
)

sofr_bundle <- make_sofr_curve(sofr_curve_data, trade_date = "2023-09-26")

cat("\nSOFR trade date :", ql_iso(sofr_bundle$trade_date), "\n")
cat("SOFR settle date:", ql_iso(sofr_bundle$settle_date), "\n")
show_tbl(sofr_bundle$quote_tbl, "SOFR input quotes")
show_tbl(curve_tbl(sofr_bundle$curve), "SOFR curve table")

show_tbl(
  tibble(
    trade_date = ql_iso(sofr_bundle$trade_date),
    settle_date = ql_iso(sofr_bundle$index$valueDate(sofr_bundle$trade_date)),
    fixing_days = sofr_bundle$index$fixingDays(),
    fixing_date = ql_iso(sofr_bundle$index$fixingDate(sofr_bundle$index$valueDate(sofr_bundle$trade_date))),
    tenor = ql_chr(sofr_bundle$index$tenor()),
    day_counter = DayCounter_name(sofr_bundle$index$dayCounter()),
    fixing_calendar = ql_chr(sofr_bundle$index$fixingCalendar()),
    maturity_date = ql_iso(sofr_bundle$index$maturityDate(sofr_bundle$index$valueDate(sofr_bundle$trade_date)))
  ),
  "SOFR index summary"
)

sofr_swap_bundle <- make_ois_swap(
  index_obj = sofr_bundle$index,
  curve_handle = sofr_bundle$curve_handle,
  effective_date = "2023-09-28",
  maturity_date = "2025-09-28",
  nominal = 10000000,
  fixed_rate = 5.0 / 100,
  spread = 0.0,
  fixed_day_count = dc_a360,
  schedule_tenor = period_years(1),
  pay_lag = 2,
  calendar_obj = sofr_bundle$index$fixingCalendar()
)

cat("\nSOFR OIS valuation\n")
cat("Fixed leg NPV    :", sofr_swap_bundle$swap$legNPV(0), "\n")
cat("Floating leg NPV :", sofr_swap_bundle$swap$legNPV(1), "\n")
cat("Swap NPV         :", sofr_swap_bundle$swap$NPV(), "\n")
cat("Fair rate        :", sofr_swap_bundle$swap$fairRate(), "\n")
cat("Fair spread      :", sofr_swap_bundle$swap$fairSpread(), "\n")

sofr_float_cf_tbl <- ois_cashflow_tbl(sofr_swap_bundle$swap, sofr_bundle$curve, leg = 1, day_count_obj = dc_a360)
show_tbl(sofr_float_cf_tbl, "SOFR floating leg cashflows", n = 20)

if (nrow(sofr_float_cf_tbl) >= 3) {
  eff_df <- sofr_bundle$curve$discount(ql_date(sofr_float_cf_tbl$accrual_end[2]))
  mat_df <- sofr_bundle$curve$discount(ql_date(sofr_float_cf_tbl$accrual_end[3]))
  annuity <- 364 / 360 * mat_df
  cat("Forward swap rate (rough notebook analogue):", sprintf("%.6f%%", 100 * (eff_df - mat_df) / annuity), "\n")
}

# ============================================================
# 6. Optional: Term SOFR basis bootstrap
# ============================================================

make_term_sofr_basis_curve <- function(sofr_bundle,
                                       term_sofr_3m_rate = 5.38558,
                                       basis_data = list(
                                         c("6m", 0.0),
                                         c("1y", 0.0),
                                         c("2y", 0.0),
                                         c("3y", 0.0)
                                       )) {
  term_curve_handle <- RelinkableYieldTermStructureHandle()
  term_sofr_index <- IborIndex(
    "TermSofr",
    period_months(3),
    2,
    USDCurrency(),
    sofr_bundle$index$fixingCalendar(),
    "ModifiedFollowing",
    TRUE,
    dc_a360,
    term_curve_handle
  )
  
  helper_list <- list(
    DepositRateHelper(
      simple_quote_handle(term_sofr_3m_rate / 100),
      term_sofr_index
    )
  )
  
  basis_helpers <- purrr::map(basis_data, function(x) {
    tenor <- as.character(x[[1]])
    basis_bp <- as.numeric(x[[2]])
    
    OvernightIborBasisSwapRateHelper(
      simple_quote_handle(basis_bp / 100),
      parse_tenor(tenor),
      2,
      sofr_bundle$index$fixingCalendar(),
      "ModifiedFollowing",
      FALSE,
      sofr_bundle$index,
      term_sofr_index,
      sofr_bundle$curve_handle
    )
  })
  
  helper_vec <- build_rate_helper_vector(c(helper_list, basis_helpers))
  
  term_curve_obj <- PiecewiseLogLinearDiscount(
    2,
    sofr_bundle$index$fixingCalendar(),
    helper_vec,
    dc_a360
  )
  TermStructure_enableExtrapolation(term_curve_obj)
  RelinkableYieldTermStructureHandle_linkTo(term_curve_handle, term_curve_obj)
  
  list(
    index = term_sofr_index,
    curve = term_curve_obj,
    curve_handle = term_curve_handle
  )
}

term_sofr_bundle <- tryCatch(
  make_term_sofr_basis_curve(sofr_bundle),
  error = function(e) NULL
)

if (!is.null(term_sofr_bundle)) {
  cat("\nTerm SOFR curve built successfully\n")
  cat("Reference date:", ql_iso(term_sofr_bundle$curve$referenceDate()), "\n")
  show_tbl(curve_tbl(term_sofr_bundle$curve), "Term SOFR basis curve")
} else {
  show_tbl(
    tibble(note = "Term SOFR basis helper depends on SWIG build; skipped in this environment"),
    "Term SOFR basis bootstrap"
  )
}
tona_daily_fixing_tbl <- function(
    accrual_start,
    accrual_end,
    index_obj,
    eval_date
) {
  eval_date_r <- as.Date(eval_date)
  
  make_business_dates_tbl(
    start_date = accrual_start,
    end_date   = accrual_end,
    calendar_obj = jp_calendar
  ) %>%
    mutate(
      fixing_date = accrual_date,
      fixing_date_r = as.Date(fixing_date),
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval = fixing_date_r == eval_date_r,
      fixing_value = purrr::map_dbl(
        fixing_date,
        ~ tryCatch(index_obj$fixing(ql_date(.x)), error = function(e) NA_real_)
      ),
      fixing_source = case_when(
        fixing_date_r < eval_date_r ~ "historical fixing expected",
        fixing_date_r == eval_date_r ~ "today fixing boundary",
        TRUE ~ "forward projection expected"
      )
    ) %>%
    select(
      accrual_date,
      fixing_date,
      fixing_before_eval,
      fixing_on_eval,
      fixing_value,
      fixing_source
    )
}
tona_daily_tbl <- tona_daily_fixing_tbl(
  accrual_start = "2022-08-23",
  accrual_end   = "2022-08-30",
  index_obj     = tona_swap_bundle$index,
  eval_date     = "2022-08-25"
)

show_tbl(tona_daily_tbl, "TONA daily fixing decomposition", n = 20)
cat("\nch03 OIS rewrite completed successfully.\n")
