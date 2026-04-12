suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch02.R
# ------------------------------------------------------------
# Ibor 金利スワップ
# Python notebook ch02.ipynb を、R + QuantLib(SWIG) 向けに
# tidyverse 風の命名で書き直した完成版。
#
# 実務寄りの流れ:
# 1. マーケットデータとカーブ構築
# 2. スワップ条件の設定
# 3. スケジュール確認
# 4. 必要な fixing の確認
# 5. fixing 登録
# 6. 評価
# 7. cashflow / fixing diagnostics
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

advance_days <- function(calendar_obj, date_obj, n_days) {
  Calendar_advance(calendar_obj, ql_date(date_obj), as.integer(n_days), "Days")
}

period_months <- function(n) Period(as.integer(n), "Months")
period_years  <- function(n) Period(as.integer(n), "Years")

simple_quote_handle <- function(value) {
  QuoteHandle(SimpleQuote(value))
}

build_rate_helper_vector <- function(helper_list) {
  helper_vec <- RateHelperVector()
  purrr::walk(helper_list, ~ RateHelperVector_append(helper_vec, .x))
  helper_vec
}

normalize_curve_data <- function(curve_data) {
  if (is.data.frame(curve_data)) {
    return(
      curve_data %>%
        transmute(
          kind = as.character(kind),
          tenor = as.character(tenor),
          rate_pct = as.numeric(rate_pct)
        )
    )
  }
  
  tibble(
    kind = purrr::map_chr(curve_data, ~ as.character(.x[[1]])),
    tenor = purrr::map_chr(curve_data, ~ as.character(.x[[2]])),
    rate_pct = purrr::map_dbl(curve_data, ~ as.numeric(.x[[3]]))
  )
}

# enums
comp_simple <- Compounding_Simple_get()
comp_continuous <- Compounding_Continuous_get()
freq_annual <- Frequency_Annual_get()
freq_semiannual <- Frequency_Semiannual_get()

# calendars / counters
jp_calendar <- Japan()
dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()

# ------------------------------------------------------------
# 1. Curve helpers
# ------------------------------------------------------------

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

make_tibor_curve <- function(curve_data, trade_date = "2022-08-19") {
  curve_input_tbl <- normalize_curve_data(curve_data)
  
  trade_date_ql <- ql_date(trade_date)
  set_eval_date(trade_date_ql)
  settle_date_ql <- advance_days(jp_calendar, trade_date_ql, 2)
  
  tibor_curve_handle <- RelinkableYieldTermStructureHandle()
  tibor_index <- Tibor(period_months(6), tibor_curve_handle)
  
  helper_list <- purrr::pmap(
    curve_input_tbl,
    function(kind, tenor, rate_pct) {
      tenor_obj <- switch(
        tenor,
        `6m` = period_months(6),
        `1y` = period_years(1),
        `18m` = period_months(18),
        `2y` = period_years(2),
        `3y` = period_years(3),
        `4y` = period_years(4),
        `5y` = period_years(5),
        `6y` = period_years(6),
        `7y` = period_years(7),
        stop("Unsupported tenor: ", tenor)
      )
      
      if (kind == "depo") {
        return(
          DepositRateHelper(
            simple_quote_handle(rate_pct / 100),
            tibor_index
          )
        )
      }
      
      if (kind == "swap") {
        return(
          SwapRateHelper(
            simple_quote_handle(rate_pct / 100),
            tenor_obj,
            jp_calendar,
            freq_semiannual,
            "ModifiedFollowing",
            dc_a365,
            tibor_index
          )
        )
      }
      
      stop("Unsupported helper type: ", kind)
    }
  )
  
  helper_vec <- build_rate_helper_vector(helper_list)
  
  tibor_curve_obj <- PiecewiseLogLinearDiscount(
    settle_date_ql,
    helper_vec,
    dc_a365
  )
  TermStructure_enableExtrapolation(tibor_curve_obj)
  RelinkableYieldTermStructureHandle_linkTo(tibor_curve_handle, tibor_curve_obj)
  
  list(
    trade_date = trade_date_ql,
    settle_date = settle_date_ql,
    tibor_index = tibor_index,
    curve = tibor_curve_obj,
    curve_handle = tibor_curve_handle,
    par_tbl = curve_input_tbl
  )
}

# ------------------------------------------------------------
# 2. Schedule and swap helpers
# ------------------------------------------------------------

make_schedule_tbl_from_dates <- function(
    effective_date,
    maturity_date,
    tenor_months = 6
) {
  effective_date_r <- as.Date(effective_date)
  maturity_date_r  <- as.Date(maturity_date)
  
  tibble(
    schedule_date = seq.Date(
      from = effective_date_r,
      to   = maturity_date_r,
      by   = paste0(tenor_months, " months")
    )
  ) %>%
    mutate(schedule_date = as.character(schedule_date))
}

make_schedule_safe <- function(effective_date, maturity_date, tenor_obj,
                               calendar_obj = jp_calendar,
                               convention = "ModifiedFollowing",
                               rule = "Backward") {
  Schedule(
    ql_date(effective_date),
    ql_date(maturity_date),
    tenor_obj,
    calendar_obj,
    convention,
    convention,
    rule,
    FALSE
  )
}

make_vanilla_tibor_swap <- function(curve_handle,
                                    effective_date,
                                    maturity_date,
                                    nominal = 1e8,
                                    fixed_rate = 0.0020,
                                    fixed_tenor = period_months(6),
                                    float_tenor = period_months(6),
                                    spread = 0.0) {
  fixed_schedule <- make_schedule_safe(effective_date, maturity_date, fixed_tenor)
  float_schedule <- make_schedule_safe(effective_date, maturity_date, float_tenor)
  
  tibor_index <- Tibor(period_months(6), curve_handle)
  
  swap_obj <- VanillaSwap(
    Swap_Payer_get(),
    nominal,
    fixed_schedule,
    fixed_rate,
    dc_a365,
    float_schedule,
    tibor_index,
    spread,
    dc_a365
  )
  
  engine_obj <- DiscountingSwapEngine(curve_handle)
  Instrument_setPricingEngine(swap_obj, engine_obj)
  
  list(
    swap = swap_obj,
    fixed_schedule = fixed_schedule,
    float_schedule = float_schedule,
    tibor_index = tibor_index,
    engine = engine_obj,
    effective_date = effective_date,
    maturity_date = maturity_date,
    spread = spread
  )
}

# ------------------------------------------------------------
# 3. Leg / cashflow helpers
# ------------------------------------------------------------

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

swap_cashflow_tbl <- function(swap_obj, curve_obj, leg = 1, day_count_obj = dc_a365) {
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

coupon_fixing_status_tbl <- function(swap_obj, curve_obj, index_obj, leg = 1, eval_date = NULL) {
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

# ------------------------------------------------------------
# 4. Market data and curve build
# ------------------------------------------------------------

curve_data <- list(
  c("depo", "6m", 0.13636),
  c("swap", "1y", 0.15249),
  c("swap", "18m", 0.18742),
  c("swap", "2y", 0.20541),
  c("swap", "3y", 0.23156),
  c("swap", "4y", 0.25653),
  c("swap", "5y", 0.28528),
  c("swap", "6y", 0.32341),
  c("swap", "7y", 0.36591)
)

curve_bundle <- make_tibor_curve(curve_data, trade_date = "2022-08-19")

cat("Trade date :", ql_iso(curve_bundle$trade_date), "\n")
cat("Settle date:", ql_iso(curve_bundle$settle_date), "\n")

show_tbl(curve_bundle$par_tbl, "Input market quotes")
show_tbl(curve_tbl(curve_bundle$curve), "Tibor curve table")

# ------------------------------------------------------------
# 5. Swap setup
# ------------------------------------------------------------

swap_bundle_base <- make_vanilla_tibor_swap(
  curve_handle = curve_bundle$curve_handle,
  effective_date = ql_iso(curve_bundle$settle_date),
  maturity_date = "2029-08-23",
  nominal = 1e8,
  fixed_rate = 0.0030,
  spread = 0.0
)

swap_bundle_spread <- make_vanilla_tibor_swap(
  curve_handle = curve_bundle$curve_handle,
  effective_date = ql_iso(curve_bundle$settle_date),
  maturity_date = "2029-08-23",
  nominal = 1e8,
  fixed_rate = 0.0030,
  spread = 0.0010
)

# ------------------------------------------------------------
# 6. Schedule check
# ------------------------------------------------------------

show_tbl(
  make_schedule_tbl_from_dates(
    effective_date = swap_bundle_base$effective_date,
    maturity_date = swap_bundle_base$maturity_date,
    tenor_months = 6
  ),
  "Fixed schedule",
  n = 20
)

show_tbl(
  make_schedule_tbl_from_dates(
    effective_date = swap_bundle_base$effective_date,
    maturity_date = swap_bundle_base$maturity_date,
    tenor_months = 6
  ),
  "Floating schedule",
  n = 20
)

# ------------------------------------------------------------
# 7. Fixing check BEFORE addFixing
# ------------------------------------------------------------

set_eval_date("2022-08-19")

float_fixing_check_before <- coupon_fixing_status_tbl(
  swap_obj = swap_bundle_spread$swap,
  curve_obj = curve_bundle$curve,
  index_obj = swap_bundle_spread$tibor_index,
  leg = 1,
  eval_date = "2022-08-19"
)

show_tbl(float_fixing_check_before, "Floating leg fixing check: BEFORE addFixing", n = 20)

# ------------------------------------------------------------
# 8. Register required fixing
# ------------------------------------------------------------

swap_bundle_spread$tibor_index$addFixing(
  ql_date("2022-08-19"),
  0.13636 / 100,
  TRUE
)

show_tbl(
  tibble(
    fixing_date = "2022-08-19",
    fixing_value = 0.13636 / 100
  ),
  "Added fixing"
)

# ------------------------------------------------------------
# 9. Valuation results
# ------------------------------------------------------------

cat("\nBase swap (no spread)\n")
cat("Swap NPV      :", swap_bundle_base$swap$NPV(), "\n")
cat("Swap fairRate :", swap_bundle_base$swap$fairRate(), "\n")
cat("Fixed leg NPV :", swap_bundle_base$swap$fixedLegNPV(), "\n")
cat("Float leg NPV :", swap_bundle_base$swap$floatingLegNPV(), "\n")

cat("\nSpread swap (+10bp spread)\n")
cat("Swap NPV      :", swap_bundle_spread$swap$NPV(), "\n")
cat("Swap fairRate :", swap_bundle_spread$swap$fairRate(), "\n")
cat("Fixed leg NPV :", swap_bundle_spread$swap$fixedLegNPV(), "\n")
cat("Float leg NPV :", swap_bundle_spread$swap$floatingLegNPV(), "\n")

show_tbl(
  tibble(
    case = c("base", "spread_10bp"),
    npv = c(swap_bundle_base$swap$NPV(), swap_bundle_spread$swap$NPV()),
    fair_rate = c(swap_bundle_base$swap$fairRate(), swap_bundle_spread$swap$fairRate()),
    float_leg_npv = c(swap_bundle_base$swap$floatingLegNPV(), swap_bundle_spread$swap$floatingLegNPV())
  ),
  "Spread effect summary"
)

# ------------------------------------------------------------
# 10. Cashflow diagnostics
# ------------------------------------------------------------

float_cf_tbl <- swap_cashflow_tbl(swap_bundle_spread$swap, curve_bundle$curve, leg = 1)
fixed_cf_tbl <- swap_cashflow_tbl(swap_bundle_spread$swap, curve_bundle$curve, leg = 0)

show_tbl(float_cf_tbl, "Floating leg cashflows", n = 20)
show_tbl(fixed_cf_tbl, "Fixed leg cashflows", n = 20)

cat("\nFloating leg PV (manual):", sum(float_cf_tbl$pv, na.rm = TRUE), "\n")
cat("Fixed leg PV (manual)   :", sum(fixed_cf_tbl$pv, na.rm = TRUE), "\n")

# ------------------------------------------------------------
# 11. Fixing diagnostics AFTER addFixing
# ------------------------------------------------------------

float_fixing_check_after <- coupon_fixing_status_tbl(
  swap_obj = swap_bundle_spread$swap,
  curve_obj = curve_bundle$curve,
  index_obj = swap_bundle_spread$tibor_index,
  leg = 1,
  eval_date = "2022-08-19"
)

show_tbl(float_fixing_check_after, "Floating leg fixing check: AFTER addFixing", n = 20)

# ------------------------------------------------------------
# 12. Time series view (if available)
# ------------------------------------------------------------

fixing_dates_tbl <- tryCatch({
  tibble(
    fixing_date = purrr::map_chr(swap_bundle_spread$tibor_index$timeSeries()$dates(), ql_iso),
    fixing_value = swap_bundle_spread$tibor_index$timeSeries()$values()
  )
}, error = function(e) tibble(note = "timeSeries extraction depends on SWIG build"))

show_tbl(fixing_dates_tbl, "Fixing time series", n = 20)

cat("\nch02 rewrite completed successfully.\n")

