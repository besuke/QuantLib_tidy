suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch04.R
# ------------------------------------------------------------
# Bonds
# Python notebook ch04.ipynb を R + QuantLib(SWIG) 向けに
# tidyverse 風の流れで書き直した版。
#
# 内容:
# 1. FixedRateBond の基本評価
# 2. Price <-> Yield
# 3. Duration / BPV / Convexity
# 4. Cashflow table
# 5. TONA curve による discounting
# 6. z-spread / zero-spreaded curve
# 7. AS spread hand calculation
# 8. AssetSwap
#
# SWIG-safe 方針:
# - Date は DateParser_parseISO() を使う
# - zeroRate() は避け、discount() から計算する箇所を優先
# - cashflow / coupon は tryCatch を多めに使う
# - bondYield は build によって bondYield / yield が揺れるので wrapper を使う
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

make_date_vector <- function(x) {
  dv <- DateVector()
  purrr::walk(x, ~ DateVector_append(dv, ql_date(.x)))
  dv
}

# enums / calendars / counters
jp_calendar <- Japan()
dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()
dc_30 <- Thirty360("BondBasis")
cmpd_compounded <- Compounding_Compounded_get()
cmpd_simple     <- Compounding_Simple_get()
freq_annual      <- Frequency_Annual_get()
freq_semiannual  <- Frequency_Semiannual_get()

# clean price wrapper for builds where BondPrice object is required
bond_clean_price_arg <- function(x) {
  out <- tryCatch(BondPrice(x, "Clean"), error = function(e) NULL)
  if (!is.null(out)) return(out)
  x
}

bond_yield_safe <- function(bond_obj, clean_price, day_count_obj, compounding, frequency) {
  cp <- bond_clean_price_arg(clean_price)
  
  out <- tryCatch(
    bond_obj$bondYield(cp, day_count_obj, compounding, frequency),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  out <- tryCatch(
    BondFunctions_bondYield(bond_obj, cp, day_count_obj, compounding, frequency),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  out <- tryCatch(
    bond_obj$yield(cp, day_count_obj, compounding, frequency),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  stop("Could not compute bond yield in this SWIG build")
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

bond_cashflow_tbl <- function(bond_obj, yts = NULL, settle_date = NULL, dc_for_t = dc_a365) {
  cf_leg <- bond_obj$cashflows()
  
  if (is.null(settle_date)) {
    settle_date <- tryCatch(bond_obj$settlementDate(), error = function(e) Settings_instance()$evaluationDate())
  }
  
  settle_r <- as.Date(ql_iso(settle_date))
  
  purrr::map_dfr(seq_len(cf_leg$size()), function(i) {
    cf_obj <- leg_cashflow_at(cf_leg, i)
    pay_date_obj <- CashFlow_date(cf_obj)
    pay_date_chr <- ql_iso(pay_date_obj)
    pay_date_r <- as.Date(pay_date_chr)
    
    coupon_obj <- tryCatch(as_coupon(cf_obj), error = function(e) NULL)
    
    amount_value <- tryCatch(CashFlow_amount(cf_obj), error = function(e) NA_real_)
    
    df_value <- {
      if (!is.null(yts)) {
        tryCatch(yts$discount(pay_date_obj), error = function(e) NA_real_)
      } else {
        if (pay_date_r <= settle_r) 1 else NA_real_
      }
    }
    
    tibble(
      pay_date = pay_date_chr,
      coupon = if (!is.null(coupon_obj)) tryCatch(coupon_obj$rate(), error = function(e) NA_real_) else NA_real_,
      accrual_start = if (!is.null(coupon_obj)) ql_iso(Coupon_accrualStartDate(coupon_obj)) else NA_character_,
      accrual_end = if (!is.null(coupon_obj)) ql_iso(Coupon_accrualEndDate(coupon_obj)) else NA_character_,
      amount = amount_value,
      DF = df_value,
      pv = amount_value * df_value,
      flow_type = if (!is.null(coupon_obj)) "coupon" else "principal"
    )
  }) %>%
    mutate(pay_date_r = as.Date(pay_date)) %>%
    arrange(pay_date_r) %>%
    select(-pay_date_r)
}

# ------------------------------------------------------------
# 1. Bond basic setup
# ------------------------------------------------------------

trade_date <- ql_date("2022-08-19")
settlement_days <- 2
effective_date <- ql_date("2022-07-28")
maturity_date <- ql_date("2025-07-28")
face_amount <- 100.0
coupon_list <- c(0.00370)

set_eval_date(trade_date)
settle_date <- advance_days(jp_calendar, trade_date, settlement_days)

bond_schedule <- Schedule(
  effective_date,
  maturity_date,
  period_months(6),
  jp_calendar,
  "Unadjusted",
  "Unadjusted",
  "Backward",
  FALSE
)

bond_obj <- FixedRateBond(
  settlement_days,
  face_amount,
  bond_schedule,
  coupon_list,
  dc_30
)

show_tbl(make_schedule_tbl(bond_schedule), "Bond schedule", n = 20)

# ------------------------------------------------------------
# 2. Price -> Yield
# ------------------------------------------------------------

clean_price_input <- 97.0
price_to_yield <- bond_yield_safe(
  bond_obj,
  clean_price_input,
  dc_30,
  cmpd_compounded,
  freq_semiannual
)

cat("Price 97.0 -> yield :", sprintf("%.6f%%", 100 * price_to_yield), "\n")

# ------------------------------------------------------------
# 3. Yield -> Price
# ------------------------------------------------------------

prev_coupon_date <- bond_schedule$previousDate(settle_date)
accrual_days <- dc_30$dayCount(prev_coupon_date, settle_date)

accrued_amount <- bond_obj$accruedAmount()
clean_price_from_yield <- bond_obj$cleanPrice(price_to_yield, dc_30, cmpd_compounded, freq_semiannual)
dirty_price_from_yield <- bond_obj$dirtyPrice(price_to_yield, dc_30, cmpd_compounded, freq_semiannual)

show_tbl(
  tibble(
    item = c(
      "settle_date",
      "previous_coupon_date",
      "accrual_days",
      "accrued_amount",
      "clean_price",
      "dirty_price"
    ),
    value = c(
      ql_iso(settle_date),
      ql_iso(prev_coupon_date),
      as.character(accrual_days),
      sprintf("%.8f", accrued_amount),
      sprintf("%.8f", clean_price_from_yield),
      sprintf("%.8f", dirty_price_from_yield)
    )
  ),
  "Yield -> price summary",
  n = 20
)

# ------------------------------------------------------------
# 4. Risk measures
# ------------------------------------------------------------

interest_rate_obj <- InterestRate(
  price_to_yield,
  dc_30,
  cmpd_compounded,
  freq_semiannual
)

mac_dur <- BondFunctions_duration(bond_obj, interest_rate_obj, "Macaulay")
mod_dur <- BondFunctions_duration(bond_obj, interest_rate_obj, "Modified")
bpv <- BondFunctions_basisPointValue(bond_obj, interest_rate_obj)
convexity_value <- BondFunctions_convexity(bond_obj, interest_rate_obj)

show_tbl(
  tibble(
    metric = c("macaulay_duration", "modified_duration", "bpv", "convexity"),
    value = c(mac_dur, mod_dur, bpv, convexity_value)
  ),
  "Bond risk measures",
  n = 20
)

# hand calculations
price_up_1bp <- bond_obj$dirtyPrice(price_to_yield + 0.0001, dc_30, cmpd_compounded, freq_semiannual)
price_dn_1bp <- bond_obj$dirtyPrice(price_to_yield - 0.0001, dc_30, cmpd_compounded, freq_semiannual)
hand_bpv <- (price_up_1bp - price_dn_1bp) / 2
hand_mod_dur <- -bpv * 100 / dirty_price_from_yield
hand_convexity <- ((price_up_1bp - dirty_price_from_yield) - (dirty_price_from_yield - price_dn_1bp)) * 10000 / dirty_price_from_yield
price_up_100bp <- bond_obj$dirtyPrice(price_to_yield + 0.01, dc_30, cmpd_compounded, freq_semiannual)
delta_est <- -mod_dur / 100 * dirty_price_from_yield
gamma_est <- convexity_value / 10000 * dirty_price_from_yield
price_delta_gamma <- dirty_price_from_yield + delta_est + 0.5 * gamma_est

show_tbl(
  tibble(
    metric = c(
      "hand_bpv",
      "hand_modified_duration",
      "hand_convexity",
      "price_up_100bp",
      "delta_gamma_estimate"
    ),
    value = c(hand_bpv, hand_mod_dur, hand_convexity, price_up_100bp, price_delta_gamma)
  ),
  "Hand calculations",
  n = 20
)

# ------------------------------------------------------------
# 5. Bond cashflow table under flat yield
# ------------------------------------------------------------

flat_yield_handle <- YieldTermStructureHandle(
  FlatForward(settle_date, price_to_yield, dc_30, cmpd_compounded, freq_semiannual)
)
flat_engine <- DiscountingBondEngine(flat_yield_handle)
Instrument_setPricingEngine(bond_obj, flat_engine)

bond_cf_tbl_flat <- bond_cashflow_tbl(
  bond_obj,
  yts = flat_yield_handle,
  settle_date = settle_date,
  dc_for_t = dc_30
)

show_tbl(bond_cf_tbl_flat, "Bond cashflow table (flat yield discounting)", n = 20)
cat("Hand dirty price from CF table:", sprintf("%.8f", sum(bond_cf_tbl_flat$pv, na.rm = TRUE)), "\n")

macaulay_hand <- {
  cf_future <- bond_cf_tbl_flat %>% filter(!is.na(pv), pay_date > ql_iso(settle_date))
  year_frac <- purrr::map_dbl(cf_future$pay_date, ~ dc_30$yearFraction(settle_date, ql_date(.x)))
  sum(year_frac * cf_future$pv) / dirty_price_from_yield
}
cat("Macaulay duration (hand):", sprintf("%.8f", macaulay_hand), "\n")

bond_cf_2025_tbl <- bond_cf_tbl_flat %>%
  filter(as.Date(pay_date) >= as.Date("2025-01-01"))
show_tbl(bond_cf_2025_tbl, "Bond cashflows from 2025 onward", n = 20)

# ------------------------------------------------------------
# 6. TONA curve for spread analysis
# ------------------------------------------------------------

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
  c("swap", "1m", -0.01807),
  c("swap", "6m", -0.01043),
  c("swap", "12m", 0.01250),
  c("swap", "18m", 0.03125),
  c("swap", "2y", 0.04875),
  c("swap", "3y", 0.07375),
  c("swap", "5y", 0.11854),
  c("swap", "7y", 0.19146)
)

tona_bundle <- make_tona_curve(tona_curve_data, trade_date = "2022-08-19")
show_tbl(tona_bundle$quote_tbl, "TONA input quotes")
show_tbl(curve_tbl(tona_bundle$curve), "TONA curve table")

# rough interpolated TONA par-rate proxy at bond maturity using time-based interpolation
curve_proxy_tbl <- curve_tbl(tona_bundle$curve, n = 500)
mat_time <- dc_a365$yearFraction(tona_bundle$curve$referenceDate(), maturity_date)
interp_rate <- approx(curve_proxy_tbl$time, curve_proxy_tbl$zero, xout = mat_time, rule = 2)$y

cat(
  "Interpolated TONA rate at maturity:", sprintf("%.6f%%", 100 * interp_rate),
  ", bond yield:", sprintf("%.6f%%", 100 * price_to_yield),
  ", I-spread:", sprintf("%.6f%%", 100 * (price_to_yield - interp_rate)),
  "\n"
)

# ------------------------------------------------------------
# 7. Discounting with flat forward and TONA curve
# ------------------------------------------------------------

flat_forward_curve <- FlatForward(settle_date, 0.01418713, dc_30, cmpd_compounded, freq_semiannual)
flat_forward_handle <- YieldTermStructureHandle(flat_forward_curve)
flat_forward_engine <- DiscountingBondEngine(flat_forward_handle)
Instrument_setPricingEngine(bond_obj, flat_forward_engine)
cat("Dirty price with 1.418713% flat yield:", sprintf("%.6f", bond_obj$NPV()), "\n")

tona_engine <- DiscountingBondEngine(tona_bundle$curve_handle)
Instrument_setPricingEngine(bond_obj, tona_engine)
tona_discount_price <- bond_obj$NPV()
cat("Dirty price discounted on TONA curve:", sprintf("%.6f", tona_discount_price), "\n")

# ------------------------------------------------------------
# 8. z-spread / zero-spreaded curve
# ------------------------------------------------------------

z_spread_value <- BondFunctions_zSpread(
  bond_obj,
  bond_clean_price_arg(97.0),
  tona_bundle$curve,
  dc_30,
  cmpd_compounded,
  freq_semiannual
)
cat("z-spread for clean price 97.0:", sprintf("%.6f%%", 100 * z_spread_value), "\n")

zero_spread_curve <- ZeroSpreadedTermStructure(
  tona_bundle$curve_handle,
  simple_quote_handle(z_spread_value),
  cmpd_compounded,
  freq_annual,
  dc_a365
)
zero_spread_handle <- YieldTermStructureHandle(zero_spread_curve)
zero_spread_engine <- DiscountingBondEngine(zero_spread_handle)
Instrument_setPricingEngine(bond_obj, zero_spread_engine)
cat("Dirty price on TONA + z-spread curve:", sprintf("%.6f", bond_obj$NPV()), "\n")

bond_cf_tbl_spread <- bond_cashflow_tbl(
  bond_obj,
  yts = zero_spread_handle,
  settle_date = settle_date,
  dc_for_t = dc_a365
)
show_tbl(bond_cf_tbl_spread, "Bond cashflow table (TONA + z-spread)", n = 20)
cat("Hand price on TONA + z-spread:", sprintf("%.6f", sum(bond_cf_tbl_spread$pv, na.rm = TRUE)), "\n")

# ------------------------------------------------------------
# 9. AS spread scenarios
# ------------------------------------------------------------

calc_as_spread_from_price <- function(target_dirty_price, discount_price, annuity) {
  (discount_price - target_dirty_price) / annuity
}

Instrument_setPricingEngine(bond_obj, tona_engine)

bond_cf_tbl_tona <- bond_cashflow_tbl(
  bond_obj,
  yts = tona_bundle$curve_handle,
  settle_date = settle_date,
  dc_for_t = dc_a365
) %>%
  filter(flow_type == "coupon")

yr_frac_vec <- purrr::map_dbl(
  bond_cf_tbl_tona$pay_date,
  ~ dc_a365$yearFraction(settle_date, ql_date(.x))
)

tenor_flow <- diff(c(0, yr_frac_vec))
tona_annuity <- sum(bond_cf_tbl_tona$DF * tenor_flow, na.rm = TRUE)

benchmark_dirty_price <- 90.0256

as_spread_input_tbl <- tibble(
  price_label = c(
    "model_dirty_price",
    "benchmark_dirty_price"
  ),
  target_dirty_price = c(
    dirty_price_from_yield,
    benchmark_dirty_price
  )
)

as_spread_result_tbl <- as_spread_input_tbl %>%
  mutate(
    discount_price = tona_discount_price,
    annuity = tona_annuity,
    as_spread = purrr::map_dbl(
      target_dirty_price,
      ~ calc_as_spread_from_price(
        target_dirty_price = .x,
        discount_price = tona_discount_price,
        annuity = tona_annuity
      )
    )
  )

as_spread_hand <- as_spread_result_tbl %>%
  filter(price_label == "model_dirty_price") %>%
  pull(as_spread)

show_tbl(
  tibble(
    metric = c("tona_discount_price", "tona_annuity"),
    value = c(tona_discount_price, tona_annuity)
  ),
  "AS spread base inputs",
  n = 20
)

show_tbl(
  as_spread_result_tbl,
  "AS spread scenarios",
  n = 20
)

# ------------------------------------------------------------
# 10. AssetSwap (build-dependent)
# ------------------------------------------------------------

floating_schedule_asw <- Schedule(
  settle_date,
  maturity_date,
  period_years(1),
  jp_calendar,
  "Unadjusted",
  "Unadjusted",
  "Backward",
  FALSE
)

clean_price_asw <- 97.0
credit_spread_asw <- 1.3 / 100
is_par_asset_swap <- TRUE
pay_fixed_rate <- TRUE

show_tbl(
  make_schedule_tbl(floating_schedule_asw),
  "AssetSwap floating schedule",
  n = 20
)

tibor6m_for_asw <- Tibor(period_months(6), tona_bundle$curve_handle)

try_make_asset_swap <- function() {
  candidate_builders <- list(
    function() {
      AssetSwap(
        pay_fixed_rate,
        bond_obj,
        clean_price_asw,
        tibor6m_for_asw,
        credit_spread_asw,
        floating_schedule_asw,
        dc_a365,
        is_par_asset_swap
      )
    },
    function() {
      AssetSwap(
        pay_fixed_rate,
        bond_obj,
        clean_price_asw,
        tibor6m_for_asw,
        credit_spread_asw,
        floating_schedule_asw,
        is_par_asset_swap
      )
    },
    function() {
      AssetSwap(
        pay_fixed_rate,
        bond_obj,
        clean_price_asw,
        tibor6m_for_asw,
        credit_spread_asw,
        is_par_asset_swap
      )
    },
    function() {
      AssetSwap(
        bond_obj,
        clean_price_asw,
        tibor6m_for_asw,
        credit_spread_asw,
        floating_schedule_asw,
        dc_a365,
        pay_fixed_rate,
        is_par_asset_swap
      )
    }
  )
  
  for (builder in candidate_builders) {
    obj <- tryCatch(builder(), error = function(e) NULL)
    if (!is.null(obj)) {
      return(obj)
    }
  }
  
  NULL
}

asset_swap_obj <- try_make_asset_swap()

if (is.null(asset_swap_obj)) {
  show_tbl(
    tibble(
      note = "AssetSwap constructor is not available in this SWIG build; skipped",
      fallback_metric = c("as_spread_model_dirty", "as_spread_benchmark_dirty"),
      fallback_value = c(
        as_spread_result_tbl %>%
          filter(price_label == "model_dirty_price") %>%
          pull(as_spread),
        as_spread_result_tbl %>%
          filter(price_label == "benchmark_dirty_price") %>%
          pull(as_spread)
      )
    ),
    "AssetSwap demo",
    n = 20
  )
} else {
  asset_swap_engine <- DiscountingSwapEngine(tona_bundle$curve_handle)
  Instrument_setPricingEngine(asset_swap_obj, asset_swap_engine)
  
  cat(
    "Asset swap fair spread for clean price ",
    sprintf("%.2f", clean_price_asw),
    ": ",
    sprintf("%.6f%%", 100 * asset_swap_obj$fairSpread()),
    "\n",
    sep = ""
  )
  
  cat(
    "Fair clean price for spread ",
    sprintf("%.6f%%", 100 * credit_spread_asw),
    ": ",
    sprintf("%.6f", asset_swap_obj$fairCleanPrice()),
    "\n",
    sep = ""
  )
}

cat("\nch04 bonds rewrite completed successfully.\n")

