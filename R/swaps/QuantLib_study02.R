suppressMessages(library(QuantLib))

## =========================================================
## 0. Utility functions
## =========================================================

show_df <- function(df, title = NULL, n = 10) {
  if (!is.null(title)) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
    cat(title, "\n")
    cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
  }
  print(utils::head(df, n))
}

show_cashflows <- function(data, title = NULL, n = 20) {
  df <- data.frame(
    date   = sapply(data, function(x) x$date),
    amount = sapply(data, function(x) x$amount),
    df     = sapply(data, function(x) x$df),
    pv     = sapply(data, function(x) x$pv)
  )
  show_df(df, title = title, n = n)
  invisible(df)
}

curve_dates_to_df <- function(curve) {
  curve_dates <- curve$dates()
  n_dates <- curve_dates$size()

  out <- NULL
  for (i in seq_len(n_dates)) {
    d <- curve_dates[i][[1]]
    out <- rbind(
      out,
      data.frame(
        date = Date_ISO(d),
        DF   = curve$discount(d)
      )
    )
  }
  out
}

curve_zeros_to_df <- function(curve, day_counter) {
  curve_dates <- curve$dates()
  n_dates <- curve_dates$size()

  out <- NULL
  for (i in seq_len(n_dates)) {
    d <- curve_dates[i][[1]]
    zr <- curve$zeroRate(d, day_counter, "Continuous")

    out <- rbind(
      out,
      data.frame(
        date = Date_ISO(d),
        zero_rate = InterestRate_rate(zr) * 100.0
      )
    )
  }
  out
}

build_ratehelper_vector <- function(helper_list) {
  v <- RateHelperVector()
  for (h in helper_list) {
    RateHelperVector_append(v, h)
  }
  v
}

plot_curve_zero_rates <- function(curve, max_years = 60, n_points = 720,
                                  main = "Zero curve") {
  ref_date <- curve$referenceDate()
  day_counter <- curve$dayCounter()

  times <- seq(0.0, max_years, length.out = n_points)
  rates <- sapply(times, function(t) {
    curve$zeroRate(t, "Continuous")$rate() * 100.0
  })

  plot(times, rates, type = "l",
       xlab = "Years", ylab = "Zero rate (%)",
       main = main)
}

cashflows_from_leg <- function(leg, discount_handle) {
  out <- list()
  for (i in seq_len(leg$size())) {
    cf <- leg[i][[1]]
    d  <- CashFlow_date(cf)
    a  <- CashFlow_amount(cf)
    z  <- discount_handle$discount(d)

    out[[i]] <- list(
      date   = Date_ISO(d),
      amount = a,
      df     = z,
      pv     = a * z
    )
  }
  out
}

## =========================================================
## 1. Evaluation date
## =========================================================

today <- Date(10, "May", 2022)
invisible(Settings_instance()$setEvaluationDate(d = today))

cat("Today:", today$`__str__`(), "\n")

## =========================================================
## 2. Build TONA index and register fixings
## =========================================================

tona <- OvernightIndex(
  "TONA",
  0,
  JPYCurrency(),
  Japan(),
  Actual365Fixed()
)

cat("TONA fixing calendar :", tona$fixingCalendar()$name(), "\n")
cat("TONA fixing days     :", tona$fixingDays(), "\n")
cat("TONA tenor           :", tona$tenor()$`__str__`(), "\n")
cat("TONA day counter     :", DayCounter_name(tona$dayCounter()), "\n")

Index_addFixing(tona, today, 0.04 / 100.0)

fixing_dates <- DateVector()
DateVector_append(fixing_dates, Date(9, "May", 2022))
DateVector_append(fixing_dates, Date(6, "May", 2022))

fixing_rates <- c(
  0.04 / 100.0,
  0.04 / 100.0
)

Index_addFixings(tona, fixing_dates, fixing_rates)

## =========================================================
## 3. Market data for TONA OIS curve
## =========================================================

depo_rates <- list(
  list(fixing_days = 0, quote = 0.04),  # O/N
  list(fixing_days = 1, quote = 0.04),  # T/N
  list(fixing_days = 2, quote = 0.04)   # S/N
)

ois_rates <- list(
  list(period = Period(1,  "Weeks"),  quote = 0.070),
  list(period = Period(2,  "Weeks"),  quote = 0.069),
  list(period = Period(3,  "Weeks"),  quote = 0.078),
  list(period = Period(1,  "Months"), quote = 0.074),
  list(period = Period(2,  "Months"), quote = 0.046),
  list(period = Period(3,  "Months"), quote = 0.016),
  list(period = Period(6,  "Months"), quote = -0.007),
  list(period = Period(9,  "Months"), quote = -0.013),
  list(period = Period(12, "Months"), quote = -0.014),
  list(period = Period(15, "Months"), quote = 0.002),
  list(period = Period(18, "Months"), quote = 0.008),
  list(period = Period(21, "Months"), quote = 0.021),
  list(period = Period(2,  "Years"),  quote = 0.036),
  list(period = Period(3,  "Years"),  quote = 0.127),
  list(period = Period(4,  "Years"),  quote = 0.274),
  list(period = Period(5,  "Years"),  quote = 0.456),
  list(period = Period(6,  "Years"),  quote = 0.647),
  list(period = Period(7,  "Years"),  quote = 0.827),
  list(period = Period(8,  "Years"),  quote = 0.996),
  list(period = Period(9,  "Years"),  quote = 1.147),
  list(period = Period(10, "Years"),  quote = 1.280),
  list(period = Period(11, "Years"),  quote = 1.404),
  list(period = Period(12, "Years"),  quote = 1.516),
  list(period = Period(15, "Years"),  quote = 1.764),
  list(period = Period(20, "Years"),  quote = 1.939),
  list(period = Period(25, "Years"),  quote = 2.003),
  list(period = Period(30, "Years"),  quote = 2.038)
)

## =========================================================
## 4. Build RateHelpers
## =========================================================

depo_helpers <- lapply(depo_rates, function(x) {
  DepositRateHelper(
    QuoteHandle(SimpleQuote(x$quote / 100.0)),
    tona$tenor(),
    x$fixing_days,
    tona$fixingCalendar(),
    tona$businessDayConvention(),
    tona$endOfMonth(),
    tona$dayCounter()
  )
})

ois_helpers <- lapply(ois_rates, function(x) {
  OISRateHelper(
    2,
    x$period,
    QuoteHandle(SimpleQuote(x$quote / 100.0)),
    tona
  )
})

helpers_vec <- build_ratehelper_vector(c(depo_helpers, ois_helpers))

## =========================================================
## 5. Build TONA curve
## =========================================================

curve_calendar <- Japan()
curve_day_counter <- Actual365Fixed()

tona_curve <- PiecewiseLogCubicDiscount(
  0,
  curve_calendar,
  helpers_vec,
  curve_day_counter
)

TermStructure_enableExtrapolation(tona_curve)

tona_curve_df <- curve_dates_to_df(tona_curve)
show_df(tona_curve_df, title = "TONA curve discount factors", n = 40)

tona_zero_df <- curve_zeros_to_df(tona_curve, curve_day_counter)
show_df(tona_zero_df, title = "TONA curve zero rates", n = 40)

plot_curve_zero_rates(tona_curve, main = "TONA zero curve")

## =========================================================
## 6. Build OIS trade on TONA curve
## =========================================================

tona_yts_handle <- YieldTermStructureHandle(tona_curve)

tona_with_curve <- OvernightIndex(
  "TONA", 0,
  JPYCurrency(),
  Japan(),
  Actual365Fixed(),
  tona_yts_handle
)

swap_engine <- DiscountingSwapEngine(tona_yts_handle)
Swap_Payer_get
Swap_Receiver_get
swap_side <- Swap_Payer_get()
nominal <- 10000000
fixed_rate <- 1.28 / 100.0
fixed_leg_day_counter <- Actual365Fixed()
spread <- 0.0
payment_lag <- 2
payment_adjustment <- "Following"
payment_calendar <- Japan()

spot_date <- Calendar_advance(curve_calendar, today, 2, "Days", "ModifiedFollowing")
maturity_date <- Calendar_advance(curve_calendar, spot_date, Period(10, "Years"), "Following")

payment_schedule <- Schedule(
  spot_date,
  maturity_date,
  Period(1, "Years"),
  Japan(),
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Backward",
  FALSE
)

ois_trade <- OvernightIndexedSwap(
  swap_side,
  nominal,
  payment_schedule,
  fixed_rate,
  fixed_leg_day_counter,
  tona_with_curve,
  spread,
  payment_lag,
  payment_adjustment,
  payment_calendar
)

Instrument_setPricingEngine(ois_trade, swap_engine)

cat("\nTONA OIS pricing results\n")
cat("NPV              :", Instrument_NPV(ois_trade), "\n")
cat("fairRate         :", ois_trade$fairRate(), "\n")
cat("NPV              :", ois_trade$NPV(), "\n")
cat("fixedLegNPV      :", ois_trade$fixedLegNPV(), "\n")
cat("overnightLegNPV  :", ois_trade$overnightLegNPV(), "\n")

## =========================================================
## 7. Cashflow tables
## =========================================================

fixed_leg <- ois_trade$fixedLeg()
overnight_leg <- ois_trade$overnightLeg()

fixed_cf_df <- show_cashflows(
  cashflows_from_leg(fixed_leg, tona_yts_handle),
  title = "TONA OIS fixed leg cashflows"
)

overnight_cf_df <- show_cashflows(
  cashflows_from_leg(overnight_leg, tona_yts_handle),
  title = "TONA OIS overnight leg cashflows"
)

## =========================================================
## 8. Rebuild a second TONA curve with reduced market data
## =========================================================

ois_rates_2 <- list(
  list(period = Period(1,  "Weeks"),  quote = 0.070),
  list(period = Period(2,  "Weeks"),  quote = 0.069),
  list(period = Period(3,  "Weeks"),  quote = 0.078),
  list(period = Period(1,  "Months"), quote = 0.074),
  list(period = Period(2,  "Months"), quote = 0.046),
  list(period = Period(3,  "Months"), quote = 0.016),
  list(period = Period(6,  "Months"), quote = -0.007),
  list(period = Period(9,  "Months"), quote = -0.013),
  list(period = Period(12, "Months"), quote = -0.014),
  list(period = Period(15, "Months"), quote = 0.002),
  list(period = Period(18, "Months"), quote = 0.008),
  list(period = Period(21, "Months"), quote = 0.021),
  list(period = Period(2,  "Years"),  quote = 0.036),
  list(period = Period(3,  "Years"),  quote = 0.127),
  list(period = Period(5,  "Years"),  quote = 0.456),
  list(period = Period(7,  "Years"),  quote = 0.827),
  list(period = Period(10, "Years"),  quote = 1.280),
  list(period = Period(11, "Years"),  quote = 1.404),
  list(period = Period(12, "Years"),  quote = 1.516),
  list(period = Period(15, "Years"),  quote = 1.764),
  list(period = Period(20, "Years"),  quote = 1.939),
  list(period = Period(25, "Years"),  quote = 2.003),
  list(period = Period(30, "Years"),  quote = 2.038)
)

ois_helpers_2 <- lapply(ois_rates_2, function(x) {
  OISRateHelper(
    2,
    x$period,
    QuoteHandle(SimpleQuote(x$quote / 100.0)),
    tona
  )
})

helpers_vec_2 <- build_ratehelper_vector(c(depo_helpers, ois_helpers_2))

tona_curve_2 <- PiecewiseLogCubicDiscount(
  0,
  curve_calendar,
  helpers_vec_2,
  curve_day_counter
)

TermStructure_enableExtrapolation(tona_curve_2)

## =========================================================
## 9. Relinkable handle and repricing
## =========================================================

tona_yts_relink_handle <- RelinkableYieldTermStructureHandle()
RelinkableYieldTermStructureHandle_linkTo(tona_yts_relink_handle, tona_curve)

tona_with_curve_relink <- OvernightIndex(
  "TONA", 0,
  JPYCurrency(),
  Japan(),
  Actual365Fixed(),
  tona_yts_relink_handle
)

swap_engine_relink <- DiscountingSwapEngine(tona_yts_relink_handle)

ois_trade_relink <- OvernightIndexedSwap(
  swap_side,
  nominal,
  payment_schedule,
  fixed_rate,
  fixed_leg_day_counter,
  tona_with_curve_relink,
  spread,
  payment_lag,
  payment_adjustment,
  payment_calendar
)

Instrument_setPricingEngine(ois_trade_relink, swap_engine_relink)

cat("\nRelinkable handle test\n")
cat("old curve NPV :", Instrument_NPV(ois_trade_relink), "\n")

RelinkableYieldTermStructureHandle_linkTo(tona_yts_relink_handle, tona_curve_2)
cat("new curve NPV :", Instrument_NPV(ois_trade_relink), "\n")

RelinkableYieldTermStructureHandle_linkTo(tona_yts_relink_handle, tona_curve)
cat("reset curve NPV :", Instrument_NPV(ois_trade_relink), "\n")

## =========================================================
## 10. Amortizing OIS
## =========================================================

nominals <- c(
  10000000, 9000000, 8000000, 7000000, 6000000,
  5000000, 4000000, 3000000, 2000000, 1000000
)

amortized_ois_trade <- OvernightIndexedSwap(
  swap_side,
  nominals,
  payment_schedule,
  fixed_rate,
  fixed_leg_day_counter,
  tona_with_curve_relink,
  spread,
  payment_lag,
  payment_adjustment,
  payment_calendar
)

Instrument_setPricingEngine(amortized_ois_trade, swap_engine_relink)

cat("\nAmortizing OIS\n")
cat("NPV:", Instrument_NPV(amortized_ois_trade), "\n")

cat("fixed leg cashflows\n")
for (i in seq_len(amortized_ois_trade$fixedLeg()$size())) {
  cf <- amortized_ois_trade$fixedLeg()[i][[1]]
  cat(CashFlow_amount(cf), "\n")
}

cat("overnight leg cashflows\n")
for (i in seq_len(amortized_ois_trade$overnightLeg()$size())) {
  cf <- amortized_ois_trade$overnightLeg()[i][[1]]
  cat(CashFlow_amount(cf), "\n")
}

## =========================================================
## 11. NonStandardSwap
## =========================================================

fixed_rates <- c(
  0.0128, 0.0130, 0.0132, 0.0134, 0.0136,
  0.0138, 0.0140, 0.0142, 0.0144, 0.0146
)

gearings <- rep(1.0, length(nominals))
spreads_nonstd <- rep(0.0, length(nominals))

non_std_swap <- NonstandardSwap(
  Swap_Payer_get(),
  nominals,
  nominals,
  payment_schedule,
  fixed_rates,
  fixed_leg_day_counter,
  payment_schedule,
  tona_with_curve_relink,
  gearings,
  spreads_nonstd,
  tona_with_curve_relink$dayCounter()
)

Instrument_setPricingEngine(non_std_swap, swap_engine_relink)

cat("\nNonStandardSwap\n")
cat("NPV      :", Instrument_NPV(non_std_swap), "\n")
cat("leg 0 NPV:", Swap_legNPV(non_std_swap, 0), "\n")
cat("leg 1 NPV:", Swap_legNPV(non_std_swap, 1), "\n")
cat("sum legs :", Swap_legNPV(non_std_swap, 0) + Swap_legNPV(non_std_swap, 1), "\n")

cat("fixed leg cashflows\n")
for (i in seq_len(non_std_swap$fixedLeg()$size())) {
  cf <- non_std_swap$fixedLeg()[i][[1]]
  cat(CashFlow_amount(cf), "\n")
}

cat("floating leg cashflows\n")
for (i in seq_len(non_std_swap$floatingLeg()$size())) {
  cf <- non_std_swap$floatingLeg()[i][[1]]
  cat(CashFlow_amount(cf), "\n")
}

## =========================================================
## 12. SOFR example
## =========================================================

sofr <- Sofr()

Index_addFixing(sofr, today, 1.10 / 100.0)

sofr_fixing_dates <- DateVector()
DateVector_append(sofr_fixing_dates, Date(9, "May", 2022))
DateVector_append(sofr_fixing_dates, Date(6, "May", 2022))

sofr_fixing_rates <- c(
  1.10 / 100.0,
  1.10 / 100.0
)

Index_addFixings(sofr, sofr_fixing_dates, sofr_fixing_rates)

sofr_depo_rates <- list(
  list(fixing_days = 0, tenor = Period(1, "Days"), quote = 1.10),
  list(fixing_days = 1, tenor = Period(1, "Days"), quote = 1.10)
)

sofr_ois_rates <- list(
  list(fixing_days = 2, tenor = Period(1, "Weeks"),  quote = 1.245),
  list(fixing_days = 2, tenor = Period(2, "Weeks"),  quote = 1.269),
  list(fixing_days = 2, tenor = Period(3, "Weeks"),  quote = 1.277),
  list(fixing_days = 2, tenor = Period(1, "Months"), quote = 1.281),
  list(fixing_days = 2, tenor = Period(2, "Months"), quote = 1.180),
  list(fixing_days = 2, tenor = Period(3, "Months"), quote = 1.143),
  list(fixing_days = 2, tenor = Period(5, "Years"),  quote = 2.523),
  list(fixing_days = 2, tenor = Period(10, "Years"), quote = 3.380),
  list(fixing_days = 2, tenor = Period(30, "Years"), quote = 3.369)
)

sofr_depo_helpers <- lapply(sofr_depo_rates, function(x) {
  DepositRateHelper(
    QuoteHandle(SimpleQuote(x$quote / 100.0)),
    x$tenor,
    x$fixing_days,
    sofr$fixingCalendar(),
    sofr$businessDayConvention(),
    sofr$endOfMonth(),
    sofr$dayCounter()
  )
})

sofr_ois_helpers <- lapply(sofr_ois_rates, function(x) {
  OISRateHelper(
    x$fixing_days,
    x$tenor,
    QuoteHandle(SimpleQuote(x$quote / 100.0)),
    sofr
  )
})

sofr_helpers_vec <- build_ratehelper_vector(c(sofr_depo_helpers, sofr_ois_helpers))

sofr_curve <- PiecewiseLogLinearDiscount(
  0,
  sofr$fixingCalendar(),
  sofr_helpers_vec,
  Actual365Fixed()
)

TermStructure_enableExtrapolation(sofr_curve)

sofr_curve_df <- curve_dates_to_df(sofr_curve)
show_df(sofr_curve_df, title = "SOFR curve discount factors", n = 40)

## =========================================================
## 13. Sensitivity example with ESTR + SimpleQuotes
## =========================================================

today2 <- Date(25, "May", 2022)
invisible(Settings_instance()$setEvaluationDate(d = today2))

quotes <- list()

estr <- Estr()
Index_addFixing(estr, today2, 0.312 / 100.0)

depo_rates_estr <- list(
  list(fixing_days = 0, quote = 0.312),
  list(fixing_days = 1, quote = 0.312),
  list(fixing_days = 2, quote = 0.312)
)

ois_rates_estr <- list(
  list(period = Period(1,  "Months"), quote = 0.293),
  list(period = Period(2,  "Months"), quote = 0.272),
  list(period = Period(3,  "Months"), quote = 0.260),
  list(period = Period(4,  "Months"), quote = 0.256),
  list(period = Period(5,  "Months"), quote = 0.252),
  list(period = Period(6,  "Months"), quote = 0.248),
  list(period = Period(7,  "Months"), quote = 0.254),
  list(period = Period(8,  "Months"), quote = 0.261),
  list(period = Period(9,  "Months"), quote = 0.267),
  list(period = Period(10, "Months"), quote = 0.279),
  list(period = Period(11, "Months"), quote = 0.291),
  list(period = Period(1,  "Years"),  quote = 0.303),
  list(period = Period(15, "Months"), quote = 0.318),
  list(period = Period(18, "Months"), quote = 0.338),
  list(period = Period(21, "Months"), quote = 0.351),
  list(period = Period(2,  "Years"),  quote = 0.369),
  list(period = Period(3,  "Years"),  quote = 0.424),
  list(period = Period(4,  "Years"),  quote = 0.576),
  list(period = Period(5,  "Years"),  quote = 0.762),
  list(period = Period(6,  "Years"),  quote = 0.954),
  list(period = Period(7,  "Years"),  quote = 1.135),
  list(period = Period(8,  "Years"),  quote = 1.303),
  list(period = Period(9,  "Years"),  quote = 1.452),
  list(period = Period(10, "Years"),  quote = 1.584),
  list(period = Period(12, "Years"),  quote = 1.809),
  list(period = Period(15, "Years"),  quote = 2.037),
  list(period = Period(20, "Years"),  quote = 2.187),
  list(period = Period(25, "Years"),  quote = 2.234),
  list(period = Period(30, "Years"),  quote = 2.256),
  list(period = Period(35, "Years"),  quote = 2.295),
  list(period = Period(40, "Years"),  quote = 2.348),
  list(period = Period(50, "Years"),  quote = 2.421),
  list(period = Period(60, "Years"),  quote = 2.463)
)

depo_helpers_estr <- list()
for (x in depo_rates_estr) {
  q <- SimpleQuote(x$quote / 100.0)
  quotes[[length(quotes) + 1]] <- q

  depo_helpers_estr[[length(depo_helpers_estr) + 1]] <- DepositRateHelper(
    QuoteHandle(q),
    estr$tenor(),
    x$fixing_days,
    estr$fixingCalendar(),
    estr$businessDayConvention(),
    estr$endOfMonth(),
    estr$dayCounter()
  )
}

ois_helpers_estr <- list()
for (x in ois_rates_estr) {
  q <- SimpleQuote(x$quote / 100.0)
  quotes[[length(quotes) + 1]] <- q

  ois_helpers_estr[[length(ois_helpers_estr) + 1]] <- OISRateHelper(
    2,
    x$period,
    QuoteHandle(q),
    estr
  )
}

helpers_vec_estr <- build_ratehelper_vector(c(depo_helpers_estr, ois_helpers_estr))

ois_curve <- PiecewiseLogCubicDiscount(
  0,
  TARGET(),
  helpers_vec_estr,
  Actual365Fixed()
)
TermStructure_enableExtrapolation(ois_curve)

ois_curve_handle <- RelinkableYieldTermStructureHandle()
RelinkableYieldTermStructureHandle_linkTo(ois_curve_handle, ois_curve)

estr_with_curve <- Estr(ois_curve_handle)
swap_engine_estr <- DiscountingSwapEngine(ois_curve_handle)

start_date <- Date(4, "August", 2022)
end_date   <- Date(4, "August", 2032)

payment_schedule_estr <- Schedule(
  start_date,
  end_date,
  Period(1, "Years"),
  TARGET(),
  "Following",
  "Following",
  "Backward",
  FALSE
)

ois <- OvernightIndexedSwap(
  Swap_Receiver_get(),
  100000,
  payment_schedule_estr,
  0.017,
  Actual360(),
  estr_with_curve,
  0.0,
  2,
  "Following",
  TARGET()
)

Instrument_setPricingEngine(ois, swap_engine_estr)

cat("\nESTR OIS base NPV:", Instrument_NPV(ois), "\n")

bp <- 1.0e-4

raw_quote <- quotes[[7]]$value()
cat("Before single shift:", Instrument_NPV(ois), "\n")

SimpleQuote_setValue(quotes[[7]], raw_quote + bp)
cat("After +1bp quote[7]:", Instrument_NPV(ois), "\n")

SimpleQuote_setValue(quotes[[7]], raw_quote)
cat("Reset:", Instrument_NPV(ois), "\n")

cat("Before parallel shift:", Instrument_NPV(ois), "\n")
for (q in quotes) {
  q$setValue(q$value() + bp)
}
cat("After parallel +1bp:", Instrument_NPV(ois), "\n")
for (q in quotes) {
  q$setValue(q$value() - bp)
}
cat("Reset:", Instrument_NPV(ois), "\n")

## =========================================================
## 14. Zero-spreaded curve
## =========================================================

base_curve <- YieldTermStructureHandle(ois_curve)

spread_quote <- SimpleQuote(bp)
zero_spread_curve <- ZeroSpreadedTermStructure(
  base_curve,
  QuoteHandle(spread_quote)
)
TermStructure_enableExtrapolation(zero_spread_curve)

cat("\nZero spreaded curve\n")
cat("Base NPV:", Instrument_NPV(ois), "\n")
RelinkableYieldTermStructureHandle_linkTo(ois_curve_handle, zero_spread_curve)
cat("Zero-spreaded NPV:", Instrument_NPV(ois), "\n")

SimpleQuote_setValue(spread_quote, 5 * bp)
cat("Zero-spreaded +5bp NPV:", Instrument_NPV(ois), "\n")

SimpleQuote_setValue(spread_quote, bp)
RelinkableYieldTermStructureHandle_linkTo(ois_curve_handle, ois_curve)
cat("Reset curve NPV:", Instrument_NPV(ois), "\n")

## =========================================================
## 15. Forward-spreaded curve
## =========================================================

fwd_spread_quote <- SimpleQuote(bp)
fwd_spread_curve <- ForwardSpreadedTermStructure(
  base_curve,
  QuoteHandle(fwd_spread_quote)
)

RelinkableYieldTermStructureHandle_linkTo(ois_curve_handle, fwd_spread_curve)
cat("\nForward-spreaded NPV:", Instrument_NPV(ois), "\n")

RelinkableYieldTermStructureHandle_linkTo(ois_curve_handle, ois_curve)
cat("Reset:", Instrument_NPV(ois), "\n")

