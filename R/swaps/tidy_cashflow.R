suppressPackageStartupMessages({
  library(QuantLib)
  library(tidyverse)
})

## =========================================================
## 0. helper
## =========================================================

to_ql_date <- function(x) {
  DateParser_parseISO(as.character(x))
}

build_date_vector <- function(date_list) {
  dv <- DateVector()
  purrr::walk(
    date_list,
    function(d) {
      DateVector_append(dv, d)
    }
  )
  dv
}

leg_to_cashflow_tbl <- function(leg) {
  tibble(
    idx = seq_len(leg$size())
  ) %>%
    mutate(
      cashflow = map(
        idx,
        function(i) leg[i][[1]]
      ),
      date = map_chr(
        cashflow,
        function(cf) Date_ISO(CashFlow_date(cf))
      ),
      amount = map_dbl(
        cashflow,
        function(cf) CashFlow_amount(cf)
      )
    ) %>%
    select(date, amount)
}

fixed_leg_detail_tbl <- function(leg, eval_date) {
  tibble(
    idx = seq_len(leg$size())
  ) %>%
    mutate(
      cashflow = map(
        idx,
        function(i) leg[i][[1]]
      ),
      coupon = map(
        cashflow,
        function(cf) as_coupon(cf)
      ),
      fixed_coupon = map(
        cashflow,
        function(cf) as_fixed_rate_coupon(cf)
      ),
      ir = map(
        fixed_coupon,
        function(fc) FixedRateCoupon_interestRate(fc)
      ),
      date = map_chr(
        cashflow,
        function(cf) Date_ISO(CashFlow_date(cf))
      ),
      amount = map_dbl(
        cashflow,
        function(cf) CashFlow_amount(cf)
      ),
      accrualStartDate = map_chr(
        coupon,
        function(cp) Date_ISO(Coupon_accrualStartDate(cp))
      ),
      accrualEndDate = map_chr(
        coupon,
        function(cp) Date_ISO(Coupon_accrualEndDate(cp))
      ),
      accrualPeriod = map_dbl(
        coupon,
        function(cp) Coupon_accrualPeriod(cp)
      ),
      accrualDays = map_dbl(
        coupon,
        function(cp) Coupon_accrualDays(cp)
      ),
      accrualDayCounter = map_chr(
        coupon,
        function(cp) DayCounter_name(Coupon_dayCounter(cp))
      ),
      accruedAmount = map_dbl(
        coupon,
        function(cp) Coupon_accruedAmount(cp, eval_date)
      ),
      rate = map_dbl(
        ir,
        function(x) InterestRate_rate(x)
      ),
      rateDayCount = map_chr(
        ir,
        function(x) DayCounter_name(InterestRate_dayCounter(x))
      )
    ) %>%
    select(
      date,
      amount,
      accrualStartDate,
      accrualEndDate,
      accrualPeriod,
      accrualDays,
      accrualDayCounter,
      accruedAmount,
      rate,
      rateDayCount
    )
}

floating_leg_detail_tbl <- function(leg, eval_date) {
  tibble(
    idx = seq_len(leg$size())
  ) %>%
    mutate(
      cashflow = map(
        idx,
        function(i) leg[i][[1]]
      ),
      coupon = map(
        cashflow,
        function(cf) as_coupon(cf)
      ),
      float_coupon = map(
        cashflow,
        function(cf) as_floating_rate_coupon(cf)
      ),
      date = map_chr(
        cashflow,
        function(cf) Date_ISO(CashFlow_date(cf))
      ),
      amount = map_dbl(
        cashflow,
        function(cf) CashFlow_amount(cf)
      ),
      accrualStartDate = map_chr(
        coupon,
        function(cp) Date_ISO(Coupon_accrualStartDate(cp))
      ),
      accrualEndDate = map_chr(
        coupon,
        function(cp) Date_ISO(Coupon_accrualEndDate(cp))
      ),
      accrualPeriod = map_dbl(
        coupon,
        function(cp) Coupon_accrualPeriod(cp)
      ),
      accrualDays = map_dbl(
        coupon,
        function(cp) Coupon_accrualDays(cp)
      ),
      accrualDayCounter = map_chr(
        coupon,
        function(cp) DayCounter_name(Coupon_dayCounter(cp))
      ),
      accruedAmount = map_dbl(
        coupon,
        function(cp) Coupon_accruedAmount(cp, eval_date)
      ),
      spread = map_dbl(
        float_coupon,
        function(fc) FloatingRateCoupon_spread(fc)
      ),
      gearing = map_dbl(
        float_coupon,
        function(fc) FloatingRateCoupon_gearing(fc)
      ),
      adjFix = map_dbl(
        float_coupon,
        function(fc) FloatingRateCoupon_adjustedFixing(fc)
      ),
      dtFix = map_chr(
        float_coupon,
        function(fc) Date_ISO(FloatingRateCoupon_fixingDate(fc))
      ),
      convexityAdj = map_dbl(
        float_coupon,
        function(fc) FloatingRateCoupon_convexityAdjustment(fc)
      )
    ) %>%
    select(
      date,
      amount,
      accrualStartDate,
      accrualEndDate,
      accrualPeriod,
      accrualDays,
      accrualDayCounter,
      accruedAmount,
      spread,
      gearing,
      adjFix,
      dtFix,
      convexityAdj
    )
}

## =========================================================
## 1. global data
## =========================================================

calendar <- TARGET()

todaysDate <- to_ql_date("2020-10-19")
invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

settlementDays <- 3
settlementDate <- Calendar_advance(calendar, todaysDate, settlementDays, "Days")

cat("Today          :", todaysDate$`__str__`(), "\n")
cat("Settlement Date:", settlementDate$`__str__`(), "\n")

## =========================================================
## 2. term structure construction
## =========================================================

curve_input_tbl <- tibble(
  date_chr = c(
    "2020-10-19",
    "2020-11-19",
    "2021-01-19",
    "2021-04-19",
    "2021-10-19",
    "2022-04-19",
    "2022-10-19",
    "2023-10-19",
    "2025-10-19",
    "2030-10-19",
    "2035-10-19",
    "2040-10-19"
  ),
  rate = c(
    -0.004,
    -0.002,
    0.001,
    0.005,
    0.009,
    0.010,
    0.010,
    0.012,
    0.017,
    0.019,
    0.028,
    0.032
  )
) %>%
  mutate(
    ql_date = map(
      date_chr,
      function(x) to_ql_date(x)
    )
  )

dates <- build_date_vector(curve_input_tbl$ql_date)

forecast_curve <- ZeroCurve(
  dates,
  curve_input_tbl$rate,
  Actual365Fixed()
)

forecast_handle <- YieldTermStructureHandle(forecast_curve)

## =========================================================
## 3. swap construction
## =========================================================

swapBuilder <- MakeOIS(
  swapTenor = Period(5, "Years"),
  overnightIndex = Eonia(forecast_handle),
  fixedRate = 0.002
)

swap <- MakeOIS_makeOIS(swapBuilder)

## =========================================================
## 4. fixed leg basic cashflows
## =========================================================

fixed_leg <- swap$fixedLeg()

cat("Fixed leg maturity:", Date_ISO(CashFlows_maturityDate(fixed_leg)), "\n")

fixed_leg_cf_tbl <- leg_to_cashflow_tbl(fixed_leg)
print(fixed_leg_cf_tbl, n = Inf)

## =========================================================
## 5. fixed leg detailed cashflows
## =========================================================

fixed_leg_detail <- fixed_leg_detail_tbl(fixed_leg, todaysDate)
print(fixed_leg_detail, n = Inf)

## =========================================================
## 6. floating / overnight leg detailed cashflows
## =========================================================

floating_leg <- swap$overnightLeg()

floating_leg_detail <- floating_leg_detail_tbl(floating_leg, todaysDate)
print(floating_leg_detail, n = Inf)