suppressPackageStartupMessages({
  library(QuantLib)
  library(tidyverse)
})

## =========================================================
## 0. helper
## =========================================================

qldate <- function(x) {
  DateParser_parseISO(x)
}

to_qh <- function(x) {
  QuoteHandle(SimpleQuote(x))
}

push_helpers <- function(x) {
  vec <- RateHelperVector()
  purrr::walk(x, ~ RateHelperVector_push_back(vec, .x))
  vec
}

## =========================================================
## 1. evaluation date
## =========================================================

calendar <- UnitedStates("GovernmentBond")

settlementDate <- qldate("2008-09-18")
settlementDate <- Calendar_adjust(calendar, settlementDate)

fixingDays <- 3
settlementDays <- 3

todaysDate <- Calendar_advance(calendar, settlementDate, -fixingDays, "Days")
invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

cat("Today          :", todaysDate$`__str__`(), "\n")
cat("Settlement Date:", settlementDate$`__str__`(), "\n")

## =========================================================
## 2. Zero curve helpers (deposits + bonds)
## =========================================================

zc_quotes <- tibble(
  rate = c(0.0096, 0.0145, 0.0194),
  tenor_n = c(3, 6, 1),
  tenor_unit = c("Months", "Months", "Years")
) %>%
  mutate(
    tenor = purrr::map2(tenor_n, tenor_unit, Period)
  )

zcBondsDayCounter <- Actual365Fixed()

deposit_helpers_bond_curve <- purrr::pmap(
  list(zc_quotes$rate, zc_quotes$tenor),
  function(rate, tenor) {
    DepositRateHelper(
      QuoteHandle(SimpleQuote(rate)),
      tenor,
      fixingDays,
      calendar,
      "ModifiedFollowing",
      TRUE,
      zcBondsDayCounter
    )
  }
)

bond_quotes <- tibble(
  issue_date   = c("2005-03-15", "2005-06-15", "2006-06-30", "2002-11-15", "1987-05-15"),
  maturity     = c("2010-08-31", "2011-08-31", "2013-08-31", "2018-08-15", "2038-05-15"),
  coupon_rate  = c(0.02375, 0.04625, 0.03125, 0.04000, 0.04500),
  market_quote = c(100.390625, 106.21875, 100.59375, 101.6875, 102.140625)
) %>%
  mutate(
    issue_qldate    = purrr::map(issue_date, qldate),
    maturity_qldate = purrr::map(maturity, qldate)
  )

redemption <- 100.0

bond_helpers <- purrr::pmap(
  list(
    bond_quotes$issue_qldate,
    bond_quotes$maturity_qldate,
    bond_quotes$coupon_rate,
    bond_quotes$market_quote
  ),
  function(issueDate, maturityDate, couponRate, marketQuote) {
    schedule <- Schedule(
      issueDate,
      maturityDate,
      Period("Semiannual"),
      UnitedStates("GovernmentBond"),
      "Unadjusted",
      "Unadjusted",
      copyToR(DateGeneration(), "Backward"),
      FALSE
    )
    
    FixedRateBondHelper(
      QuoteHandle(SimpleQuote(marketQuote)),
      settlementDays,
      100.0,
      schedule,
      couponRate,
      ActualActual("Bond"),
      "Unadjusted",
      redemption,
      issueDate
    )
  }
)

bondInstruments <- push_helpers(c(deposit_helpers_bond_curve, bond_helpers))

termStructureDayCounter <- ActualActual("ISDA")

bondDiscountingTermStructure <- PiecewiseFlatForward(
  settlementDate,
  bondInstruments,
  termStructureDayCounter
)

## =========================================================
## 3. Libor forecasting curve (deposits + swaps)
## =========================================================

dQuotes <- tibble(
  rate = c(0.043375, 0.031875, 0.0320375, 0.03385, 0.0338125, 0.0335125),
  tenor_n = c(1, 1, 3, 6, 9, 1),
  tenor_unit = c("Weeks", "Months", "Months", "Months", "Months", "Years")
) %>%
  mutate(
    tenor = purrr::map2(tenor_n, tenor_unit, Period)
  )

sQuotes <- tibble(
  rate = c(0.0295, 0.0323, 0.0359, 0.0412, 0.0433),
  tenor_n = c(2, 3, 5, 10, 15),
  tenor_unit = c("Years", "Years", "Years", "Years", "Years")
) %>%
  mutate(
    tenor = purrr::map2(tenor_n, tenor_unit, Period)
  )

depositDayCounter <- Actual360()

depo_helpers_swap_curve <- purrr::pmap(
  list(dQuotes$rate, dQuotes$tenor),
  function(rate, tenor) {
    DepositRateHelper(
      QuoteHandle(SimpleQuote(rate)),
      tenor,
      fixingDays,
      calendar,
      "ModifiedFollowing",
      TRUE,
      depositDayCounter
    )
  }
)

swFixedLegFrequency   <- "Annual"
swFixedLegConvention  <- "Unadjusted"
swFixedLegDayCounter  <- Thirty360("European")
swFloatingLegIndex    <- Euribor6M()
forwardStart          <- Period(1, "Days")

swap_helpers <- purrr::pmap(
  list(sQuotes$rate, sQuotes$tenor),
  function(rate, tenor) {
    SwapRateHelper(
      QuoteHandle(SimpleQuote(rate)),
      tenor,
      calendar,
      swFixedLegFrequency,
      swFixedLegConvention,
      swFixedLegDayCounter,
      swFloatingLegIndex,
      QuoteHandle(),
      forwardStart
    )
  }
)

depoSwapInstruments <- push_helpers(c(depo_helpers_swap_curve, swap_helpers))

depoSwapTermStructure <- PiecewiseFlatForward(
  settlementDate,
  depoSwapInstruments,
  termStructureDayCounter
)

## =========================================================
## 4. term structure handles
## =========================================================

discountingTermStructure <- RelinkableYieldTermStructureHandle()
forecastingTermStructure <- RelinkableYieldTermStructureHandle()

faceAmount <- 100

invisible(RelinkableYieldTermStructureHandle_linkTo(
  forecastingTermStructure,
  depoSwapTermStructure
))

invisible(RelinkableYieldTermStructureHandle_linkTo(
  discountingTermStructure,
  bondDiscountingTermStructure
))

bondEngine <- DiscountingBondEngine(discountingTermStructure)

## =========================================================
## 5. Zero coupon bond
## =========================================================

zeroCouponBond <- ZeroCouponBond(
  settlementDays,
  UnitedStates("GovernmentBond"),
  faceAmount,
  qldate("2013-08-15"),
  "Following",
  116.92,
  qldate("2003-08-15")
)

invisible(Instrument_setPricingEngine(zeroCouponBond, bondEngine))

## =========================================================
## 6. Fixed rate bond
## =========================================================

fixedBondSchedule <- Schedule(
  qldate("2007-05-15"),
  qldate("2017-05-15"),
  Period("Semiannual"),
  UnitedStates("GovernmentBond"),
  "Unadjusted",
  "Unadjusted",
  copyToR(DateGeneration(), "Backward"),
  FALSE
)

fixedRateBond <- FixedRateBond(
  settlementDays,
  faceAmount,
  fixedBondSchedule,
  0.045,
  ActualActual("Bond"),
  "ModifiedFollowing",
  100.0,
  qldate("2007-05-15")
)

invisible(Instrument_setPricingEngine(fixedRateBond, bondEngine))

## =========================================================
## 7. Floating rate bond
## =========================================================

liborTermStructure <- RelinkableYieldTermStructureHandle()
libor3m <- USDLibor(Period(3, "Months"), liborTermStructure)

libor3m$clearFixings()
invisible(Index_addFixing(libor3m, qldate("2008-07-17"), 0.0278625))

floatingBondSchedule <- Schedule(
  qldate("2005-10-21"),
  qldate("2010-10-21"),
  Period("Quarterly"),
  UnitedStates("NYSE"),
  "Unadjusted",
  "Unadjusted",
  copyToR(DateGeneration(), "Backward"),
  TRUE
)

floatingRateBond <- FloatingRateBond(
  settlementDays,
  faceAmount,
  floatingBondSchedule,
  libor3m,
  Actual360(),
  "ModifiedFollowing",
  2,
  1.0,
  0.001,
  numeric(0),
  numeric(0),
  TRUE,
  100.0,
  qldate("2005-10-21")
)

invisible(Instrument_setPricingEngine(floatingRateBond, bondEngine))

pricer <- BlackIborCouponPricer()

volatility <- 0.0
vol <- ConstantOptionletVolatility(
  settlementDays,
  calendar,
  "ModifiedFollowing",
  volatility,
  Actual365Fixed()
)

invisible(
  IborCouponPricer_setCapletVolatility(
    pricer,
    OptionletVolatilityStructureHandle(vol)
  )
)

invisible(setCouponPricer(Bond_cashflows(floatingRateBond), pricer))

invisible(RelinkableYieldTermStructureHandle_linkTo(
  liborTermStructure,
  depoSwapTermStructure
))

## =========================================================
## 8. Results table
## =========================================================

result_tbl <- tibble(
  item = c(
    "NPV",
    "Clean Price",
    "Dirty Price",
    "Accrued Amount",
    "Previous Coupon",
    "Next Coupon",
    "Yield"
  ),
  zeroCoupon = c(
    Instrument_NPV(zeroCouponBond),
    Bond_cleanPrice(zeroCouponBond),
    Bond_dirtyPrice(zeroCouponBond),
    Bond_accruedAmount(zeroCouponBond),
    NA_real_,
    NA_real_,
    100 * Bond_yield(zeroCouponBond, Actual360(), "Compounded", "Annual")
  ),
  fixedRate = c(
    Instrument_NPV(fixedRateBond),
    Bond_cleanPrice(fixedRateBond),
    Bond_dirtyPrice(fixedRateBond),
    Bond_accruedAmount(fixedRateBond),
    100 * Bond_previousCouponRate(fixedRateBond),
    100 * Bond_nextCouponRate(fixedRateBond),
    100 * Bond_yield(fixedRateBond, Actual360(), "Compounded", "Annual")
  ),
  floatingRate = c(
    Instrument_NPV(floatingRateBond),
    Bond_cleanPrice(floatingRateBond),
    Bond_dirtyPrice(floatingRateBond),
    Bond_accruedAmount(floatingRateBond),
    100 * Bond_previousCouponRate(floatingRateBond),
    100 * Bond_nextCouponRate(floatingRateBond),
    100 * Bond_yield(floatingRateBond, Actual360(), "Compounded", "Annual")
  )
)

cat("\nResults:\n")
print(result_tbl, n = Inf)

## =========================================================
## 9. Sample indirect computations
## =========================================================

cat("\nSample indirect computations (for the floating rate bond):\n")

yld <- Bond_yield(
  floatingRateBond,
  Actual360(),
  "Compounded",
  "Annual"
)

clnPrc <- Bond_cleanPrice(
  floatingRateBond,
  yld,
  Actual360(),
  "Compounded",
  "Annual",
  settlementDate
)

cat("Yield to Clean Price:", clnPrc, "\n")

yld2 <- Bond_yield(
  floatingRateBond,
  BondPrice(clnPrc, BondPrice_Clean_get()),
  Actual360(),
  "Compounded",
  "Annual",
  settlementDate
)

cat("Clean Price to Yield:", yld2, "\n")

