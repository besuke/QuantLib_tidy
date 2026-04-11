suppressMessages(library(QuantLib))
suppressMessages(library(tidyverse))

## =========================================================
## 0. Utility
## =========================================================

build_ratehelper_vector <- function(helper_list) {
  v <- RateHelperVector()
  walk(helper_list, ~ RateHelperVector_append(v, .x))
  v
}

curve_dates_to_df <- function(curve) {
  cds <- curve$dates()
  map_dfr(seq_len(cds$size()), function(i) {
    d <- cds[i][[1]]
    tibble(
      date = Date_ISO(d),
      DF   = curve$discount(d)
    )
  })
}

curve_zeros_to_df <- function(curve, dc) {
  cds <- curve$dates()
  map_dfr(seq_len(cds$size()), function(i) {
    d <- cds[i][[1]]
    zr <- curve$zeroRate(d, dc, "Continuous")
    tibble(
      date = Date_ISO(d),
      zero = InterestRate_rate(zr) * 100
    )
  })
}

cashflow_tbl <- function(leg, yts) {
  map_dfr(seq_len(leg$size()), function(i) {
    cf <- leg[i][[1]]
    d  <- CashFlow_date(cf)
    a  <- CashFlow_amount(cf)
    z  <- yts$discount(d)
    
    tibble(
      date = Date_ISO(d),
      amount = a,
      df = z,
      pv = a * z
    )
  })
}

## =========================================================
## 1. Date
## =========================================================

today <- Date(10, "May", 2022)
today <- DateParser_parseISO("2022-05-10")
Settings_instance()$setEvaluationDate(today)

## =========================================================
## 2. TONA index
## =========================================================

tona <- OvernightIndex(
  "TONA", 0,
  JPYCurrency(), Japan(), Actual365Fixed()
)

Index_addFixing(tona, today, 0.04/100)

## =========================================================
## 3. Market data
## =========================================================

depo_tbl <- tibble(
  fixing_days = c(0,1,2),
  quote = c(0.04,0.04,0.04)
)

ois_tbl <- tibble(
  period = list(
    Period(1,"Weeks"), Period(2,"Weeks"), Period(3,"Weeks"),
    Period(1,"Months"), Period(2,"Months"), Period(3,"Months"),
    Period(6,"Months"), Period(1,"Years"), Period(2,"Years"),
    Period(5,"Years"), Period(10,"Years")
  ),
  quote = c(0.070,0.069,0.078,0.074,0.046,0.016,-0.007,-0.014,0.036,0.456,1.280)
)

## =========================================================
## 4. Helpers (map)
## =========================================================

depo_helpers <- depo_tbl %>%
  mutate(
    helper = map2(fixing_days, quote, function(fd, q) {
      DepositRateHelper(
        QuoteHandle(SimpleQuote(q/100)),
        tona$tenor(),
        fd,
        tona$fixingCalendar(),
        tona$businessDayConvention(),
        tona$endOfMonth(),
        tona$dayCounter()
      )
    })
  ) %>% pull(helper)

ois_helpers <- ois_tbl %>%
  mutate(
    helper = map2(period, quote, function(p, q) {
      OISRateHelper(
        2,
        p,
        QuoteHandle(SimpleQuote(q/100)),
        tona
      )
    })
  ) %>% pull(helper)

helpers_vec <- build_ratehelper_vector(c(depo_helpers, ois_helpers))

## =========================================================
## 5. Curve
## =========================================================

tona_curve <- PiecewiseLogCubicDiscount(
  0,
  Japan(),
  helpers_vec,
  Actual365Fixed()
)

TermStructure_enableExtrapolation(tona_curve)

curve_dates_to_df(tona_curve) %>% print(n=10)
curve_zeros_to_df(tona_curve, Actual365Fixed()) %>% print(n=10)

## =========================================================
## 6. Swap
## =========================================================

yts <- YieldTermStructureHandle(tona_curve)

tona_with_curve <- OvernightIndex(
  "TONA", 0,
  JPYCurrency(), Japan(), Actual365Fixed(),
  yts
)

engine <- DiscountingSwapEngine(yts)

spot <- Calendar_advance(Japan(), today, 2, "Days")
maturity <- Calendar_advance(Japan(), spot, Period(10,"Years"), "Following")

schedule <- Schedule(
  spot, maturity,
  Period(1,"Years"),
  Japan(),
  "ModifiedFollowing","ModifiedFollowing",
  "Backward", FALSE
)

swap <- OvernightIndexedSwap(
  Swap_Payer_get(),
  1e7,
  schedule,
  0.0128,
  Actual365Fixed(),
  tona_with_curve,
  0,
  2,
  "Following",
  Japan()
)

swap$setPricingEngine(engine)

cat("\n=== Swap ===\n")
cat("NPV:", swap$NPV(), "\n")
cat("fairRate:", swap$fairRate(), "\n")

## =========================================================
## 7. Cashflows
## =========================================================

fixed_cf <- cashflow_tbl(swap$fixedLeg(), yts)
float_cf <- cashflow_tbl(swap$overnightLeg(), yts)

fixed_cf %>% print(n=10)
float_cf %>% print(n=10)

## =========================================================
## 8. Relinkable (重要)
## =========================================================

yts_link <- RelinkableYieldTermStructureHandle()
RelinkableYieldTermStructureHandle_linkTo(yts_link, tona_curve)

engine2 <- DiscountingSwapEngine(yts_link)

swap2 <- OvernightIndexedSwap(
  Swap_Payer_get(),
  1e7,
  schedule,
  0.0128,
  Actual365Fixed(),
  OvernightIndex("TONA",0,JPYCurrency(),Japan(),Actual365Fixed(),yts_link),
  0,2,"Following",Japan()
)

swap2$setPricingEngine(engine2)

cat("\nold:", swap2$NPV(), "\n")

RelinkableYieldTermStructureHandle_linkTo(yts_link, tona_curve)
cat("relinked:", swap2$NPV(), "\n")

## =========================================================
## 9. Sensitivity（map版）
## =========================================================

quotes <- ois_tbl$quote %>%
  map(~ SimpleQuote(.x/100))

raw <- map_dbl(quotes, ~ .x$value())

bp <- 1e-4

walk(quotes, ~ .x$setValue(.x$value() + bp))
walk2(quotes, raw, ~ .x$setValue(.y))

## =========================================================
## END
## =========================================================