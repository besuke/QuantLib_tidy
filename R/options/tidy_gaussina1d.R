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

append_quote_handles <- function(qh_list) {
  vec <- QuoteHandleVector()
  purrr::walk(
    qh_list,
    function(qh) {
      QuoteHandleVector_append(vec, qh)
    }
  )
  vec
}

schedule_dates_to_shifted_exercise_dates <- function(schedule_obj, calendar, lag_days = 2) {
  exercise_dates <- Schedule_dates(self = schedule_obj, .copy = TRUE)
  
  for (i in seq(0, exercise_dates$size() - 1)) {
    shifted_date <- Calendar_advance(
      calendar,
      DateVector___getitem__(exercise_dates, i),
      -lag_days,
      "Days"
    )
    DateVector___setitem__(exercise_dates, i, shifted_date)
  }
  
  DateVector___delitem__(exercise_dates, 0)
  DateVector___delitem__(exercise_dates, DateVector___len__(exercise_dates) - 1)
  
  exercise_dates
}

set_swaption_engines <- function(basket, engine) {
  purrr::walk(
    seq_len(basket$size()),
    function(i) {
      basket_i <- basket[i][[1]]
      BlackCalibrationHelper_setPricingEngine(basket_i, engine)
    }
  )
  invisible(NULL)
}

basket_data_tbl <- function(basket) {
  tibble(idx = seq_len(basket$size())) %>%
    mutate(
      basket_item = map(
        idx,
        function(i) basket[i][[1]]
      ),
      helper = map(
        basket_item,
        function(x) as_swaption_helper(x)
      ),
      swaption_obj = map(
        helper,
        function(h) SwaptionHelper_swaption(h)
      ),
      expiry = map_chr(
        helper,
        function(h) Date_ISO(SwaptionHelper_swaptionExpiryDate(h))
      ),
      maturity = map_chr(
        helper,
        function(h) Date_ISO(SwaptionHelper_swaptionMaturityDate(h))
      ),
      nominal = map_dbl(
        helper,
        function(h) SwaptionHelper_swaptionNominal(h)
      ),
      strike = map_dbl(
        helper,
        function(h) SwaptionHelper_swaptionStrike(h)
      ),
      optType = map_chr(
        swaption_obj,
        function(swp) Swaption_type(swp)
      )
    ) %>%
    select(expiry, maturity, nominal, strike, optType)
}

calibration_data_tbl <- function(basket, volatilities) {
  tibble(idx = seq_len(basket$size())) %>%
    mutate(
      basket_item = map(
        idx,
        function(i) basket[i][[1]]
      ),
      modelSigma = map_dbl(
        idx,
        function(i) volatilities[i][[1]]
      ),
      helper = map(
        basket_item,
        function(x) as_swaption_helper(x)
      ),
      expiry = map_chr(
        helper,
        function(h) Date_ISO(SwaptionHelper_swaptionExpiryDate(h))
      ),
      modelPrice = map_dbl(
        basket_item,
        function(x) x$modelValue()
      ),
      marketPrice = map_dbl(
        basket_item,
        function(x) x$marketValue()
      ),
      modelImpVol = map2_dbl(
        basket_item,
        modelPrice,
        function(x, px) {
          BlackCalibrationHelper_impliedVolatility(
            self = x,
            targetValue = px,
            accuracy = 1e-6,
            maxEvaluations = 1000,
            minVol = 0.0,
            maxVol = 2.0
          )
        }
      ),
      marketImpVol = map_dbl(
        basket_item,
        function(x) QuoteHandle_value(x$volatility())
      )
    ) %>%
    select(expiry, modelSigma, modelPrice, marketPrice, modelImpVol, marketImpVol)
}

## =========================================================
## 1. global data
## =========================================================

calendar <- TARGET()

todaysDate <- qldate("2014-04-30")
invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

settlementDays <- 3
settlementDate <- Calendar_advance(calendar, todaysDate, settlementDays, "Days")

cat("Today          :", Date_ISO(todaysDate), "\n")
cat("Settlement Date:", Date_ISO(settlementDate), "\n")

refDate <- todaysDate

## =========================================================
## 2. market / model inputs
## =========================================================

quote_tbl <- tibble(
  name = c("forward6m", "ois", "swaptionVol"),
  value = c(0.025, 0.020, 0.20)
)

forward6mQuote <- QuoteHandle(SimpleQuote(quote_tbl$value[quote_tbl$name == "forward6m"]))
oisQuote <- QuoteHandle(SimpleQuote(quote_tbl$value[quote_tbl$name == "ois"]))
volQuote <- QuoteHandle(SimpleQuote(quote_tbl$value[quote_tbl$name == "swaptionVol"]))

dc <- Actual365Fixed()

yts6m <- FlatForward(refDate, forward6mQuote, dc)
ytsOis <- FlatForward(refDate, oisQuote, dc)
yts6m$enableExtrapolation()
ytsOis$enableExtrapolation()

hyts6m <- RelinkableYieldTermStructureHandle(yts6m)
t0_curve <- YieldTermStructureHandle(yts6m)
t0_Ois <- YieldTermStructureHandle(ytsOis)

euribor6m <- Euribor6M(hyts6m)

swaptionVol <- ConstantSwaptionVolatility(
  0,
  calendar,
  "ModifiedFollowing",
  volQuote,
  Actual365Fixed()
)

## =========================================================
## 3. schedules
## =========================================================

effectiveDate <- Calendar_advance(calendar, refDate, 2, "Days")
maturityDate <- Calendar_advance(calendar, effectiveDate, 10, "Years")

cat(Date_ISO(effectiveDate), "\n")
cat(Date_ISO(maturityDate), "\n")

fixedSchedule <- Schedule(
  effectiveDate,
  maturityDate,
  Period(1, "Years"),
  calendar,
  "ModifiedFollowing",
  "ModifiedFollowing",
  DateGeneration_Forward_get(),
  FALSE
)

floatSchedule <- Schedule(
  effectiveDate,
  maturityDate,
  Period(6, "Months"),
  calendar,
  "ModifiedFollowing",
  "ModifiedFollowing",
  DateGeneration_Forward_get(),
  FALSE
)

## =========================================================
## 4. standard 10Y bermudan payer swaption
## =========================================================

fixedNominal    <- rep(1, fixedSchedule$size() - 1)
floatingNominal <- rep(1, floatSchedule$size() - 1)
strike          <- rep(0.04, fixedSchedule$size() - 1)
gearing         <- rep(1, floatSchedule$size() - 1)
spread          <- rep(0, floatSchedule$size() - 1)

underlying <- NonstandardSwap(
  Swap_Payer_get(),
  fixedNominal,
  floatingNominal,
  fixedSchedule,
  strike,
  Thirty360(Thirty360_BondBasis_get()),
  floatSchedule,
  euribor6m,
  gearing,
  spread,
  Actual360(),
  FALSE,
  FALSE,
  "ModifiedFollowing"
)

exerciseDates <- schedule_dates_to_shifted_exercise_dates(fixedSchedule, calendar, lag_days = 2)

exercise_dates_tbl <- tibble(idx = seq_len(exerciseDates$size())) %>%
  mutate(
    date = map_chr(
      idx,
      function(i) Date_ISO(exerciseDates[i][[1]])
    )
  )

print(exercise_dates_tbl, n = Inf)

exercise <- BermudanExercise(exerciseDates)
swaption <- NonstandardSwaption(underlying, exercise, Settlement_Physical_get())

## =========================================================
## 5. GSR model
## =========================================================

stepDates <- DateVector(exerciseDates)
DateVector___delitem__(stepDates, DateVector___len__(stepDates) - 1)

sigma_tbl <- tibble(sigma = rep(0.01, 9)) %>%
  mutate(
    qh = map(
      sigma,
      function(x) QuoteHandle(SimpleQuote(x))
    )
  )

reversion_tbl <- tibble(reversion = 0.01) %>%
  mutate(
    qh = map(
      reversion,
      function(x) QuoteHandle(SimpleQuote(x))
    )
  )

sigmas <- append_quote_handles(sigma_tbl$qh)
reversion <- append_quote_handles(reversion_tbl$qh)

gsr <- Gsr(t0_curve, stepDates, sigmas, reversion)

swaptionEngine <- Gaussian1dSwaptionEngine(
  gsr,
  64,
  7.0,
  TRUE,
  FALSE,
  t0_Ois
)

nonstandardSwaptionEngine <- Gaussian1dNonstandardSwaptionEngine(
  gsr,
  64,
  7.0,
  TRUE,
  FALSE,
  QuoteHandle(SimpleQuote(0)),
  t0_Ois
)

swaption$setPricingEngine(nonstandardSwaptionEngine)

swapBase <- EuriborSwapIsdaFixA(
  Period(10, "Years"),
  t0_curve,
  t0_Ois
)

method <- LevenbergMarquardt()
ec <- EndCriteria(1000, 10, 1e-8, 1e-8, 1e-8)

## =========================================================
## 6. Naive basket
## =========================================================

basket_naive <- swaption$calibrationBasket(swapBase, swaptionVol, "Naive")
set_swaption_engines(basket_naive, swaptionEngine)

gsr$calibrateVolatilitiesIterative(basket_naive, method, ec)

cat("\n=== Naive basket ===\n")
print(basket_data_tbl(basket_naive), n = Inf)

cat("\n=== Naive calibration ===\n")
print(calibration_data_tbl(basket_naive, gsr$volatility()), n = Inf)

cat("\nBermudan swaption NPV (ATM calibrated GSR):\n")
print(swaption$NPV())

## =========================================================
## 7. MaturityStrikeByDeltaGamma basket
## =========================================================

basket_delta_gamma <- swaption$calibrationBasket(
  swapBase,
  swaptionVol,
  "MaturityStrikeByDeltaGamma"
)

cat("\n=== Delta/Gamma basket ===\n")
print(basket_data_tbl(basket_delta_gamma), n = Inf)

set_swaption_engines(basket_delta_gamma, swaptionEngine)
gsr$calibrateVolatilitiesIterative(basket_delta_gamma, method, ec)

cat("\n=== Delta/Gamma calibration ===\n")
print(calibration_data_tbl(basket_delta_gamma, gsr$volatility()), n = Inf)

cat("\nBermudan swaption NPV (deal strike calibrated GSR):\n")
print(swaption$NPV())

## =========================================================
## 8. amortizing underlying
## =========================================================

for (i in seq_len(fixedSchedule$size() - 1)) {
  tmp <- 1.0 - i / fixedSchedule$size()
  fixedNominal[i] <- tmp
  floatingNominal[i * 2 - 1] <- tmp
  floatingNominal[i * 2] <- tmp
}

underlying2 <- NonstandardSwap(
  Swap_Payer_get(),
  fixedNominal,
  floatingNominal,
  fixedSchedule,
  strike,
  Thirty360(Thirty360_BondBasis_get()),
  floatSchedule,
  euribor6m,
  gearing,
  spread,
  Actual360(),
  FALSE,
  FALSE,
  "ModifiedFollowing"
)

swaption2 <- NonstandardSwaption(underlying2, exercise, Settlement_Physical_get())
swaption2$setPricingEngine(nonstandardSwaptionEngine)

basket_amortizing <- swaption2$calibrationBasket(
  swapBase,
  swaptionVol,
  "MaturityStrikeByDeltaGamma"
)

cat("\n=== Amortizing basket ===\n")
print(basket_data_tbl(basket_amortizing), n = Inf)

## =========================================================
## 9. callable bond style setup
## =========================================================

fixedNominal2 <- rep(1, fixedSchedule$size() - 1)
floatingNominal2 <- rep(0, fixedSchedule$size() * 2 - 2)

underlying3 <- NonstandardSwap(
  Swap_Receiver_get(),
  fixedNominal2,
  floatingNominal2,
  fixedSchedule,
  strike,
  Thirty360(Thirty360_BondBasis_get()),
  floatSchedule,
  euribor6m,
  gearing,
  spread,
  Actual360(),
  FALSE,
  TRUE,
  "ModifiedFollowing"
)

rebateAmount <- rep(-1, exerciseDates$size())
exercise2 <- RebatedExercise(exercise, rebateAmount, 2, calendar)
swaption3 <- NonstandardSwaption(underlying3, exercise2, Settlement_Physical_get())

oas0 <- SimpleQuote(0)
oas100 <- SimpleQuote(0.01)
oas <- RelinkableQuoteHandle(oas0)

nonstandardSwaptionEngine2 <- Gaussian1dNonstandardSwaptionEngine(
  gsr,
  64,
  7.0,
  TRUE,
  FALSE,
  oas,
  t0_curve
)

swaption3$setPricingEngine(nonstandardSwaptionEngine2)

basket_callable_0 <- swaption3$calibrationBasket(
  swapBase,
  swaptionVol,
  "MaturityStrikeByDeltaGamma"
)

cat("\n=== Callable basket (OAS = 0) ===\n")
print(basket_data_tbl(basket_callable_0), n = Inf)

set_swaption_engines(basket_callable_0, swaptionEngine)
gsr$calibrateVolatilitiesIterative(basket_callable_0, method, ec)

cat("\nCallable right NPV (OAS = 0):\n")
print(swaption3$NPV())

## =========================================================
## 10. callable bond with OAS = 100bp
## =========================================================

oas$linkTo(oas100)

basket_callable_100 <- swaption3$calibrationBasket(
  swapBase,
  swaptionVol,
  "MaturityStrikeByDeltaGamma"
)

cat("\n=== Callable basket (OAS = 100bp) ===\n")
print(basket_data_tbl(basket_callable_100), n = Inf)

set_swaption_engines(basket_callable_100, swaptionEngine)
gsr$calibrateVolatilitiesIterative(basket_callable_100, method, ec)

cat("\nCallable right NPV (OAS = 100bp):\n")
print(swaption3$NPV())


suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

dc_plot <- Actual365Fixed()

step_tbl <- tibble(
  idx = seq_len(stepDates$size())
) %>%
  mutate(
    step_date = map(
      idx,
      function(i) stepDates[i][[1]]
    ),
    step_date_chr = map_chr(
      step_date,
      function(d) Date_ISO(d)
    ),
    time = map_dbl(
      step_date,
      function(d) DayCounter_yearFraction(dc_plot, refDate, d)
    ),
    sigma = map_dbl(
      idx,
      function(i) gsr$volatility()[i][[1]]
    )
  )

print(step_tbl, n = Inf)

ggplot(step_tbl, aes(x = time, y = sigma)) +
  geom_step(direction = "hv") +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Calibrated GSR sigma term structure",
    x = "Time from valuation date (years)",
    y = "Sigma"
  )
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

dc_plot <- Actual365Fixed()

step_tbl <- tibble(
  idx = seq_len(stepDates$size())
) %>%
  mutate(
    step_date = map(
      idx,
      function(i) stepDates[i][[1]]
    ),
    step_date_chr = map_chr(
      step_date,
      function(d) Date_ISO(d)
    ),
    time = map_dbl(
      step_date,
      function(d) DayCounter_yearFraction(dc_plot, refDate, d)
    ),
    sigma = map_dbl(
      idx,
      function(i) gsr$volatility()[i][[1]]
    )
  ) %>% 
  select(-step_date)

print(step_tbl, n = Inf)

ggplot(step_tbl, aes(x = time, y = sigma)) +
  geom_step(direction = "hv") +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Calibrated GSR sigma term structure",
    x = "Time from valuation date (years)",
    y = "Sigma"
  )
