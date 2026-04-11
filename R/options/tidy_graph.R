suppressPackageStartupMessages({
  library(QuantLib)
  library(tidyverse)
})

qldate <- function(x) {
  DateParser_parseISO(x)
}

option_npv_at_spot <- function(spot_value) {
  todaysDate <- qldate("1998-05-15")
  invisible(Settings_instance()$setEvaluationDate(d = todaysDate))
  
  settlementDate <- qldate("1998-05-17")
  
  riskFreeRate <- FlatForward(
    settlementDate,
    0.05,
    Actual365Fixed()
  )
  
  exercise <- EuropeanExercise(qldate("1999-05-17"))
  payoff <- PlainVanillaPayoff("Call", 8.0)
  
  underlying <- SimpleQuote(spot_value)
  
  volatility <- BlackConstantVol(
    todaysDate,
    TARGET(),
    0.10,
    Actual365Fixed()
  )
  
  dividendYield <- FlatForward(
    settlementDate,
    0.05,
    Actual365Fixed()
  )
  
  process <- BlackScholesMertonProcess(
    QuoteHandle(underlying),
    YieldTermStructureHandle(dividendYield),
    YieldTermStructureHandle(riskFreeRate),
    BlackVolTermStructureHandle(volatility)
  )
  
  option <- VanillaOption(payoff, exercise)
  Instrument_setPricingEngine(
    option,
    s_arg2 = AnalyticEuropeanEngine(process)
  )
  
  option$NPV()
}

spot_curve_tbl <- tibble(
  spot = seq(1, 20, length.out = 101)
) %>%
  mutate(
    npv = map_dbl(
      spot,
      function(x) {
        option_npv_at_spot(x)
      }
    )
  )

print(spot_curve_tbl, n = 10)

ggplot(spot_curve_tbl, aes(x = spot, y = npv)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "European Call NPV by Spot",
    x = "Spot",
    y = "NPV"
  )
