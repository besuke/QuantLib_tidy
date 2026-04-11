suppressPackageStartupMessages({
  library(QuantLib)
  library(tidyverse)
  library(lubridate)
})

## =========================================================
## 0. helper
## =========================================================

to_ql_date <- function(x) {
  DateParser_parseISO(as.character(x))
}

build_corr_matrix <- function(rho = 0.5) {
  m <- Matrix(2, 2)
  invisible(Matrix_setitem(m, 0, 0, 1.0))
  invisible(Matrix_setitem(m, 1, 1, 1.0))
  invisible(Matrix_setitem(m, 0, 1, rho))
  invisible(Matrix_setitem(m, 1, 0, rho))
  m
}

build_process_vector <- function(process_list) {
  vec <- StochasticProcess1DVector(length(process_list))
  purrr::walk(
    seq_along(process_list),
    function(i) {
      invisible(
        StochasticProcess1DVector___setitem__(
          self = vec,
          i = i - 1,
          x = process_list[[i]]
        )
      )
    }
  )
  vec
}

build_european_engine <- function(process, seed = 42) {
  MCPREuropeanBasketEngine(
    process = process,
    timeSteps = NA,
    timeStepsPerYear = 1,
    brownianBridge = FALSE,
    antitheticVariate = FALSE,
    requiredSamples = NA,
    requiredTolerance = 0.02,
    maxSamples = 10000,
    seed = seed
  )
}

build_american_engine <- function(process, seed = 42) {
  MCPRAmericanBasketEngine(
    process = process,
    timeSteps = 10,
    timeStepsPerYear = NA,
    brownianBridge = FALSE,
    antitheticVariate = FALSE,
    requiredSamples = 50000,
    requiredTolerance = 0.02,
    maxSamples = 100000,
    seed = seed,
    nCalibrationSamples = 5000,
    polynomOrder = 5,
    polynomType = LsmBasisSystem_Hermite_get()
  )
}

price_basket_option <- function(option, engine) {
  option$setPricingEngine(engine)
  option$NPV()
}

## =========================================================
## 1. global data
## =========================================================

calendar <- TARGET()

todaysDate <- ymd(19980515) %>%
  to_ql_date()

invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

settlementDays <- 3
settlementDate <- Calendar_advance(calendar, todaysDate, settlementDays, "Days")

cat("Today          :", todaysDate$`__str__`(), "\n")
cat("Settlement Date:", settlementDate$`__str__`(), "\n")

riskFreeRate <- FlatForward(settlementDate, 0.05, Actual365Fixed())

## =========================================================
## 2. option parameters
## =========================================================

exerciseDate <- dmy("17May1999") %>%
  to_ql_date()

exercise <- EuropeanExercise(exerciseDate)
payoff <- PlainVanillaPayoff("Call", 8.0)

americanExercise <- AmericanExercise(settlementDate, exerciseDate)

## =========================================================
## 3. market data
## =========================================================

asset_tbl <- tibble(
  asset_id = c("asset_1", "asset_2"),
  spot = c(7.0, 7.0),
  vol = c(0.10, 0.10),
  dividend = c(0.05, 0.05)
) %>%
  mutate(
    underlying = map(
      spot,
      function(x) {
        SimpleQuote(x)
      }
    ),
    volatility = map(
      vol,
      function(x) {
        BlackConstantVol(todaysDate, calendar, x, Actual365Fixed())
      }
    ),
    dividendYield = map(
      dividend,
      function(x) {
        FlatForward(settlementDate, x, Actual365Fixed())
      }
    ),
    process1d = pmap(
      list(underlying, dividendYield, volatility),
      function(underlying, dividendYield, volatility) {
        BlackScholesMertonProcess(
          QuoteHandle(underlying),
          YieldTermStructureHandle(dividendYield),
          YieldTermStructureHandle(riskFreeRate),
          BlackVolTermStructureHandle(volatility)
        )
      }
    )
  )

corrMatrix <- build_corr_matrix(rho = 0.5)
cat(corrMatrix$`__str__`())

procVector <- build_process_vector(asset_tbl$process1d)

process <- StochasticProcessArray(
  array = procVector,
  correlation = corrMatrix
)

## =========================================================
## 4. pricing - european basket options
## =========================================================

european_engine <- build_european_engine(process, seed = 42)

european_option_tbl <- tibble(
  payoff_name = c("Max", "Min", "Average"),
  basket_payoff = list(
    MaxBasketPayoff(payoff),
    MinBasketPayoff(payoff),
    AverageBasketPayoff(payoff, 2)
  )
) %>%
  mutate(
    option = map(
      basket_payoff,
      function(bp) {
        BasketOption(bp, exercise)
      }
    ),
    npv = map_dbl(
      option,
      function(opt) {
        price_basket_option(opt, european_engine)
      }
    )
  ) %>%
  select(payoff_name, npv)

print(european_option_tbl, n = Inf)

## =========================================================
## 5. pricing - american basket option
## =========================================================

american_engine <- build_american_engine(process, seed = 42)

american_option_tbl <- tibble(
  payoff_name = "Max",
  basket_payoff = list(MaxBasketPayoff(payoff))
) %>%
  mutate(
    option = map(
      basket_payoff,
      function(bp) {
        BasketOption(bp, americanExercise)
      }
    ),
    npv = map_dbl(
      option,
      function(opt) {
        price_basket_option(opt, american_engine)
      }
    )
  ) %>%
  select(payoff_name, npv)

print(american_option_tbl, n = Inf)