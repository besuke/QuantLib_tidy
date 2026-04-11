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

append_rate_helpers <- function(helper_list) {
  vec <- RateHelperVector()
  purrr::walk(
    helper_list,
    function(h) {
      RateHelperVector_append(vec, h)
    }
  )
  vec
}

show_swap_tbl <- function(swap_obj) {
  tibble(
    NPV = swap_obj$NPV(),
    fair_spread_pct = swap_obj$fairSpread() * 100,
    fair_rate_pct = swap_obj$fairRate() * 100
  )
}

print_curve_tbl <- function(curve, helpers) {
  n_helpers <- helpers$size()
  
  tibble(idx = seq_len(n_helpers)) %>%
    mutate(
      helper = map(
        idx,
        function(i) helpers[i][[1]]
      ),
      pillar = map(
        helper,
        function(h) RateHelper_pillarDate(h)
      ),
      pillar_chr = map_chr(
        pillar,
        function(d) Date_ISO(d)
      ),
      zero_rate = map_dbl(
        pillar,
        function(d) {
          r <- YieldTermStructure_zeroRate(
            curve,
            d,
            Actual365Fixed(),
            "Continuous",
            "Annual"
          )
          InterestRate_rate(r) * 100.0
        }
      ),
      df = map_dbl(
        pillar,
        function(d) YieldTermStructure_discount(curve, d)
      ),
      implied_rate = map_dbl(
        helper,
        function(h) RateHelper_impliedQuote(h)
      )
    ) %>%
    select(
      pillar = pillar_chr,
      zeroRate = zero_rate,
      df,
      impliedRate = implied_rate
    )
}

## =========================================================
## 1. global data
## =========================================================

calendar <- TARGET()

todaysDate <- qldate("2001-09-06")
invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

settlementDays <- 2
settlementDate <- Calendar_advance(calendar, todaysDate, settlementDays, "Days")

cat("Today          :", Date_ISO(todaysDate), "\n")
cat("Settlement Date:", Date_ISO(settlementDate), "\n")

## =========================================================
## 2. market quotes
## =========================================================

deposits_tbl <- tibble(
  tenor = c("3M"),
  rate = c(0.0363)
)

fras_tbl <- tibble(
  fra = c("3x6", "6x9", "9x12"),
  rate = c(0.037125, 0.037125, 0.037125)
) %>%
  separate(fra, into = c("n", "m"), sep = "x", convert = TRUE)

futures_tbl <- tibble(
  imm_date = c(
    "2001-12-19",
    "2002-03-20",
    "2002-06-19",
    "2002-09-18",
    "2002-12-18",
    "2003-03-19",
    "2003-06-18",
    "2003-09-17"
  ),
  price = c(
    96.2875,
    96.7875,
    96.9875,
    96.6875,
    96.4875,
    96.3875,
    96.2875,
    96.0875
  )
) %>%
  mutate(
    imm_qldate = map(imm_date, qldate)
  )

swaps_tbl <- tibble(
  tenor = c("3Y", "5Y", "10Y", "15Y"),
  rate = c(0.0398, 0.0443, 0.05165, 0.055175)
)

## =========================================================
## 3. deposit / FRA / futures / swap helpers
## =========================================================

deposit_helpers_tbl <- deposits_tbl %>%
  mutate(
    period = map(
      tenor,
      function(x) PeriodParser_parse(x)
    ),
    helper = map2(
      rate,
      period,
      function(r, p) {
        DepositRateHelper(
          to_qh(r),
          p,
          settlementDays,
          calendar,
          "ModifiedFollowing",
          FALSE,
          Actual360()
        )
      }
    )
  )

fra_helpers_tbl <- fras_tbl %>%
  mutate(
    helper = pmap(
      list(rate, n, m),
      function(r, n_month, m_month) {
        FraRateHelper(
          to_qh(r),
          n_month,
          m_month,
          settlementDays,
          calendar,
          "ModifiedFollowing",
          FALSE,
          Actual360()
        )
      }
    )
  )

futures_helpers_tbl <- futures_tbl %>%
  mutate(
    helper = map2(
      price,
      imm_qldate,
      function(px, d) {
        FuturesRateHelper(
          to_qh(px),
          d,
          3,
          calendar,
          "ModifiedFollowing",
          TRUE,
          Actual360(),
          to_qh(0.0)
        )
      }
    )
  )

discountTermStructure <- YieldTermStructureHandle(
  FlatForward(settlementDate, 0.04, Actual360())
)

fixedLegFrequency <- "Annual"
fixedLegTenor <- PeriodParser_parse("1Y")
fixedLegAdjustment <- "Unadjusted"
fixedLegDayCounter <- Thirty360(Thirty360_BondBasis_get())
floatingLegTenor <- PeriodParser_parse("3M")

swap_helpers_tbl <- swaps_tbl %>%
  mutate(
    period = map(
      tenor,
      function(x) PeriodParser_parse(x)
    ),
    helper = map2(
      rate,
      period,
      function(r, p) {
        SwapRateHelper(
          to_qh(r),
          p,
          calendar,
          fixedLegFrequency,
          fixedLegAdjustment,
          fixedLegDayCounter,
          Euribor3M(),
          QuoteHandle(),
          PeriodParser_parse("0D"),
          discountTermStructure
        )
      }
    )
  )

## =========================================================
## 4. helper vectors
## =========================================================

allHelpers_DepoFutSwap <- append_rate_helpers(
  c(
    deposit_helpers_tbl$helper,
    futures_helpers_tbl$helper,
    swap_helpers_tbl$helper
  )
)

allHelpers_DepoFraSwap <- append_rate_helpers(
  c(
    deposit_helpers_tbl$helper,
    fra_helpers_tbl$helper,
    swap_helpers_tbl$helper
  )
)

## =========================================================
## 5. term structure construction
## =========================================================

depoFuturesSwapCurve <- PiecewiseFlatForward(
  settlementDate,
  allHelpers_DepoFutSwap,
  Actual360()
)

depoFraSwapCurve <- PiecewiseFlatForward(
  settlementDate,
  allHelpers_DepoFraSwap,
  Actual360()
)

cat("\n=== Deposit / Futures / Swap curve ===\n")
curve_tbl_depo_fut_swap <- print_curve_tbl(depoFuturesSwapCurve, allHelpers_DepoFutSwap)
print(curve_tbl_depo_fut_swap, n = Inf)

cat("\n=== Deposit / FRA / Swap curve ===\n")
curve_tbl_depo_fra_swap <- print_curve_tbl(depoFraSwapCurve, allHelpers_DepoFraSwap)
print(curve_tbl_depo_fra_swap, n = Inf)

## =========================================================
## 6. swap pricing setup
## =========================================================

forecastTermStructure <- RelinkableYieldTermStructureHandle()
swapEngine <- DiscountingSwapEngine(discountTermStructure)

nominal <- 1000000
swap_length_years <- 5
maturity <- Calendar_advance(calendar, settlementDate, swap_length_years, "Years")

fixedRate <- swaps_tbl %>%
  filter(tenor == "5Y") %>%
  pull(rate)

index <- Euribor3M(forecastTermStructure)
floatingLegDayCounter <- index$dayCounter()

## =========================================================
## 7. spot swap
## =========================================================

spot_fixedSchedule <- Schedule(
  settlementDate,
  maturity,
  fixedLegTenor,
  calendar,
  fixedLegAdjustment,
  fixedLegAdjustment,
  DateGeneration_Forward_get(),
  FALSE
)

spot_floatingSchedule <- Schedule(
  settlementDate,
  maturity,
  floatingLegTenor,
  calendar,
  "ModifiedFollowing",
  "ModifiedFollowing",
  DateGeneration_Forward_get(),
  FALSE
)

spot_swap <- VanillaSwap(
  Swap_Payer_get(),
  nominal,
  spot_fixedSchedule,
  fixedRate,
  fixedLegDayCounter,
  spot_floatingSchedule,
  index,
  0.0,
  floatingLegDayCounter
)

spot_swap$setPricingEngine(swapEngine)

## =========================================================
## 8. forward swap
## =========================================================

forwardStart <- Calendar_advance(calendar, settlementDate, 1, "Years")
forwardEnd <- Calendar_advance(calendar, forwardStart, swap_length_years, "Years")

forward_fixedSchedule <- Schedule(
  forwardStart,
  forwardEnd,
  fixedLegTenor,
  calendar,
  fixedLegAdjustment,
  fixedLegAdjustment,
  DateGeneration_Forward_get(),
  FALSE
)

forward_floatingSchedule <- Schedule(
  forwardStart,
  forwardEnd,
  floatingLegTenor,
  calendar,
  "ModifiedFollowing",
  "ModifiedFollowing",
  DateGeneration_Forward_get(),
  FALSE
)

forward_swap <- VanillaSwap(
  Swap_Payer_get(),
  nominal,
  forward_fixedSchedule,
  fixedRate,
  fixedLegDayCounter,
  forward_floatingSchedule,
  index,
  0.0,
  floatingLegDayCounter
)

forward_swap$setPricingEngine(swapEngine)

cat("\nQuoted 5Y market rate:\n")
print(swaps_tbl %>% filter(tenor == "5Y") %>% select(tenor, rate))

## =========================================================
## 9. scenario pricing
## =========================================================

scenario_tbl <- tibble(
  scenario = c(
    "spot / depo-futures-swap",
    "spot / depo-fra-swap",
    "forward / depo-futures-swap",
    "forward / depo-fra-swap"
  ),
  curve = list(
    depoFuturesSwapCurve,
    depoFraSwapCurve,
    depoFuturesSwapCurve,
    depoFraSwapCurve
  ),
  swap = list(
    spot_swap,
    spot_swap,
    forward_swap,
    forward_swap
  )
)

scenario_result_tbl <- scenario_tbl %>%
  mutate(
    linked = map(
      curve,
      function(curve_obj) {
        forecastTermStructure$linkTo(curve_obj)
        TRUE
      }
    ),
    result = map(
      swap,
      function(swap_obj) {
        show_swap_tbl(swap_obj)
      }
    )
  ) %>%
  select(-linked) %>%
  unnest(result)

cat("\n=== Swap pricing results ===\n")
print(scenario_result_tbl, n = Inf)