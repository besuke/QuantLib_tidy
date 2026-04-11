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

append_rate_helpers <- function(helper_list) {
  vec <- RateHelperVector()
  purrr::walk(
    helper_list,
    function(h) {
      vec$append(h)
    }
  )
  vec
}

append_dates <- function(date_list) {
  vec <- DateVector()
  purrr::walk(
    date_list,
    function(d) {
      vec$append(d)
    }
  )
  vec
}

extract_curve_report_tbl <- function(curve, helpers_vec) {
  tibble(
    idx = seq_len(helpers_vec$size())
  ) %>%
    mutate(
      helper = map(
        idx,
        function(i) {
          helpers_vec[i][[1]]
        }
      ),
      pillar = map(
        helper,
        function(h) {
          RateHelper_pillarDate(h)
        }
      ),
      pillar_chr = map_chr(
        pillar,
        function(d) {
          Date_ISO(d)
        }
      ),
      day_counter = map(
        idx,
        function(i) {
          if (i <= 13) {
            Actual360()
          } else {
            Thirty360(Thirty360_BondBasis_get())
          }
        }
      ),
      compounding = map_chr(
        idx,
        function(i) {
          if (i <= 13) {
            "Simple"
          } else {
            "SimpleThenCompounded"
          }
        }
      ),
      zero_rate = pmap_dbl(
        list(pillar, day_counter, compounding),
        function(pillar_date, dc, comp) {
          r <- YieldTermStructure_zeroRate(
            curve,
            pillar_date,
            dc,
            comp,
            "Annual"
          )
          InterestRate_rate(r) * 100.0
        }
      )
    ) %>%
    select(
      pillar = pillar_chr,
      zeroRate = zero_rate
    )
}

## =========================================================
## 1. global data
## =========================================================

calendar <- TARGET()

todaysDate <- qldate("2019-09-26")
invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

settlementDays <- 2
settlementDate <- Calendar_advance(calendar, todaysDate, settlementDays, "Days")
spot <- settlementDate

cat("Today          :", Date_ISO(todaysDate), "\n")
cat("Settlement Date:", Date_ISO(settlementDate), "\n")

## =========================================================
## 2. market data
## =========================================================

refMktRates <- c(
  -0.373,
  -0.388,
  -0.402,
  -0.418,
  -0.431,
  -0.441,
  -0.450,
  -0.457,
  -0.463,
  -0.469,
  -0.461,
  -0.463,
  -0.479,
  -0.4511,
  -0.45418,
  -0.439,
  -0.4124,
  -0.37703,
  -0.3335,
  -0.28168,
  -0.22725,
  -0.1745,
  -0.12425,
  -0.07746,
  0.0385,
  0.1435,
  0.17525,
  0.17275,
  0.1515,
  0.1225,
  0.095,
  0.0644
)

index <- Euribor6M()

## =========================================================
## 3. market instruments as tibbles
## =========================================================

deposit_tbl <- tibble(
  instrument_type = "deposit",
  order_id = 1L,
  rate = refMktRates[1],
  tenor_n = 6L,
  tenor_unit = "Months"
) %>%
  mutate(
    tenor = map2(
      tenor_n,
      tenor_unit,
      function(n, unit) {
        Period(n, unit)
      }
    ),
    helper = map2(
      rate,
      tenor,
      function(r, tenor_obj) {
        DepositRateHelper(
          r / 100.0,
          tenor_obj,
          2,
          calendar,
          "ModifiedFollowing",
          TRUE,
          Actual360()
        )
      }
    )
  )

fra_tbl <- tibble(
  instrument_type = "fra",
  order_id = 2:13,
  rate = refMktRates[2:13],
  fra_start = 3:14
) %>%
  mutate(
    helper = map2(
      rate,
      fra_start,
      function(r, start_month) {
        FraRateHelper(
          r / 100.0,
          start_month,
          index
        )
      }
    )
  )

swapTenors <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 20, 25, 30, 35, 40, 45, 50)

swap_tbl <- tibble(
  instrument_type = "swap",
  order_id = 14:length(refMktRates),
  rate = refMktRates[14:length(refMktRates)],
  tenor_n = swapTenors,
  tenor_unit = "Years"
) %>%
  mutate(
    tenor = map2(
      tenor_n,
      tenor_unit,
      function(n, unit) {
        Period(n, unit)
      }
    ),
    helper = map2(
      rate,
      tenor,
      function(r, tenor_obj) {
        SwapRateHelper(
          r / 100.0,
          tenor_obj,
          calendar,
          "Annual",
          "ModifiedFollowing",
          Thirty360(Thirty360_BondBasis_get()),
          index
        )
      }
    )
  )

market_helpers_tbl <- bind_rows(
  deposit_tbl %>% select(instrument_type, order_id, rate, helper),
  fra_tbl %>% select(instrument_type, order_id, rate, helper),
  swap_tbl %>% select(instrument_type, order_id, rate, helper)
) %>%
  arrange(order_id)

helpers <- append_rate_helpers(market_helpers_tbl$helper)

## =========================================================
## 4. additional synthetic helpers
## =========================================================

additional_helper_tbl <- tibble(
  fra_start = 12:18,
  rate = -0.004
) %>%
  mutate(
    helper = map2(
      rate,
      fra_start,
      function(r, start_month) {
        FraRateHelper(
          r,
          start_month,
          index
        )
      }
    )
  )

additional_date_tbl <- tibble(
  month_ahead = 1:5
) %>%
  mutate(
    date = map(
      month_ahead,
      function(m) {
        Calendar_advance(calendar, spot, Period(m, "Months"))
      }
    ),
    date_chr = map_chr(
      date,
      function(d) {
        Date_ISO(d)
      }
    )
  )

additional_helpers <- append_rate_helpers(additional_helper_tbl$helper)
additional_dates <- append_dates(additional_date_tbl$date)

## =========================================================
## 5. global bootstrap + curve
## =========================================================

gb <- GlobalBootstrap(
  additional_helpers,
  additional_dates,
  1.0e-4
)

curve <- suppressWarnings(
  GlobalLinearSimpleZeroCurve(
    spot,
    helpers,
    Actual365Fixed(),
    gb
  )
)

curve$enableExtrapolation()

## =========================================================
## 6. report
## =========================================================

report_tbl <- extract_curve_report_tbl(curve, helpers)

print(report_tbl, n = Inf)

