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

period_chr <- function(p) {
  Period___str__(p)
}

append_default_helpers <- function(helper_list) {
  vec <- DefaultProbabilityHelperVector()
  purrr::walk(
    helper_list,
    function(h) {
      vec$append(h)
    }
  )
  vec
}

extract_hazard_tbl <- function(hazard_curve) {
  curve_dates <- hazard_curve$dates()
  hz_rates <- HazardRateCurve_hazardRates(hazard_curve)
  
  tibble(
    idx = seq_len(curve_dates$size())
  ) %>%
    mutate(
      date = map_chr(
        idx,
        function(i) Date_ISO(curve_dates[i][[1]])
      ),
      hazard_rate = map_dbl(
        idx,
        function(i) hz_rates[i]
      )
    ) %>%
    select(date, hazard_rate)
}

## =========================================================
## 1. global data
## =========================================================

calendar <- TARGET()

todaysDate <- qldate("2007-05-15")
invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

settlementDays <- 3
settlementDate <- Calendar_advance(calendar, todaysDate, settlementDays, "Days")

cat("Today          :", Date_ISO(todaysDate), "\n")
cat("Settlement Date:", Date_ISO(settlementDate), "\n")

risk_free_rate <- YieldTermStructureHandle(
  FlatForward(todaysDate, 0.01, Actual365Fixed())
)

## =========================================================
## 2. CDS parameters
## =========================================================

recovery_rate <- 0.5
nominal <- 1000000.0

cds_quotes <- tibble(
  spread = c(0.0150, 0.0150, 0.0150, 0.0150),
  tenor_n = c(3, 6, 1, 2),
  tenor_unit = c("Months", "Months", "Years", "Years")
) %>%
  mutate(
    tenor = map2(
      tenor_n,
      tenor_unit,
      function(n, unit) {
        Period(n, unit)
      }
    ),
    tenor_chr = map_chr(
      tenor,
      function(p) period_chr(p)
    ),
    maturity = map(
      tenor,
      function(p) {
        Calendar_adjust(
          calendar,
          Calendar_advance(calendar, todaysDate, p),
          "Following"
        )
      }
    ),
    maturity_chr = map_chr(
      maturity,
      function(d) Date_ISO(d)
    )
  )

## =========================================================
## 3. Helper construction
## =========================================================

cds_quotes <- cds_quotes %>%
  mutate(
    helper = map2(
      spread,
      tenor,
      function(s, tenor_obj) {
        SpreadCdsHelper(
          to_qh(s),
          tenor_obj,
          0,
          calendar,
          "Quarterly",
          "Following",
          DateGeneration_TwentiethIMM_get(),
          Actual365Fixed(),
          recovery_rate,
          risk_free_rate
        )
      }
    )
  )

instruments <- append_default_helpers(cds_quotes$helper)

## =========================================================
## 4. Hazard curve bootstrap
## =========================================================

hazard_curve <- PiecewiseFlatHazardRate(
  todaysDate,
  instruments,
  Actual365Fixed()
)

hazard_tbl <- extract_hazard_tbl(hazard_curve)

cat("\nCalibrated hazard rate values:\n")
print(hazard_tbl, n = Inf)

## =========================================================
## 5. Survival probabilities
## =========================================================

survival_tbl <- tibble(
  horizon = c("1Y", "2Y"),
  period = list(
    Period(1, "Years"),
    Period(2, "Years")
  ),
  expected = c(0.9704, 0.9418)
) %>%
  mutate(
    target_date = map(
      period,
      function(p) {
        Calendar_advance(calendar, todaysDate, p)
      }
    ),
    survival_probability = map_dbl(
      target_date,
      function(d) {
        DefaultProbabilityTermStructure_survivalProbability(hazard_curve, d)
      }
    )
  ) %>%
  select(horizon, survival_probability, expected)

cat("\nSurvival probabilities:\n")
print(survival_tbl, n = Inf)

## =========================================================
## 6. Repricing setup
## =========================================================

probability <- DefaultProbabilityTermStructureHandle(hazard_curve)

cds_quotes <- cds_quotes %>%
  mutate(
    schedule = map(
      maturity,
      function(mat) {
        Schedule(
          todaysDate,
          mat,
          Period(3, "Months"),
          calendar,
          "Following",
          "Unadjusted",
          DateGeneration_TwentiethIMM_get(),
          FALSE
        )
      }
    ),
    cds = map2(
      spread,
      schedule,
      function(s, sched) {
        CreditDefaultSwap(
          Protection_Seller_get(),
          nominal,
          s,
          sched,
          "Following",
          Actual365Fixed()
        )
      }
    ),
    engine = map(
      seq_len(n()),
      function(i) {
        MidPointCdsEngine(probability, recovery_rate, risk_free_rate)
      }
    )
  )

## pricing engine をセット
purrr::walk2(
  cds_quotes$cds,
  cds_quotes$engine,
  function(cds_obj, eng) {
    cds_obj$setPricingEngine(eng)
  }
)

## =========================================================
## 7. Repricing results
## =========================================================

repricing_tbl <- cds_quotes %>%
  transmute(
    tenor = tenor_chr,
    maturity = maturity_chr,
    quoted_spread = spread,
    fair_spread = map_dbl(
      cds,
      function(cds_obj) {
        cds_obj$fairSpread()
      }
    ),
    NPV = map_dbl(
      cds,
      function(cds_obj) {
        cds_obj$NPV()
      }
    ),
    default_leg_NPV = map_dbl(
      cds,
      function(cds_obj) {
        cds_obj$defaultLegNPV()
      }
    ),
    coupon_leg_NPV = map_dbl(
      cds,
      function(cds_obj) {
        cds_obj$couponLegNPV()
      }
    )
  )

cat("\nRepricing of quoted CDSs employed for calibration:\n")
print(repricing_tbl, n = Inf)

## =========================================================
## 8. optional plots
## =========================================================

hazard_tbl %>%
  ggplot(aes(x = seq_along(hazard_rate), y = hazard_rate)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = seq_len(nrow(hazard_tbl)),
    labels = hazard_tbl$date
  ) +
  theme_minimal() +
  labs(
    title = "Calibrated hazard rates",
    x = "Date",
    y = "Hazard rate"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

repricing_tbl %>%
  select(tenor, quoted_spread, fair_spread) %>%
  pivot_longer(
    cols = c(quoted_spread, fair_spread),
    names_to = "type",
    values_to = "spread"
  ) %>%
  ggplot(aes(x = tenor, y = spread, fill = type)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Quoted spread vs fair spread",
    x = "Tenor",
    y = "Spread"
  )