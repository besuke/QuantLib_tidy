suppressPackageStartupMessages({
  library(QuantLib)
  library(tidyverse)
})

## =========================================================
## utility
## =========================================================

to_qh <- function(x) {
  QuoteHandle(SimpleQuote(x))
}

build_ratehelper_vector <- function(helpers) {
  vec <- RateHelperVector()
  for (h in helpers) {
    RateHelperVector_append(vec, h)
  }
  vec
}

curve_dfs_tbl <- function(curve) {
  curve_dates <- curve$dates()
  n_dates <- curve_dates$size()
  
  date_chr <- character(n_dates)
  df_vec   <- numeric(n_dates)
  
  for (i in seq_len(n_dates)) {
    d <- curve_dates[i][[1]]
    date_chr[i] <- Date_ISO(d)
    df_vec[i]   <- curve$discount(d)
  }
  
  tibble(
    date = date_chr,
    DF   = df_vec
  )
}

curve_zeros_tbl <- function(curve, day_counter) {
  curve_dates <- curve$dates()
  n_dates <- curve_dates$size()
  
  date_chr <- character(n_dates)
  zero_vec <- numeric(n_dates)
  
  for (i in seq_len(n_dates)) {
    d <- curve_dates[i][[1]]
    zr <- curve$zeroRate(d, day_counter, "Continuous")
    date_chr[i] <- Date_ISO(d)
    zero_vec[i] <- InterestRate_rate(zr) * 100.0
  }
  
  tibble(
    date = date_chr,
    zero_rate = zero_vec
  )
}

plot_zero_curves <- function(curves, horizon = 60, n = 720) {
  times <- seq(0, horizon, length.out = n)
  
  plot_df <- map_dfr(seq_along(curves), function(i) {
    tibble(
      curve_id = paste0("curve_", i),
      t = times,
      zero_rate = map_dbl(
        times,
        function(x) {
          zr <- curves[[i]]$zeroRate(x, "Continuous")
          InterestRate_rate(zr) * 100.0
        }
      )
    )
  })
  
  ggplot(plot_df, aes(x = t, y = zero_rate, color = curve_id)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Years", y = "Zero rate (%)", color = "Curve")
}

## =========================================================
## Chapter 3 : OIS curve construction (TONA)
## =========================================================

today <- DateParser_parseISO("2022-05-10")
invisible(Settings_instance()$setEvaluationDate(d = today))

tona <- OvernightIndex(
  "TONA",
  0,
  JPYCurrency(),
  Japan(),
  Actual365Fixed()
)

## Fixings
Index_addFixing(tona, today, 0.04 / 100.0)

fixing_tbl <- tibble(
  fixing_date = c("2022-05-09", "2022-05-06"),
  rate        = c(0.04 / 100.0, 0.04 / 100.0)
)

fixing_dates <- DateVector()
## 先に既存の TONA fixing を消す
tona$clearFixings()

## その後で入れ直す
Index_addFixing(tona, today, 0.04 / 100.0)

fixing_tbl <- tibble::tibble(
  fixing_date = c("2022-05-09", "2022-05-06"),
  rate        = c(0.04 / 100.0, 0.04 / 100.0)
)

fixing_dates <- DateVector()
fixing_ql_dates <- lapply(fixing_tbl$fixing_date, DateParser_parseISO)
fixing_ql_dates <- map(fixing_tbl$fixing_date, ~DateParser_parseISO(.x))
for (d in fixing_ql_dates) {
  DateVector_append(fixing_dates, d)
}

Index_addFixings(tona, fixing_dates, fixing_tbl$rate)

for (d in fixing_ql_dates) {
  DateVector_append(fixing_dates, d)
}

## Market quotes
depo_rates <- tribble(
  ~fixing_days, ~quote,
  0,  0.04,
  1,  0.04,
  2,  0.04
)

ois_rates <- tribble(
  ~n, ~unit,     ~quote,
  1, "Weeks",   0.070,
  2, "Weeks",   0.069,
  3, "Weeks",   0.078,
  1, "Months",  0.074,
  2, "Months",  0.046,
  3, "Months",  0.016,
  6, "Months", -0.007,
  9, "Months", -0.013,
  12, "Months", -0.014,
  15, "Months",  0.002,
  18, "Months",  0.008,
  21, "Months",  0.021,
  2, "Years",   0.036,
  3, "Years",   0.127,
  4, "Years",   0.274,
  5, "Years",   0.456,
  6, "Years",   0.647,
  7, "Years",   0.827,
  8, "Years",   0.996,
  9, "Years",   1.147,
  10, "Years",   1.280,
  11, "Years",   1.404,
  12, "Years",   1.516,
  15, "Years",   1.764,
  20, "Years",   1.939,
  25, "Years",   2.003,
  30, "Years",   2.038
)

ois_periods <- map2(ois_rates$n, ois_rates$unit, Period)

## Helpers
depo_helpers <- pmap(
  list(depo_rates$fixing_days, depo_rates$quote),
  function(fixing_days, quote) {
    DepositRateHelper(
      to_qh(quote / 100.0),
      tona$tenor(),
      fixing_days,
      tona$fixingCalendar(),
      tona$businessDayConvention(),
      tona$endOfMonth(),
      tona$dayCounter()
    )
  }
)

ois_helpers <- map2(
  ois_periods, ois_rates$quote,
  function(period, quote) {
    OISRateHelper(
      2,
      period,
      to_qh(quote / 100.0),
      tona
    )
  }
)

helpers_vec <- build_ratehelper_vector(c(depo_helpers, ois_helpers))

curve_calendar    <- Japan()
curve_day_counter <- Actual365Fixed()

tona_curve <- PiecewiseLogCubicDiscount(
  0,
  curve_calendar,
  helpers_vec,
  curve_day_counter
)

TermStructure_enableExtrapolation(tona_curve)

## Nodes-based outputs
curve_df <- curve_dfs_tbl(tona_curve)
curve_zero_df <- curve_zeros_tbl(tona_curve, curve_day_counter)

print(curve_df, n = Inf)
print(curve_zero_df, n = Inf)

## Interpolated grid by time
grid_tbl <- tibble(
  years = seq(0, 50, by = 1 / 365)
) %>%
  mutate(
    DF = map_dbl(
      years,
      function(t) tona_curve$discount(t)
    ),
    zero_rate = map_dbl(
      years,
      function(t) {
        zr <- tona_curve$zeroRate(t, "Continuous")
        InterestRate_rate(zr) * 100.0
      }
    )
  )

ggplot(grid_tbl, aes(x = years, y = DF)) +
  geom_line() +
  theme_minimal() +
  labs(title = "TONA discount factors", x = "Years", y = "DF")

ggplot(grid_tbl, aes(x = years, y = zero_rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "TONA zero rates", x = "Years", y = "Zero rate (%)")

## =========================================================
## Chapter 4 : Sensitivities (ESTR)
## =========================================================

today <- DateParser_parseISO("2022-05-25")
invisible(Settings_instance()$setEvaluationDate(d = today))

estr <- Estr()
Index_addFixing(estr, today, 0.312 / 100.0)

depo_rates_estr <- tibble::tribble(
  ~fixing_days, ~quote,
  0, 0.312,
  1, 0.312,
  2, 0.312
)

ois_rates_estr <- tibble::tribble(
  ~n, ~unit,     ~quote,
  1, "Months", 0.293,
  2, "Months", 0.272,
  3, "Months", 0.260,
  4, "Months", 0.256,
  5, "Months", 0.252,
  6, "Months", 0.248,
  7, "Months", 0.254,
  8, "Months", 0.261,
  9, "Months", 0.267,
  10, "Months", 0.279,
  11, "Months", 0.291,
  1, "Years",  0.303,
  15, "Months", 0.318,
  18, "Months", 0.338,
  21, "Months", 0.351,
  2, "Years",  0.369,
  3, "Years",  0.424,
  4, "Years",  0.576,
  5, "Years",  0.762,
  6, "Years",  0.954,
  7, "Years",  1.135,
  8, "Years",  1.303,
  9, "Years",  1.452,
  10, "Years",  1.584,
  12, "Years",  1.809,
  15, "Years",  2.037,
  20, "Years",  2.187,
  25, "Years",  2.234,
  30, "Years",  2.256,
  35, "Years",  2.295,
  40, "Years",  2.348,
  50, "Years",  2.421,
  60, "Years",  2.463
)
quotes <- list()

depo_helpers_estr <- vector("list", nrow(depo_rates_estr))
for (i in seq_len(nrow(depo_rates_estr))) {
  q <- SimpleQuote(depo_rates_estr$quote[i] / 100.0)
  quotes[[length(quotes) + 1]] <- q
  
  depo_helpers_estr[[i]] <- DepositRateHelper(
    QuoteHandle(q),
    estr$tenor(),
    depo_rates_estr$fixing_days[i],
    estr$fixingCalendar(),
    estr$businessDayConvention(),
    estr$endOfMonth(),
    estr$dayCounter()
  )
}

ois_helpers_estr <- vector("list", nrow(ois_rates_estr))
for (i in seq_len(nrow(ois_rates_estr))) {
  q <- SimpleQuote(ois_rates_estr$quote[i] / 100.0)
  quotes[[length(quotes) + 1]] <- q
  
  tenor_period <- Period(ois_rates_estr$n[i], ois_rates_estr$unit[i])
  
  ois_helpers_estr[[i]] <- OISRateHelper(
    2,
    tenor_period,
    QuoteHandle(q),
    estr
  )
}
helpers_vec_estr <- build_ratehelper_vector(c(depo_helpers_estr, ois_helpers_estr))

curve_calendar <- TARGET()
curve_day_counter <- Actual365Fixed()

ois_curve <- PiecewiseLogCubicDiscount(
  0,
  curve_calendar,
  helpers_vec_estr,
  curve_day_counter
)

TermStructure_enableExtrapolation(ois_curve)

## curve check
times <- seq(0, 60, length.out = 720)
zero_plot_tbl <- tibble(
  years = times,
  zero_rate = map_dbl(
    times,
    function(t) {
      zr <- ois_curve$zeroRate(t, "Continuous")
      InterestRate_rate(zr) * 100.0
    }
  )
)

ggplot(zero_plot_tbl, aes(x = years, y = zero_rate)) +
  geom_line() +
  theme_minimal() +
  labs(title = "ESTR zero curve", x = "Years", y = "Zero rate (%)")

