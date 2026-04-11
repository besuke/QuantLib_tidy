suppressPackageStartupMessages({
  library(QuantLib)
  library(tidyverse)
})

## =========================================================
## 0. 小物関数
## =========================================================

to_qh <- function(x) {
  QuoteHandle(SimpleQuote(x))
}

build_ratehelper_vector <- function(helpers) {
  vec <- RateHelperVector()
  purrr::walk(helpers, ~ RateHelperVector_append(vec, .x))
  vec
}

curve_dfs_tbl <- function(curve) {
  curve_dates <- curve$dates()
  n_dates <- curve_dates$size()
  
  ql_date_list <- purrr::map(seq_len(n_dates), function(i) curve_dates[i][[1]])
  date_chr     <- purrr::map_chr(ql_date_list, Date_ISO)
  df_vec       <- purrr::map_dbl(ql_date_list, function(d) curve$discount(d))
  
  tibble(
    date = date_chr,
    DF   = df_vec
  )
}

curve_zeros_tbl <- function(curve, day_counter) {
  curve_dates <- curve$dates()
  n_dates <- curve_dates$size()
  
  ql_date_list <- purrr::map(seq_len(n_dates), function(i) curve_dates[i][[1]])
  date_chr     <- purrr::map_chr(ql_date_list, Date_ISO)
  zero_vec     <- purrr::map_dbl(
    ql_date_list,
    function(d) {
      zr <- curve$zeroRate(d, day_counter, "Continuous")
      InterestRate_rate(zr) * 100.0
    }
  )
  
  tibble(
    date      = date_chr,
    zero_rate = zero_vec
  )
}

## =========================================================
## 1. 基準日の設定
## =========================================================

today <- DateParser_parseISO("2022-05-10")
invisible(Settings_instance()$setEvaluationDate(d = today))

cat("Today :", today$`__str__`(), "\n")

## =========================================================
## 2. TONA index の作成
## =========================================================

tona <- OvernightIndex(
  "TONA",
  0,
  JPYCurrency(),
  Japan(),
  Actual365Fixed()
)

tibble(
  item = c(
    "Fixing calendar",
    "Fixing days",
    "Tenor",
    "Business day convention",
    "End of month",
    "Day counter"
  ),
  value = c(
    tona$fixingCalendar()$name(),
    as.character(tona$fixingDays()),
    tona$tenor()$`__str__`(),
    as.character(tona$businessDayConvention()),
    as.character(tona$endOfMonth()),
    DayCounter_name(tona$dayCounter())
  )
) %>%
  print(n = Inf)

## =========================================================
## 3. Fixing の登録
## =========================================================

Index_addFixing(tona, today, 0.04 / 100.0)

fixing_tbl <- tibble(
  fixing_date = c("2022-05-09", "2022-05-06"),
  rate        = c(0.04 / 100.0, 0.04 / 100.0)
)

fixing_dates <- DateVector()
fixing_ql_dates <- purrr::map(fixing_tbl$fixing_date, ~DateParser_parseISO(.x))

purrr::walk(fixing_ql_dates, ~ DateVector_append(fixing_dates, .x))
Index_addFixings(tona, fixing_dates, fixing_tbl$rate)

## =========================================================
## 4. マーケットレート
## =========================================================

depo_rates <- tribble(
  ~fixing_days, ~quote,
  0,   0.04,   # O/N
  1,   0.04,   # T/N
  2,   0.04    # S/N
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

ois_periods <- purrr::map2(ois_rates$n, ois_rates$unit, Period)

## =========================================================
## 5. DepositRateHelper
## =========================================================

depo_helpers <- purrr::pmap(
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

## =========================================================
## 6. OISRateHelper
##   ※ SWIG 版では 4引数版を採用
## =========================================================

ois_helpers <- purrr::map2(
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

## =========================================================
## 7. Helper を vector に詰める
## =========================================================

helpers_vec <- c(depo_helpers, ois_helpers) %>%
  build_ratehelper_vector()

## =========================================================
## 8. カーブ構築
## =========================================================

curve_calendar    <- Japan()
curve_day_counter <- Actual365Fixed()

tona_curve <- PiecewiseLogCubicDiscount(
  0,
  curve_calendar,
  helpers_vec,
  curve_day_counter
)

TermStructure_enableExtrapolation(tona_curve)

## =========================================================
## 9. ノード上の DF
## =========================================================
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
  
  tibble::tibble(
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
  
  tibble::tibble(
    date = date_chr,
    zero_rate = zero_vec
  )
}
curve_df <- curve_dfs_tbl(tona_curve)
print(curve_df, n = Inf)

## =========================================================
## 10. ノード上のゼロレート
## =========================================================

curve_zero_df <- curve_zeros_tbl(tona_curve, curve_day_counter)
print(curve_zero_df, n = Inf)

## =========================================================
## 11. 補間グリッド上の DF / Zero Rate
##   ※ Date グリッドではなく time グリッドを使う
## =========================================================

grid_tbl <- tibble(
  years = seq(0, 50, by = 1 / 365)
) %>%
  mutate(
    DF = purrr::map_dbl(
      years,
      function(t) tona_curve$discount(t)
    ),
    zero_rate = purrr::map_dbl(
      years,
      function(t) {
        zr <- tona_curve$zeroRate(t, "Continuous")
        InterestRate_rate(zr) * 100.0
      }
    )
  )

## DF plot
grid_tbl %>%
  ggplot(aes(x = years, y = DF)) +
  geom_line() +
  labs(
    title = "TONA discount factors",
    x = "Years",
    y = "DF"
  ) +
  theme_minimal()

## Zero plot
grid_tbl %>%
  ggplot(aes(x = years, y = zero_rate)) +
  geom_line() +
  labs(
    title = "TONA zero rates",
    x = "Years",
    y = "Zero rate (%)"
  ) +
  theme_minimal()
