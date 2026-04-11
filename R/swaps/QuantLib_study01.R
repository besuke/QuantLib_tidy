suppressMessages(library(QuantLib))

## =========================================================
## 1. 基準日の設定
## =========================================================

today <- Date(10, "May", 2022)
invisible(Settings_instance()$setEvaluationDate(d = today))

cat("Today :", today$`__str__`(), "\n")


## =========================================================
## 2. TONA index の作成
## =========================================================

tona <- OvernightIndex(
  "TONA",
  0,                 # fixing days
  JPYCurrency(),
  Japan(),
  Actual365Fixed()
)

cat("Fixing calendar        :", tona$fixingCalendar()$name(), "\n")
cat("Fixing days            :", tona$fixingDays(), "\n")
cat("Tenor                  :", tona$tenor()$`__str__`(), "\n")
cat("Business day convention:", tona$businessDayConvention(), "\n")
cat("End of month           :", tona$endOfMonth(), "\n")
cat("Day counter            :", DayCounter_name(tona$dayCounter()), "\n")


## =========================================================
## 3. Fixing の登録
## =========================================================

Index_addFixing(tona, today, 0.04 / 100.0)

fixing_dates <- DateVector()
DateVector_append(fixing_dates, Date(9, "May", 2022))
DateVector_append(fixing_dates, Date(6, "May", 2022))

fixing_rates <- c(
  0.04 / 100.0,
  0.04 / 100.0
)

Index_addFixings(tona, fixing_dates, fixing_rates)


## =========================================================
## 4. マーケットレート
## =========================================================

depo_rates <- list(
  list(fixing_days = 0, quote = 0.04),  # O/N
  list(fixing_days = 1, quote = 0.04),  # T/N
  list(fixing_days = 2, quote = 0.04)   # S/N
)

ois_rates <- list(
  list(period = Period(1,  "Weeks"),  quote = 0.070),
  list(period = Period(2,  "Weeks"),  quote = 0.069),
  list(period = Period(3,  "Weeks"),  quote = 0.078),
  list(period = Period(1,  "Months"), quote = 0.074),
  list(period = Period(2,  "Months"), quote = 0.046),
  list(period = Period(3,  "Months"), quote = 0.016),
  list(period = Period(6,  "Months"), quote = -0.007),
  list(period = Period(9,  "Months"), quote = -0.013),
  list(period = Period(12, "Months"), quote = -0.014),
  list(period = Period(15, "Months"), quote = 0.002),
  list(period = Period(18, "Months"), quote = 0.008),
  list(period = Period(21, "Months"), quote = 0.021),
  list(period = Period(2,  "Years"),  quote = 0.036),
  list(period = Period(3,  "Years"),  quote = 0.127),
  list(period = Period(4,  "Years"),  quote = 0.274),
  list(period = Period(5,  "Years"),  quote = 0.456),
  list(period = Period(6,  "Years"),  quote = 0.647),
  list(period = Period(7,  "Years"),  quote = 0.827),
  list(period = Period(8,  "Years"),  quote = 0.996),
  list(period = Period(9,  "Years"),  quote = 1.147),
  list(period = Period(10, "Years"),  quote = 1.280),
  list(period = Period(11, "Years"),  quote = 1.404),
  list(period = Period(12, "Years"),  quote = 1.516),
  list(period = Period(15, "Years"),  quote = 1.764),
  list(period = Period(20, "Years"),  quote = 1.939),
  list(period = Period(25, "Years"),  quote = 2.003),
  list(period = Period(30, "Years"),  quote = 2.038)
)


## =========================================================
## 5. DepositRateHelper
## =========================================================

depo_helpers <- lapply(depo_rates, function(x) {
  DepositRateHelper(
    QuoteHandle(SimpleQuote(x$quote / 100.0)),
    tona$tenor(),
    x$fixing_days,
    tona$fixingCalendar(),
    tona$businessDayConvention(),
    tona$endOfMonth(),
    tona$dayCounter()
  )
})


## =========================================================
## 6. OISRateHelper
##  ※ この SWIG 版では 4引数版が通りやすい
## =========================================================

ois_helpers <- lapply(ois_rates, function(x) {
  OISRateHelper(
    2,
    x$period,
    QuoteHandle(SimpleQuote(x$quote / 100.0)),
    tona
  )
})


## =========================================================
## 7. Helper を vector に詰める
## =========================================================

helpers_vec <- RateHelperVector()

for (h in c(depo_helpers, ois_helpers)) {
  RateHelperVector_append(helpers_vec, h)
}


## =========================================================
## 8. カーブ構築
## =========================================================

curve_calendar <- Japan()
curve_day_counter <- Actual365Fixed()

tona_curve <- PiecewiseLogCubicDiscount(
  0,
  curve_calendar,
  helpers_vec,
  curve_day_counter
)

TermStructure_enableExtrapolation(tona_curve)


## =========================================================
## 9. nodes の代わりに dates() + discount() で確認
##   ※ nodes() は環境によって stack overflow のことがある
## =========================================================

curve_dates <- tona_curve$dates()
n_dates <- curve_dates$size()

curve_df <- NULL

for (i in seq_len(n_dates)) {
  d <- curve_dates[i][[1]]
  curve_df <- rbind(
    curve_df,
    data.frame(
      date = Date_ISO(d),
      DF   = tona_curve$discount(d)
    )
  )
}

print(curve_df)


## =========================================================
## 10. ゼロレート確認
## =========================================================

curve_zero_df <- NULL

for (i in seq_len(n_dates)) {
  d <- curve_dates[i][[1]]
  zr <- tona_curve$zeroRate(d, curve_day_counter, "Continuous")
  
  curve_zero_df <- rbind(
tu    curve_zero_df,
    data.frame(
      date = Date_ISO(d),
      zero_rate = InterestRate_rate(zr) * 100.0
    )
  )
}

print(curve_zero_df)


## =========================================================
## 11. 補間グリッド上の DF / Zero Rate
## =========================================================

curve_ref_date <- tona_curve$referenceDate()
spot_date <- Calendar_advance(curve_calendar, curve_ref_date, 2, "Days")
max_date <- Calendar_advance(curve_calendar, spot_date, Period(50, "Years"))

serials <- seq(
  curve_ref_date$serialNumber(),
  max_date$serialNumber() - 1
)

interpl_dates <- lapply(serials, function(s) Date(s))

interpl_years <- sapply(interpl_dates, function(d) {
  DayCounter_yearFraction(curve_day_counter, curve_ref_date, d)
})

interpl_dfs <- sapply(interpl_dates, function(d) {
  tona_curve$discount(d)
})

plot(interpl_years, interpl_dfs, type = "l",
     main = "TONA discount factors",
     xlab = "Years", ylab = "DF")

interpl_zeros <- sapply(interpl_dates, function(d) {
  tona_curve$zeroRate(d, curve_day_counter, "Continuous")$rate() * 100.0
})

plot(interpl_years, interpl_zeros, type = "l",
     main = "TONA zero rates",
     xlab = "Years", ylab = "Zero rate (%)")

