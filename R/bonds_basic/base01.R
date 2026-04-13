suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch01.R (FULL SWIG-SAFE VERSION)
# ============================================================

# ------------------------------------------------------------
# 0. Utility helpers
# ------------------------------------------------------------

ql_date <- function(x) {
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  if (is.character(x)) return(DateParser_parseISO(x))
  stop("Unsupported date type")
}

advance_days <- function(cal, d, n) {
  Calendar_advance(cal, ql_date(d), as.integer(n), "Days")
}

period_months <- function(n) Period(as.integer(n), "Months")
period_years  <- function(n) Period(as.integer(n), "Years")

show_tbl <- function(tbl, title=NULL, n=10) {
  if (!is.null(title)) {
    cat("\n", strrep("=",70),"\n",title,"\n",strrep("=",70),"\n",sep="")
  }
  print(dplyr::slice_head(tbl,n=n))
}

build_rate_helper_vector <- function(lst) {
  v <- RateHelperVector()
  purrr::walk(lst, ~ RateHelperVector_append(v,.x))
  v
}

set_eval_date <- function(d) {
  Settings_instance()$setEvaluationDate(ql_date(d))
}

# ------------------------------------------------------------
# SAFE curve (NO zeroRate calls)
# ------------------------------------------------------------

curve_tbl <- function(curve, n=200) {
  TermStructure_enableExtrapolation(curve)
  max_t <- curve$maxTime()
  times <- seq(0,max_t,length.out=n)
  
  tibble(time=times) |> 
    mutate(
      discount = purrr::map_dbl(time, ~ curve$discount(.x)),
      zero = if_else(time>0, -log(discount)/time, 0)
    )
}

curve_tbl_with_dates <- function(curve,n=200) {
  ref <- as.Date(Date_ISO(curve$referenceDate()))
  curve_tbl(curve,n) |> 
    mutate(curve_date = ref + round(time*365))
}

# ------------------------------------------------------------
# 1. Calendar
# ------------------------------------------------------------

jp <- Japan()

show_tbl(tibble(
  date="2022-08-11",
  holiday=Calendar_isHoliday(jp, ql_date("2022-08-11"))
),"Holiday check")

show_tbl(tibble(
  start="2022-08-10",
  advanced=Date_ISO(advance_days(jp,"2022-08-10",2))
),"Advance example")

# ------------------------------------------------------------
# 2. ZeroCurve
# ------------------------------------------------------------

dv <- DateVector()
purrr::walk(c("2022-08-03","2022-11-04","2023-02-03","2023-08-03"),
            ~ DateVector_append(dv, ql_date(.x)))

rates <- c(0,0.01,0.02,0.03)
curve <- ZeroCurve(dv,rates,Actual365Fixed(),jp)
TermStructure_enableExtrapolation(curve)

show_tbl(curve_tbl(curve),"Curve table")

show_tbl(tibble(
  df = curve$discount(0.75),
  zero = -log(curve$discount(0.75))/0.75
),"Time query")

plot_tbl <- curve_tbl_with_dates(curve)
plot(plot_tbl$curve_date, plot_tbl$zero, type="l")

# ------------------------------------------------------------
# 3. Tibor
# ------------------------------------------------------------

set_eval_date("2022-08-01")

cal <- Japan()
settle <- advance_days(cal,"2022-08-01",2)

idx3 <- Tibor(period_months(3))
idx6 <- Tibor(period_months(6))
idx12<- Tibor(period_months(12))

q3 <- SimpleQuote(0.01)
q6 <- SimpleQuote(0.02)
q12<- SimpleQuote(0.03)

helpers <- list(
  DepositRateHelper(QuoteHandle(q3), idx3),
  DepositRateHelper(QuoteHandle(q6), idx6),
  DepositRateHelper(QuoteHandle(q12), idx12)
)

v <- build_rate_helper_vector(helpers)

curve_tibor <- PiecewiseLogLinearDiscount(settle,v,Actual365Fixed())
TermStructure_enableExtrapolation(curve_tibor)

show_tbl(curve_tbl(curve_tibor),"Tibor curve")

# SAFE summary
val_date <- tryCatch(idx3$valueDate(ql_date("2022-08-01")),
                     error=function(e) InterestRateIndex_valueDate(idx3,ql_date("2022-08-01")))
fix_date <- tryCatch(idx3$fixingDate(val_date),
                     error=function(e) InterestRateIndex_fixingDate(idx3,val_date))

show_tbl(tibble(
  trade="2022-08-01",
  settlement=Date_ISO(val_date),
  fixing=Date_ISO(fix_date),
  fixing_days=idx3$fixingDays()
),"Tibor summary")

# ------------------------------------------------------------
# 4. InterestRate
# ------------------------------------------------------------

ir <- InterestRate(0.01, Actual365Fixed(), Compounding_Simple_get(), Frequency_Annual_get())

show_tbl(tibble(
  rate=ir$rate(),
  df=ir$discountFactor(ql_date("2022-08-03"), ql_date("2022-11-04"))
),"InterestRate")

# ------------------------------------------------------------
# 5. FlatForward
# ------------------------------------------------------------

flat <- FlatForward(ql_date("2022-08-03"),0.01,Actual365Fixed())
TermStructure_enableExtrapolation(flat)

show_tbl(tibble(
  df = flat$discount(ql_date("2022-11-04"))
),"FlatForward")

cat("\nSUCCESS: script runs end-to-end\n")

