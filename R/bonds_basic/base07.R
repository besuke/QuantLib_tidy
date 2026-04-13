suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch07.R
# ------------------------------------------------------------
# Interest-rate derivatives
# Python notebook ch07.ipynb を R + QuantLib(SWIG) 向けに
# tidyverse 風に書き直した版。
#
# 内容:
# 1. フラット金利カーブ
# 2. Euribor6M index と VanillaSwap
# 3. Cap / Floor の基本評価
# 4. Swaption の基本評価
# 5. Black / Bachelier / Hull-White の比較土台
#
# 方針:
# - Date は DateParser_parseISO() を使う
# - Schedule / curve / cashflow を tibble 化
# - build 差が大きい engine / vol handle は tryCatch で吸収
# - まず end-to-end の骨格を優先
# ============================================================

# ------------------------------------------------------------
# 0. Utility helpers
# ------------------------------------------------------------

ql_date <- function(x) {
  if (inherits(x, "POSIXt")) x <- format(as.Date(x), "%Y-%m-%d")
  if (inherits(x, "Date"))   x <- format(x, "%Y-%m-%d")
  if (is.character(x) && length(x) == 1L) return(DateParser_parseISO(x))
  x
}

ql_iso <- function(x) {
  tryCatch(Date_ISO(x), error = function(e) as.character(x))
}

show_tbl <- function(tbl, title = NULL, n = 10) {
  if (!is.null(title)) {
    cat("\n", strrep("=", 72), "\n", title, "\n", strrep("=", 72), "\n", sep = "")
  }
  print(dplyr::slice_head(tbl, n = n))
  invisible(tbl)
}

set_eval_date <- function(eval_date) {
  Settings_instance()$setEvaluationDate(ql_date(eval_date))
}

period_months <- function(n) Period(as.integer(n), "Months")
period_years  <- function(n) Period(as.integer(n), "Years")

schedule_date_at <- function(schedule_obj, i_one_based) {
  idx0 <- as.integer(i_one_based - 1)
  
  out <- tryCatch(schedule_obj$date(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(schedule_obj$get(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(schedule_obj[[i_one_based]][[1]], error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  stop("Unable to access schedule date at index ", i_one_based)
}

make_schedule_tbl <- function(schedule_obj) {
  tibble(
    schedule_date = purrr::map_chr(seq_len(schedule_obj$size()), ~ ql_iso(schedule_date_at(schedule_obj, .x)))
  )
}

leg_cashflow_at <- function(leg_obj, i_one_based) {
  idx0 <- as.integer(i_one_based - 1)
  
  out <- tryCatch(leg_obj$get(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(Leg___getitem__(leg_obj, idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(leg_obj[[i_one_based]][[1]], error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  stop("Unable to access leg cashflow at index ", i_one_based)
}

cashflow_leg_tbl <- function(leg_obj, curve_obj = NULL) {
  ref_date_r <- if (!is.null(curve_obj)) as.Date(ql_iso(curve_obj$referenceDate())) else NA
  
  purrr::map_dfr(seq_len(leg_obj$size()), function(i) {
    cf_obj <- leg_cashflow_at(leg_obj, i)
    pay_date_obj <- CashFlow_date(cf_obj)
    pay_date_chr <- ql_iso(pay_date_obj)
    pay_date_r <- as.Date(pay_date_chr)
    amount_value <- tryCatch(CashFlow_amount(cf_obj), error = function(e) NA_real_)
    
    df_value <- if (!is.null(curve_obj)) {
      tryCatch(curve_obj$discount(pay_date_obj), error = function(e) NA_real_)
    } else {
      NA_real_
    }
    
    coupon_obj <- tryCatch(as_coupon(cf_obj), error = function(e) NULL)
    
    tibble(
      pay_date = pay_date_chr,
      accrual_start = if (!is.null(coupon_obj)) ql_iso(Coupon_accrualStartDate(coupon_obj)) else NA_character_,
      accrual_end = if (!is.null(coupon_obj)) ql_iso(Coupon_accrualEndDate(coupon_obj)) else NA_character_,
      amount = amount_value,
      rate = if (!is.null(coupon_obj)) tryCatch(coupon_obj$rate(), error = function(e) NA_real_) else NA_real_,
      df = df_value,
      pv = amount_value * df_value
    )
  })
}

curve_tbl <- function(curve_obj, n = 200, extrapolate = TRUE) {
  if (extrapolate) {
    TermStructure_enableExtrapolation(curve_obj)
  }
  
  ref_date_r <- as.Date(ql_iso(curve_obj$referenceDate()))
  max_t <- curve_obj$maxTime()
  times <- seq(0, max_t, length.out = n)
  
  tibble(time = times) %>%
    mutate(
      discount = purrr::map_dbl(time, ~ curve_obj$discount(.x)),
      zero = if_else(time > 0, -log(discount) / time, 0),
      curve_date = ref_date_r + round(time * 365)
    )
}

safe_npv <- function(obj) {
  tryCatch(obj$NPV(), error = function(e) NA_real_)
}

safe_engine_set <- function(obj, engine) {
  tryCatch({
    Instrument_setPricingEngine(obj, engine)
    TRUE
  }, error = function(e) FALSE)
}

# ------------------------------------------------------------
# 1. Market setup
# ------------------------------------------------------------

trade_date <- ql_date("2023-01-03")
set_eval_date(trade_date)

cal_target <- TARGET()
dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()
dc_30 <- Thirty360("BondBasis")
cmpd_compounded <- Compounding_Compounded_get()
freq_annual <- Frequency_Annual_get()
freq_semiannual <- Frequency_Semiannual_get()

flat_rate <- 0.03
flat_curve <- FlatForward(trade_date, flat_rate, dc_a365)
TermStructure_enableExtrapolation(flat_curve)
flat_curve_handle <- YieldTermStructureHandle(flat_curve)

show_tbl(curve_tbl(flat_curve), "Flat curve", n = 12)

# ------------------------------------------------------------
# 2. Vanilla swap setup
# ------------------------------------------------------------

effective_date <- ql_date("2023-01-05")
maturity_date <- ql_date("2028-01-05")
nominal <- 1e6
fixed_rate <- 0.03
spread <- 0.0

euribor6m_index <- Euribor6M(flat_curve_handle)

fixed_schedule <- Schedule(
  effective_date,
  maturity_date,
  period_years(1),
  cal_target,
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Backward",
  FALSE
)

float_schedule <- Schedule(
  effective_date,
  maturity_date,
  period_months(6),
  cal_target,
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Backward",
  FALSE
)

swap_obj <- VanillaSwap(
  Swap_Payer_get(),
  nominal,
  fixed_schedule,
  fixed_rate,
  dc_30,
  float_schedule,
  euribor6m_index,
  spread,
  dc_a360
)

swap_engine <- DiscountingSwapEngine(flat_curve_handle)
safe_engine_set(swap_obj, swap_engine)

show_tbl(make_schedule_tbl(fixed_schedule), "Fixed schedule", n = 20)
show_tbl(make_schedule_tbl(float_schedule), "Floating schedule", n = 20)

cat("Swap NPV      :", safe_npv(swap_obj), "\n")
cat("Swap fairRate :", tryCatch(swap_obj$fairRate(), error = function(e) NA_real_), "\n")
cat("Fixed leg NPV :", tryCatch(swap_obj$fixedLegNPV(), error = function(e) NA_real_), "\n")
cat("Float leg NPV :", tryCatch(swap_obj$floatingLegNPV(), error = function(e) NA_real_), "\n")

fixed_leg_tbl <- cashflow_leg_tbl(swap_obj$fixedLeg(), flat_curve)
float_leg_tbl <- cashflow_leg_tbl(swap_obj$floatingLeg(), flat_curve)

show_tbl(fixed_leg_tbl, "Swap fixed leg cashflows", n = 20)
show_tbl(float_leg_tbl, "Swap floating leg cashflows", n = 20)

# ------------------------------------------------------------
# 3. Cap / Floor setup
# ------------------------------------------------------------

float_leg_obj <- swap_obj$floatingLeg()
strike_rate <- 0.03

cap_obj <- tryCatch(Cap(float_leg_obj, c(strike_rate)), error = function(e) NULL)
floor_obj <- tryCatch(Floor(float_leg_obj, c(strike_rate)), error = function(e) NULL)

# Try Black engine first
cap_floor_vol <- 0.20
black_vol_quote <- SimpleQuote(cap_floor_vol)
black_vol_handle <- QuoteHandle(black_vol_quote)

cap_engine_black <- tryCatch(
  BlackCapFloorEngine(flat_curve_handle, black_vol_handle, dc_a365),
  error = function(e) NULL
)
if (is.null(cap_engine_black)) {
  cap_engine_black <- tryCatch(
    BlackCapFloorEngine(flat_curve_handle, black_vol_handle),
    error = function(e) NULL
  )
}

cap_engine_bachelier <- tryCatch(
  BachelierCapFloorEngine(flat_curve_handle, black_vol_handle, dc_a365),
  error = function(e) NULL
)
if (is.null(cap_engine_bachelier)) {
  cap_engine_bachelier <- tryCatch(
    BachelierCapFloorEngine(flat_curve_handle, black_vol_handle),
    error = function(e) NULL
  )
}

if (!is.null(cap_obj) && !is.null(cap_engine_black)) safe_engine_set(cap_obj, cap_engine_black)
if (!is.null(floor_obj) && !is.null(cap_engine_black)) safe_engine_set(floor_obj, cap_engine_black)

cap_floor_tbl <- tibble(
  instrument = c("Cap", "Floor"),
  available = c(!is.null(cap_obj), !is.null(floor_obj)),
  black_npv = c(if (!is.null(cap_obj)) safe_npv(cap_obj) else NA_real_, if (!is.null(floor_obj)) safe_npv(floor_obj) else NA_real_)
)

show_tbl(cap_floor_tbl, "Cap / Floor valuation (Black if available)", n = 20)

if (!is.null(cap_obj) && !is.null(cap_engine_bachelier)) {
  cap_obj_bach <- tryCatch(Cap(float_leg_obj, c(strike_rate)), error = function(e) NULL)
  if (!is.null(cap_obj_bach)) {
    safe_engine_set(cap_obj_bach, cap_engine_bachelier)
    show_tbl(
      tibble(
        model = c("Black", "Bachelier"),
        cap_npv = c(cap_floor_tbl$black_npv[1], safe_npv(cap_obj_bach))
      ),
      "Cap model comparison",
      n = 20
    )
  }
}

# ------------------------------------------------------------
# 4. Swaption setup
# ------------------------------------------------------------

exercise_date <- ql_date("2024-01-03")
exercise_obj <- EuropeanExercise(exercise_date)
swaption_obj <- tryCatch(Swaption(swap_obj, exercise_obj), error = function(e) NULL)

black_swaption_vol <- 0.20
swaption_vol_quote <- SimpleQuote(black_swaption_vol)
swaption_vol_handle <- QuoteHandle(swaption_vol_quote)

swaption_engine_black <- tryCatch(
  BlackSwaptionEngine(flat_curve_handle, swaption_vol_handle),
  error = function(e) NULL
)

hull_white_model <- tryCatch(HullWhite(flat_curve_handle, 0.03, 0.01), error = function(e) NULL)
jamshidian_engine <- if (!is.null(hull_white_model)) {
  tryCatch(JamshidianSwaptionEngine(hull_white_model), error = function(e) NULL)
} else {
  NULL
}

swaption_result_tbl <- tibble(
  engine = c("Black", "Jamshidian"),
  available = c(!is.null(swaption_engine_black), !is.null(jamshidian_engine)),
  npv = c(NA_real_, NA_real_)
)

if (!is.null(swaption_obj) && !is.null(swaption_engine_black)) {
  swaption_black_obj <- tryCatch(Swaption(swap_obj, exercise_obj), error = function(e) NULL)
  if (!is.null(swaption_black_obj)) {
    safe_engine_set(swaption_black_obj, swaption_engine_black)
    swaption_result_tbl$npv[swaption_result_tbl$engine == "Black"] <- safe_npv(swaption_black_obj)
  }
}

if (!is.null(swaption_obj) && !is.null(jamshidian_engine)) {
  swaption_hw_obj <- tryCatch(Swaption(swap_obj, exercise_obj), error = function(e) NULL)
  if (!is.null(swaption_hw_obj)) {
    safe_engine_set(swaption_hw_obj, jamshidian_engine)
    swaption_result_tbl$npv[swaption_result_tbl$engine == "Jamshidian"] <- safe_npv(swaption_hw_obj)
  }
}

show_tbl(
  tibble(
    exercise_date = ql_iso(exercise_date),
    swap_start = ql_iso(effective_date),
    swap_maturity = ql_iso(maturity_date),
    strike_rate = strike_rate
  ),
  "Swaption setup summary",
  n = 20
)

show_tbl(swaption_result_tbl, "Swaption valuation", n = 20)

# ------------------------------------------------------------
# 5. Long-format comparison tables
# ------------------------------------------------------------

leg_compare_tbl <- bind_rows(
  fixed_leg_tbl %>% mutate(leg = "fixed"),
  float_leg_tbl %>% mutate(leg = "floating")
)

cap_floor_long_tbl <- cap_floor_tbl %>%
  pivot_longer(
    cols = c(black_npv),
    names_to = "metric",
    values_to = "value"
  )

show_tbl(leg_compare_tbl, "Swap cashflow comparison (long table)", n = 20)
show_tbl(cap_floor_long_tbl, "Cap/Floor long table", n = 20)

cat("\nch07 interest-rate derivatives rewrite completed successfully.\n")
