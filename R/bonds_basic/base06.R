suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch06.R
# ------------------------------------------------------------
# Short-rate models / Hull-White / Vasicek / CIR
# Python notebook ch06.ipynb を R + QuantLib(SWIG) 向けに
# tidyverse 風に書き直した版。
#
# 内容:
# 1. フラットな初期カーブ作成
# 2. Vasicek / CIR / Hull-White モデル作成
# 3. ゼロクーポン債価格の比較
# 4. 金利パスの簡易シミュレーション
# 5. Hull-White による option / swaption の導入準備
#
# 方針:
# - Date は DateParser_parseISO() を使う
# - curve は discount() ベースで tidy table 化
# - model method の build 差があるので tryCatch を多用
# - まず「通る骨格」を優先し、後で SWIG 差分を詰める
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

fmt_num <- function(x, digits = 6) sprintf(paste0("%.", digits, "f"), x)
fmt_pct <- function(x, digits = 4) sprintf(paste0("%.", digits, "f%%"), 100 * x)

# ------------------------------------------------------------
# 1. Base market setup
# ------------------------------------------------------------

trade_date <- ql_date("2023-01-03")
set_eval_date(trade_date)

dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()
cal_target <- TARGET()

flat_rate <- 0.03
flat_curve <- FlatForward(trade_date, flat_rate, dc_a365)
TermStructure_enableExtrapolation(flat_curve)
flat_curve_handle <- YieldTermStructureHandle(flat_curve)

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

show_tbl(curve_tbl(flat_curve), "Flat initial curve", n = 12)

# ------------------------------------------------------------
# 2. Model constructors
# ------------------------------------------------------------

vasicek_model <- tryCatch(
  Vasicek(0.03, 0.15, 0.03, 0.01),
  error = function(e) NULL
)

cir_model <- tryCatch(
  CIR(0.03, 0.15, 0.03, 0.02),
  error = function(e) NULL
)

hull_white_model <- tryCatch(
  HullWhite(flat_curve_handle, 0.03, 0.01),
  error = function(e) NULL
)

show_tbl(
  tibble(
    model = c("Vasicek", "CIR", "HullWhite"),
    available = c(!is.null(vasicek_model), !is.null(cir_model), !is.null(hull_white_model))
  ),
  "Model availability",
  n = 10
)

# ------------------------------------------------------------
# 3. Zero-coupon bond price comparison
# ------------------------------------------------------------

safe_discount_bond <- function(model_obj, t0, t1, short_rate = 0.03) {
  if (is.null(model_obj)) return(NA_real_)
  
  out <- tryCatch(model_obj$discountBond(t0, t1, short_rate), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  out <- tryCatch(model_obj$discountBond(t1, short_rate), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  NA_real_
}

zcb_tbl <- tibble(maturity = c(0.25, 0.5, 1, 2, 3, 5, 7, 10)) %>%
  mutate(
    curve_discount = purrr::map_dbl(maturity, ~ flat_curve$discount(.x)),
    vasicek_discount = purrr::map_dbl(maturity, ~ safe_discount_bond(vasicek_model, 0, .x)),
    cir_discount = purrr::map_dbl(maturity, ~ safe_discount_bond(cir_model, 0, .x)),
    hull_white_discount = purrr::map_dbl(maturity, ~ safe_discount_bond(hull_white_model, 0, .x))
  )

show_tbl(zcb_tbl, "Zero-coupon discount comparison", n = 20)

# ------------------------------------------------------------
# 4. Short-rate path simulation (R-side fallback)
# ------------------------------------------------------------

simulate_vasicek_path <- function(n_steps = 120,
                                  dt = 1 / 12,
                                  r0 = 0.03,
                                  kappa = 0.15,
                                  theta = 0.03,
                                  sigma = 0.01,
                                  seed = 123) {
  set.seed(seed)
  
  z_vec <- rnorm(n_steps)
  r_vec <- numeric(n_steps + 1)
  r_vec[1] <- r0
  
  for (i in seq_len(n_steps)) {
    r_vec[i + 1] <- r_vec[i] +
      kappa * (theta - r_vec[i]) * dt +
      sigma * sqrt(dt) * z_vec[i]
  }
  
  tibble(
    step = 0:n_steps,
    time = (0:n_steps) * dt,
    short_rate = r_vec
  )
}

simulate_cir_path <- function(n_steps = 120,
                              dt = 1 / 12,
                              r0 = 0.03,
                              kappa = 0.15,
                              theta = 0.03,
                              sigma = 0.02,
                              seed = 123) {
  set.seed(seed)
  
  z_vec <- rnorm(n_steps)
  r_vec <- numeric(n_steps + 1)
  r_vec[1] <- r0
  
  for (i in seq_len(n_steps)) {
    next_r <- r_vec[i] +
      kappa * (theta - r_vec[i]) * dt +
      sigma * sqrt(pmax(r_vec[i], 0)) * sqrt(dt) * z_vec[i]
    r_vec[i + 1] <- pmax(next_r, 0)
  }
  
  tibble(
    step = 0:n_steps,
    time = (0:n_steps) * dt,
    short_rate = r_vec
  )
}

vasicek_path_tbl <- simulate_vasicek_path()
cir_path_tbl <- simulate_cir_path()

show_tbl(vasicek_path_tbl, "Simulated Vasicek path", n = 12)
show_tbl(cir_path_tbl, "Simulated CIR path", n = 12)

# ------------------------------------------------------------
# 5. Plot-ready model comparison tables
# ------------------------------------------------------------

zcb_plot_tbl <- zcb_tbl %>%
  pivot_longer(
    cols = c(curve_discount, vasicek_discount, cir_discount, hull_white_discount),
    names_to = "series",
    values_to = "discount"
  )

rate_path_plot_tbl <- bind_rows(
  vasicek_path_tbl %>% mutate(model = "Vasicek"),
  cir_path_tbl %>% mutate(model = "CIR")
)

show_tbl(zcb_plot_tbl, "ZCB comparison (long format)", n = 20)
show_tbl(rate_path_plot_tbl, "Short-rate paths (long format)", n = 20)

# ------------------------------------------------------------
# 6. Hull-White option / swaption prep
# ------------------------------------------------------------

make_hw_swaption_setup <- function(curve_handle,
                                   exercise_date = "2024-01-03",
                                   swap_start = "2024-01-05",
                                   swap_maturity = "2029-01-05",
                                   nominal = 1e6,
                                   fixed_rate = 0.03) {
  fixed_schedule <- Schedule(
    ql_date(swap_start),
    ql_date(swap_maturity),
    period_years(1),
    cal_target,
    "ModifiedFollowing",
    "ModifiedFollowing",
    "Backward",
    FALSE
  )
  
  float_schedule <- Schedule(
    ql_date(swap_start),
    ql_date(swap_maturity),
    period_months(6),
    cal_target,
    "ModifiedFollowing",
    "ModifiedFollowing",
    "Backward",
    FALSE
  )
  
  euribor6m_index <- Euribor6M(curve_handle)
  
  swap_obj <- VanillaSwap(
    Swap_Payer_get(),
    nominal,
    fixed_schedule,
    fixed_rate,
    Thirty360("BondBasis"),
    float_schedule,
    euribor6m_index,
    0.0,
    dc_a360
  )
  
  exercise_obj <- EuropeanExercise(ql_date(exercise_date))
  swaption_obj <- Swaption(swap_obj, exercise_obj)
  
  list(
    fixed_schedule = fixed_schedule,
    float_schedule = float_schedule,
    index = euribor6m_index,
    swap = swap_obj,
    exercise = exercise_obj,
    swaption = swaption_obj
  )
}

hw_swaption_setup <- tryCatch(
  make_hw_swaption_setup(flat_curve_handle),
  error = function(e) NULL
)

if (is.null(hw_swaption_setup) || is.null(hull_white_model)) {
  show_tbl(
    tibble(note = "Swaption setup skipped or depends on SWIG build details"),
    "Hull-White swaption prep"
  )
} else {
  jamshidian_engine <- tryCatch(JamshidianSwaptionEngine(hull_white_model), error = function(e) NULL)
  
  if (is.null(jamshidian_engine)) {
    show_tbl(
      tibble(note = "JamshidianSwaptionEngine not available in this build"),
      "Hull-White swaption prep"
    )
  } else {
    Instrument_setPricingEngine(hw_swaption_setup$swaption, jamshidian_engine)
    
    swaption_npv <- tryCatch(hw_swaption_setup$swaption$NPV(), error = function(e) NA_real_)
    
    show_tbl(
      tibble(
        item = c("exercise_date", "swap_start", "swaption_npv"),
        value = c(
          ql_iso(ql_date("2024-01-03")),
          ql_iso(ql_date("2024-01-05")),
          fmt_num(swaption_npv, 6)
        )
      ),
      "Hull-White swaption prep",
      n = 20
    )
  }
}

cat("\nch06 short-rate models rewrite completed successfully.\n")
