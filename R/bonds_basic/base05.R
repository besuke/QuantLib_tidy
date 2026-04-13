suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ============================================================
# ch05.R
# ------------------------------------------------------------
# US Treasury and futures
# Python notebook ch05.ipynb を R + QuantLib(SWIG) 向けに
# tidyverse 風に書き直した版。
#
# 内容:
# 1. US Treasury bond の作成
# 2. 利回り -> 価格
# 3. 経過利息の手計算
# 4. グロスベーシス
# 5. ネットベーシス / フォワード価格 / インプライドレポ
#
# SWIG-safe 方針:
# - Date は DateParser_parseISO() を使う
# - Thirty360 / ActualActual は規約を明示
# - bondYield 系は使わず、今回は cleanPrice / dirtyPrice 主体
# - tibble / mutate / pmap / map_dbl で整形
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

advance_days <- function(calendar_obj, date_obj, n_days) {
  Calendar_advance(calendar_obj, ql_date(date_obj), as.integer(n_days), "Days")
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

bond_cashflow_tbl <- function(bond_obj, ir_obj = NULL, past = TRUE) {
  cf_leg <- bond_obj$cashflows()
  settle_date <- tryCatch(bond_obj$settlementDate(), error = function(e) Settings_instance()$evaluationDate())
  settle_date_r <- as.Date(ql_iso(settle_date))
  
  purrr::map_dfr(seq_len(cf_leg$size()), function(i) {
    cf_obj <- leg_cashflow_at(cf_leg, i)
    pay_date_obj <- CashFlow_date(cf_obj)
    pay_date_chr <- ql_iso(pay_date_obj)
    pay_date_r <- as.Date(pay_date_chr)
    
    coupon_obj <- tryCatch(as_coupon(cf_obj), error = function(e) NULL)
    
    amount_value <- tryCatch(CashFlow_amount(cf_obj), error = function(e) NA_real_)
    
    df_value <- if (!is.null(ir_obj) && pay_date_r >= settle_date_r) {
      tryCatch(ir_obj$discountFactor(settle_date, pay_date_obj), error = function(e) NA_real_)
    } else if (pay_date_r < settle_date_r) {
      NA_real_
    } else {
      NA_real_
    }
    
    tibble(
      pay_date = pay_date_chr,
      coupon_rate = if (!is.null(coupon_obj)) tryCatch(coupon_obj$rate(), error = function(e) NA_real_) else NA_real_,
      accrual_start = if (!is.null(coupon_obj)) ql_iso(Coupon_accrualStartDate(coupon_obj)) else NA_character_,
      accrual_end = if (!is.null(coupon_obj)) ql_iso(Coupon_accrualEndDate(coupon_obj)) else NA_character_,
      amount = amount_value,
      df = df_value,
      pv = amount_value * df_value,
      flow_type = if (!is.null(coupon_obj)) "coupon" else "principal"
    )
  }) %>%
    { if (past) . else filter(., as.Date(pay_date) >= settle_date_r) }
}

fmt_num <- function(x, digits = 6) sprintf(paste0("%.", digits, "f"), x)
fmt_pct <- function(x, digits = 4) sprintf(paste0("%.", digits, "f%%"), 100 * x)

# ------------------------------------------------------------
# 1. Calendars / counters / enums
# ------------------------------------------------------------

cal_us_gov <- UnitedStates("GovernmentBond")
dc_act_act_bond <- ActualActual("Bond")
dc_act_360 <- Actual360()
cmpd_compounded <- Compounding_Compounded_get()
freq_semiannual <- Frequency_Semiannual_get()
settlement_days_t1 <- 1L

# ------------------------------------------------------------
# 2. US Treasury bond helper
# ------------------------------------------------------------

make_us_tsy_bond <- function(effective_date,
                             maturity_date,
                             coupon_rate_pct,
                             face_amount = 100.0,
                             settlement_days = settlement_days_t1) {
  effective_date_ql <- ql_date(effective_date)
  maturity_date_ql <- ql_date(maturity_date)
  
  bond_schedule <- Schedule(
    effective_date_ql,
    maturity_date_ql,
    period_months(6),
    cal_us_gov,
    "Unadjusted",
    "Unadjusted",
    "Backward",
    FALSE
  )
  
  bond_obj <- FixedRateBond(
    settlement_days,
    face_amount,
    bond_schedule,
    c(coupon_rate_pct / 100),
    dc_act_act_bond
  )
  
  list(
    bond = bond_obj,
    schedule = bond_schedule,
    effective_date = effective_date_ql,
    maturity_date = maturity_date_ql,
    coupon_rate_pct = coupon_rate_pct,
    face_amount = face_amount
  )
}

bond_bundle <- make_us_tsy_bond(
  effective_date = "2022-09-30",
  maturity_date = "2027-09-30",
  coupon_rate_pct = 4.125
)

bond_obj <- bond_bundle$bond
show_tbl(make_schedule_tbl(bond_bundle$schedule), "US Treasury bond schedule", n = 20)

cf_prc <- bond_obj$cleanPrice(
  6 / 100,
  dc_act_act_bond,
  cmpd_compounded,
  freq_semiannual,
  ql_date("2023-07-06")
)

cat("Yield 6%, settle 2023-07-06 clean price:", fmt_num(cf_prc, 4), "\n")

# ------------------------------------------------------------
# 3. Yield 3.70% -> price
# ------------------------------------------------------------

trade_date <- ql_date("2023-04-20")
bond_yield <- 3.7 / 100
futures_price <- 109 + 10 / 32
set_eval_date(trade_date)
settle_date <- advance_days(cal_us_gov, trade_date, settlement_days_t1)

accrued_amount <- bond_obj$accruedAmount(settle_date)
clean_price <- bond_obj$cleanPrice(
  bond_yield,
  dc_act_act_bond,
  cmpd_compounded,
  freq_semiannual,
  settle_date
)

i_rate_obj <- InterestRate(
  bond_yield,
  dc_act_act_bond,
  cmpd_compounded,
  freq_semiannual
)

show_tbl(
  tibble(
    settle_date = ql_iso(settle_date),
    bond_yield = bond_yield,
    accrued_amount = accrued_amount,
    clean_price = clean_price
  ),
  "Bond price from yield",
  n = 20
)

show_tbl(
  bond_cashflow_tbl(bond_obj, ir_obj = i_rate_obj, past = TRUE),
  "Bond cashflow table (top rows)",
  n = 3
)

# ------------------------------------------------------------
# 4. Accrued interest hand calculation
# ------------------------------------------------------------

mar31 <- ql_date("2023-03-31")
apr21 <- ql_date("2023-04-21")
sep30 <- ql_date("2023-09-30")

days_to_apr21 <- dc_act_act_bond$dayCount(mar31, apr21)
days_coupon_period <- dc_act_act_bond$dayCount(mar31, sep30)
accrued_interest_hand <- 4.125 / 2 * days_to_apr21 / days_coupon_period

show_tbl(
  tibble(
    days_to_apr21 = days_to_apr21,
    days_coupon_period = days_coupon_period,
    accrued_interest_hand = accrued_interest_hand
  ),
  "Accrued interest hand calculation",
  n = 20
)

# ------------------------------------------------------------
# 5. Deliverable basket / gross basis
# ------------------------------------------------------------

deliverable_tbl <- tibble::tribble(
  ~issue_date,   ~maturity_date, ~coupon_rate_pct, ~conversion_factor, ~market_yield_pct,
  "2022-09-30", "2027-09-30", 4.125,             0.9305,             3.70,
  "2022-08-31", "2027-08-31", 3.125,             0.8953,             3.69,
  "2023-01-31", "2028-01-31", 3.500,             0.9011,             3.65
)

calc_gross_basis_row <- function(issue_date,
                                 maturity_date,
                                 coupon_rate_pct,
                                 conversion_factor,
                                 market_yield_pct) {
  bond_i <- make_us_tsy_bond(issue_date, maturity_date, coupon_rate_pct)$bond
  ir_i <- InterestRate(
    market_yield_pct / 100,
    dc_act_act_bond,
    cmpd_compounded,
    freq_semiannual
  )
  
  bpv_value <- BondFunctions_basisPointValue(bond_i, ir_i)
  clean_price_i <- bond_i$cleanPrice(
    market_yield_pct / 100,
    dc_act_act_bond,
    cmpd_compounded,
    freq_semiannual,
    settle_date
  )
  dirty_price_i <- bond_i$dirtyPrice(
    market_yield_pct / 100,
    dc_act_act_bond,
    cmpd_compounded,
    freq_semiannual,
    settle_date
  )
  gross_basis <- clean_price_i - futures_price * conversion_factor
  
  tibble(
    issue_date = issue_date,
    maturity = ql_iso(bond_i$maturityDate()),
    coupon = bond_i$nextCouponRate(),
    yield_pct = market_yield_pct,
    bpv = bpv_value,
    clean_price = clean_price_i,
    dirty_price = dirty_price_i,
    conversion_factor = conversion_factor,
    gross_basis = gross_basis,
    bond_obj = list(bond_i)
  )
}

gross_basis_tbl <- purrr::pmap_dfr(deliverable_tbl, calc_gross_basis_row)

cat(
  "Futures price:", fmt_num(futures_price, 6),
  ", spot settlement date:", ql_iso(settle_date),
  "\n"
)
show_tbl(gross_basis_tbl %>% select(-bond_obj), "Gross basis table", n = 20)

# ------------------------------------------------------------
# 6. Net basis / forward / implied repo
# ------------------------------------------------------------

repo_rate <- 5.10 / 100
repo_end_date <- ql_date("2023-07-06")
repo_year_frac <- dc_act_360$yearFraction(settle_date, repo_end_date)
repo_days <- dc_act_360$dayCount(settle_date, repo_end_date)

calc_net_basis_row <- function(bond_obj,
                               conversion_factor,
                               clean_price,
                               dirty_price,
                               gross_basis) {
  accrued_start <- bond_obj$accruedAmount(settle_date)
  accrued_end <- bond_obj$accruedAmount(repo_end_date)
  coupon_income <- accrued_end - accrued_start
  repo_cost <- repo_rate * repo_year_frac * dirty_price
  carry <- coupon_income - repo_cost
  net_basis <- gross_basis - carry
  forward_price <- clean_price - carry
  implied_repo <- ((futures_price * conversion_factor + accrued_end) / dirty_price - 1) / repo_year_frac
  
  tibble(
    accrued_start = accrued_start,
    accrued_end = accrued_end,
    coupon_income = coupon_income,
    dirty_price = dirty_price,
    repo_cost = repo_cost,
    carry = carry,
    net_basis = net_basis,
    forward_price = forward_price,
    implied_repo = implied_repo
  )
}


net_basis_tbl <- purrr::pmap_dfr(
  list(
    bond_obj = gross_basis_tbl$bond_obj,
    conversion_factor = gross_basis_tbl$conversion_factor,
    clean_price = gross_basis_tbl$clean_price,
    dirty_price = gross_basis_tbl$dirty_price,
    gross_basis = gross_basis_tbl$gross_basis
  ),
  ~ calc_net_basis_row(..1, ..2, ..3, ..4, ..5)
)
cat(
  "Repo rate:", fmt_pct(repo_rate),
  ", repo end date:", ql_iso(repo_end_date),
  ", repo days:", repo_days,
  ", repo year fraction:", fmt_num(repo_year_frac, 4),
  "\n"
)

show_tbl(net_basis_tbl, "Net basis table", n = 20)

# ------------------------------------------------------------
# 7. Combined deliverable basket view
# ------------------------------------------------------------

basket_summary_tbl <- gross_basis_tbl %>%
  select(-bond_obj) %>%
  bind_cols(net_basis_tbl) %>%
  mutate(
    ct_d_proxy = rank(net_basis, ties.method = "first") == 1
  ) %>%
  arrange(net_basis)

show_tbl(basket_summary_tbl, "Deliverable basket summary", n = 20)

cat("\nch05 treasury futures rewrite completed successfully.\n")

