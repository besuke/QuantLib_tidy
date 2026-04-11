suppressMessages({
  library(tidyverse)
  library(QuantLib)
})

# ============================================================
# tidy_ois_curve.R
# ============================================================

ql_date <- function(x) {
  if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d")
  }
  stopifnot(is.character(x), length(x) == 1)
  DateParser_parseISO(x)
}

ql_iso <- function(x) {
  tryCatch(
    Date_ISO(x),
    error = function(e) as.character(x)
  )
}

set_eval_date <- function(date) {
  qd <- ql_date(date)
  invisible(Settings_instance()$setEvaluationDate(d = qd))
  qd
}

make_date_vector <- function(dates) {
  dv <- DateVector()
  for (d in dates) {
    DateVector_append(dv, ql_date(d))
  }
  dv
}

build_flatforward_from_nodes <- function(nodes, day_counter = "Actual365Fixed") {
  stopifnot(all(c("date", "zero_rate") %in% names(nodes)))
  
  dates_chr <- nodes$date
  dates_ql  <- make_date_vector(dates_chr)
  
  dc <- switch(
    day_counter,
    Actual365Fixed = Actual365Fixed(),
    Actual360 = Actual360(),
    Thirty360 = Thirty360(),
    stop("Unsupported day counter: ", day_counter)
  )
  
  origin <- as.Date(dates_chr[[1]])
  times <- as.numeric(as.Date(dates_chr) - origin) / 365
  times[1] <- 0
  
  dfs <- exp(-nodes$zero_rate * times)
  dfs[1] <- 1.0
  
  DiscountCurve(dates_ql, dfs, dc)
}

# SWIGの discount() は使わず、
# 入力ノードから discount を tidy に再計算する
extract_discount_table <- function(nodes) {
  first_date <- as.Date(nodes$date[[1]])
  
  tibble(
    date = nodes$date,
    zero_rate_input = nodes$zero_rate
  ) |>
    mutate(
      year_frac = as.numeric(as.Date(date) - first_date) / 365,
      discount = if_else(
        year_frac > 0,
        exp(-zero_rate_input * year_frac),
        1
      ),
      implied_zero = if_else(
        year_frac > 0,
        -log(discount) / year_frac,
        0
      )
    )
}

ois_nodes <- tibble::tribble(
  ~date,         ~zero_rate,
  "2026-04-12",  0.0030,
  "2026-05-12",  0.0032,
  "2026-07-12",  0.0034,
  "2026-10-12",  0.0038,
  "2027-04-12",  0.0045,
  "2028-04-12",  0.0065,
  "2029-04-12",  0.0080,
  "2031-04-12",  0.0105
)

eval_date <- set_eval_date(ois_nodes$date[[1]])
cat("Evaluation date:", ql_iso(eval_date), "\n")

ois_curve <- build_flatforward_from_nodes(
  nodes = ois_nodes,
  day_counter = "Actual365Fixed"
)

curve_tbl <- extract_discount_table(ois_nodes)
print(curve_tbl)

plot_curve_tbl <- curve_tbl |>
  pivot_longer(
    cols = c(zero_rate_input, implied_zero),
    names_to = "series",
    values_to = "rate"
  )

p <- ggplot(plot_curve_tbl, aes(x = as.Date(date), y = rate, group = series, color = series)) +
  geom_line() +
  geom_point() +
  labs(
    title = "OIS Curve (tidy starter)",
    x = "Date",
    y = "Rate",
    color = NULL
  ) +
  theme_minimal()

print(p)

make_tidy_ois_curve <- function(nodes_tbl, day_counter = "Actual365Fixed") {
  stopifnot(is.data.frame(nodes_tbl))
  stopifnot(all(c("date", "zero_rate") %in% names(nodes_tbl)))
  
  set_eval_date(nodes_tbl$date[[1]])
  
  curve <- build_flatforward_from_nodes(
    nodes = nodes_tbl,
    day_counter = day_counter
  )
  
  out_tbl <- extract_discount_table(nodes_tbl)
  
  list(
    curve = curve,
    table = out_tbl
  )
}

# Example:
# result <- make_tidy_ois_curve(ois_nodes)
# result$table