suppressPackageStartupMessages({
  library(QuantLib)
  library(tidyverse)
  library(lattice)
  library(grid)
})

## =========================================================
## 0. helper
## =========================================================

qldate <- function(x) {
  DateParser_parseISO(x)
}

build_option_surface_row <- function(spot_value, vol_value,
                                     underlying, volatilityQuote, option) {
  underlying$setValue(value = spot_value)
  volatilityQuote$setValue(value = vol_value)
  
  tibble(
    NPV = option$NPV(),
    gamma = option$gamma(),
    delta = option$delta(),
    vega = option$vega()
  )
}

## =========================================================
## 1. grid data
## =========================================================

surface_grid_tbl <- tidyr::expand_grid(
  spot = seq(10, 95, length.out = 20),
  vol  = seq(0.20, 1.00, length.out = 20)
)

## =========================================================
## 2. market / model setup
## =========================================================

todaysDate <- qldate("1998-05-15")
invisible(Settings_instance()$setEvaluationDate(d = todaysDate))

settlementDate <- qldate("1998-05-17")

riskQuote <- SimpleQuote(0.05)
riskFreeRate <- FlatForward(
  settlementDate,
  QuoteHandle(riskQuote),
  Actual365Fixed()
)

exercise <- EuropeanExercise(qldate("1999-05-17"))
payoff <- PlainVanillaPayoff("Call", 50.0)

dividendYield <- FlatForward(
  settlementDate,
  0.05,
  Actual365Fixed()
)

underlying <- SimpleQuote(10.0)

volatilityQuote <- SimpleQuote(0.05)
volatility <- BlackConstantVol(
  todaysDate,
  TARGET(),
  QuoteHandle(volatilityQuote),
  Actual365Fixed()
)

process <- BlackScholesMertonProcess(
  QuoteHandle(underlying),
  YieldTermStructureHandle(dividendYield),
  YieldTermStructureHandle(riskFreeRate),
  BlackVolTermStructureHandle(volatility)
)

engine <- AnalyticEuropeanEngine(process)

option <- VanillaOption(payoff, exercise)
option$setPricingEngine(s_arg2 = engine)

## =========================================================
## 3. pricing on grid
## =========================================================

surface_result_tbl <- surface_grid_tbl %>%
  mutate(
    metrics = pmap(
      list(spot, vol),
      function(spot, vol) {
        build_option_surface_row(
          spot_value = spot,
          vol_value = vol,
          underlying = underlying,
          volatilityQuote = volatilityQuote,
          option = option
        )
      }
    )
  ) %>%
  unnest(metrics)

print(surface_result_tbl, n = 10)

## =========================================================
## 4. lattice 3D plots
## =========================================================

plot_npv <- wireframe(
  NPV ~ spot * vol,
  data = surface_result_tbl,
  drape = TRUE,
  main = "NPV"
)

plot_gamma <- wireframe(
  gamma ~ spot * vol,
  data = surface_result_tbl,
  drape = TRUE,
  main = "Gamma"
)

plot_delta <- wireframe(
  delta ~ spot * vol,
  data = surface_result_tbl,
  drape = TRUE,
  main = "Delta"
)

plot_vega <- wireframe(
  vega ~ spot * vol,
  data = surface_result_tbl,
  drape = TRUE,
  main = "Vega"
)

plot_list <- list(
  plot_npv,
  plot_gamma,
  plot_delta,
  plot_vega
)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

for (i in seq_along(plot_list)) {
  pushViewport(
    viewport(
      layout.pos.col = ((i - 1) %% 2) + 1,
      layout.pos.row = ((i - 1) %/% 2) + 1
    )
  )
  print(plot_list[[i]], newpage = FALSE)
  popViewport()
}

popViewport()

## =========================================================
## 5. optional ggplot heatmaps
## =========================================================

surface_result_tbl %>%
  ggplot(aes(x = spot, y = vol, fill = NPV)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "NPV heatmap")

surface_result_tbl %>%
  ggplot(aes(x = spot, y = vol, fill = gamma)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Gamma heatmap")

surface_result_tbl %>%
  ggplot(aes(x = spot, y = vol, fill = delta)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Delta heatmap")

surface_result_tbl %>%
  ggplot(aes(x = spot, y = vol, fill = vega)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Vega heatmap")