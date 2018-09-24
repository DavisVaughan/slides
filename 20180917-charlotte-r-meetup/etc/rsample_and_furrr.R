library(rsample)
library(tibbletime) # for FB data
library(dplyr)
library(purrr)
# devtools::install_github("tidymodels/yardstick", ref = "feature/yardstick-rewrite-for-groups-vecs-and-extensibility")
library(yardstick)
library(ggplot2)
library(forecast)
data(FB)

h <- 30

fb_slices <- FB %>%
  select(date, adjusted) %>%
  rolling_origin(
    initial = 800, 
    assess = h, 
    cumulative = FALSE, 
    skip = 10
  )

# what is this?
fb_slices
# <train/test/total>
fb_slices$splits[[1]]
analysis(fb_slices$splits[[1]])
assessment(fb_slices$splits[[1]])

fit_auto_arima <- function(split) {
  analysis_set <- analysis(split)
  auto.arima(y = analysis_set$adjusted)
}

forecast_arima <- function(model, h = 5) {
  f_cast <- forecast(object = model, h = h)
  as.numeric(f_cast$mean)
}

fb_fit <- fb_slices %>%
  mutate(
    model = map(splits, fit_auto_arima)
  )

fb_assess <- fb_fit %>%
  mutate(
    stats = map(model, broom::glance),
    assess = map(splits, assessment),
    pred  = map(model, ~forecast_arima(model = .x, h = h))
  ) %>%
  unnest(assess, pred)

y_expr <- expression(paste("RMSE = ", sqrt(frac(sum((y - hat(y))^2),n))))

fb_assess %>%
  group_by(id) %>%
  rmse(truth = adjusted, estimate = pred) %>%
  right_join(fb_assess) %>%
  group_by(id) %>%
  slice(h) %>%
  ggplot(aes(x = date, y = .estimate)) +
  geom_point() +
  labs(x = "End of prediction period", y = y_expr, title = "RMSE out of sample over time")


# VS furrr

library(tictoc)

fb_slices <- FB %>%
  select(date, adjusted) %>%
  rolling_origin(
    initial = 800, 
    assess = h, 
    cumulative = FALSE
  )

tic()
fb_fit <- fb_slices %>%
  mutate(
    model = map(splits, fit_auto_arima)
  )
toc()

library(furrr)
plan(multiprocess)

tic()
fb_fit <- fb_slices %>%
  mutate(
    model = future_map(splits, fit_auto_arima)
  )
toc()
