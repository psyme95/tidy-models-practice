# Load packages ----
library(tidymodels)
library(readr)
library(broom.mixed)
library(dotwhisker)

# Load data ----
urchins <-
  read_csv("https://tidymodels.org/start/models/urchins.csv") |> 
  setNames(c("food_regime", "initial_volume", "width")) |> 
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

urchins

# Plot data ----
ggplot(urchins, 
       aes(x = initial_volume,
           y = width,
           group = food_regime,
           col = food_regime)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)

# Fit linear regression ----
lm_fit <- 
  linear_reg() %>%
  fit(width ~ initial_volume * food_regime, data = urchins)

lm_fit

lm_summary <- tidy(lm_fit)
print(lm_summary)

# Plot estimates ----
tidy(lm_fit) %>%
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype=2))

# Predict on new data ----
new_points <- expand.grid(initial_volume = c(20), 
                          food_regime = c("Initial", "Low", "High"))
new_points

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred

# Now combine 
plot_data <- 
  new_points |> 
  bind_cols(mean_pred) |> 
  bind_cols(conf_int_pred)

# and plot
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "urchin size")
