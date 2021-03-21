# Introduction

library(tidyverse)
library(tidymodels)
library(broom.mixed)
library(dotwhisker)
library(rstanarm)
urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
setNames(c("food_regime", "initial_volume", "width")) %>%
mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))


urchins

ggplot(urchins,
        aes(x = initial_volume,
            y = width,
            group = food_regime,
            col = food_regime)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    scale_color_viridis_d(option = "plasma", end = .7)

lm_mod <-
    linear_reg() %>%
    set_engine("lm")

lm_mod

lm_fit <-
    lm_mod %>%
    fit(width ~ initial_volume * food_regime, data = urchins)

lm_fit

tidy(lm_fit)

tidy(lm_fit) %>%
    dwplot(dot_args = list(size = 2, color = "black"),
        whisker_arges = list(color = "black"),
        vline = geom_vline(xintercept = 0, color = "grey50", linetype = 2))


# Use a model to predict

new_points <- expand.grid(initial_volume = 20,
                          food_regime = c("Initial", "Low", "High"))
new_points

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

conf_int_pred <- predict(lm_fit,
                         new_data = new_points,
                         type = "conf_int")

conf_int_pred

# now combine
plot_data <-
    new_points %>%
    bind_cols(mean_pred) %>%
    bind_cols(conf_int_pred)

plot_data

ggplot(plot_data, aes(x = food_regime)) +
    geom_point(aes(y = .pred)) +
    geom_errorbar(aes(ymin = .pred_lower,
                      ymax = .pred_upper),
                  width = .2) +
    labs(y = "urchin size")


# Model with a different engine

# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make the parsnip model
bayes_mod <-
    linear_reg() %>%
    set_engine("stan",
               prior_intercept = prior_dist,
               prior = prior_dist)

bayes_mod

# train the model
bayes_fit <-
    bayes_mod %>%
    fit(width ~ initial_volume * food_regime, data = urchins)

print(bayes_fit, digits = 5)


tidy(bayes_fit, conf.int = TRUE)


bayes_plot_data <-
    new_points %>%
    bind_cols(predict(bayes_fit, new_data = new_points)) %>%
    bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

bayes_plot_data

ggplot(bayes_plot_data, aes(x = food_regime)) +
    geom_point(aes(y = .pred)) +
    geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) +
    labs(y = "urchin size") +
    ggtitle("Bayesian model with t(1) prior distribution")
