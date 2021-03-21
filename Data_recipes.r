# PREPROCESS YOUR DATA WITH RECIPES

library(tidymodels)
library(nycflights13)
library(skimr)

set.seed(123)

flight_data <-
  flights %>%
  mutate(
    # convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = as.Date(time_hour)
  ) %>%
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>%
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance,
         carrier, date, arr_delay, time_hour) %>%
  # Exclude missing data
  na.omit() %>%
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)


flight_data %>%
  count(arr_delay) %>%
  mutate(prop = n/sum(n))


glimpse(flight_data)

flight_data %>%
  skimr::skim(dest, carrier)

# DATA SPLITTING

# Fix the random numbers by setting the seed
# This enables the analysis to be reproducible when random numbers are used

set.seed(555)
# put 3/4 of the data into the training set
data_split <- initial_split(flight_data, prop = 3/4)


# Create data frames for the two sets:
train_data <- training(data_split)
test_data <- testing(data_split)


# CREATE RECIPE AND ROLES

flight_rec <-
  recipe(arr_delay ~ ., data = train_data)

flight_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID")

flight_rec

summary(flight_rec)

# CREATE FEATURES

flight_data %>%
  distinct(date) %>%
  mutate(numeric_date = as.numeric(date))

flight_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID") %>%
  step_date(date, features - c("dow", "month")) %>%
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  step_rm(date)

flight_rec


flight_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID") %>%
  step_date(date, features = c("dow", "month")) %>%
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  step_rm(date) %>%
  step_dummy(all_nominal(), -all_outcomes())

flight_rec

test_data %>%
  distinct(dest) %>%
  anti_join(train_data)
  flight_rec <-
    recipe(arr_delay ~ ., data = train_data) %>%
    update_role(flight, time_hour, new_role = "ID") %>%
    step_date(date, features = c("dow", "month")) %>%
    step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
    step_rm(date) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_zv(all_predictors())

flight_rec

lr_mod <-
  logistic_reg() %>%
  set_engine("glm")

lr_mod

flights_wflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(flight_rec)

flights_wflow


flights_fit <-
  flights_wflow %>%
  fit(data = train_data)


flights_fit

flights_fit %>%
  pull_workflow_fit() %>%
  tidy()

# The goal of this is to predict whether a plane arrives 30 minutes late

"""
We have just:

1. Built the model (lr_mod)
2. Created a preprocessing recipe (flights_rec)
3. Bundled the model and recipe (flights_wflow)
4. Trained our workflow using a single call to fit()
"""

predict(flights_fit, test_data)

# let's return predicted class properties for each flight instead

flights_pred <-
  predict(flights_fit, test_data, type = "prob") %>%
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred

# evaluate the accurracy of the model

flights_pred %>%
  roc_curve(truth = arr_delay, .pred_late) %>%
  autoplot()

flights_pred %>%
  roc_auc(truth = arr_delay, .pred_late)
  
