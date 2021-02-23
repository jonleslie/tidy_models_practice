library(tidymodels)
source("7_Fitting-models-with-parsnip.R")

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow

lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% update_formula(Sale_Price ~ Longitude)

# 8.3 Workflows and recipes -----------------------------------------------

lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_recipe(ames_rec)
lm_wflow

# Does `prep()`, `bake()`, and `fit()` in one step:
lm_fit <- fit(lm_wflow, ames_train)


# Does `bake()` and `predict()` automatically:
predict(lm_fit, ames_test %>% slice(1:3))

# Get the recipe and run `tidy()` method: 
lm_fit %>% 
  pull_workflow_prepped_recipe() %>% 
  tidy()

# To tidy the model fit: 
lm_fit %>% 
  # This returns the parsnip object:
  pull_workflow_fit() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)

# 8.6 Chapter summary -----------------------------------------------------

library(tidymodels)
data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(123)
ames_split <- initial_split(ames, prob = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)
