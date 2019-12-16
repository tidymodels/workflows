
<!-- README.md is generated from README.Rmd. Please edit that file -->

# workflows

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/tidymodels/workflows.svg?branch=master)](https://travis-ci.org/tidymodels/workflows)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/workflows/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/workflows?branch=master)
[![R build
status](https://github.com/tidymodels/workflows/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/workflows)
<!-- badges: end -->

## What is a workflow?

A workflow is an object that can bundle together your pre-processing,
modeling, and post-processing requests. For example, if you have a
`recipe` and `parsnip` model, these can be combined into a workflow. The
advantages are:

  - You don’t have to keep track of separate objects in your workspace.

  - The recipe prepping and model fitting can be executed using a single
    call to `fit()`.

  - If you have custom tuning parameter settings, these can be defined
    using a simpler interface when combined with
    [tune](https://github.com/tidymodels/tune).

  - In the future, workflows will be able to add post-processing
    operations, such as modifying the probability cutoff for two-class
    models.

## Example

Suppose you were modeling data on cars. Say…the fuel efficiency of 32
cars. You know that the relationship between engine displacement and
miles-per-gallon is nonlinear, and you would like to model that as a
spline before adding it to a Bayesian linear regression model. You might
have a recipe to specify the spline:

``` r
library(recipes)
library(parsnip)
library(workflows)

spline_cars <- recipe(mpg ~ ., data = mtcars) %>% 
  step_ns(disp, deg_free = 10)
```

and a model object:

``` r
bayes_lm <- linear_reg() %>% 
  set_engine("stan")
```

To use these, you would generally run:

``` r
spline_cars_prepped <- prep(spline_cars, mtcars)
bayes_lm_fit <- fit(bayes_lm, mpg ~ ., data = juice(spline_cars_prepped))
```

You can’t predict on new samples using `bayes_lm_fit` without the
prepped version of `spline_cars` around. You also might have other
models and recipes in your workspace. This might lead to getting them
mixed-up or forgetting to save the model/recipe pair that you are most
interested in.

workflows makes this easier by combining these objects together:

``` r
car_wflow <- workflow() %>% 
  add_recipe(spline_cars) %>% 
  add_model(Bayes_lm)
```

Now you can prepare the recipe and estimate the model via a single call
to `fit()`:

``` r
car_wflow_fit <- fit(car_wflow, data = mtcars)
```

You can alter existing workflows using `update_recipe()` /
`update_model()` and `remove_recipe()` / `remove_model()`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tidymodels/workflows")
```