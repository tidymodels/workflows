---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# workflows

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/DavisVaughan/workflows.svg?branch=master)](https://travis-ci.org/DavisVaughan/workflows)
[![Codecov test coverage](https://codecov.io/gh/DavisVaughan/workflows/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/workflows?branch=master)
<!-- badges: end -->

## What's a workflow? 

It is an object that can bundle together your pre-processing, modeling, and post-processing values. For example, if you have a `recipe` and `parsnip` model, these can be combined into a recipe. The advantages are:

 * You don't have to keep track of separate objects in your workspace. 

 * The recipe prep and model fitting can be executed using a single call to `fit()`. 

 * If you have custom tuning parameter settings, these can be defined using a simpler interface. 

 * Workflows will be able to add post-processing operations, such as modifying the probability cutoff for two-class models. These will be released in upcoming versions. 

Suppose you were modeling data on cars. Say... the fuel efficiency of 32 cars. You know that the relationship between engine displacement and miles-per-gallon is nonlinear and you would like to model that as a spline before adding it to a Bayesian linear regression models. You might have a recipe to specify the spline:


```r
library(recipes)
library(parsnip)
library(workflows)

spline_cars <- 
  recipe(mpg ~ ., data = mtcars) %>% 
  step_ns(disp, deg_free = 10)
```

and a model object:


```r
Bayes_lm <- 
  linear_reg() %>% 
  set_engine("stan")
```

To use these, you would run `prep(spline_cars)` and then


```r
Bayes_lm_fit <- 
  Bayes_lm %>% 
  fit(mpg ~ ., data = juice(spline_cars))
```

You can't predict samples using `Bayes_lm_fit` without the prepared version of `spline_cars` around. You might have other models and recipes in your workspace. This might lead to getting them mixed-up or forgetting to save the model/recipe pair that you are most interested in. 

`workflows` makes this easier by combining these objects together:


```r
car_wflow <- 
  workflow() %>% 
  add_recipe(spline_cars) %>% 
  add_model(Bayes_lm)
```

Now, you can prepare the recipe and estimate the model via a single call to `fit()`:


```r
car_wflow_fit <- 
  car_wflow %>% 
  fit(data = mtcars)
```

You can create alternate workflows using `update_recipe()` or `remove_recipe()` (and similarly for models). 


## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/workflows")
```
