# Create a workflow

A `workflow` is a container object that aggregates information required
to fit and predict from a model. This information might be a recipe used
in preprocessing, specified through
[`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md),
or the model specification to fit, specified through
[`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md),
or a tailor used in postprocessing, specified through
[`add_tailor()`](https://workflows.tidymodels.org/dev/reference/add_tailor.md).

The `preprocessor` and `spec` arguments allow you to add components to a
workflow quickly, without having to go through the `add_*()` functions,
such as
[`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md)
or
[`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md).
However, if you need to control any of the optional arguments to those
functions, such as the `blueprint` or the model `formula`, then you
should use the `add_*()` functions directly instead.

## Usage

``` r
workflow(preprocessor = NULL, spec = NULL, postprocessor = NULL)
```

## Arguments

- preprocessor:

  An optional preprocessor to add to the workflow. One of:

  - A formula, passed on to
    [`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md).

  - A recipe, passed on to
    [`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md).

  - A
    [`workflow_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
    object, passed on to
    [`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md).

- spec:

  An optional parsnip model specification to add to the workflow. Passed
  on to
  [`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md).

- postprocessor:

  An optional
  [`tailor::tailor()`](https://tailor.tidymodels.org/reference/tailor.html)
  defining post-processing steps to add to the workflow. Passed on to
  [`add_tailor()`](https://workflows.tidymodels.org/dev/reference/add_tailor.md).

## Value

A new `workflow` object.

## Indicator Variable Details

Some modeling functions in R create indicator/dummy variables from
categorical data when you use a model formula, and some do not. When you
specify and fit a model with a `workflow()`, parsnip and workflows match
and reproduce the underlying behavior of the user-specified model’s
computational engine.

### Formula Preprocessor

In the
[modeldata::Sacramento](https://modeldata.tidymodels.org/reference/Sacramento.html)
data set of real estate prices, the `type` variable has three levels:
`"Residential"`, `"Condo"`, and `"Multi-Family"`. This base `workflow()`
contains a formula added via
[`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md)
to predict property price from property type, square footage, number of
beds, and number of baths:

    set.seed(123)

    library(parsnip)
    library(recipes)
    library(workflows)
    library(modeldata)

    data("Sacramento")

    base_wf <- workflow() |>
      add_formula(price ~ type + sqft + beds + baths)

This first model does create dummy/indicator variables:

    lm_spec <- linear_reg() |>
      set_engine("lm")

    base_wf |>
      add_model(lm_spec) |>
      fit(Sacramento)

    ## == Workflow [trained] ================================================
    ## Preprocessor: Formula
    ## Model: linear_reg()
    ##
    ## -- Preprocessor ------------------------------------------------------
    ## price ~ type + sqft + beds + baths
    ##
    ## -- Model -------------------------------------------------------------
    ##
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ##
    ## Coefficients:
    ##      (Intercept)  typeMulti_Family   typeResidential
    ##          32919.4          -21995.8           33688.6
    ##             sqft              beds             baths
    ##            156.2          -29788.0            8730.0

There are **five** independent variables in the fitted model for this
OLS linear regression. With this model type and engine, the factor
predictor `type` of the real estate properties was converted to two
binary predictors, `typeMulti_Family` and `typeResidential`. (The third
type, for condos, does not need its own column because it is the
baseline level).

This second model does not create dummy/indicator variables:

    rf_spec <- rand_forest() |>
      set_mode("regression") |>
      set_engine("ranger")

    base_wf |>
      add_model(rf_spec) |>
      fit(Sacramento)

    ## == Workflow [trained] ================================================
    ## Preprocessor: Formula
    ## Model: rand_forest()
    ##
    ## -- Preprocessor ------------------------------------------------------
    ## price ~ type + sqft + beds + baths
    ##
    ## -- Model -------------------------------------------------------------
    ## Ranger result
    ##
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1))
    ##
    ## Type:                             Regression
    ## Number of trees:                  500
    ## Sample size:                      932
    ## Number of independent variables:  4
    ## Mtry:                             2
    ## Target node size:                 5
    ## Variable importance mode:         none
    ## Splitrule:                        variance
    ## OOB prediction error (MSE):       7058847504
    ## R squared (OOB):                  0.5894647

Note that there are **four** independent variables in the fitted model
for this ranger random forest. With this model type and engine,
indicator variables were not created for the `type` of real estate
property being sold. Tree-based models such as random forest models can
handle factor predictors directly, and don’t need any conversion to
numeric binary variables.

### Recipe Preprocessor

When you specify a model with a `workflow()` and a recipe preprocessor
via
[`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md),
the *recipe* controls whether dummy variables are created or not; the
recipe overrides any underlying behavior from the model’s computational
engine.

## Examples

``` r
library(parsnip)
library(recipes)
library(magrittr)
library(modeldata)
#> 
#> Attaching package: ‘modeldata’
#> The following object is masked from ‘package:datasets’:
#> 
#>     penguins

data("attrition")

model <- logistic_reg() |>
  set_engine("glm")

formula <- Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime

wf_formula <- workflow(formula, model)

fit(wf_formula, attrition)
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: logistic_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:  stats::glm(formula = ..y ~ ., family = stats::binomial, data = data)
#> 
#> Coefficients:
#>                     (Intercept)  BusinessTravelTravel_Frequently  
#>                        -2.82571                          1.29473  
#>     BusinessTravelTravel_Rarely          YearsSinceLastPromotion  
#>                         0.64727                         -0.03092  
#>                     OverTimeYes  
#>                         1.31904  
#> 
#> Degrees of Freedom: 1469 Total (i.e. Null);  1465 Residual
#> Null Deviance:       1299 
#> Residual Deviance: 1194  AIC: 1204

recipe <- recipe(Attrition ~ ., attrition) |>
  step_dummy(all_nominal(), -Attrition) |>
  step_corr(all_predictors(), threshold = 0.8)

wf_recipe <- workflow(recipe, model)

fit(wf_recipe, attrition)
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: logistic_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> 2 Recipe Steps
#> 
#> • step_dummy()
#> • step_corr()
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:  stats::glm(formula = ..y ~ ., family = stats::binomial, data = data)
#> 
#> Coefficients:
#>                      (Intercept)                               Age  
#>                       -2.535e+00                        -3.131e-02  
#>                        DailyRate                  DistanceFromHome  
#>                       -3.126e-04                         4.927e-02  
#>                       HourlyRate                     MonthlyIncome  
#>                        2.762e-03                         1.127e-06  
#>                      MonthlyRate                NumCompaniesWorked  
#>                        1.663e-06                         1.956e-01  
#>                PercentSalaryHike                  StockOptionLevel  
#>                       -2.495e-02                        -1.968e-01  
#>                TotalWorkingYears             TrainingTimesLastYear  
#>                       -6.820e-02                        -1.863e-01  
#>                   YearsAtCompany                YearsInCurrentRole  
#>                        8.916e-02                        -1.371e-01  
#>          YearsSinceLastPromotion              YearsWithCurrManager  
#>                        1.849e-01                        -1.516e-01  
#> BusinessTravel_Travel_Frequently      BusinessTravel_Travel_Rarely  
#>                        1.940e+00                         1.080e+00  
#>                      Education_1                       Education_2  
#>                       -1.391e-01                        -2.753e-01  
#>                      Education_3                       Education_4  
#>                       -7.324e-02                         3.858e-02  
#>     EducationField_Life_Sciences          EducationField_Marketing  
#>                       -6.939e-01                        -2.212e-01  
#>           EducationField_Medical              EducationField_Other  
#>                       -7.210e-01                        -6.755e-01  
#>  EducationField_Technical_Degree         EnvironmentSatisfaction_1  
#>                        2.936e-01                        -9.501e-01  
#>        EnvironmentSatisfaction_2         EnvironmentSatisfaction_3  
#>                        4.383e-01                        -2.491e-01  
#>                      Gender_Male                  JobInvolvement_1  
#>                        4.243e-01                        -1.474e+00  
#>                 JobInvolvement_2                  JobInvolvement_3  
#>                        2.297e-01                        -2.855e-01  
#>          JobRole_Human_Resources     JobRole_Laboratory_Technician  
#>                        1.441e+00                         1.549e+00  
#>                  JobRole_Manager    JobRole_Manufacturing_Director  
#>                        1.900e-01                         3.726e-01  
#>        JobRole_Research_Director        JobRole_Research_Scientist  
#>                       -9.581e-01                         6.055e-01  
#>          JobRole_Sales_Executive      JobRole_Sales_Representative  
#>                        1.056e+00                         2.149e+00  
#>                JobSatisfaction_1                 JobSatisfaction_2  
#>                       -9.446e-01                        -8.929e-03  
#>                JobSatisfaction_3             MaritalStatus_Married  
#>                       -2.860e-01                         3.135e-01  
#> 
#> ...
#> and 14 more lines.

variables <- workflow_variables(
  Attrition,
  c(BusinessTravel, YearsSinceLastPromotion, OverTime)
)

wf_variables <- workflow(variables, model)

fit(wf_variables, attrition)
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Variables
#> Model: logistic_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> Outcomes: Attrition
#> Predictors: c(BusinessTravel, YearsSinceLastPromotion, OverTime)
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:  stats::glm(formula = ..y ~ ., family = stats::binomial, data = data)
#> 
#> Coefficients:
#>                     (Intercept)  BusinessTravelTravel_Frequently  
#>                        -2.82571                          1.29473  
#>     BusinessTravelTravel_Rarely          YearsSinceLastPromotion  
#>                         0.64727                         -0.03092  
#>                     OverTimeYes  
#>                         1.31904  
#> 
#> Degrees of Freedom: 1469 Total (i.e. Null);  1465 Residual
#> Null Deviance:       1299 
#> Residual Deviance: 1194  AIC: 1204
```
