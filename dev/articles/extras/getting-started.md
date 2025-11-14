# Getting Started

This is an example of a fairly realistic interactive data analysis
project to demonstrate how workflows can be used.

## Introduction

In this `bivariate` data set, there are two predictors that can be used
to differentiate two classes in the outcome.

There are three partitions of the original data: training (n = 1009),
validation (n = 300), and testing (n = 710). We will work with the
training set the most, use the validation set to compare models during
the development process, and then use the test set once we think that we
have a good algorithm for making predictions.

``` r
library(modeldata)
#> 
#> Attaching package: 'modeldata'
#> The following object is masked from 'package:datasets':
#> 
#>     penguins

# This gives us access to the 3 partitions:
# - `bivariate_train`: Training set
# - `bivariate_val`: Validation set
# - `bivariate_test`: Test set
data("bivariate")
```

Here is the training set:

``` r
library(workflows)
library(ggplot2)
library(dplyr)

ggplot(bivariate_train, aes(x = A, y = B, col = Class)) + 
  geom_point(alpha = .3) + 
  coord_equal(ratio = 20)
```

![A ggplot dotplot with predictor 'A' on the x axis and 'B' on the y,
demonstrating a positive, linear, moderately strong correlation. Dots
are colored according to two classes, red for 'One' and blue for 'Two',
and the red dots tend to have slightly larger values of 'B' for given
values of 'A', though the separation is far from
perfect.](getting-started_files/figure-html/plot-data-1.png)

Both predictors have positive values and their distributions are
right-skewed. There seems to be a separation of the classes, but only
when the variables are used together. For example, when the predictors
are visualized individually, there is little evidence in separation of
the classes.

``` r
library(tidyr)

bivariate_train |> 
  pivot_longer(cols = c(A, B), names_to = "predictor") |> 
  ggplot(aes(x = Class, y = value)) + 
  geom_boxplot() + 
  facet_wrap(~predictor, scales = "free_y") + 
  scale_y_log10()
```

![A pair of boxplots showing the same predictors, this time one at a
time. The predictors on their own do little to separate the two outcome
classes.](getting-started_files/figure-html/plot-marginals-1.png)

In the first plot above, the separation appears to happen linearly, and
a straight, diagonal boundary might do well. We could use
[`glm()`](https://rdrr.io/r/stats/glm.html) directly to create a
logistic regression, but we will use the `tidymodels` infrastructure and
start by making a `parsnip` model object.

``` r
library(parsnip)

logit_mod <-
  logistic_reg() |>
  set_engine("glm")
```

This data analysis will involve looking at a few different approaches of
representing the two predictors so that we have a high-quality model.
We’ll walk though the thought process of this analysis as we go. This
will emulate how most data analysis projects happen: an initial approach
is taken and then potential steps are attempted to make improvements.
There is no pre-defined blueprint to this process and the R4DS diagram
summarizes the process nicely.

Since we are going to try different combinations of feature engineering
and models, `workflows` are really useful since you can have one object
that contains all of these operations. It helps organize your work
instead of having different objects in your workspace that, at some
point, have been used in pairs.

## A first set of models

The obvious place to start is by adding both predictors as-is into the
model:

``` r
# Create a workflow with just the model. We will add to this as we go. 
glm_workflow <-
  workflow() |>
  add_model(logit_mod)

simple_glm <-
  glm_workflow |>
  # Add both predictors in
  add_formula(Class ~ .) |>
  # Fit the model:
  fit(data = bivariate_train)
```

To evaluate this model, the ROC curve will be computed along with its
corresponding AUC.

``` r
library(yardstick)

simple_glm_probs <-
  predict(simple_glm, bivariate_val, type = "prob") |>
  bind_cols(bivariate_val)

simple_glm_roc <- 
  simple_glm_probs |> 
  roc_curve(Class, .pred_One)

simple_glm_probs |> roc_auc(Class, .pred_One)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.773

autoplot(simple_glm_roc)
```

![A ggplot2 line plot, with 1 minus specificity on the x axis,
sensitivity on the y, and a dotted line tracing the y equals x line. A
solid line is shown always above the dotted line, with greatest
separation from it towards the center of the
plot.](getting-started_files/figure-html/simple-roc-1.png)

This seems reasonable. One potential issue is that the two predictors
have a high degree of correlation 0.848, and this might cause some
instability in the model.

Since there are two correlated predictors with skewed distributions and
strictly positive values, it might be intuitive to use their ratio
instead of the pair. We’ll try that next by recycling the initial
workflow and just adding a different formula:

``` r
ratio_glm <-
  glm_workflow |>
  add_formula(Class ~ I(A/B)) |> 
  fit(data = bivariate_train)

ratio_glm_probs <-
  predict(ratio_glm, bivariate_val, type = "prob") |>
  bind_cols(bivariate_val)

ratio_glm_roc <- 
  ratio_glm_probs |> 
  roc_curve(Class, .pred_One)

ratio_glm_probs |> roc_auc(Class, .pred_One)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.765

autoplot(simple_glm_roc) + 
  geom_path(
    data = ratio_glm_roc, 
    aes(x = 1 - specificity, y = sensitivity), 
    col = "#FDE725FF"
  )
```

![The same plot as before, this time with a yellow line also added to
the plot. The yellow line roughly traces the solid black one, except
that it's y values are slightly higher than those of the solid black
line towards the left portion of the plot and vice versa for the other
portion.](getting-started_files/figure-html/ratios-1.png)

The original analysis shows a slight edge, but the two models are
probably within the experimental noise of one another.

## More complex feature engineering

Instead of combining the two predictors, would it help the model if we
were to resolve the skewness of the variables? To test this theory, one
option would be to use the Box-Cox transformation on each predictor
individually to see if it recommends a nonlinear transformation. The
transformation can encode a variety of different functions including the
log transform, square root, inverse, and fractional transformations
in-between these.

This cannot be easily done via the formula interface, so a recipe is
used. A recipe is a list of sequential data processing steps that are
conducted before the data are used in a model. For example, to use the
Box-Cox method, a simple recipe would be:

``` r
library(recipes)

trans_recipe <- 
  recipe(Class ~ ., data = bivariate_train) |> 
  step_BoxCox(all_predictors())
```

Creating the recipe only makes an object with the instructions; it does
not carry out the instructions (e.g. estimate the transformation
parameter). To actually execute the recipe, we add it to our workflow
with
[`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md)
and then call [`fit()`](https://generics.r-lib.org/reference/fit.html).
Fitting the workflow evaluates both the model and the recipe.

``` r
trans_glm <-
  glm_workflow |>
  add_recipe(trans_recipe) |> 
  fit(data = bivariate_train)

trans_glm_probs <-
  predict(trans_glm, bivariate_val, type = "prob") |>
  bind_cols(bivariate_val)

trans_glm_roc <- 
  trans_glm_probs |> 
  roc_curve(Class, .pred_One)

trans_glm_probs |> roc_auc(Class, .pred_One)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.815

autoplot(simple_glm_roc) + 
  geom_path(
    data = ratio_glm_roc, 
    aes(x = 1 - specificity, y = sensitivity), 
    col = "#FDE725FF"
  ) + 
  geom_path(
    data = trans_glm_roc, 
    aes(x = 1 - specificity, y = sensitivity), 
    col = "#21908CFF"
  )
```

![The same plot as above, but with another line, shown in green, added
to the plot. Its y values are nearly always consistently higher than the
existing black and yellow
lines.](getting-started_files/figure-html/rec-trans-1.png)

That is a potential, if slight, improvement.

The Box-Cox procedure recommended transformations that are pretty close
to the inverse.

The model above creates a class boundary for these data:

``` r
ggplot(bivariate_train, aes(x = 1/A, y = 1/B, col = Class)) + 
  geom_point(alpha = .3) + 
  coord_equal(ratio = 1/12)
```

![A dotplot like that shown early on in the article, plotting the
inverse of the previous two predictors and the same binary outcome
variable. A boundary between the two outcome classes seems a bit more
evident in this plot than
before.](getting-started_files/figure-html/plot-inverse-1.png)

The correlation between these is about the same as in the original data.
It might help the model to de-correlate them, and the standard technique
for this is principal component analysis. A recipe step can be added
that will conduct PCA and return the score values. The scores, instead
of the original predictors, can then be used in the model. PCA chases
variability, so it is important to normalize the two predictors so that
they have the same units. Traditionally, each column could be centered
and scaled. For this reason, a step is used prior to PCA that normalizes
the two predictors.

``` r
pca_recipe <- 
  trans_recipe |> 
  step_normalize(A, B) |>
  step_pca(A, B, num_comp = 2)

pca_glm <-
  glm_workflow |>
  add_recipe(pca_recipe) |> 
  fit(data = bivariate_train)

pca_glm_probs <-
  predict(pca_glm, bivariate_val, type = "prob") |>
  bind_cols(bivariate_val)

pca_glm_roc <- 
  pca_glm_probs |> 
  roc_curve(Class, .pred_One)

pca_glm_probs |> roc_auc(Class, .pred_One)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.815
```

These results are almost identical to the transformed model.

## The test set

Based on these results, the model with the logistic regression model
with inverse terms is probably our best bet. Using the test set:

``` r
test_probs <- 
  predict(trans_glm, bivariate_test, type = "prob") |>
  bind_cols(bivariate_test)

test_roc <- 
  test_probs |> 
  roc_curve(Class, .pred_One)

# A little more optimistic than the validation set
test_probs |> roc_auc(Class, .pred_One)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.862

autoplot(test_roc) 
```

![An ROC curve plot like before, back to showing just one solid black
line, representing what had been shown as green in the previous plot.
There's quite a bit of separation between this line and the identity,
showing a model that's similarly as performant as it was on the training
set.](getting-started_files/figure-html/test-set-1.png)
