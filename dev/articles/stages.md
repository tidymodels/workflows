# Workflow Stages

Workflows encompasses the three main stages of the modeling *process*:
pre-processing of data, model fitting, and post-processing of results.
This page enumerates the possible operations for each stage that have
been implemented to date.

## Pre-processing

There are three options for pre-processing but you can only use one of
them in a single workflow:

- A standard [model
  formula](https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Formulae-for-statistical-models)
  via
  [`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md).

- A tidyselect interface via
  [`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
  that [strictly preserves the
  class](https://www.tidyverse.org/blog/2020/09/workflows-0-2-0/) of
  your columns.

- A recipe object via
  [`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md).

## Model Fitting

`parsnip` model specifications are the only option here, specified via
[`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md).

When using a preprocessor, you may need an additional formula for
special model terms (e.g. for mixed models or generalized linear
models). In these cases, specify that formula using
[`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md)’s
`formula` argument, which will be passed to the underlying model when
[`fit()`](https://generics.r-lib.org/reference/fit.html) is called.

## Post-processing

`tailor` post-processors are the only option here, specified via
[`add_tailor()`](https://workflows.tidymodels.org/dev/reference/add_tailor.md).
Some examples of post-processing model predictions could include adding
a probability threshold for two-class problems, calibration of
probability estimates, truncating the possible range of predictions, and
so on.
