# Add a tailor to a workflow

- `add_tailor()` specifies post-processing steps to apply through the
  usage of a tailor.

- `remove_tailor()` removes the tailor as well as any downstream objects
  that might get created after the tailor is used for post-processing,
  such as the fitted tailor.

- `update_tailor()` first removes the tailor, then replaces the previous
  tailor with the new one.

## Usage

``` r
add_tailor(x, tailor, ...)

remove_tailor(x)

update_tailor(x, tailor, ...)
```

## Arguments

- x:

  A workflow

- tailor:

  A tailor created using
  [`tailor::tailor()`](https://tailor.tidymodels.org/reference/tailor.html).
  The tailor should not have been trained already with
  [`tailor::fit()`](https://tailor.tidymodels.org/reference/reexports.html);
  workflows will handle training internally.

- ...:

  Not used.

## Value

`x`, updated with either a new or removed tailor postprocessor.

## Data Usage

While preprocessors and models are trained on data in the usual sense,
postprocessors are trained on *predictions* on data. When a workflow is
fitted, the user typically supplies training data with the `data`
argument. When workflows don't contain a postprocessor that requires
training, users can pass all of the available data to the `data`
argument to train the preprocessor and model. However, in the case where
a postprocessor must be trained as well, allotting all of the available
data to the `data` argument to train the preprocessor and model would
leave no data to train the postprocessor with—if that were the case,
workflows would need to
[`predict()`](https://rdrr.io/r/stats/predict.html) from the
preprocessor and model on the same `data` that they were trained on,
with the postprocessor then training on those predictions. Predictions
on data that a model was trained on likely follow different
distributions than predictions on unseen data; thus, workflows must
split up the supplied `data` into two training sets, where the first is
used to train the preprocessor and model and the second, called the
"calibration set," is passed to that trained postprocessor and model to
generate predictions, which then form the training data for the
postprocessor.

When fitting a workflow with a postprocessor that requires training
(i.e. one that returns `TRUE` in
`.workflow_postprocessor_requires_fit(workflow)`), users must pass two
data arguments–the usual `fit.workflow(data)` will be used to train the
preprocessor and model while `fit.workflow(data_calibration)` will be
used to train the postprocessor.

In some situations, randomly splitting `fit.workflow(data)` (with
[`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html),
for example) is sufficient to prevent data leakage. However,
`fit.workflow(data)` could also have arisen as:

    boots <- rsample::bootstraps(some_other_data)
    split <- rsample::get_rsplit(boots, 1)
    data <- rsample::analysis(split)

In this case, some of the rows in `data` will be duplicated. Thus,
randomly allotting some of them to train the preprocessor and model and
others to train the preprocessor would likely result in the same rows
appearing in both datasets, resulting in the preprocessor and model
generating predictions on rows they've seen before. Similarly
problematic situations could arise in the context of other resampling
situations, like time-based splits. In general,
[`rsample::internal_calibration_split()`](https://rsample.tidymodels.org/reference/internal_calibration_split.html)
offers a way to prevent data leakage when resampling. When workflows
with postprocessors that require training are passed to the tune
package, this is handled internally.

## Examples

``` r
library(tailor)
library(magrittr)

tailor <- tailor()
tailor_1 <- adjust_probability_threshold(tailor, .1)

workflow <- workflow() |>
  add_tailor(tailor_1)

workflow
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: None
#> Postprocessor: tailor
#> 
#> ── Postprocessor ──────────────────────────────────────────────────────
#> 
#> ── tailor ─────────────────────────────────────────────────────────────
#> A binary postprocessor with 1 adjustment:
#> 
#> • Adjust probability threshold to 0.1.
#> NA
#> NA
#> NA

remove_tailor(workflow)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: None

update_tailor(workflow, adjust_probability_threshold(tailor, .2))
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: None
#> Postprocessor: tailor
#> 
#> ── Postprocessor ──────────────────────────────────────────────────────
#> 
#> ── tailor ─────────────────────────────────────────────────────────────
#> A binary postprocessor with 1 adjustment:
#> 
#> • Adjust probability threshold to 0.2.
#> NA
#> NA
#> NA
```
