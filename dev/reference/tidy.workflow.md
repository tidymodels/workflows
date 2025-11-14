# Tidy a workflow

This is a
[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
method for a workflow that calls
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) on either the
underlying parsnip model, recipe, or tailor, depending on the value of
`what`.

`x` must be a fitted workflow, resulting in fitted parsnip model,
prepped recipe or fitted tailor that you want to tidy.

## Usage

``` r
# S3 method for class 'workflow'
tidy(x, what = "model", ...)
```

## Arguments

- x:

  A workflow

- what:

  A single string. Either `"model"`, `"recipe"` or `"tailor"` to select
  which part of the workflow to tidy. Defaults to tidying the model.

- ...:

  Arguments passed on to methods

## Details

To tidy the unprepped recipe, use
[`extract_preprocessor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
and [`tidy()`](https://generics.r-lib.org/reference/tidy.html) that
directly. To tidy the untrained tailor, use
[`extract_postprocessor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
and [`tidy()`](https://generics.r-lib.org/reference/tidy.html) that
directly.
