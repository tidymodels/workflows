# Determine required packages for a workflow

Determine required packages for a workflow

## Usage

``` r
# S3 method for class 'workflow'
required_pkgs(x, infra = TRUE, ...)
```

## Arguments

- x:

  A `workflow` object.

- infra:

  Should the core packages themselves be included in the result? Those
  are workflows and parsnip (for the model), as well as recipes if the
  workflow includes a recipes preprocessor and tailor if it includes a
  tailor post-processor.

## Value

A character vector.
