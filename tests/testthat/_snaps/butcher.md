# fails if not a fitted workflow

    Code
      butcher::butcher(workflow())
    Condition
      Error in `extract_fit_parsnip()`:
      ! The workflow does not have a model fit. Have you called `fit()` yet?

