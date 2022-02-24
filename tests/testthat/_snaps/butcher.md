# fails if not a fitted workflow

    Code
      butcher::butcher(workflow())
    Condition
      Error in `extract_fit_parsnip()`:
      ! Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

