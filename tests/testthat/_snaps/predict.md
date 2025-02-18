# workflow must have been `fit()` before prediction can be done

    Code
      predict(workflow(), mtcars)
    Condition
      Error in `predict()`:
      ! Can't predict on an untrained workflow.
      i Do you need to call `fit()`?

