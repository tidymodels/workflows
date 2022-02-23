# workflow must have been `fit()` before prediction can be done

    Code
      predict(workflow(), mtcars)
    Condition
      Error in `predict()`:
      ! Workflow has not yet been trained. Do you need to call `fit()`?

