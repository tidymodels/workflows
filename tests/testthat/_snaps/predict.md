# workflow must have been `fit()` before prediction can be done

    Code
      predict(workflow(), mtcars)
    Condition
      Error in `predict()`:
      ! Can't predict on an untrained workflow.
      i Do you need to call `fit()`?

# predict(type) is respected with a postprocessor (#251)

    Code
      predict(wflow_fit, d[1:5, ], type = "boop")
    Condition
      Error in `predict()`:
      ! Unsupported prediction `type` "boop" for a workflow with a postprocessor.

