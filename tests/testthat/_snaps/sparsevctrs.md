# sparse tibble can be passed to `fit() - formula

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Condition
      Error in `fit()`:
      ! Sparse data cannot be used with formula interface. Please use `add_recipe()` or `add_variables()` instead.

