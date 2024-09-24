# sparse tibble can be passed to `fit() - formula

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Condition
      Error in `fit()`:
      ! Sparse data cannot be used with formula interface. Please use `add_recipe()` or `add_variables()` instead.

# sparse matrices can be passed to `fit() - recipe

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Output
      sparsevctrs: Sparse vector materialized

# sparse matrices can be passed to `fit() - formula

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Condition
      Error in `fit()`:
      ! Sparse data cannot be used with formula interface. Please use `add_recipe()` or `add_variables()` instead.

# sparse matrices can be passed to `fit() - xy

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Output
      sparsevctrs: Sparse vector materialized

# sparse matrix can be passed to `predict()

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Output
      sparsevctrs: Sparse vector materialized

