# sparse tibble can be passed to `fit() - formula

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Condition
      Error in `fit()`:
      ! Sparse data cannot be used with the formula interface. Please use `add_recipe()` or `add_variables()` instead.

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
      ! Sparse data cannot be used with the formula interface. Please use `add_recipe()` or `add_variables()` instead.

# sparse matrices can be passed to `fit() - xy

    Code
      wf_fit <- fit(wf_spec, hotel_data)
    Output
      sparsevctrs: Sparse vector materialized

# fit() errors if sparse matrix has no colnames

    Code
      fit(wf_spec, hotel_data)
    Condition
      Error in `fit()`:
      ! `x` must have column names.

