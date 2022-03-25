# case weights `col` can't select >1 columns in `data`

    Code
      fit(wf, mtcars)
    Condition
      Error in `fit()`:
      ! `col` must specify exactly one column from `data` to extract case weights from.

# case weights must inherit from the base case weights class

    Code
      fit(wf, df)
    Condition
      Error in `fit()`:
      ! `col` must select a classed case weights column, as determined by `hardhat::is_case_weights()`. For example, it could be a column created by `hardhat::frequency_weights()` or `hardhat::importance_weights()`.

