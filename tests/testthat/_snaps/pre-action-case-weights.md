# case weights + recipe doesn't allow the recipe to drop the case weights column

    Code
      fit(wf, df)
    Condition
      Error in `fit()`:
      ! No columns with a "case_weights" role exist in the data after processing the recipe.
      i Did you remove or modify the case weights while processing the recipe?

# case weights + recipe doesn't allow the recipe to adjust the case weights column class

    Code
      fit(wf, df)
    Condition
      Error in `fit()`:
      ! The column with a recipes role of "case_weights" must be a classed case weights column, as determined by `hardhat::is_case_weights()`.
      i Did you modify the case weights while processing the recipe?

# case weights + recipe doesn't allow the recipe to change the name of the case weights column

    Code
      fit(wf, df)
    Condition
      Error in `fit()`:
      ! Can't select columns that don't exist.
      x Column `w` doesn't exist.

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
      ! `col` must select a classed case weights column, as determined by `hardhat::is_case_weights()`.
      i For example, it could be a column created by `hardhat::frequency_weights()` or `hardhat::importance_weights()`.

