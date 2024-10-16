# error if no preprocessor

    Code
      extract_preprocessor(workflow())
    Condition
      Error in `extract_preprocessor()`:
      ! The workflow does not have a preprocessor.

# error if not a workflow

    Code
      extract_preprocessor(1)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'extract_preprocessor' applied to an object of class "c('double', 'numeric')"

---

    Code
      extract_spec_parsnip(1)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'extract_spec_parsnip' applied to an object of class "c('double', 'numeric')"

---

    Code
      extract_fit_parsnip(1)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'extract_fit_parsnip' applied to an object of class "c('double', 'numeric')"

---

    Code
      extract_mold(1)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'extract_mold' applied to an object of class "c('double', 'numeric')"

---

    Code
      extract_recipe(1)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'extract_recipe' applied to an object of class "c('double', 'numeric')"

# error if no spec

    Code
      extract_spec_parsnip(workflow())
    Condition
      Error in `extract_spec_parsnip()`:
      ! The workflow does not have a model spec.

# error if no parsnip fit

    Code
      extract_fit_parsnip(workflow())
    Condition
      Error in `extract_fit_parsnip()`:
      ! Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

# error if no mold

    Code
      extract_mold(workflow())
    Condition
      Error in `extract_mold()`:
      ! Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      extract_recipe(workflow)
    Condition
      Error in `extract_mold()`:
      ! Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

# can extract a prepped recipe

    Code
      extract_recipe(workflow, FALSE)
    Condition
      Error in `extract_recipe()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = FALSE
      i Did you forget to name an argument?

---

    Code
      extract_recipe(workflow, estimated = "yes please")
    Condition
      Error in `extract_recipe()`:
      ! `estimated` must be a single `TRUE` or `FALSE`.

# error if no recipe preprocessor

    Code
      extract_recipe(workflow())
    Condition
      Error in `extract_recipe()`:
      ! The workflow must have a recipe preprocessor.

# extract parameter set from workflow with potentially conflicting ids (#266)

    Code
      extract_parameter_set_dials(wflow)
    Condition
      Error in `extract_parameter_set_dials()`:
      x Element id should have unique values.
      i Duplicates exist for item: threshold

