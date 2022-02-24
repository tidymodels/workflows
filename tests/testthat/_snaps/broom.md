# can't tidy the model of an unfit workflow

    Code
      tidy(x)
    Condition
      Error in `extract_fit_parsnip()`:
      ! Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

# can't tidy the recipe of an unfit workflow

    Code
      tidy(x, what = "recipe")
    Condition
      Error in `extract_recipe()`:
      ! The workflow must have a recipe preprocessor.

---

    Code
      tidy(x, what = "recipe")
    Condition
      Error in `extract_mold()`:
      ! Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

# can't glance at the model of an unfit workflow

    Can't extract a model fit from an untrained workflow.
    i Do you need to call `fit()`?

# can't augment with the model of an unfit workflow

    Can't extract a model fit from an untrained workflow.
    i Do you need to call `fit()`?

