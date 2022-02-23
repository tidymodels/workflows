# can't tidy the model of an unfit workflow

    Code
      tidy(x)
    Condition
      Error in `extract_fit_parsnip()`:
      ! The workflow does not have a model fit. Have you called `fit()` yet?

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
      ! The workflow does not have a mold. Have you called `fit()` yet?

# can't glance at the model of an unfit workflow

    The workflow does not have a model fit. Have you called `fit()` yet?

# can't augment with the model of an unfit workflow

    The workflow does not have a model fit. Have you called `fit()` yet?

