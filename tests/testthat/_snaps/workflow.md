# workflow must be the first argument when adding actions

    Code
      add_formula(1, mpg ~ cyl)
    Condition
      Error in `add_formula()`:
      ! `x` must be a workflow, not a <numeric>.

---

    Code
      add_recipe(1, rec)
    Condition
      Error in `add_recipe()`:
      ! `x` must be a workflow, not a <numeric>.

---

    Code
      add_model(1, mod)
    Condition
      Error in `add_model()`:
      ! `x` must be a workflow, not a <numeric>.

# confirms compatibility of model and tailor

    Code
      workflow(preprocessor = mpg ~ cyl, spec = parsnip::linear_reg(), postprocessor = tailor)
    Condition
      Error in `workflow()`:
      ! The model mode "regression" and the tailor type "binary" are incompatible.

# model spec is validated

    Code
      workflow(spec = 1)
    Condition
      Error in `add_model()`:
      ! `spec` must be a <model_spec>.

# preprocessor is validated

    Code
      workflow(preprocessor = 1)
    Condition
      Error in `workflow()`:
      ! `preprocessor` must be a formula, recipe, or a set of workflow variables.

# postprocessor is validated

    Code
      workflow(postprocessor = 1)
    Condition
      Error in `workflow()`:
      ! `postprocessor` must be a <tailor>.

# constructor validates input

    Code
      new_workflow(pre = 1)
    Condition
      Error in `new_workflow()`:
      ! `pre` must be a `stage`.

---

    Code
      new_workflow(fit = 1)
    Condition
      Error in `new_workflow()`:
      ! `fit` must be a `stage`.

---

    Code
      new_workflow(post = 1)
    Condition
      Error in `new_workflow()`:
      ! `post` must be a `stage`.

---

    Code
      new_workflow(trained = 1)
    Condition
      Error in `new_workflow()`:
      ! `trained` must be a single logical value.

# input must be a workflow

    `x` must be a workflow, not a <numeric>.

