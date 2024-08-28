# error if no preprocessor

    Code
      pull_workflow_preprocessor(workflow())
    Condition
      Error in `extract_preprocessor()`:
      ! The workflow does not have a preprocessor.

# error if not a workflow

    Code
      pull_workflow_preprocessor(1)
    Condition
      Error in `pull_workflow_preprocessor()`:
      ! `x` must be a workflow, not a <numeric>.

---

    Code
      pull_workflow_spec(1)
    Condition
      Error in `pull_workflow_spec()`:
      ! `x` must be a workflow, not a <numeric>.

---

    Code
      pull_workflow_fit(1)
    Condition
      Error in `pull_workflow_fit()`:
      ! `x` must be a workflow, not a <numeric>.

---

    Code
      pull_workflow_mold(1)
    Condition
      Error in `pull_workflow_mold()`:
      ! `x` must be a workflow, not a <numeric>.

---

    Code
      pull_workflow_prepped_recipe(1)
    Condition
      Error in `pull_workflow_prepped_recipe()`:
      ! `x` must be a workflow, not a <numeric>.

# `pull_workflow_preprocessor()` is soft-deprecated

    Code
      x <- pull_workflow_preprocessor(workflow)
    Condition
      Warning:
      `pull_workflow_preprocessor()` was deprecated in workflows 0.2.3.
      i Please use `extract_preprocessor()` instead.

# error if no spec

    Code
      pull_workflow_spec(workflow())
    Condition
      Error in `extract_spec_parsnip()`:
      ! The workflow does not have a model spec.

# `pull_workflow_spec()` is soft-deprecated

    Code
      x <- pull_workflow_spec(workflow)
    Condition
      Warning:
      `pull_workflow_spec()` was deprecated in workflows 0.2.3.
      i Please use `extract_spec_parsnip()` instead.

# error if no fit

    Code
      pull_workflow_fit(workflow())
    Condition
      Error in `extract_fit_parsnip()`:
      ! Can't extract a model fit from an untrained workflow.
      i Do you need to call `fit()`?

# `pull_workflow_fit()` is soft-deprecated

    Code
      x <- pull_workflow_fit(workflow)
    Condition
      Warning:
      `pull_workflow_fit()` was deprecated in workflows 0.2.3.
      i Please use `extract_fit_parsnip()` instead.

# error if no mold

    Code
      pull_workflow_mold(workflow())
    Condition
      Error in `extract_mold()`:
      ! Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

---

    Code
      pull_workflow_prepped_recipe(workflow)
    Condition
      Error in `extract_mold()`:
      ! Can't extract a mold from an untrained workflow.
      i Do you need to call `fit()`?

# `pull_workflow_mold()` is soft-deprecated

    Code
      x <- pull_workflow_mold(workflow)
    Condition
      Warning:
      `pull_workflow_mold()` was deprecated in workflows 0.2.3.
      i Please use `extract_mold()` instead.

# error if no recipe preprocessor

    Code
      pull_workflow_prepped_recipe(workflow())
    Condition
      Error in `extract_recipe()`:
      ! The workflow must have a recipe preprocessor.

# `pull_workflow_prepped_recipe()` is soft-deprecated

    Code
      x <- pull_workflow_prepped_recipe(workflow)
    Condition
      Warning:
      `pull_workflow_prepped_recipe()` was deprecated in workflows 0.2.3.
      i Please use `extract_recipe()` instead.

