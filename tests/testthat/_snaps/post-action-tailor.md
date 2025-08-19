# postprocessor is validated

    Code
      add_tailor(workflow(), 1)
    Condition
      Error in `add_tailor()`:
      ! `tailor` must be a tailor.

# add_tailor() confirms compatibility of model and tailor

    Code
      add_tailor(workflow, tailor)
    Condition
      Error in `add_tailor()`:
      ! The model mode "regression" and the tailor type "binary" are incompatible.

# cannot add two postprocessors

    Code
      add_tailor(workflow, post)
    Condition
      Error in `add_tailor()`:
      ! A `tailor` action has already been added to this workflow.

