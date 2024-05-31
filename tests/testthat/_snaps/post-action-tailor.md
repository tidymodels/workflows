# postprocessor is validated

    Code
      add_tailor(workflow(), 1)
    Condition
      Error in `add_tailor()`:
      ! `tailor` must be a tailor.

# cannot add two postprocessors

    Code
      add_tailor(workflow, post)
    Condition
      Error in `add_tailor()`:
      ! A `tailor` action has already been added to this workflow.

