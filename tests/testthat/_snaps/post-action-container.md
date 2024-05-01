# postprocessor is validated

    Code
      add_container(workflow(), 1)
    Condition
      Error in `add_container()`:
      ! `container` must be a container.

# cannot add two postprocessors

    Code
      add_container(workflow, post)
    Condition
      Error in `add_container()`:
      ! A `container` action has already been added to this workflow.

