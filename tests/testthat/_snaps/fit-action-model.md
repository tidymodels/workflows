# model is validated

    Code
      add_model(workflow(), 1)
    Condition
      Error in `add_model()`:
      ! `spec` must be a `model_spec`.

# model must contain a known mode (#160)

    Code
      add_model(workflow, mod)
    Condition
      Error in `add_model()`:
      ! `spec` must have a known mode.
      i Set the mode of `spec` by using `parsnip::set_mode()` or by setting the mode directly in the parsnip specification function.

# cannot add two models

    Code
      add_model(workflow, mod)
    Condition
      Error in `add_model()`:
      ! A `model` action has already been added to this workflow.

