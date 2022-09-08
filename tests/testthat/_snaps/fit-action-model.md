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

# prompt on spec without a loaded implementation (#174)

    Code
      add_model(workflow, mod)
    Message
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications using the `rpart` engine.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      == Workflow ====================================================================
      Preprocessor: None
      Model: bag_tree()
      
      -- Model -----------------------------------------------------------------------
    Message
      ! parsnip could not locate an implementation for `bag_tree` regression model specifications using the `rpart` engine.
      i The parsnip extension package baguette implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Bagged Decision Tree Model Specification (regression)
      
      Main Arguments:
        cost_complexity = 0
        min_n = 2
      
      Computational engine: rpart 
      

# cannot add two models

    Code
      add_model(workflow, mod)
    Condition
      Error in `add_model()`:
      ! A `model` action has already been added to this workflow.

