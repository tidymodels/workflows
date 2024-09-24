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

# warns when supplied arguments that will be ignored

    Code
      add_tailor(workflow(), tailor::adjust_probability_threshold(tailor::tailor(),
      0.2), prop = 0.2)
    Condition
      Error in `add_tailor()`:
      ! `prop` will be ignored as `tailor` does not require training.

---

    Code
      add_tailor(workflow(), tailor::adjust_probability_threshold(tailor::tailor(),
      0.2), method = "mc_split")
    Condition
      Error in `add_tailor()`:
      ! `method` will be ignored as `tailor` does not require training.

