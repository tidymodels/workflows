# errors informatively with bad input

    Code
      weight_propensity(wf, silly_wt_fn, data = two_class_dat)
    Condition
      Error in `weight_propensity()`:
      ! `weight_propensity()` is not well-defined for an unfitted workflow.

---

    Code
      weight_propensity(wf_fit, data = two_class_dat)
    Condition
      Error in `weight_propensity()`:
      ! `wt_fn` must be a function.

---

    Code
      weight_propensity(wf_fit, "boop", data = two_class_dat)
    Condition
      Error in `weight_propensity()`:
      ! `wt_fn` must be a function.

---

    Code
      weight_propensity(wf_fit, function(...) {
        -1L
      }, data = two_class_dat)
    Condition
      Error in `hardhat::importance_weights()`:
      ! `x` can't contain negative weights.

---

    Code
      weight_propensity(wf_fit, silly_wt_fn)
    Condition
      Error in `weight_propensity()`:
      ! `data` must be the data supplied as the data argument to `fit()`.

---

    Code
      weight_propensity(wf_fit, silly_wt_fn, data = "boop")
    Condition
      Error in `weight_propensity()`:
      ! `data` must be the data supplied as the data argument to `fit()`.

