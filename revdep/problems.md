# workflowsets

<details>

* Version: 0.1.0
* GitHub: https://github.com/tidymodels/workflowsets
* Source code: https://github.com/cran/workflowsets
* Date/Publication: 2021-07-22 14:00:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "workflowsets")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─testthat:::quasi_capture(...)
        4. │     ├─testthat .capture(...)
        5. │     │ └─base::withCallingHandlers(...)
        6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. ├─hardhat::extract_fit_engine(car_set_1, id = "reg_lm")
        8. └─workflowsets:::extract_fit_engine.workflow_set(car_set_1, id = "reg_lm")
        9.   ├─hardhat::extract_fit_engine(y$info[[1]]$workflow[[1]])
       10.   └─workflows:::extract_fit_engine.workflow(y$info[[1]]$workflow[[1]])
       11.     ├─hardhat::extract_fit_parsnip(x)
       12.     └─workflows:::extract_fit_parsnip.workflow(x)
       13.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 4 | SKIP 6 | PASS 316 ]
      Error: Test failures
      Execution halted
    ```

