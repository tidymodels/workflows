# modeltime

<details>

* Version: 1.2.2
* GitHub: https://github.com/business-science/modeltime
* Source code: https://github.com/cran/modeltime
* Date/Publication: 2022-06-07 21:50:02 UTC
* Number of recursive dependencies: 243

Run `cloud_details(, "modeltime")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─... |> fit(data_set) at test-panel-data.R:33:0
       2. ├─generics::fit(., data_set)
       3. ├─workflows::add_recipe(., recipe_spec |> step_rm(date))
       4. │ └─workflows:::add_action(x, action, "recipe")
       5. │   └─workflows:::validate_is_workflow(x, call = call)
       6. │     └─workflows:::is_workflow(x)
       7. └─workflows::add_model(., svm_rbf() |> set_engine("kernlab"))
       8.   └─workflows:::new_action_model(spec, formula)
       9.     └─rlang::abort(message, call = call)
      
      [ FAIL 2 | WARN 2 | SKIP 22 | PASS 477 ]
      Error: Test failures
      Execution halted
    ```

# modeltime.ensemble

<details>

* Version: 1.0.1
* GitHub: https://github.com/business-science/modeltime.ensemble
* Source code: https://github.com/cran/modeltime.ensemble
* Date/Publication: 2022-06-09 12:20:02 UTC
* Number of recursive dependencies: 214

Run `cloud_details(, "modeltime.ensemble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─... |> fit(data_set) at test-panel-data.R:28:0
       2. ├─generics::fit(., data_set)
       3. ├─workflows::add_recipe(., recipe_spec |> step_rm(date))
       4. │ └─workflows:::add_action(x, action, "recipe")
       5. │   └─workflows:::validate_is_workflow(x, call = call)
       6. │     └─workflows:::is_workflow(x)
       7. └─workflows::add_model(., boost_tree() |> set_engine("xgboost"))
       8.   └─workflows:::new_action_model(spec, formula)
       9.     └─rlang::abort(message, call = call)
      
      [ FAIL 2 | WARN 16 | SKIP 5 | PASS 52 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘parsnip’
      All declared Imports should be used.
    ```

# modeltime.resample

<details>

* Version: 0.2.1
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2022-06-07 14:30:03 UTC
* Number of recursive dependencies: 212

Run `cloud_details(, "modeltime.resample")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    ── Attaching packages ────────────────────────────────────── tidymodels 1.0.0 ──
    ✔ broom        1.0.1      ✔ recipes      1.0.1 
    ✔ dials        1.0.0      ✔ rsample      1.1.0 
    ✔ dplyr        1.0.10     ✔ tibble       3.1.8 
    ✔ ggplot2      3.3.6      ✔ tidyr        1.2.1 
    ✔ infer        1.0.3      ✔ tune         1.0.0 
    ✔ modeldata    1.0.1      ✔ workflows    1.1.0 
    ✔ parsnip      1.0.1      ✔ workflowsets 1.0.0 
    ...
    Error: processing vignette 'panel-data.Rmd' failed with diagnostics:
    `spec` must have a known mode.
    ℹ Set the mode of `spec` by using `parsnip::set_mode()` or by setting the mode directly in the parsnip specification function.
    --- failed re-building ‘panel-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘panel-data.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘crayon’ ‘dials’ ‘glue’ ‘parsnip’
      All declared Imports should be used.
    ```
