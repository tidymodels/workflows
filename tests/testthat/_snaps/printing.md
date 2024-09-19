# can print empty workflow

    Code
      workflow()
    Output
      == Workflow ====================================================================
      Preprocessor: None
      Model: None

# can print workflow with recipe

    Code
      add_recipe(workflow(), rec)
    Output
      == Workflow ====================================================================
      Preprocessor: Recipe
      Model: None
      
      -- Preprocessor ----------------------------------------------------------------
      0 Recipe Steps

# can print workflow with formula

    Code
      add_formula(workflow(), y ~ x)
    Output
      == Workflow ====================================================================
      Preprocessor: Formula
      Model: None
      
      -- Preprocessor ----------------------------------------------------------------
      y ~ x

# can print workflow with variables

    Code
      add_variables(workflow(), y, c(x1, x2))
    Output
      == Workflow ====================================================================
      Preprocessor: Variables
      Model: None
      
      -- Preprocessor ----------------------------------------------------------------
      Outcomes: y
      Predictors: c(x1, x2)

# can print workflow with model

    Code
      add_model(workflow(), model)
    Output
      == Workflow ====================================================================
      Preprocessor: None
      Model: linear_reg()
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

# can print workflow with model with engine specific args

    Code
      add_model(workflow(), model)
    Output
      == Workflow ====================================================================
      Preprocessor: None
      Model: linear_reg()
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 0.01
      
      Engine-Specific Arguments:
        dfmax = 5
      
      Computational engine: glmnet 
      

# can print workflow with fit model

    Code
      fit(workflow, mtcars)
    Output
      == Workflow [trained] ==========================================================
      Preprocessor: Formula
      Model: linear_reg()
      
      -- Preprocessor ----------------------------------------------------------------
      mpg ~ cyl
      
      -- Model -----------------------------------------------------------------------
      
      Call:
      stats::lm(formula = ..y ~ ., data = data)
      
      Coefficients:
      (Intercept)          cyl  
           37.885       -2.876  
      

# can print workflow with >10 recipe steps

    Code
      add_recipe(workflow(), rec)
    Output
      == Workflow ====================================================================
      Preprocessor: Recipe
      Model: None
      
      -- Preprocessor ----------------------------------------------------------------
      11 Recipe Steps
      
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * ...
      * and 1 more step.

---

    Code
      add_recipe(workflow(), rec)
    Output
      == Workflow ====================================================================
      Preprocessor: Recipe
      Model: None
      
      -- Preprocessor ----------------------------------------------------------------
      12 Recipe Steps
      
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * step_log()
      * ...
      * and 2 more steps.

# can print workflow with just case weights

    Code
      workflow
    Output
      == Workflow ====================================================================
      Preprocessor: None
      Model: None
      
      -- Case Weights ----------------------------------------------------------------
      disp

# can print workflow with case weights, preprocessor, and model

    Code
      workflow
    Output
      == Workflow ====================================================================
      Preprocessor: Formula
      Model: linear_reg()
      
      -- Preprocessor ----------------------------------------------------------------
      mpg ~ .
      
      -- Case Weights ----------------------------------------------------------------
      disp
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

# can print workflow with postprocessor

    Code
      workflow
    Output
      == Workflow ====================================================================
      Preprocessor: None
      Model: None
      Postprocessor: tailor
      
      -- Postprocessor ---------------------------------------------------------------
    Message
      
      -- tailor ----------------------------------------------------------------------
      A postprocessor with 0 adjustments.
    Output
      NA
      NA
      NA

