## Current state

Workflows is stable.
It currently supports all implemented model types in tidymodels, including those in censored or modeltime.
Often if it looks like a model type cannot be supported with the typical workflows model, the user can supply a "model formula" that gets passed directly through to parsnip as a workaround (i.e. `add_model(formula = )`).

The general model of workflows is that it is split into 3 stages: `pre`, `fit`, and `post`.

-   `pre` controls the preprocessing, and is further divided into "actions".

    -   The formula, recipes, and variables actions correspond to the 3 preprocessor types in hardhat.
        You can only use one of these per workflow.

    -   The case weights action controls how case weights are extracted from the data and passed on to the parsnip model.
        Internally we force this action to run before the preprocessor actions.

-   `fit` controls the model fit.
    There is only 1 "model" action here, and I do not anticipate any more actions in this stage of the workflow.

-   `post` controls the postprocessing.
    There is only 1 "tailor" action here, though may be others in the future.

Once a workflow is specified, `fit()` is called to fit all of the "actions".
It loops through the actions in the workflow, and calls `fit()` on each of the actions as well (there are S3 methods for `fit()` for each action).
This is similar to recipes, where each step has a `prep()` method.

Keep in mind that people do save their fitted workflows and reload them for prediction, which has considerations for backwards compatibility.
Any time you add a new feature, or change an existing one, you will need to keep in mind whether or not old workflows saved to disk will continue to run with the new version of workflows.
Historically this has been more of a problem for hardhat, so if the backwards compatibility issues seem like a hardhat problem, then I would suggest adding backwards compatibility tests to hardhat directly instead.

## Known issues

-   I think that we still don't support `parsnip::multi_predict()`. At the time I remember not seeing a clear way to integrate this, but maybe the landscape has changed since then <https://github.com/tidymodels/workflows/issues/4>.

## Future directions

The only known feature we want to add to workflows is support for postprocessing.
As mentioned above, this requires some tooling in the probably package, along with tight integration to tune (which will likely need a 3rd inner loop to control tuning over the postprocessing options).
