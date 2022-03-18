## revdepcheck results

We checked 27 reverse dependencies (25 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
  
## Submission 0.2.6

This patch release is intended to fix a bug in the new tests added in 0.2.5. It should fix the failing CRAN check machines.
  
## Submission 0.2.5

This is a small release that adds methods for `extract_parameter_set_dials()` and `extract_parameter_dials()` for workflows objects.
 
## Submission 0.2.4

This is a patch release mainly intended to fix a lazy S3 registration issue for `generics::required_pkgs()`.
 
## Submission 0.2.3

This is a minor release adding support for the `extract_*()` generics from hardhat, along with adding new capabilities to `workflow()`.

We are aware that finetune will fail with this submission. We maintain this package and will fix it quickly after workflows is on CRAN. See https://github.com/tidymodels/finetune/issues/22.
 
## Submission 0.2.2

This is a minor release adding new features to `add_variables()` and a few
new helpers.

## Submission 0.2.1

This is a small release to export an internal function for use in the tune
package.

## Submission 0.2.0

This is a minor release that adds a new preprocessing method through `add_variables()`.

## Submission 0.1.3

This is a small release to fix a test that broke with parsnip 0.1.3.

## Submission 0.1.2

This small release uses model-specific information from 'parsnip' to determine
how to encode factor variables in `add_formula()`.

## Submission 0.1.1

This patch release just bumps the minimum required version of hardhat.
