## Test environments
* local R installation, R 3.6.0
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Submission 0.1.3

This is a small release to fix a test that broke with parsnip 0.1.3.

## Submission 0.1.2

This small release uses model-specific information from 'parsnip' to determine
how to encode factor variables in `add_formula()`.

## Submission 0.1.1

This patch release just bumps the minimum required version of hardhat.
