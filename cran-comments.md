
This is an update to the package spatialwarnings. It introduces small bug
fixes, improvement in documentation and some additional functionality.
Some functions have been renamed as part of a peer-review process
prior to publication of an article describing the package.

Note that this is a reupload that reduces the required testing time on
CRAN machines.

## Changes in this release

New features:
  * Support for custom indicators (see ?create_indicator)

Minor changes:
  * Documentation improvements
  * Safer handling of small matrices in SDR computation

Name changes
  * `*_spews` functions are now deprecated in favor of `*_sews` functions

Bug fixes
  * Counting patches in matrices with 1 line or 1 column
    does not crash R anymore
  * The R package should now build on Solaris

## Test environments

 - Travis-ci (Ubuntu 14.04.5, R 3.5.0 and devel (2018-05-31 r74811) ):
  https://travis-ci.org/spatial-ews/spatialwarnings

 - local linux computer (Arch Linux as of 2018/05/24, R 3.5.0)

 - Windows building service (R-devel at win-builder.r-project.org)

## R CMD check results

There were no NOTEs, ERRORs or WARNINGs on any of the above platforms.


## Package Description

spatialwarnings is a package that assists ecologists in carrying out
computations of early-warning signals (EWS) of ecosystem degradation.

These EWS are based on the fact that some ecosystems are expected to
show specific spatial patterns before undergoing non-linear transitions (e.g.
a wide shift in their state despite a small change in external forcings).
For example, such ecosystems are expected to show an increase in spatial
autocorrelation, variance and skewness, or, for patchy ecosystems,
specific changes in the patch size distribution.

This packages assists users with computing these metrics efficiently on matrix
objects in R, test their significance based on randomizing spatial structure,
and plot their trends based on ggplot2. A convenient, three-step
workflow is provided based on summary/plot/etc. generic functions.

Homepage and usage example:

  https://github.com/spatial-ews/spatialwarnings

Reference:

  * Kéfi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., Livina,
    V.N., et al. (2014). Early Warning Signals of Ecological Transitions:
    Methods for Spatial Patterns. PLoS ONE, 9, e92097.
    http://dx.plos.org/10.1371/journal.pone.0092097

  * A dedicated article presenting in more detail the package is under
    preparation (submitted to Methods in Ecology and Evolution)

