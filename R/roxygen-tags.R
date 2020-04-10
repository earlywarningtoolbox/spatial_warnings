#
# These tags are required to use functions in other packages
# 
#' @import ggplot2
#' @import plyr
#' @import stats
#' @import utils
# 
# These tags are required by Rcpp
# 
#' @useDynLib spatialwarnings
#' @importFrom Rcpp sourceCpp
# 
# 
# The NULL is mandatory so that devtools actually reads this file.
NULL

#
# These tags are here to explicitely show what is imported
#
#' @import stats
#' @import utils

# We need to pretend to import moments and poweRlaw as they
# are used in the package build process. They are not
# used in the final package tree though.
# We import here two harmless functions so that CRAN
# checks do note produce NOTEs (on certain archs only).
# See: https://groups.google.com/forum/#!topic/rdevtools/qT6cJt6DLJ0
#' @importFrom moments moment
#' @importFrom poweRlaw get_n

# Spatialwarnings will load the `future` package at boot, but does not use 
# directly its functions. Here we import plan() explicitely to make sure 
# check() does not complain that we depend on a package we don't use. 
#
#'@importFrom future plan 
