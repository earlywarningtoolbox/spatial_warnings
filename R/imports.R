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
