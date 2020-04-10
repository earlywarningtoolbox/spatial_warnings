# 
# Spatialwarnings will load the `future` package at boot, but does not use 
# directly its functions. Here we import plan() explicitely to make sure 
# check() does not complain that we depend on a package we don't use. 
#
#'@importFrom future plan 
NULL
