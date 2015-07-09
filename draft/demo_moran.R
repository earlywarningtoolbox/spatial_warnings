#'#'##############################
#'  Demo moran autocorrelation  #'
#'###############################'

# If you want to test the up-to-date package version from github
library(devtools)
install_github("fdschneider/spatial_warnings")
library(spatialwarnings)

# If you want to test the local modifications you did in your local repository
library(devtools)
load_all()

load("../data/B.rda")# B is a binary matrix
load("../data/L.rda")# L is a list of binary matrix

# Default tests:
Moran1(B)
Moran1(L)


# Parameters test:

Moran1(B, subsize = 2, detrending = TRUE, discrete = FALSE)$MoranCorr == Moran1(B)$MoranCorr


