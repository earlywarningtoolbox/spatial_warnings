################################
#  Demo moran autocorrelation  #
################################

# If you want to test the up-to-date package version from github
library(devtools)
install_github("spatial-ews/spatialwarnings")
library(spatialwarnings)

# If you want to test the local modifications you did in your local repository
library(devtools)
load_all()

# Load an example binary matrix B and a list of binary matrices L
data(B)
data(L)

# Default tests:
indicator_moran(B)
indicator_moran(L)


# Parameters test:
indicator_moran(B, subsize = 2, discrete = TRUE)$MoranCorr 
 

