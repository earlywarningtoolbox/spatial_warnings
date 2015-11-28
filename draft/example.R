# 
# This file provides an example of use for the spatialwarning and the caspr
# packages.
# 


# Prerequisite: an installed version of spatialwarnings as user
# 
# This requires a working copy of devtools if installing from github.
library(devtools)
install_github('fdschneider/spatial_warnings')

# We now have the package installed: we can load it
library(spatialwarnings) # mind the absence of underscore ! 

# An example dataset is provided with the package: forestdat
# It is a list that contains simulations produced by the forestgap model. 
# The list has two elements: matrices and parameters which are respectively
# the 2D matrices of the last time step of each simulation and the parameters
# used for each simulation. 
# 
# Let's make the matrices and parameters variables available to us
data(forestdat)
attach(forestdat) # take care to only run once

# For example, we can have a look at the last landscape: 
image(matrices[[10]]) 
# ... and the parameters used for its simulation
parameters[10, ]

# Let's see what its psd look like: 
psdfit <- indicator_fitpsd(matrices[[10]])
plot(psdfit) # best fit is a power law 
summary(psdfit) # with a slope of -0.8 +/- 0.1

# Let's compute the generic EWS on the whole dataset and see whether it changes 
# along the gradient (not only on the last mtx)
generic_ic <- generic_spews(matrices)
plot(generic_ic, along = parameters[ ,'delta']) # increases !

generic_ic_summary <- summary(generic_ic) # can be slow
generic_ic_summary
plot(generic_ic_summary, along = parameters[ ,'delta'])

