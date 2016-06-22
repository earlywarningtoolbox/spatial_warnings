# 
# This file provides an example of use for the spatialwarning package. It will 
# change and be adapted as the interaction with the spatialwarnings package
# changes.
# 

library(lattice)

# Prerequisite: an installed version of spatialwarnings as user
# 
# This requires a working copy of devtools if installing from github.
library(devtools)
# install_github('fdschneider/spatial_warnings')

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
levelplot(matrices[[10]]) 
# ... and the parameters used for its simulation
parameters[10, ]

# Let's compute the generic EWS on the whole dataset and see whether it changes 
# along the gradient (not only on the last mtx)
generic_ic <- generic_spews(matrices)
plot(generic_ic, along = parameters[ ,'delta']) # increases !

# Now we can assess significance by comparing it to a reshuffled matrix of same
# density, using the function indictest.
generic_ic_test <- indictest(generic_ic) # can be slow
generic_ic_test # this will print a pretty table
plot(generic_ic_test, along = parameters[ ,'delta'])



# Second example with a real dataset
# -----------------

# Let's compute stuff on Sonia's images
data(desertification)
levelplot(desertification, 
          col.regions = colorRampPalette(c('#FFF299', '#37A42C'))(20))

# Classify into 0/1
desertification <- as.binary_matrix( desertification > median(desertification) ) 
levelplot(desertification, 
          col.regions = colorRampPalette(c('#FFF299', '#37A42C'))(20))

# Compute generic generic_spews
desert_spews <- generic_spews(desertification)

# Assess significance
indictest(desert_spews)

# Does not work because there is only one value !
plot(desert_spews)




# Compute spectral spatial EWS
# -----------------

# On model data
spectral_ic <- spectral_spews(matrices)
# (Mind the warning !)

# Plot the trend
plot(spectral_ic) # error 

# Assess significance
spectral_test <- indictest(spectral_ic, .progress = 'time') # slow because computing the spectrum takes a while

# Plot SDR trend
plot(spectral_test, along = parameters[ ,'delta'])

# Or plot spectrum trend
plot_spectrum(spectral_test)

# Do the same workflow on the real dataset
desert_spectral_ic <- spectral_spews(desertification)
desert_spectral_test <- indictest(desert_spectral_ic)

# Plot spectrum
plot_spectrum(desert_spectral_test)

# This code produces an error as there is only one value 
# of SDR available.
plot(desert_spectral_test)  

