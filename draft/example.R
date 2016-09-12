# 
# This file provides an example of use for the spatialwarning package. It will 
# change and be adapted as the spatialwarnings package changes.
# 

library(lattice)

# Prerequisite: an installed version of spatialwarnings as user
# 
# This requires a working copy of devtools if installing from github.
library(devtools)

# Set up IISC proxy beforehand if needed
# Sys.setenv(https_proxy = "proxy.iisc.ernet.in:3128")
# Sys.setenv(http_proxy  = "proxy.iisc.ernet.in:3128")
install_github('fdschneider/spatial_warnings')
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




# Generic indicators
# -----------------

# Let's compute the generic EWS on the whole forest dataset and see whether 
# it changes along the gradient (not only on the last mtx)
# See ?generic_spews for options such as coarse-graining length
generic_ic <- generic_spews(matrices) 
plot(generic_ic, along = parameters[ ,'delta']) # increases !

# Now we can assess significance by comparing it to a reshuffled matrix of same
# density, using the function indictest.
generic_ic_test <- indictest(generic_ic) # can be slow
generic_ic_test # this will print a pretty table
plot(generic_ic_test, along = parameters[ ,'delta'])


# Second example with a real dataset. This is a greyscale aerial picture 
# -----------------

# Let's plot first the data
data(arid)
levelplot(arid[[1]], 
          col.regions = colorRampPalette(c('#FFF299', '#37A42C'))(20))

# Compute the EWS on continuous, raw data. Note that this coarse-grains 
#   the date using a 4x4 window as default -> we set the subsize to 1. 
desert_spews_cont <- generic_spews(arid, subsize = 1)
plot(desert_spews_cont)

# Assess significance and plot
indictest(desert_spews_cont)
plot(desert_spews_cont)


# Note that we can compute each indicator alone if we only need a raw value
indicator_variance(arid)
indicator_skewness(arid)
indicator_moran(arid)




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
desert_spectral_ic <- spectral_spews(arid)
desert_spectral_test <- indictest(desert_spectral_ic)

# Plot spectrum
plot_spectrum(desert_spectral_test)
# Plot SDR trend
plot(desert_spectral_test)  


# Similarly, we can compute each "indicator" alone
indicator_sdr(arid)
rspectrum(arid[[1]]) # not an indicator by itself -> does not accept lists of matrices





# Compute patch-based EWS (this is still a WIP so errors might occur)
# -----------------

patch_ic <- patchdistr_spews(arid)

# The above code produces an error as we provided continous data -> we need 
#   to threshold it beforehand
arid_bw <- lapply(arid, function(x) x > mean(x))

patch_ic <- patchdistr_spews(arid_bw)

# For now only the plot_distr method is implemented (displays psds with 
#   fits overlayed)
plot_distr(patch_ic)

plot_distr(patch_ic, best_only = TRUE) # display only best fit (using AIC)






