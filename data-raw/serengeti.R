# 
# This file reads the raw data for the serengeti dataset, and convert it 
# to a form that is compatible with ditributing it within and R package. 
# 
# Note that ideally it would convert raw data into R-ready data, but we 
# actually start from intermediate files. 
# 
# See also: https://github.com/tee-lab/spacetime-csd/
# 
# 

datdir <- './data-raw/serengeti/'
files  <- dir(paste0(datdir, "matrices"), full = TRUE)

# Read matrices
matrices <- lapply(files, read.csv, sep = ",", header = FALSE)
names(matrices) <- files
matrices <- lapply(matrices, function(x) as.matrix(x)>0)

# Number of points to keep
minn <- 3 # 2
maxn <- length(matrices) - 4 # 11

matrices <- matrices[minn:maxn]

ics <- generic_spews(matrices, 
                     subsize = 5, 
                     abs_skewness = FALSE, 
                     moranI_coarse_grain = TRUE)

# Read rain data 
rain <- read.csv(paste0(datdir, 'serengeti_rain.dat'), header = FALSE)
rain <- rain[minn:maxn, "V1"]

# Note: we compensate a shift in water values here as transect 5 shifts at 
# 730 mm/y and not 590. Ideally we would start from the raw data but for 
# illustration purposes this is good enough and the slicing of matrices is hard 
# to reproduce exactly (see article). 
rain <- rain + (730 - 590)

plot(ics, rain) + 
  geom_vline(xintercept = 730)

# Compute indicators for testing
# spcs <- spectral_spews(matrices, 
#                        sdr_low_range = c(0, .2), 
#                        sdr_high_range = c(.8, 1))
# spcs.test <- indictest(spcs)
# 
# plot(spcs, along = rain)
# plot(spcs.test, along = rain) + 
#   geom_vline(xintercept = 730, color = "red", linetype = "dashed") 


# Save datasets 
serengeti <- matrices 
names(serengeti) <- NULL 
serengeti.rain <- rain 

use_data(serengeti, overwrite = TRUE)
use_data(serengeti.rain, overwrite = TRUE)

