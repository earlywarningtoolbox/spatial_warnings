# 
# This file will download and prepare the Serengeti data. See also: 
#   https://github.com/tee-lab/spacetime-csd
# 

# Download and prepare data
serengeti.dat <- read.csv("https://raw.githubusercontent.com/tee-lab/spacetime-csd/master/tran5_veg_30m.csv")
serengeti.dat <- as.matrix(serengeti.dat)
dimnames(serengeti.dat) <- NULL

# We set -9999 to one
for (j in seq.int(ncol(serengeti.dat))) { 
  for (i in seq.int(nrow(serengeti.dat))) { 
    if ( abs(serengeti.dat[i,j] - -9999) < .01 ) { 
      serengeti.dat[i,j] <- 1
    }
  }
}

# lattice::levelplot(serengeti.dat)

# Download and prepare rain data
raindat <- read.csv('https://raw.githubusercontent.com/tee-lab/spacetime-csd/master/tran5_rainfall2500_Oct30_2014.csv')
raindat <- as.matrix(raindat)
dimnames(raindat) <- NULL

rain_interp <- approx(x = seq(1, nrow(serengeti.dat), length.out = nrow(raindat)), 
                      y = raindat[ ,2], # this is the mean (?)
                      xout = seq(1, nrow(serengeti.dat)))$y


# Now splice up the data
serengeti <- list()
serengeti.rain <- numeric(0)
for (n in seq(0, floor(4081/250)-1, by = 0.5)) { 
    rainlvl <- mean(rain_interp[(80+n*250):(80+(n+1)*250-1)])
    if (rainlvl <= 730 && rainlvl > 600) { 
      serengeti.rain <- append(serengeti.rain, rainlvl)
      serengeti <- c(serengeti, list(serengeti.dat[(80+n*250):(80+(n+1)*250-1), 1:250] > 0))
    }
}

use_data(serengeti)
use_data(serengeti.rain)

# library(spatialwarnings)

plop <- indictest( generic_spews(serengeti, subsize = 5, 
                                 # Sabiha's code uses coarse graining
                                 moranI_coarse_grain = TRUE) )

plot(plop, along = serengeti.rain)

