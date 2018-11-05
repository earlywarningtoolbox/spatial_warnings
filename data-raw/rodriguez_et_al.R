# 
# 

library(ggplot2)
library(jpeg)
library(EBImage) # Install from Bioconductor

picdir <- "./data-raw/rodriguez_etal"
pics <- dir(picdir, full = TRUE, pattern = "*.jpg")

# Read the pictures 
imgs <- lapply(pics, readJPEG)
names(imgs) <- gsub(".jpg", "", basename(pics))

# Convert to B/W
# The images have three bands (R/G/B). We transform them into a black
# and white image using principal component analysis (PCA)
summarise_pca <- function(arr) {
  values <- matrix(as.vector(arr), ncol = 3)
  pca <- prcomp(values)
  values <- pca[["x"]][ ,1]
  matrix(values, ncol = ncol(arr), nrow = nrow(arr))
}
imgs_bw <- lapply(imgs, summarise_pca)
names(imgs_bw) <- names(imgs)

# Compute indicators 
imgs_classif <- lapply(imgs_bw, function(mat) { 
  clust <- mat > mean(mat)
#   clust <- gblur(clust, sigma = 1) > .4 
})
lattice::levelplot(imgs_classif[[2]])

psds <- patchdistr_spews(imgs_classif)

plot_distr(psds, along = names(psds))

plot(psds, along = names(psds))

# 
arizona <- imgs_classif
use_data(arizona, overwrite = TRUE, compress = TRUE)




