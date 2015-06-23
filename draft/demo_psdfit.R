library(caspr)


l <- init_landscape(c("+","0","-"), cover = c(0.7,0.1,0.2), 50, 50)
re <- ca(l, musselbed)
x_old <- re$landscapes[[8]]
plot(x_old)

x_patches <- patches(x_old)

x_patches_list <- lapply(re$landscapes, patches)


psd(x_patches)
psd(x_patches_list)

out <- fitpsd(x_patches)  # fit for patch vector of single landscape object

summary(out)
plot(out)


# ToDo: 
# fitting for a list of vector (multiple snapshots)

fitpsd(x_patches_list) # fitting for list of patch vectors (pooled)
