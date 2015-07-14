library(caspr)

# 50x50
## musselbed
l <- init_landscape(musselbed$states, c(0.5,0.2,0.3))
r <- ca(l, musselbed, t_max = 600, t_eval = 200, saveeach = 50)

L50m <- lapply(r$landscapes, function(x) as.matrix(x) == "+")
B50m <- as.matrix(r$landscapes[[5]]) == "+"

save(L50m, file = "data/L50m.rda")
save(B50m, file = "data/B50m.rda")

# 200 x 200
## forestgap
l <- init_landscape(forestgap$states, c(0.8,0.2), width = 200)
r <- ca(l, forestgap, t_max = 600, t_eval = 200, saveeach = 50)
plot(r$landscapes[[3]])

L200f <- lapply(r$landscapes, function(x) as.matrix(x) == "+")
B200f <- as.matrix(r$landscapes[[5]]) == "+"

save(L200f, file = "data/L200f.rda")
save(B200f, file = "data/B200f.rda")

## predprey
p <- predprey$parms
p$betaf <- 0.01
p$delta <- 0.2
l <- init_landscape(predprey$states, c(0.5,0.2,0.3), width = 200) 
r <- ca(l, predprey, parms = p, t_max = 600, t_eval = 200, saveeach = 50)

L200p <- lapply(r$landscapes, function(x) as.matrix(x) == "f")
B200p <- as.matrix(r$landscapes[[5]]) == "f"

save(L200p, file = "data/L200p.rda")
save(B200p, file = "data/B200p.rda")
