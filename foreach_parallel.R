

library(doParallel)
# Find out how many cores are available (if you don't already know)
detectCores()
## [1] 4
# Create cluster with desired number of cores
cl <- makeCluster(3)
# Register cluster
registerDoParallel(cl)
# Find out how many cores are being used
getDoParWorkers()

tree.df <- data.frame(species = rep(c(1:100), each = 100), 
                      girth = runif(10000,7, 40))
tree.df$volume <- tree.df$species/10 + 5 * tree.df$girth + rnorm(10000, 0, 3)
# Extract species IDs to iterate over
species <- unique(tree.df$species)
# Run foreach loop and store results in fits object

fits <- foreach(i = species, .combine = rbind) %dopar% {
  sp <- subset(tree.df, subset = species == i)
  fit <- lm(volume ~ girth, data = sp)
  return(c(i, fit$coefficients))
}

stopCluster(cl)
