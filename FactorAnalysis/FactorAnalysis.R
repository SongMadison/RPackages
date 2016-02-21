# R-script about factor analysis

#############################
# Principal factor analysis #
#############################

# We use the package 'bootstrap' because it contains the data set
# op open-closed book exams discussed in MKB
# You need to install the package once, and can then load it via
# 'library(bootstrap)'

library(bootstrap)

# load data:
data(scor)
help(scor)

# compute correlation matrix:
cor <- cor(scor)
cor

# remove diagonal:
diag.1 <- diag(rep(1,5))
diag.1
cor.min <- cor - diag.1
cor.min

# compute max_{i!=j} |r_{ij}| for each column
h2 <- apply(abs(cor.min), 2, max)
h2

# compute eigenvalues and eigenvectors of reduced correlation matrix
cor.reduced <- cor.min + diag(h2)
cor.reduced
eig <- eigen(cor.reduced)
eig

# let's consider the model with k=1:
pf.1 <- eig$vectors[,1] * eig$values[1]^{1/2}
pf.1
# h^2:
pf.1^2

# let's consider the model with k=2
pf.2 <- eig$vectors[,1:2] %*% diag(eig$values[1:2]^{1/2})
pf.2
# h^2:
apply(pf.2^2, 1, sum)
# note that factor loadings of first component remain unchanged

# repeat the procedure for the 2-factor model
# update estimates for commonalities, and repeat 100 times:
for (i in 1:100){
  h2 <- apply(pf.2^2, 1, sum)
  cor.reduced <- cor.min + diag(h2)
  eig <- eigen(cor.reduced)
  pf.2 <- eig$vectors[,1:2] %*% diag(eig$values[1:2]^{1/2})
}
pf.2
h2

# check that constraint 2 is satisfied:
round(t(pf.2) %*% pf.2, 5)

# check how close our estimated Lambda*Lambda' + Psi is to
# the correlation matrix R
fit <- pf.2 %*% t(pf.2) + diag(1-h2)
fit
cor(scor)
round(cor(scor)-fit, 5)

# compare results to PCA
prcomp(scor, scale=TRUE, center=TRUE)
# interpretation is qualitatively the same


##############################
# Maximum likelihood method: #
##############################

# compare model with 1 factor and 2 factors:
res1 <- factanal(scor, factors=1, rotation="none")
res2 <- factanal(scor, factors=2, rotation="none")    
res1
res2
# note that first principal factor is not the same for the two models
# discuss output: uniquenesses (variances of specific factors u_i)
#                 loadings (factor loadings)
#                 hypothesis test

# compare fitted correlation matrix to the true correlation matrix:
fit1 <- res1$loadings %*% t(res1$loadings) + diag(res1$uniquenesses)
round(cor(scor) - fit1, 5)
fit2 <- res2$loadings %*% t(res2$loadings) + diag(res2$uniquenesses)
round(cor(scor) - fit2, 5)

# compare different rotations:
res2 <- factanal(scor, factors=2, rotation="none")    # no rotation
res3 <- factanal(scor, factors=2, rotation="varimax") # default
res4 <- factanal(scor, factors=2)				# same as res3
res5 <- factanal(scor, factors=2, rotation="promax")  # oblique rotation

# note the uniquenesses (variances of specific factors u_i) are the same:
res2$uniquenesses
res3$uniquenesses
res4$uniquenesses
res5$uniquenesses

# note that they also match up quite well with 
# 1-h2 that we computed from Principal factor analysis:
1-h2

# factor loadings change considerably:
res2$loadings
res3$loadings
res4$loadings
res5$loadings
# see transparency for interpretation
# note that res3$loadings is also given by 'varimax(res2$loadings)'
# similarly, res5$loadings is given by 'promax(res2$loadings)'

# note that (factor loadings) %*% (factor loadings)' is the same 
# for "no rotation" and "varimax rotation"
res2$loadings %*% t(res2$loadings)
res3$loadings %*% t(res3$loadings)
res5$loadings %*% t(res5$loadings)
# for the oblique rotation "promax", the factors are no longer 
# uncorrelated, and hence the decomposition 
# Sigma = Lambda * Lambda' + Psi does no longer hold


#########################
# Compute factor scores #
#########################

# thompson's scores
res2.t <- factanal(scor, factors=2, rotation="none", scores="regression")   
# bartlett's scores
res2.b <- factanal(scor, factors=2, rotation="none", scores="Bartlett")   
cbind(res2.t$scores, res2.b$scores)

# plot
plot(res2.b$scores,pch="")
text(res2.b$scores, labels=c(1:88))
points(res2.t$scores,pch="")
text(res2.t$scores, labels=c(1:88), col="red")

# interpret plot
apply(scor, 1, mean) # first factor is some kind of overall performance
scor[66,]
scor[81,] # second factor is high when student is better at 
          # closed book exams than at open book exams


###################
# Another example #
###################

# Download zip-file "chap4druguse" 
#   from http://biostatistics.iop.kcl.ac.uk/publications/everitt/
# Unzip file into your working directory

# These data were collected by Huba et al (1981).
# The data describe drug usage rates for 1634 students in 
#   7th-9th grade in 11 schools in the area of Los Angelos
# Responses were on a five point scale:
#  1 = never tried
#  2 = only once
#  3 = a few times
#  4 = many times
#  5 = regularly

# read correlation matrix:
druguse.cor<-source("chap4druguse.dat")$value
druguse.cor

# plot correlation matrix:
image(c(1:13), c(1:13), druguse.cor, col=gray((32:0)/32), 
      xlab="", ylab="", main="Image plot of correlation matrix")
row.names(druguse.cor)

# fit factor models with 4, 5, 6 factors:
res4 <- factanal(covmat = druguse.cor, factors=4, n.obs=1634)
res4
res5 <- factanal(covmat = druguse.cor, factors=5, n.obs=1634)
res5
res6 <- factanal(covmat = druguse.cor, factors=6, n.obs=1634)
res6

# compare fitted correlation matrix to true correlation matrix:
fitted <- res4$loadings %*% t(res4$loadings) + diag(res4$uniquenesses)
round(druguse.cor-fitted, 2)

# compare results:
res4$loadings
res5$loadings
# note that the first 4 factors do not remain the same
# there are considerable changes in factors 3 and 4

# how could we interpret the factors in the 4-factor model?

# Answer:
# first factor: high on beer, wine, liquor => soft drug use
# second factor: high on cocaine, tranquilizers, heroine => hard drug use
# third factor: high on marijuana
# fourth factor: high on amphetamine

# Note that interpreting the unrotated factor loadings seems harder:
res7 <- factanal(covmat = druguse.cor, factors=4, n.obs=1634, rotation="none")
res7$loadings



