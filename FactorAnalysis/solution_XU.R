## readin the lower triangle matrix and transformed in a corr matrix
R <- scan("corr.txt")
M <- matrix(0,11,11)
t=1
for( i in 1:11){
    for (j in 1:i){
        M [i,j] = R[t]
        M [j,i] = R[t]
        t <- t+1
    }
}

#### PCA analysis (using package)
pr <- princomp(covmat = M, cor = T)
pr $loadings

## interpretation:  to explain the meaning of those combinations
# the first component: all opsitive of simiar weight, like the average all the 11 variables -- overall effect
# the second component:  the effect of first 6 variables excluding third Arithmetic - the effects of last five ariables
## and so on

##PCA and eigen-decomposition(Not recommended)
plot(  cumsum(eigenM$values)/sum(eigenM$values), ylab = 'variance Explained')
abline(h=0.8,col="red") ## usually wants to explain 80% of the total variation, which needs 6 components
eigenM$values[1:6]
eigenM$vectors[,1:6]

#-------------------------
 
#### Question b) factor analysis with maximum likelihood mothod.
# question to determine number of factors
factanal(covmat = M, factors = 1) 
factanal(covmat = M, factors = 2) 
factanal(covmat = M, factors = 3) 
factanal(covmat = M, factors = 4) 
factanal(covmat = M, factors = 5)   # pvalue <0.05
## method is always= "mle"
# rotation is defacult as "varimax"
fact.5 <- factanal(covmat = M, factors = 5) 
##question c)
# The specific variances for each variable are labeled "uniquenesses".

fact.5$uniquenesses
# Since we are using a correlation matrix, the communalities are 1 minus the specific variances:
communities <- 1-fact.5$uniquenesses
communities[1] # information
communities[6] # vacubulary

## question d)
fitted.cov <- fact.5$loadings %*%t (fact.5$loadings) +diag(fact.5$uniquenesses)
round(diff.cov <- fitted.cov - M, 5)
diff.cov[1,2]  # this the residual asked

