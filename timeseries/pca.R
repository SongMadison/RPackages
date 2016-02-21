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
eigenM <- eigen(M)
# the top  few eigen values, eigen vectors
# corresponding to the proportion of variance explained and top principal component


##determine the number of components to keep
plot(  cumsum(eigenM$values)/sum(eigenM$values), ylab = 'variance Explained')
abline(h=0.8,col="red") ## usually wants to explain 80% of the total variation, which needs 6 components


eigenM$values[1:6]

eigenM$vectors[,1:6]

## interpretation:  to explain the meaning of those combinations
# the first component, all opsitive of simiar weight, like the average all the 11 variables
# the second component the effect of first 6 variables excluding third Arithmetic - the effects of last five ariables
## and so on 

