##########################################################################
######################                                                   #           
# R example code for factor analysis:                                    #
######################                                                   #
##########################################################################


## Example 1(a):

# The 13 observed variables in the WAIS study are as follows:
#X1 = Information
#X2 = Comprehension
#X3 = Arithmetic 
#X4 = Similarities 
#X5 = Digit.span 
#X6 = Vocabulary 
#X7 = Digit.symbol 
#X8 = Picture.completion 
#X9 = Block.design 
#X10 = Picture.arrangement 
#X11 = Object.assembly 
#X12 = Age 
#X13 = Education 

# The observed correlation matrix is:

wais.corr <- matrix( c(
    1,0.67,0.62,0.66,0.47,0.81,0.47,0.60,0.49,0.51,0.41,-0.07,0.66,
    .67,1,0.54,0.60,0.39,0.72,0.40,0.54,0.45,0.49,0.38,-0.08,0.52,
    .62,.54,1,0.51,0.51,0.58,0.41,0.46,0.48,0.43,0.37,-0.08,0.49,
    .66,.60,.51,1,0.41,0.68,0.49,0.56,0.50,0.50,0.41,-0.19,0.55,
    .47,.39,.51,.41,1,0.45,0.45,0.42,0.39,0.42,0.31,-0.19,0.43,
    .81,.72,.58,.68,.45,1,0.49,0.57,0.46,0.52,0.40,-0.02,0.62,
    .47,.40,.41,.49,.45,.49,1,0.50,0.50,0.52,0.46,-0.46,0.57,
    .60,.54,.46,.56,.42,.57,.50,1,0.61,0.59,0.51,-0.28,0.48,
    .49,.45,.48,.50,.39,.46,.50,.61,1,0.54,0.59,-0.32,0.44,
    .51,.49,.43,.50,.42,.52,.52,.59,.54,1,0.46,-0.37,0.49,
    .41,.38,.37,.41,.31,.40,.46,.51,.59,.46,1,-0.28,0.40,
    -.07,-.08,-.08,-.19,-.19,-.02,-.46,-.28,-.32,-.37,-.28,1,-0.29,
    .66,.52,.49,.55,.43,.62,.57,.48,.44,.49,.40,-.29,1
), nrow=13, ncol=13, byrow=T)

#Using the built-in R function factanal (which uses the maximum likelihood method):

# If we enter a covariance matrix (or correlation matrix) instead of a raw data matrix,
# we must tell R the number of observations in the data set with n.obs

factanal(covmat=wais.corr,factors=4, rotation="none", n.obs=933)
# Apparently 4 factors are not enough (P-value too small).

factanal(covmat=wais.corr,factors=5, rotation="none", n.obs=933)
# Apparently 5 factors are not enough (P-value still too small).

factanal(covmat=wais.corr,factors=6, rotation="none", n.obs=933)
# Apparently 6 factors are enough (P-value = 0.143).

# Let's save the final factor analysis solution as WAIS.fa.6 :

WAIS.fa.6 <- factanal(covmat=wais.corr,factors=6, rotation="none", n.obs=933)

# The specific variances for each variable are labeled "uniquenesses".
# We can request them by:

WAIS.fa.6$uniquenesses

# Since we are using a correlation matrix, the communalities are 1 minus the specific variances:

1-WAIS.fa.6$uniquenesses

# We see that X1 (= Information) shares very much of its variance with the other 12 variables via the factors.
# It is similar for X3 (= Arithmetic).

# The factors are not too interpretable.
# We will try a varimax rotation to aid interpretation:

WAIS.fa.6.v <- factanal(covmat=wais.corr,factors=6, rotation="varimax", n.obs=933)
WAIS.fa.6.v

# The first factor seems to be a combination of Information, Comprehension, and Vocabulary.
# Maybe a "Word ability" factor?

# The fourth factor seems to be a more mathematical factor.

## Example 1(b):

# The 11-variable WAIS correlation matrix:
# This 11-variable data set involves only the "test scores", 
# and drops the demographic variables.

wais.corr.11 <- wais.corr[1:11,1:11]

factanal(covmat=wais.corr.11,factors=4, rotation="none", n.obs=933)
# Apparently 4 factors are not enough (P-value too small).

factanal(covmat=wais.corr.11,factors=5, rotation="none", n.obs=933)
# Apparently 5 factors are enough (P-value = 0.256).

# With a varimax rotation:

WAIS.11.fa.5.v<-factanal(covmat=wais.corr.11,factors=5, rotation="varimax", n.obs=933)
WAIS.11.fa.5.v

# The first factor still seems to be a verbal factor, and the third is the math factor.

## Example 2:

######################################################################################################
######################################################################################################
##
## Entering the raw data for the life expectancy example:
##
"life" <- 
    structure(.Data = list(c(63., 34., 38., 59., 56., 62., 50., 65., 56., 69., 65., 64., 56., 60., 61., 49., 59., 63., 59., 65., 65., 64.,
                             64., 67., 61., 68., 67., 65., 59., 58., 57.)
                           , c(51., 29., 30., 42., 38., 44., 39., 44., 46., 47., 48., 50., 44., 44., 45., 40., 42., 44., 44., 48., 48., 63.,
                               43., 45., 40., 46., 45., 46., 43., 44., 46.)
                           , c(30., 13., 17., 20., 18., 24., 20., 22., 24., 24., 26., 28., 25., 22., 22., 22., 22., 23., 24., 28., 26., 21.,
                               21., 23., 21., 23., 23., 24., 23., 24., 28.)
                           , c(13., 5., 7., 6., 7., 7., 7., 7., 11., 8., 9., 11., 10., 6., 8., 9., 6., 8., 8., 14., 9., 7., 6., 8., 10., 8.,
                               8., 9., 10., 9., 9.)
                           , c(67., 38., 38., 64., 62., 69., 55., 72., 63., 75., 68., 66., 61., 65., 65., 51., 61., 67., 63., 68., 67., 68.,
                               68., 74., 67., 75., 74., 71., 66., 62., 60.)
                           , c(54., 32., 34., 46., 46., 50., 43., 50., 54., 53., 50., 51., 48., 45., 49., 41., 43., 48., 46., 51., 49., 47.,
                               47., 51., 46., 52., 51., 51., 49., 47., 49.)
                           , c(34., 17., 20., 25., 25., 28., 23., 27., 33., 29., 27., 29., 27., 25., 27., 23., 22., 26., 25., 29., 27., 25.,
                               24., 28., 25., 29., 28., 28., 27., 25., 28.)
                           , c(15., 6., 7., 8., 10., 14., 8., 9., 19., 10., 10., 11., 12., 9., 10., 8., 7., 9., 8., 13., 10., 9., 8., 10., 11.,
                               10., 10., 10., 12., 10., 11.)
    )
    , class = "data.frame"
    , names = c("m0", "m25", "m50", "m75", "w0", "w25", "w50", "w75")
    , row.names = c("Algeria", "Cameroon", "Madagascar", "Mauritius", "Reunion", "Seychelles", "South Africa(C)", "South Africa(W)",
                    "Tunisia", "Canada", "Costa Rica", "Dominican Rep", "El Salvador", "Greenland", "Grenada", "Guatemala",
                    "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Trinidad(62)", "Trinidad (67)", 
                    "United States (66)", "United States (NW66)", "United States (W66)", "United States (67)", "Argentina",
                    "Chile", "Columbia", "Ecuador")
    )
##
##
######################################################################################################
######################################################################################################

# A data set called life has been created.


factanal(life,factors=1, rotation="varimax")
# One factor is clearly not enough (tiny P-value).

factanal(life,factors=2, rotation="varimax")
# Two factors are clearly not enough (tiny P-value).

factanal(life,factors=3, rotation="varimax")
# Three factors may be enough (P-value = 0.458).

# How could we interpret the three factors?

# Saving the result as life.fa.3.v:

life.fa.3.v <- factanal(life,factors=3, rotation="varimax")

# The communalities:

1-life.fa.3.v$uniquenesses

# We see that m0, m50, w0, w25, w50 share very much of their variances with the other variables via the factors.
# m25 and m75 are more "unique".
# What does this mean?

# Estimating factor scores for the life expectancy data set:

life.fa.3.scores <- factanal(life,factors=3, rotation="varimax",scores="regression")$scores

#### Plotting the 3 factor scores for this data set using a 3-D plot:

# Creating a data frame with the original data and the scores:

life.df <- data.frame(life, life.fa.3.scores)
attach(life.df)

### Doing a 3-D plot:

library(lattice)  # loading the lattice package
cloud(Factor3 ~ Factor1 * Factor2, xlim=range(Factor1), ylim=range(Factor2), zlim=range(Factor3), 
      pch=row.names(life.df),
      scales = list(distance = rep(1, 3), arrows = FALSE))

# Looking back at the factor scores:

life.df[,c("Factor1","Factor2","Factor3")]

row.names(life.df)[order(Factor1)]
# Recall factor 1 basically measures life expectancy at birth.
# We see Madagascar and Cameroon have the smallest values for Factor 1.
# The U.S .and Canada have high values for Factor 1.

row.names(life.df)[order(Factor2)]
# Recall factor 2 basically measures life expectancy for older women.
# Cameroon also has a low score for this factor.

row.names(life.df)[order(Factor3)]
# Factor 3 reflects (mostly) life expectancy for older men.
# Cameroon has the lowest score for this factor.
# Algeria has the highest score for Factor 3.

### Doing separate 2-D scatterplots of the factor scores:

# Factor 2 vs. Factor 1:

plot(Factor1, Factor2, type='n', xlab='Factor 1 scores', ylab='Factor 2 scores')
text(Factor1, Factor2, labels = row.names(life.df), cex = 0.7 )

# Factor 3 vs. Factor 1:

plot(Factor1, Factor3, type='n', xlab='Factor 1 scores', ylab='Factor 3 scores')
text(Factor1, Factor3, labels = row.names(life.df), cex = 0.7 )

# Factor 3 vs. Factor 2:

plot(Factor2, Factor3, type='n', xlab='Factor 2 scores', ylab='Factor 3 scores')
text(Factor2, Factor3, labels = row.names(life.df), cex = 0.7 )



#########################################################################################################


# A separate R function (and some auxilary functions) to do a variety of types of factor analysis:
# (Originally written by Dr. Brian Habing)

# Copy this long section of code into R first:

########################################### BEGIN CODE ##################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
factpca<-function(x=NULL,r=cor(x)){
    #Performs Principal Components Factor Analysis using the#
    #correlation matrix.  Can be given either the raw data or the#
    #correlation matrix.#
    xeig<-eigen(r);xval<-xeig$values;xvec<-xeig$vectors
    for (i in 1:length(xval)){
        xvec[,i]<-xvec[,i]*sqrt(xval[i])}
    rownames(xvec)<-colnames(r)
    return(xvec)
}
factpf<-function(x=NULL,r=cor(x)){
    #Performs Principal Factor Factor Analysis using the#
    #correlation matrix.  Can be given either the raw data or the#
    #correlation matrix.#
    n<-ncol(r);redcormat<-r;diag(redcormat)<-apply(abs(r-diag(1,nrow=n,ncol=n)),2,max)
    xeig<-eigen(redcormat);xval<-xeig$values;xvec<-xeig$vectors
    for (i in 1:length(xval[xval>0])){
        xvec[,i]<-xvec[,i]*sqrt(xval[i])}
    rownames(xvec)<-colnames(x)
    return(xvec[,xval>0])
}
factiter<-function(x=NULL,r=cor(x),niter=100,maxfactors=ncol(r),diag=FALSE){
    #Performs Iterated Principal Factor Factor Analysis using the#
    #correlation matrix.  Can be given either the raw data or the#
    #correlation matrix.#  
    n<-ncol(r);temp<-matrix(0,nrow=n,ncol=n);comm<-matrix(0,nrow=niter+2,ncol=n);y<-factpf(r=r);m<-ncol(y)
    temp[1:n,1:m]<-y;comm[1,]<-apply(abs(r-diag(1,nrow=n,ncol=n)),2,max);comm[2,]<-apply(as.matrix(temp[,1:maxfactors])^2,1,sum)                 
    for (i in 3:(niter+2)){
        redcormat<-r;diag(redcormat)<-comm[i-1,];xeig<-eigen(redcormat);m<-min(maxfactors,length(xeig$values[xeig$values>0]))
        for (j in 1:m){
            xeig$vectors[,j]<-xeig$vectors[,j]*sqrt(xeig$values[j])} 
        temp<-matrix(0,nrow=n,ncol=n);temp[1:n,1:m]<-xeig$vectors[1:n,1:m];comm[i,]<-apply(as.matrix(temp[1:n,1:m])^2,1,sum)
    }
    loadings<-as.matrix(temp[1:n,1:m]);rownames(loadings)<-colnames(r);rownames(comm)<-(-1:niter)
    if (diag!=FALSE)
    {list(comm=comm,loadings=loadings)}
    else 
    {loadings}
}

fact<-function(x=NULL,r=cor(x),maxfactors=floor((2*ncol(r)+1-sqrt(8*ncol(r)+5))/2),
               method=c("iter","pf","pca","norm"),rotation=c("none","varimax","quartimax"),
               niter=100,full=FALSE,plot=TRUE,dec=3){
    method<-match.arg(method);rotation<-match.arg(rotation)
    if (rotation=="none"){rot="- no rotation"}
    if (rotation=="varimax"){rot="- varimax rotation"}
    if (rotation=="quartimax"){loaded<-library(GPArotation,logical.return=T)
    rot<-" - quartimax"
    if (loaded==F){
        rot<-"- varimax rotation"
        rotation<-"varimax"
        warning("GPArotation library is needed for",
                " quartimax rotation.  Varimax was used instead",
                call.=FALSE)}}
    if (method=="iter"){ftemp<-factiter(r=r,niter=niter,maxfactors=maxfactors,diag=T)
    loadings<-ftemp$loadings
    concheck<-max(abs(ftemp$comm[niter+2,]-ftemp$comm[niter+1,]))
    if (concheck>=5e-4){
        warning("Convergence not achieved, difference of ",
                as.character(round(concheck,5)),
                " after ",as.character(niter)," iterations.",
                call.=FALSE)}
    maxcon<-apply(ftemp$comm,1,max); maxlegal<-max(as.integer(names(maxcon[maxcon<1])))
    if (as.integer(maxlegal)<niter){
        warning("Communality greater than one, ",maxlegal,
                " was last legal iteration.",call.=FALSE)}
    meth<-paste("iterated principal factor",rot)}
    if (method=="pf"){loadings<-factpf(r=r)
    loadings<-loadings[,1:min(ncol(loadings),maxfactors)]
    meth<-paste("principal factor",rot)}
    if (method=="pca"){loadings<-factpca(r=r)
    loadings<-loadings[,1:min(ncol(loadings),maxfactors)]
    meth<-paste("principal components",rot)}
    if (method=="norm"){q<-ncol(r)
    maxfactors<-min(maxfactors,
                    floor((2*ncol(r)+1-sqrt(8*ncol(r)+5))/2))
    faout<-factanal(covmat=r,factors=maxfactors,rotation="none")
    loadings<-diag(rep(1,ncol(r)))%*%faout$loadings
    meth<-paste("multivariate normal",rot)}
    loadings<-as.matrix(loadings)
    if ((rotation=="varimax")&&(ncol(loadings)>1)){
        loadings<-diag(rep(1,ncol(r)))%*%varimax(loadings)$loadings}
    if ((rotation=="quartimax")&&(ncol(loadings)>1)){
        loadings<-quartimax(loadings,normalize=TRUE)$loadings}
    loadings<-as.matrix(loadings)
    loadings<-loadings[,order(apply(-loadings^2,2,sum))]
    loadings<-as.matrix(loadings)
    colnames(loadings)<-paste("Factor",as.character(1:ncol(loadings)),sep="")
    rownames(loadings)<-colnames(r)
    resid<-r-loadings%*%t(loadings)-diag(1-apply(loadings^2,1,sum))
    cortri<-r[upper.tri(r)]
    restri<-resid[upper.tri(resid)]
    predtri<-cortri-restri
    if (plot==TRUE){
        par(mfrow=c(2,2))
        plot(princomp(covmat=r),main="")
        plot(cortri,predtri,xlab="Original Correlation",
             ylab="Predicted Correlation",xlim=c(-1,1),ylim=c(-1,1))
        lines(c(-1,1),c(-1,1))
        plot(predtri,restri,xlab="Predicted Correlation",ylab="Residual Correlation",
             main=paste("r-squared=",as.character(round(cor(predtri,restri)^2,3))))
        lines(c(-1,1),c(0,0))
        abline(lm(restri~predtri))
        hist(restri,xlab="Residual Correlations",
             main=paste("mean=",as.character(round(mean(restri),3)),
                        "  s.d.=",as.character(round(sd(restri),3))))
        par(mfrow=c(1,1))
    }
    loadings<-t(t(loadings)*sign(apply(sign(loadings),2,sum)+.001))
    evs<-eigen(r)$values
    comm<-apply(loadings^2,1,sum)
    names(comm)<-colnames(r)
    vexpl<-apply(loadings^2,2,sum)
    vexpl<-rbind(vexpl,vexpl/ncol(r))
    colnames(vexpl)<-colnames(loadings)
    rownames(vexpl)<-c("variance explained","percent explained")
    resid<-resid
    if (full!=TRUE){resid<-summary(resid[upper.tri(resid)])}
    list(eigen.values=round(evs,dec),
         method=meth,
         loadings=round(loadings,dec),
         communalities=round(comm,dec),
         importance=round(vexpl,dec),
         residuals=round(resid,dec))
}
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
############################################# END CODE ##################################################

# Note this can do both ML factor analysis and principal factor analysis.

# It also does either no rotation, varimax rotation, 
# or (if you install the GPArotation package first) quartimax rotation.

# By default, it gives some excellent graphics for checking model assumptions:

# Example on the WAIS data:

# method = "norm" means ML using the normality assumption.
fact(r=wais.corr,method="norm",rotation="none",maxfactors=6)

# method = "pf" means principal factor analysis.
fact(r=wais.corr,method="pf",rotation="none",maxfactors=6)

# ML with varimax rotation:
fact(r=wais.corr,method="norm",rotation="varimax",maxfactors=6)

# principal factor analysis with varimax rotation:
fact(r=wais.corr,method="pf",rotation="varimax",maxfactors=6)


# Example on the life expectancy data:

# ML with varimax rotation:
fact(x=life,method="norm",rotation="varimax",maxfactors=3)

# principal factor analysis with varimax rotation:
fact(x=life,method="pf",rotation="varimax",maxfactors=3)


