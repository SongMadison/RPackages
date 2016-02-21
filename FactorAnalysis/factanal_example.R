?factanal

WASIWRIT.cor<-matrix(NA, 8,8) 

#input ones on the diagonal elements of the matrix 
diag(WASIWRIT.cor)<-1 

#input the lower triangle of the correlation matrix 
WASIWRIT.cor [lower.tri(WASIWRIT.cor)]<-  c(.57, .79, .62, .69, .83, .56, .51, .57, .65, .51, .54, .59, .66, .60, .70, .74, .58, .55, .53, .57, .71, .62, .71, .65, .51, .58, .53, .62)

WASIWRIT.cor[upper.tri(WASIWRIT.cor)]<- t(WASIWRIT.cor)[upper.tri(WASIWRIT.cor)]

dimnames(WASIWRIT.cor)<- list(c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste ("WRIT.", c("VerbAn", "Voc", "Mat", "Dia"), sep="")), c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste("WRIT.", c("VerbAn", "Voc", "Mat", "Dia"), sep="")))

#create a  vector of means
WASIWRIT.mean<-c(97.75, 97.87, 103.81, 99.81, 101.51, 100.63, 101.45, 100.64)
names(WASIWRIT.mean)<-c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste("WRIT. ", c("VerbAn", "Voc", "Mat", "Dia"), sep=""))
# create a vector of standard devation 
WASIWRIT.sd<-c(17.37, 14.49, 17.26, 16.61, 14.77, 16.42, 16.17, 13.92)
names(WASIWRIT.sd)<-c(paste("WASI.", c("Voc", "BD", "Sim", "MR"), sep=""), paste("WRIT." , c("VerbAn", "Voc", "Mat", "Dia"), sep=""))


factanal(covmat = WASIWRIT.cor, factors = 2, n.obs =152) 
## method is always= "mle"
#  rotation is defacult as "varimax"


library(psych)
fa(WASIWRIT.cor, nfactors=2, n.obs=152, fm="ml", rotate = "varimax")


