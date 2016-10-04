## This files shows the procedure to create and use/remvoe a package in R

## created by Song Wang
## based on 2pcackage.pdf, url: http://pages.stat.wisc.edu/~jgillett/327-3/2package/2package.pdf

getwd()

## create the packages:

#Step 0,
#install.packages("devtools")
require(devtools) # gives access to create(), check(), build()
require(roxygen2) # give access to document(), check()


#step 1,
create("./section3/hw2/robust/")

#step 2,
# define a bunch of fucntions
# with @para, @return, etx

#step3
# data (optional)


#document  and create a package.

document(pkg = "robust")
check(pkg = "robust")

build("robust")


#install.packages(pkgs, repos=getOption("repos"), type=getOption("pkgType"))
# installs the package on to your hard drive;
# using repos=NULL and type="source" allows the use of your “.tar.gz” file for pkgs. e.g.
install.packages(pkgs="./robust_0.0.0.9000.tar.gz", repos=NULL, type="source")



## how to use the package:

## r session restart  and test
require(robust)
example(lad)
example(print.lad)
example(coef)




## delete the package you don't want anymore.

#remove.packages('robust')



