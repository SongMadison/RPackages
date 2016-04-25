
#install.packages("devtools")
require(devtools) # gives access to create(), check(), build()
require(roxygen2) # give access to document(), check()

getwd()

#step 1, 
create("./section3/hw2/robust/")

#step 2,
# define a bunch of fucntions

#document 

document(pkg = "robust")
check(pkg = "robust")

build("robust")
install(pkg="./robust")



example(lad)
example(print.lad)




## delete the package you don't want anymore.
remove.packages('robust')



