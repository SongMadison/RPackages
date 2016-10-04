rm(list=ls())

require("Rcpp")

escape.time = function(x, y, limit=256) { # for Mandelbrot's fractal
  z0 = complex(real=x, imaginary=y)
  z = z0
  for (i in 0:limit) {
    if (abs(z) > 2) {
      break
    }
    z = z^2 + z0
  }
  return(i)
}

n = 400 # Set up (x, y) pairs in region defined by -2 <= x, y <= 2.
interval = seq(from=-2, to=2, length.out=n)
x = rep(interval, each=n)
y = rep(interval, times=n)

print("Timing R function escape.time() ...")
print(system.time(v <- mapply(FUN=escape.time, x, y)))

sourceCpp("escapeTime.cpp") # This line gives access to the C++ function.
print("Timing C++ function escapeTime() ...")
print(system.time(v2 <- mapply(FUN=escapeTime, x, y)))
stopifnot(all(v == v2))

#m = matrix(data=unlist(v), nrow=n, ncol=n, byrow=TRUE)
#image(x=interval, y=interval, z=m, col=rainbow(256))
