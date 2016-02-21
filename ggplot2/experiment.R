rm(list=ls())
library(ggplot2)

p1 <- ggplot(diamonds, aes(x = carat))
p2 <- p1+ 
    layer( data= NULL, geom = "bar",
           stat = "bin",
           position = "identity",
           params = list(fill = "steelblue", binwidth = 0.2, na.rm = FALSE)
    ) 
p3 <- p1 + geom_histogram(fill = "steelblue", binwidth = 0.2)


p <- ggplot(diamonds, aes(carat, colour = cut))
p <- p + geom_histogram( binwidth = 2, fill = "steelblue")
