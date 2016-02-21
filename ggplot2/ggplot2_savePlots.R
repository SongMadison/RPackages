rm(list=ls())
library(ggplot2)
library(gridExtra, quietly = TRUE)

### save plots
plot1 <- qplot(displ, hwy, data= mpg, colour = factor(cyl))
plot2 <- plot1+ geom_smooth(method = lm,position = "identity" )
print(plot2)
summary(plot2)

## save the plot object
save(plot2, file = "./plot.rdata")


rm(list = ls())  #cleanning up

# the plot object contains the data.  It is self-contained!!! ONLY need the plot object to recreate 
# OR update the plots
load("./plot.rdata") 
data <- data.frame(p2$data)
str(data)

plot1 <- plot2 +ggtitle("plot1")
##  remove the second layer: 
plot1$layers[[2]]<- NULL

plot2 <- plot2 + ggtitle("plot2")
plot3 <- plot1 + geom_smooth() + ggtitle("plot3")
plot4 <- plot2+ geom_smooth() +ggtitle("plot4")

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)






## plot object + layers
p1 <- ggplot(diamonds, aes(x = carat))
p2 <- p1+ 
    layer( data= NULL, geom = "bar",
           stat = "bin",
           position = "identity",
           params = list(fill = "steelblue", binwidth = 0.2, na.rm = FALSE)
    ) 
p3 <- p1 + geom_histogram(fill = "steelblue", binwidth = 0.2)

grid.arrange(p1,p2,p3, ncol =3)

