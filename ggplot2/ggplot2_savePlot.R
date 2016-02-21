
p1 <- qplot(displ, hwy, data= mpg, colour = factor(cyl))
p2 <- p1+ geom_smooth(method = lm,position = "identity" )
print(p2)
summary(p2)


## save the plot object
save(p2, file = "./plot.rdata")


rm(list = ls())

# the plot object contains the data.  It is self-contained!!! ONLY need the plot object to recreate 
# OR update the plots
load("./plot.rdata") 
data <- data.frame(p2$data)
str(data)

p1 <- p2 +ggtitle("p1")
##  remove the second layer: 
p1$layers[[2]]<- NULL

p2 <- p2 + ggtitle("p2")
p3 <- p1 + geom_smooth() + ggtitle("p3")
p4 <- p2+ geom_smooth() +ggtitle("p4")

grid.arrange(p1, p2, p3, p4, ncol = 2)


ggsave(p3, "./plot.png", plot = last_plot(), width = 5, height =5)

qplot(carat, data= diamonds, facets = color~., 
      geom = "histogram", binwidth = 0.1, xlim = c(0,3) )

qplot(x =carat, y = ..density..,  data= diamonds, facets = color~., 
      geom = "histogram", binwidth = 0.1, xlim = c(0,3) )




ggplot(mpg, aes(displ, hwy)) +
    layer(geom = "point", stat = "identity", position = "identity",
          params = list(na.rm = FALSE)
    )

bestfit <- geom_smooth(method = "lm", se = F,
                       colour = alpha("steelblue", 0.5), size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit


p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour = "darkblue")
p + geom_point(aes(colour = "darkblue"))


df <- data.frame(x= 1:3,y=1:3, colour = c(1,3,5) )
qplot(x,y, data= df, color=colour, size =I(5)) + 
    geom_line()

xgrid <- with(df, seq(min(x), max(x), length = 50))
interp <- data.frame(
    x = xgrid,
    y = approx(df$x, df$y, xout = xgrid)$y,
    colour = approx(df$x, df$colour, xout = xgrid)$y 
    )
qplot(x, y, data = df, colour = colour, size = I(5)) +
      geom_line(data = interp, size = 2)


d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
d + stat_bin(
    aes(size = ..density..), binwidth = 0.1,
    geom = "point", position="identity"
)

d + stat_bin(
    aes(y = 1, fill = ..count..), binwidth = 0.1,
    geom = "tile", position="identity"
)
