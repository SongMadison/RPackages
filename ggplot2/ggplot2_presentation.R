library(ggplot2)

rm(list=ls())

data(mpg)
qplot(displ, hwy, data = mpg, colour = factor(cyl))
data("diamonds")

d <- subset(diamonds, carat < 2.5 & rbinom(nrow(diamonds), 1, 0.1) == 1)
d$lcarat <- log10(d$carat)
d$lprice <- log10(d$price)


detrend <- lm(lprice ~ lcarat, data = d)
d$lprice2 <- resid(detrend)

mod <- lm(lprice2 ~ lcarat * color, data = d)

library(effects)
?"effects"
effectdf <- function(...){
    suppressWarnings(as.data.frame(effect(...)))
}
color <- effectdf("color", mod)
both1 <- effectdf("lcarat:color", mod)

carat <- effectdf("lcarat", mod, default.levels =50)
both2 <- effectdf("lcarat:color", mod, default.levels = 3)

(both2$upper-both2$lower)/both2$se

qplot( lcarat, lprice, data =d, colour=color)
qplot( lcarat, lprice2, data =d, colour=color)

fplot <- ggplot(mapping = aes(y = fit, ymin = lower, ymax = upper)) + 
                    ylim(range(both2$lower, both2$upper) )

plot1 <- fplot %+% color + aes(x = color) + geom_point() + geom_errorbar()

plot2 <- fplot %+% both2 + 
         aes(x = color, color = lcarat, group = interaction(color, lcarat)) +
         geom_errorbar() + geom_line(aes(group= lcarat))+ 
         scale_colour_gradient()


plot3 <- fplot %+% carat + aes(x= lcarat) + geom_smooth(stat = "identity")

ends <- subset(both1, lcarat == max(lcarat))
plot4 <- fplot %+% both1 + aes(x = lcarat, colour = color) +
         geom_smooth(stat = "identity") +
         scale_color_hue() + theme(legend.position = "none") +
         geom_text(aes(label= color, x = lcarat + 0.02), ends)



#chapter 5.7 draw maps
library(maps)
data(us.cities)
head(us.cities)
big_cities <- subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5) # size: width of boundary
tx_cities <- subset(us.cities, country.etc =="TX")
ggplot(data = tx_cities, aes(x = long, y = lat))+
    borders("county", "texas",colour = "grey70") +  # counterpart for map function
    geom_point(color = "black", alpha = 0.5)

library(maps)
states <- map_data("state")  
head(states)
map("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, by ="region")
choro <- choro[order(choro$order),]
head(choro)
qplot(long, lat, data = choro, group = group, fill = assault, geom = "polygon")
qplot(long, lat, data = choro, group = group, fill = assault/murder, geom = "polygon")

library(plyr)  #plyr: the split-apply-combine paradigm for R.
ia <- map_data("county","iowa")
mid_range <- function(x) mean( range(x, na.rm = TRUE) )
centres <- ddply(ia, .(subregion), colwise(mid_range,.(lat, long)) )
ggplot(ia, aes(long, lat))+
    geom_polygon(aes(group = group), fill =NA, colour = "grey60") +
    geom_text(aes(label = subregion), data = centres, size =4, angle =45)


# chapter 5.8 Revealing uncertainty



# chapter 5.9  stat_summary
midm <- function(x) mean(x, trim = 0.5)
m2 +
    stat_summary(aes(colour = "trimmed"), fun.y = midm,
                 geom = "point") +
    stat_summary(aes(colour = "raw"), fun.y = mean,
                 geom = "point") +
    scale_colour_hue("Mean")
?stat_summary

d <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
d + stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 2)

# You can supply individual functions to summarise the value at
# each x:
d + stat_summary(fun.y = "median", colour = "red", size = 2)
d + stat_summary(fun.y = "mean", colour = "red", size = 2)
d + aes(colour = factor(vs)) + stat_summary(fun.y = mean, geom="line")

d + stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max,
                 colour = "red")

#' d <- ggplot(diamonds, aes(carat, price))
d + geom_smooth()
d + geom_line(stat = "summary_bin", binwidth = 0.1, fun.y = "mean")

d <- ggplot(diamonds, aes(cut))
d + geom_bar()
d + stat_summary_bin(aes(y = price), fun.y = "mean", geom = "bar")

# A set of useful summary functions is provided from the Hmisc package:
stat_sum_df <- function(fun, geom="crossbar", ...) {
    stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...)
}



# The crossbar geom needs grouping to be specified when used with
# a continuous x axis.
d + stat_sum_df("mean_cl_boot", mapping = aes(group = cyl))
d + stat_sum_df("mean_sdl", mapping = aes(group = cyl))
d + stat_sum_df("mean_sdl", mult = 1, mapping = aes(group = cyl))
d + stat_sum_df("median_hilow", mapping = aes(group = cyl))

# There are lots of different geoms you can use to display the summaries

d + stat_sum_df("mean_cl_normal", mapping = aes(group = cyl))
d + stat_sum_df("mean_cl_normal", geom = "errorbar")
d + stat_sum_df("mean_cl_normal", geom = "pointrange")
d + stat_sum_df("mean_cl_normal", geom = "smooth")

# Summaries are more useful with a bigger data set:
mpg2 <- subset(mpg, cyl != 5L)
m <- ggplot(mpg2, aes(x=cyl, y=hwy)) +
    geom_point() +
    stat_summary(mapping = NULL, data = NULL, fun.data = "mean_sdl", geom = "linerange",
                 colour = "red", size = 2) +
    xlab("cyl")
m
# An example with highly skewed distributions:
if (require("ggplot2movies")) {
    set.seed(596)
    mov <- movies[sample(nrow(movies), 1000), ]
    m2 <- ggplot(mov, aes(x= factor(round(rating)), y=votes)) + geom_point()
    m2 <- m2 + stat_summary(fun.data = "mean_cl_boot", geom = "crossbar",
                            colour = "red", width = 0.3) + xlab("rating")
    m2
    # Notice how the overplotting skews off visual perception of the mean
    # supplementing the raw data with summary statistics is _very_ important
    
    # Next, we'll look at votes on a log scale.
    
    # Transforming the scale means the data are transformed
    # first, after which statistics are computed:
    m2 + scale_y_log10()
    # Transforming the coordinate system occurs after the
    # statistic has been computed. This means we're calculating the summary on the raw data
    # and stretching the geoms onto the log scale.  Compare the widths of the
    # standard errors.
    m2 + coord_trans(y="log10")
}

m2 <- ggplot(mov)
m2 + stat_summary(aes(colour = "trimmed"), fun.y = midm, geom = "point") +
    stat_summary(aes(colour = "raw"), fun.y = mean, geom = "point") + 
    scale_color_hue(("Mean"))


(unemp <- qplot(date, unemploy, data=economics, geom="line",
                 xlab = "", ylab = "No. unemployed (1000s)"))
presidential <- presidential[-(1:3), ]
yrng <- range(economics$unemploy)
xrng <- range(economics$date)
unemp + geom_vline(aes(xintercept = as.numeric(start)), data = presidential)

unemp + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, 
        fill = party), ymin = yrng[1], ymax = yrng[2],
        data = presidential,alpha =0.2) +
        scale_fill_manual(values = c("blue", "red"))  ## discrete values

last_plot() + geom_text(aes(x=start, y =yrng[1], label = name), data = presidential, 
                        size =3, hjust =0, vjust =0)
caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years", 
                        40), collapse="\n")   
unemp + geom_text(aes(x,y,label =caption),
                  data =data.frame(x=xrng[1],y=yrng[2]), hjust =0, vjust =1, size =4)

highest <- subset(economics, unemploy == max(unemploy))
unemp+ geom_point(data = highest, size =3, color = "red", alpha =0.5)


# chapter 5.11, Weighted data
str(midwest)
p <- qplot(percwhite, percbelowpoverty, data=midwest)
p + geom_point(aes(size = poptotal/1e6), data= midwest)+
    scale_size_area("Popultaion\n(million)", breaks = c(0.5,1,2,4))
p + geom_point(aes(size = area)) + acale_size_area()

lm_smooth <- geom_smooth(method = lm, size =1)
qplot(percwhite, percbelowpoverty, data= midwest) + lm_smooth
qplot(percwhite, percbelowpoverty, data= midwest, weight = popdensity, 
      size = popdensity) + lm_smooth

qplot(percbelowpoverty, data = midwest, binwidth = 1)
qplot(percbelowpoverty, data = midwest, weight = poptotal, binwidth = 1) + ylab("population")
