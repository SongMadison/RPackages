#Source: http://www.stat.berkeley.edu/classes/s133/dates.html
# Song Wang


# handles dates( without times); dates %h:%m:%s
date1 <- as.Date('1915-6-16', format = '%Y-%m-%d') # default format 4-digit year + 2-digit month, + 2-digit day
as.Date('1915/6/16')

weekdays(date1)
as.numeric(date1)  #reference to 1970-01-01



#handles dates and times  - no time zones
#install.packages('chron')
library(chron)
dtimes = c("2002-06-09 12:45:40","2003-01-29 09:30:40",
           "2002-09-04 16:45:40","2002-11-13 20:00:40",
           "2002-07-07 17:30:40")
dtparts = t(as.data.frame(strsplit(dtimes,split = ' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],
                     format=c('y-m-d','h:m:s'))
thetimes
# chron values are stored internally as the fractional number of days from 1970-01-01
as.numeric(thetimes)



# handles dates and times with control of time zones -- classes
POSIXct()  #-dates are stored internally as the number of days or seconds from some reference date
           # thus dates in R will generally have a numberic mode
   
POSIXlt()  #store dates/times as a list of components(hour, min, sec, mon, etc.) making it easier to 
           # extract these parts

POSIX #represnts a portable operating system interface, primary for UNIX systems
      # store date/time and timezone
      # stored as the nearest seconds, thus more accurate than chron, stored in fraction of days

dts = c("2005-10-21 18:47:22","2005-12-24 16:39:58",
            "2005-10-28 07:30:05 PDT")
as.POSIXlt(dts)
as.POSIXct(dts)

#in seconds
dts = c(1127056501,1104295502,1129233601,1113547501,
              1119826801,1132519502,1125298801,1113289201)
mydates =dts
class(mydates) <-c ('POSIXt','POSIXct')
mydates


mydate = strptime('16/Oct/2005:07:51:00',format='%d/%b/%Y:%H:%M:%S') # for input
strftime(mydate)  # output

ISOdate(2005,10,21,18,47,22,tz="UTC") #'GMT', 'UTC'
