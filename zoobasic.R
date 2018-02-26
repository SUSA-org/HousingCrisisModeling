library(zoo)

fname = "singlesize.csv" #FILENAME HERE
df <- read.csv(fname)
z <- read.zoo(df, index.column = 1, format="%m/%d/%Y")
g <- zoo(, seq(start(z), end(z), "day"))
m <- na.locf(merge(z, g))

#for monthly
goodzoo<-aggregate(m, as.yearmon, mean)
gooddf<-fortify.zoo(goodzoo)

#for yearly
as.year <- function(x) as.integer(as.yearmon(x))
goodzoo<-aggregate(m, as.year, mean)
gooddf<-fortify.zoo(goodzoo)
