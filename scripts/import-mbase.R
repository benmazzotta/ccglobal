# Ben Mazzotta
# Import currency in circulation
# Cost of Cash Global Study
# December 10, 2014


# #####
# Goals
# 1. Import IFS data on currency in circulation and M0


setwd("../data")
require(WDI); require(data.table);require(countrycode); require(ggplot2)

##      import the data
curr <- read.delim("IFS currcirc and mbase.csv", sep=",", header=T)
str(curr)

curr <- data.table(curr)
##          Take most recent observation
curr[Concept=="Monetary Base",mbase := X2013]
##          Ensure all rows have a valid measure of mbase
curr[,mbase:=max(mbase, na.rm=T), by="Country"]
str(curr)

##          Take most recent observation
curr[Concept=="Currency In Circulation", currcirc:=X2013]
##          Ensure all rows have a valid measure of currcirc
curr[,currcirc:=max(currcirc, na.rm=T), by="Country"]
str(curr)

setkey(curr, Country)
##          Keep only first mention of each country.
curr1 <- curr[,.SD, .SDcols=c("Country","mbase","currcirc")][unique(curr[,"Country",with=F]), mult="first"]
str(curr1)

curr1 <- curr1[mbase>0]

str(curr1); summary(curr1)

