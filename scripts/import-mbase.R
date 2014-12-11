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
curr <- curr[currcirc>0,.SD, .SDcols=c("Country","mbase","currcirc")][unique(curr[,"Country",with=F]), mult="first"]
str(curr); summary(curr)

curr[is.na(mbase)]

##          Add regional information with countrycode
require(countrycode)
curr[,iso2c:=countrycode(Country, origin="country.name", destination="iso2c")]
curr[,region:=countrycode(Country, origin="country.name", destination="region")]
summary(curr)

##          Stopped here
##          At present we have all the countries with monetary base and currency in circulatoin
##          To Do: label variables
##          To Do: check for sensible values
##          To Do: find out why some values are zero
##          To Do: check for duplicates
##          To Do: Add iso2c codes

table(curr$region)

# save.image("archive 2014 12 10.Rdata")
save(curr, file="monetary base and currency in circulation.Rda")
