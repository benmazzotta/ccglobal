# #####
# Import Wages Data
# Souce: International Labour Organization
# Ben Mazzotta
# December 2, 2014
# #####

# #####
# Goals
#   1. Import ILO data


##     0. Preliminiaries
setwd("../data")
require(foreign)


##      1. Import ILO data
wages <- read.dta("ILO-global-wage-database-2012.dta")

unique(wages$indicator)
wages <- data.table(wages)
wages[,iso2c:=countrycode(countryname, origin="country.name", destination="iso2c",)]
setnames(wages, c("_2000", "_2001","_2002","_2003","_2004","_2005","_2006","_2007","_2008","_2009","_2010","_2011"), c("y2000","y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010","y2011"))

##      2. Think about missing data. Let's get some robust averages to minimize the problems.

##          Average of nonmissing observations since 2008
wages[indicator=="average nominal wages 1",avg08:=mean(c(y2008, y2009, y2010, y2011), na.rm=TRUE), by="iso2c"]

##          Average of nonmissing observations since 2005
wages[indicator=="average nominal wages 1",avg05:=mean(c(y2005, y2006, y2007, y2008, y2009, y2010, y2011), na.rm=TRUE), by="iso2c"]

##          Average of nonmissing observations since 2000
wages[indicator=="average nominal wages 1",avg00:=mean(c(y2000, y2001, y2002, y2003, y2004, y2005, y2006, y2007, y2008, y2009, y2010, y2011), na.rm=TRUE), by="iso2c"]


##          Median, mean of nonmissing observations since 2008
wages[indicator=="median nominal wages",med08:=mean(c(y2008, y2009, y2010, y2011), na.rm=TRUE), by="iso2c"]

##          Median, mean of nonmissing observations since 2005
wages[indicator=="median nominal wages",med05:=mean(c(y2005, y2006, y2007, y2008, y2009, y2010, y2011), na.rm=TRUE), by="iso2c"]

##          Median, mean of nonmissing observations since 2000
wages[indicator=="median nominal wages",med00:=mean(c(y2000, y2001, y2002, y2003, y2004, y2005, y2006, y2007, y2008, y2009, y2010, y2011), na.rm=TRUE), by="iso2c"]

summary(wages)

wage.summary <- wages[indicator=="average nominal wages 1", .SD, .SDcols=c("iso2c","countryname","avg08","avg05","avg00")]
setkey(wage.summary, iso2c)
setkey(wages, iso2c)
tables()

wage.summary <- wages[indicator=="median nominal wages", .SD, .SDcols=c("iso2c","med08","med05","med00")][wage.summary,]
str(wage.summary)
setkey(wage.summary, iso2c, countryname)
wage.summary[,wageavg:=avg08]
wage.summary[is.na(wageavg),wageavg:=avg05]
wage.summary[is.na(wageavg),wageavg:=avg00]

wage.summary[,wagemed:=med08]
wage.summary[is.na(wagemed),wagemed:=med05]
wage.summary[is.na(wagemed),wagemed:=med00]

summary(wage.summary)

save(wages, file="ILOwages2012.Rda")
save(wage.summary, file="wage average and medians.Rda")

rm(wages)
