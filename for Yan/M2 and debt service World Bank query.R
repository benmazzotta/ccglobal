# #####
# Macroeconomic data for Yan
# Ben Mazzotta
# 2014 12 15
# #####


# #####
# Preliminaries

setwd("for Yan")
require(WDI); require(data.table); require(countrycode)

# #####
# Search WDI 


## Monetary aggregates

varM2 <- WDIsearch("M2")
dat.M2 <- data.table(WDI(indicator=varM2[,1], extra=TRUE, start=2005, end=2014))

write.csv(dat.M2, file="M2.csv")
write.csv(varM2, file="M2 variables.csv")


## Debt service

varDS1 <- WDIsearch("central government debt service")
dat.DS1 <- data.table(WDI(indicator=varDS1[1], extra=TRUE, start=2005, end=2014))

varDS2 <- WDIsearch("total debt service")[13:15,]
dat.DS2 <- data.table(WDI(indicator=varDS2[,1], extra=TRUE, start=2005, end=2014))

setkey(dat.DS1, country, year)
setkey(dat.DS2, country, year)
dat.DS3 <- dat.DS1[,.SD,.SDcols=c("country","year","GB.TDS.FRGN.CN")][dat.DS2]

dim(dat.DS1); dim(dat.DS2); dim(dat.DS3)


write.csv(dat.DS3, file="debt service aggregates.csv")
write.csv(rbind(varDS1, varDS2), file="debt service descriptions.csv")

rm(dat.DS1, dat.DS2, dat.DS3, varDS1, varDS2, varM2, test, varDS)

# End of script
# #####

