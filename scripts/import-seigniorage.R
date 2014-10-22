## import seigniorage data.
## Ben Mazzotta
## Cost of Cash Global


# #####
# Goals
# 1. Import IMF data on national monetary bases and interest rates.
# 2. Calculate interest burden of central bank assets on the currency supply.

# *****
# 0. Preliminaries

require(data.table)
require(ggplot2)
require(plyr)
require(foreign)

setwd("../data") ## Works from either scripts or data directory


# ##### 
# 1. Import CSV and look at the structure

curr <- read.delim("seigniorage.csv", sep=",",header = TRUE)
dim(curr) # 372x16
length(unique(curr$Country)) #166
str(curr)
setattr(curr$Concept, "levels", c("Currency In Circulation", "Interest", "Monetary Base"))

currcirc <- data.table(subset(curr, Concept=="Currency In Circulation"))
M0 <- data.table(subset(curr, Concept=="Monetary Base"))
interest <- data.table(subset(curr, Concept=="Interest"))

table(curr$Country)

##    Estimate based on 3-year average

currcirc[,estimate := rowMeans(.SD, na.rm=T), .SDcols=c("X2010","X2011","X2012")]
summary(currcirc)
M0[,estimate := rowMeans(.SD, na.rm=T), .SDcols=c("X2010","X2011","X2012")]
summary(M0)
interest[,estimate := rowMeans(.SD, na.rm=T), .SDcols=c("X2010","X2011","X2012")]
summary(interest)


interest$Country

##      Are currency and monetary base reporters overlapping or subsets?  SUBSET
setdiff(currcirc$Country, M0$Country)
setdiff(M0$Country, currcirc$Country )

##      Let's merge our best estimates
curr.dt <- currcirc[,.SD, .SDcols=c("Country","estimate")]
setnames(curr.dt, "estimate","currency")

##      Sort prior to merge
setkey(curr.dt, Country)
setkey(M0, Country)
setkey(interest, Country)
tables()

curr.dt <- M0[J(curr.dt),] ## Join
curr.dt <- curr.dt[,.SD, .SDcols=c("Country","currency","estimate")] ## Drop extraneous
setnames(curr.dt, "estimate","monbase") ## Rename

curr.dt <- interest[J(curr.dt),] ## Join
curr.dt <- curr.dt[,.SD, .SDcols=c("Country","currency","monbase","estimate")] ## Drop extraneous
setnames(curr.dt, "estimate","intbill") ## Rename

##      Result

summary(curr.dt)
curr.dt["Slovak Republic",]

##      Drop problematic Slovak Republic observation
curr.dt[is.nan(curr.dt$currency),]    ## Unique observation; to be deleted.
curr.dt <- subset(curr.dt, !is.nan(currency)) ## Deletes successfully.


tables()

##      What is the interest cost of assets backing the money supply?

curr.dt[,intburden := currency * intbill/100]

summary(curr.dt$intburden)

setkey(curr.dt, intburden)

head(curr.dt, 20)
head(curr.dt[order(intburden)], 20)
head(curr.dt[order(-intburden)], 20)
c("United States") %in% curr.dt$Country


## Those figures are all in local currency. So PAK is not the highest seigniorage country in the world.

# #####
# Convert to USD


##      Fetch forex rates from World Bank
require(WDI)
forex <- WDIsearch("Official Exchange Rate")[1]
forex.dt <- data.table(WDI(indicator = forex,start = 2010, end=2012, extra=T))
forex.dt <- subset(forex.dt, region!="Aggregates")
str(forex.dt)

##      Rename for convenience.
setnames(forex.dt, "DPANUSLCU", "LCU.USD")

##      Three year average of official exchange rate
forex.dt[,estimate := mean(LCU.USD, na.rm=T), by="iso2c"]

##      Drop missing observations
forex.dt <- subset(forex.dt, !is.na(LCU.USD))
summary(forex.dt)

##      Single record per country.
forex.dt <- forex.dt[,.SD, .SDcols=c("iso2c", "country", "estimate","region", "income")]
setkey(forex.dt)      ## Sort.
forex.dt <- unique(forex.dt)  ## Then keep unique rows.
tables()              ## Take a gander.
setnames(forex.dt, "estimate","USDrate")


##      Add ISO2C codes to curr.dt
require(countrycode)
##          Convert country names to character format from factor
curr.dt[,Country:=as.character(Country)]  
##          Create the abbreviations variable
curr.dt[,iso2c := countrycode(Country, origin = "country.name",destination = "iso2c")]
curr.dt[,iso3c := countrycode(Country, origin = "country.name",destination = "iso3c")]

##          Replace the entry for Kosovo, which is not handled by {countrycode}
curr.dt[Country=="Kosovo, Republic of",iso2c:="KV"]
curr.dt[Country=="Kosovo, Republic of",iso3c:="KSV"]
str(curr.dt)
tables()

##          World Bank data could be a patch for IMF data
officialinterest <- WDIsearch("Average interest on new external debt commitments, official")
temp <- WDI(country="all", indicator=officialinterest[[1]], start=2012, end=2012, extra=TRUE)
temp <- subset(temp, region != "Aggregates")
names(temp)

##          Create a data table with official external borrowing rates
offint.dt <- data.table(temp[,c(1:3)])
rm(temp); rm(test)

##          Human readable names
setnames(offint.dt, "DT.INR.OFFT", "externalrate")
summary(offint.dt$externalrate)

##          Sort for merge below
tables()
setkey(curr.dt, iso2c); setkey(offint.dt, iso2c)

##          Merge into curr.dt
curr.dt <- offint.dt[J(curr.dt),]
str(curr.dt)
with(curr.dt, table(is.na(externalrate), is.na(intbill)))

## According to that table, we should pick up an extra 41 countries this way.
## Leaves 27 countries with no measure of interest rates for seigniorage.

summary(curr.dt)

##        Where intburden is missing....
##          Calculate intburden as externalrate * currency.

curr.dt[is.na(intburden), intburden:=externalrate*currency]
summary(curr.dt)     ## As predicted: 27 missing observations

curr.dt[is.na(intburden),]

## Bad news ... Germany, Austria, Estonia, Denmark, Finand, Ireland, Netherlands, Russian federation, Slovak.
table(curr.dt$iso2c) ## No duplicates; no deus ex machina. Really have to track down sovereign bills rate for these countries.

#       Better source: harmonized data on European long-term rates from ECB data warehouse.
##        http://sdw.ecb.europa.eu/

save(curr.dt, file="interestburden.Rda")
save.image("working.Rdata")
# interrupted here 10/22/2014
#####
