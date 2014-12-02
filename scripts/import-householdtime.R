# Household costs: time and travel
# Ben Mazzotta
# 1 December 2014

# #####
# Goals: 
#   Ultimately: Estimate the household cost of cash transactions by country from World Bank Data.
#   ...
# 7. Estimate per-TRX household time spent to obtain cash
# 8. Estimate per-TRX houseohld transit cost to obtain cash
# 9. Data required:
#       Rural Access Index
#       ATMs per 100k population
#       Time use: USA
#       Time use: Europe
#       Time use: India

# #####
# Rural Access

## Our benchmarking strategy relies on World Bank's Rural Access Index (RAI)
## Three numbers per country: own RAI, India's RAI and own RAI as a multiple of India's.



#       0. Preliminaries
require(WDI); require(data.table); require(reshape); require(gdata)
setwd("../data")

#       1. Import from Excel

##        Create a new data object "RAI"
rai <- read.xls("rai.xls", header=T, skip=1, na.strings="..")
rai$Country <- as.character(rai$Country)
attr(rai$Method, "levels") <- c("","CN","GIS","PMGSY","CWI","CWI++","ECS","HBS","IES","IS","model","PODES","PS","SSATP")

##        Transform to data table
rai <- data.table(rai)
setkey(rai, Country)

##        Attach country abbreviations
require(countrycode)
rai[,iso2c:= countrycode(Country, origin="country.name",destination="iso2c")]


# ##    Check for errors
# table(is.na(rai$iso3c))
# rai[is.na(iso3c),]

str(rai)
summary(rai)

##      Add a column for RAI of India
rai.india <- rai[Country=="India", RAI]
rai[,RAIindia := rai.india]

##      Add a column for the ratio of RAI to India's
rai[,RAInorm := RAI/RAIindia]
attr(rai$RAIindia, "var.label") <- "Rural Access Index level of India"
attr(rai$RAInorm, "var.label") <- "RAI as a multiple of India's RAI"


summary(rai$RAInorm)
save(rai, file="RAI.Rda")


# #####
# ATMs per 100k population

atm.var <- WDIsearch("ATM")[1]
ATMs <- WDI(indicator=atm.var, start=2010,end=2012)
str(ATMs)
ATMs <- data.table(ATMs)
setkey(ATMs, country)
setnames(ATMs, atm.var, "ATMdensity")
str(ATMs)

##      Set the USA values by hand
ATMs[iso2c=="US" & year==2012, ATMdensity:=425/3.14000]
ATMs[iso2c=="US" & year==2011, ATMdensity:=425/3.12000]
ATMs[iso2c=="US" & year==2010, ATMdensity:=425/3.09000]

table(ATMs$country, is.na(ATMs$ATMdensity))

#           Take 3-year average
ATMs[,ATMd:=mean(ATMdensity, na.rm=T), by="iso2c"]
#           Keep only most recent observation
ATMs <- ATMs[,head(.SD, 1), by="iso2c"]
# table(is.na(ATMs$ATMdensity))

save(ATMs, file="ATM density.Rda")
rm(atm.var)
rm(rai.india)

save.image("working.Rdata")

# #####
# Time use variables

##    This is a Fisher and Robinson time use dataset from 2010
time <- read.delim("MTUS.csv", sep=",", na=c("..","#VALUE!"), nrows=22, header=TRUE)
time$Country <- as.character(time$Country)

time <- data.table(time, key="Country")

require(countrycode)
time[,iso2c:=countrycode(Country,origin="country.name",destination="iso2c",)]


save.image("working.Rdata")


##        Time to merge the data together.
##          First sort
setkey(ATMs, iso2c)
setkey(rai, iso2c, Country)
setkey(time, iso2c, Country)

##          Then merge
hhtime <- time[rai[!is.na(Region),]]
# str(hhtime); summary(hhtime)

##          Second merge
##          And subset for useful variables
hhtime <- ATMs[hhtime][,.SD, .SDcols=c("iso2c","Country","ATMd","Travel","Region","Lending","RAI","RAInorm","Population","Rural.Pop")]
str(hhtime); summary(hhtime)

save(hhtime, file="householdtime.Rda")
save.image("working.Rdata")
