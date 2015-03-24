# #####
# Final data integration
# Cost of Cash Global Benchmarking
# 2015 03 20
# Ben Mazzotta
# Institute for Business in the Global Context
# The Fletcher School, Tufts University
# #####


# #####
# Goals 
# 1. Integrate all data files into a single data object
#   including ATM costs, cash in transit, informal economy, consumer costs,
#   seigniorage, cash hoards, currency issuance, ~~and fraud~~.
# 2. Evaluate coverage within the full dataset.
# 3. Clean invalid data values.
# 4. Summarize the dataset.
# #####

setwd("../data")
require(data.table); require(ggplot2)

# ### 
# 1. Integrate data sources
# 1. (A) ATM costs

##      import
load("ATMcost.Rda")

##      evaluate completeness
names(ATMcost)
length(unique(ATMcost$Country.Code))

##      filter to final variables
ATMcost <- ATMcost[!is.na(Country.Code),.SD, .SDcols=c("Country.Code","Country.Name","ATMper100k","population","ATMs","GDPcap","gamma_sum","national","pop_MM","ATM_MM")]
str(ATMcost)
setnames(ATMcost, c("gamma_sum", "national"), c("ATMUnitCost", "ATMNatlCost"))
setattr(ATMcost$Country.Code, "var.label", "ISO 3-letter country abbreviation")
setattr(ATMcost$Country.Name, "var.label", "Country name according to World Bank convention")
setattr(ATMcost$ATMper100k, "var.label", "ATM density per 100,000 population")
setattr(ATMcost$population, "var.label", "Population, according to World Bank")
setattr(ATMcost$ATMs, "var.label", "Count of ATMs, national")
setattr(ATMcost$GDPcap, "var.label", "GDP per capita at PPP in 2012")
setattr(ATMcost$ATMUnitCost, "var.label", "Unit cost for monthly maintenance of one ATM, current USD")
setattr(ATMcost$ATMNatlCost, "var.label", "National, annual cost of ATM maintenance, current USD")
setattr(ATMcost$pop_MM, "var.label", "National population (in millions)")
setattr(ATMcost$ATM_MM, "var.label", "National ATM maintenance cost (in millions)")

summary(ATMcost)

##      integrate
costofcash <- ATMcost
save(costofcash, file="globalcostofcash.Rda")

rm(ATMcost)

# 1. (B) Cash in transit

load("Cash in transit.Rda")

tables()
setnames(costofcash, "Country.Code", "iso3c")


##      Change the ISO3C labels to character variables
cashintransit[, iso3c:=as.character(iso3c)]
cashintransit[, iso2c:=as.character(iso2c)]

costofcash[, iso3c:=as.character(iso3c)]

cashintransit[, country:=as.character(country)]
costofcash[, Country.Name:=as.character(Country.Name)]


##      Sort
setkey(cashintransit, iso3c)
setkey(costofcash, iso3c)

##      Merge
costofcash <- cashintransit[, .SD, .SDcols=c("country","iso2c","iso3c","year","pricelevel","gdp","region","income","banks","CIT_hat","CITest")][J(costofcash)]

str(costofcash)
summary(costofcash)

mergefail.1.countries <- costofcash[is.na(country),Country.Name]
## Missing countries: Argentina, Burkina, Bahrain, Cabo Verde, Euro area, Myanmar, Niger, Senegal, San Marino, Syria, Togo

##    Since this is empty....
##      None of the countries in mergefail.1.countries appear in both data.tables
##      All are truly missing from either ATM or CIT costs
# costofcash[country %in% mergefail.1.countries, .SD, .SDcols=c("i2o3c","gdp")]

## Wrap up and move to the next section
rm(cashintransit)
save.image("working.Rdata")


# #####
# 1. (C) Household costs: fees, time and transit

load("household_costs.Rda")

setkey(remit, iso3c)
tables()

##      Specify variables to keep from household costs
remit.keepvars <- c("iso3c","cashTRX","cashfees","remit_MM","remit_TRX","payXrem","transUSD", "iso2c")

##      Merge
costofcash <- remit[, .SD, .SDcols=remit.keepvars][J(costofcash)]

rm(remit.keepvars)

load("household_time.Rda")

##      Specify variables to keep from household time spent
hhtime.keepvars <- c("iso2c","timespent","timepcap")
##      Merge
setkey(hhtime, iso2c); setkey(costofcash, iso2c)
costofcash <- hhtime[, .SD, .SDcols=hhtime.keepvars][J(costofcash)]

rm(hhtime.keepvars)

str(costofcash)
summary(costofcash)


load("household_transit.Rda")
setkey(hhtransit, iso2c)
hhtransit.keepvars <- c("iso2c","transrate","transcost")

tables()
costofcash <- hhtransit[,.SD, .SDcols=c(hhtransit.keepvars)][J(costofcash)]


names(costofcash)
str(costofcash)
summary(costofcash)


## Fees and time; what about transit?

rm(hhtime, remit, hhtransit)

rm(hhtransit.keepvars, mergefail.1.countries)

# #####
# 1. (D) Currency Issuance


##    Yan Bai's data on seigniorage
# load("seigniorage_bai.Rda")

load("bai.seigniorage.merge.Rda")
load("bai.currissuance.merge.Rda")

##      Structure and summary statistics
currissue[,EUMember:=NULL]
names(currissue)
str(currissue)
summary(currissue)


##      Structure and summary statistics
names(seign)
str(seign)
summary(seign)


setkey(seign, "iso2c"); setkey(currissue, "iso2c")
tables()

##      Merge with selected variables
costofcash <- seign[J(costofcash)]
##      Merge with selected variables
costofcash <- currissue[J(costofcash)]

names(costofcash)
str(costofcash)
summary(costofcash)

save.image("working.Rdata")
save.image("archive 2015 03 23.Rdata")
save.image("../.Rdata")

##### Interrupted here 2015 03 25 #####