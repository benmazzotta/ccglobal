# #####
# Model HH transit and fees for cash transactions
# Ben MAzzotta
# December 3, 2014
# #####


# #####
# Goals.
# 1. Estimate HH fees for cash transactions
# 2. Estimate HH transit costs for cash transactions
# 3. Estimate national time spent on cash transactions
# 4. Evaluate that time at national median wage
# #####


# ###
# 0. Preliminaries

setwd("../data")
require(IBGCdocs); require(countrycode); require(data.table); 
require(ggplot2); require(Hmisc); require(WDI)




# ###
# 1. Household fees for cash transactions

##      Model to be estimated: fees = price * transactions
##        At present, TRX calculated mechanically from remittances, finclusion.
##        Instead, I could estimate a linear model for TRX with MI and use predicted values.
##        Postestimation step: multiply predicted value by price and imputed price data.


load("remittances.Rda")
remit[,cashfees:=price*cashTRX]
summary(remit$cashfees)
setattr(remit$cashfees, "var.label","National cost of cash access fees, USD millions")

cat("### \n These 81 countries have estimates of cash fees.")
remit[!is.na(cashfees), country]

cat("### \n These 111 countries do not.")
remit[is.na(cashfees), country]


##      This is a good candidate for MI with missingness.
##        Many missing observations are only missing either price or TRX volume.
##        Estimate prices and TRX volume from MI with WDI.

# ###
# 2. Estimate HH transit costs

cat("Mexico report gave an average of MXN 35 in transit costs per withdrawal.")

##        Convert MXN 35 to USD using PPP conversion factor
require(WDI)
WDIsearch("PPP conversion factor")
transit <- WDI(indicator="PA.NUS.PPP", country="MX",start=2012, end=2012)

##        Calculate the USD equivalent at PPP of 35 MXN
transit <- data.table(transit)
setkey(transit, iso2c)
setnames(transit, "PA.NUS.PPP", "PPP")

transit[,mxn := 35]
transit[,usd := mxn/PPP]


##        Use PPP conversion factors to approximate price levels around the world.
prices <- WDI(indicator="PA.NUS.PPPC.RF", start=2012, end=2012)
# summary(prices)

prices <- data.table(prices)
setkey(prices, iso2c)
tables()


##        Join PPP levels to MEX transit costs
prices[, region:=countrycode(iso2c, origin="iso2c", destination="region")]
prices <- prices[!is.na(region)]
prices[,mextransit:=transit$usd]
summary(prices)

##        Adjust MEX transit cost to local prices
prices[,transUSD := mextransit*PA.NUS.PPPC.RF]
attr(prices$transUSD, "var.label") <- "Cost of transit at local prices, per transaction, in USD"

##        Join transit costs to remittances
setkey(remit, iso2c)
tables()
remit <- prices[,.SD,.SDcols=c("iso2c","transUSD")][remit]
summary(remit)


##        Clean up the workspace
rm(prices, time, transit, trxtime, india.dt)

##        Transit costs

##        Create a new object in memory: fees paid for transit
hhtransit <- hhtime[,.SD, .SDcols=c("iso2c","Country", "Region","Lending","cashTRX","ATMd","RAI","RAInorm","Population","Rural.Pop")]

##        Join the national per-transaction transit costs
hhtransit <- remit[,.SD,.SDcols=c("iso2c","transUSD")][hhtransit]

##        Mexico transit incidence is 23.31% of cash transactions recalled. Not sure about per capita rates; we could break this out.
hhtransit[,transrateMEX := .2331]

##        Model national transit rate adjusted by RAInorm
hhtransit[,transrate:= RAInorm * transrateMEX]
setattr(hhtransit$transrate, "var.label", "Estimated share of cash transactions that require transit expense, as a decimal")

##        Model national cost of transit associated with cash transactions.
hhtransit[,transcost:=cashTRX*transrate*transUSD]
setattr(hhtransit$transcost, "var.label", "Estimated national cost of household transit for cash access, in USD (millions)")
setattr(hhtransit$transUSD, "var.label", "Estimated household transit cost for a single transaction, in USD")


# #####
# Findings
##        A few quick charts.
##              Boxplot of national costs by region
boxplot(transcost~Region, data=hhtransit, log="y")
boxplot(transcost/Population*1e6 ~ Region, data=hhtransit, log="y")

##              Scatterplots of population vs. national cost
qplot(Population, transcost, data=hhtransit, geom="text", label=iso2c, log="xy") + facet_wrap(~Region)

summary(hhtransit)

# #####
# Missing data

##        Calculation of the transit cost rate failed for Australia, Canada, CAR, Singapore, Swaziland, Switzerland
hhtransit[is.na(transrate), Country]

##        Calculation of the cash transactions total failed for many countries.
##          Based on sketchy remittance data and Global Findex. Need to estimate where unavailable.
hhtransit[is.na(cashTRX), Country]


##        Save your work

save(remit, file="household_costs.Rda")
save(hhtime, file="household_time.Rda")
save(hhtransit, file="household_transit.Rda")

save.image("working.Rdata")

