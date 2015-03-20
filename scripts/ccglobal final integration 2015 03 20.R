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

##### Interrupted here 2015 03 20 #####