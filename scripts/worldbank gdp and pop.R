## CC Global -- master geography file
## Ben Mazzotta
## 9/30/2014


# #####
# Goals:
# 1. Geography fields: World bank names, abbreviations, GDP/capita, GDP, population, regional affiliation.
# 2. Single master file for 150+ countries.


# #####
# Run in ~project/data folder

## 0. Preliminaries
require(WDI)



## 1. List of variables
var.gdpcap <- WDIsearch(string = "GDP per capita")[8,1]
var.gdp <- WDIsearch(string = "GDP, PPP")[1,1]
var.gdpreal <- WDIsearch(string="GDP, PPP")[2,1]
var.pop <- WDIsearch(string="Population, total")[1]

## 2. Query WDI

worldbank <- WDI(country="all", indicator = c(var.gdp, var.gdpreal, var.gdpcap, var.pop), start=2000, end=2014, extra=TRUE)

## 3. Check

with(worldbank, ftable(table(country, year)))
str(worldbank)

worldbank.regions <- subset(worldbank, region=="Aggregates")
worldbank.countries <- subset(worldbank, region!="Aggregates")

save(worldbank, file="worldbank.Rda")
save(worldbank.countries, file="worldbank countries.Rda")
save(worldbank.regions, file="worldbank regions.Rda")

rm(list=c("var.gdp","var.gdpcap","var.gdpreal","var.pop"))
save.image("archive.worldbankquery.Rdata")
