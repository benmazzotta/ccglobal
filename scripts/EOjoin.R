# #####
# Reshape Elgin and Oztunali data to long format
# #####


## Here is the Elgin Oztunali 2012 dataset.

setwd("../data")
load("ElginOztunali2012.Rda")


##      Preliminaries
require(data.table)
require(reshape2)
require(countrycode)

##      Inspect
str(EO.dt)

##      Melt
EO.melt <- melt(EO.dt, id.vars=c("Years"), measure.vars=2:163, variable.name="Country",value.name="Informality")
str(EO.melt)
setkey(EO.melt, Country)
tables()

##      Attach country codes
EO.melt[,iso2c:=countrycode(Country, origin="country.name", destination="iso2c")]
View(EO.melt[iso2c=="US"])
summary(EO.melt[Years==2008,])

setnames(EO.melt, "Years", "year")
setattr(EO.melt$Informality, "var.label", "Informal economy as a share of GDP (percent)")


##      Take a single year; most recent complete is 2008
informal08 <- EO.melt[year==2008]
informal08[is.na(iso2c),]
informal08[Country=="Hong.Kong", iso2c:="HK"]
informal08[Country=="Luxem..bourg", iso2c:="LU"]
informal08[Country=="Madagas..car", iso2c:="MG"]
informal08[Country=="Maurita..nia", iso2c:="MR"]
informal08[Country=="Mauritus", iso2c:="MU"]
informal08[Country=="Mozam..bique", iso2c:="MZ"]
informal08[Country=="Nether..lands", iso2c:="NL"]
informal08[Country=="Philip..pines", iso2c:="PH"]
informal08[Country=="Switzer..land", iso2c:="CH"]
informal08[Country=="United.Kingdom", iso2c:="GB"]
informal08[Country=="Azerbai..jan", iso2c:="AZ"]
informal08[Country=="Macedo..nia", iso2c:="MK"]
informal08[Country=="Quatar", iso2c:="QA"]
informal08[Country=="Kaza..khstan", iso2c:="KZ"]


save(informal08, file="ElginOztunaliInformal08.Rda")

##      Add Tax/GDP ratio
require(WDI)
taxvar <- WDIsearch("Tax revenue")[[4]]

##      Query WDI
taxgdp <- WDI(indicator=taxvar, start=2008, end=2008)
summary(taxgdp)
setnames(taxgdp, taxvar, "TaxGDP")
setDT(taxgdp)
setkey(taxgdp, iso2c)
setkey(informal08, iso2c)

##      Join
informal08 <- taxgdp[J(informal08)]
summary(informal08)
informal08[, i.year:=NULL]

##      Calculate the tax gap
informal08[,taxgap := TaxGDP*Informality/100]
setattr(informal08$taxgap, "var.label", "Tax Gap, i.e., uncollected revenue due to the informal economy, as a percent of GDP")

##      Drop missing data
informal08 <- informal08[!is.na(year) & !is.na(Informality)]
informal08[,Country:=NULL]

str(informal08)
summary(informal08)


##      Good results... keeps 110 observations in 2008.
##      With more time we could chase additional observations here.

save(informal08, file="ElginOztunaliInformal08.Rda")

## End of script