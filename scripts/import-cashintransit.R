## Ben Mazzotta
## CCGLOBAL Section 2: Cash in Transit
## September 30, 2014


# #####
# Goals
# 1. Estimate the number of bank branches in each country.
# 2. Calculate CIT per bank branch in the USA.
# 3. Estimate CIT costs worldwide from bank branches, adjusting for PPP price levels.


#     0. Preliminaries
require(WDI)


#     1. World Bank query

#       Goals: branches per capita, population, and ppp price level
v.bankbranches <- WDIsearch("branches")[1]
v.pricelevel <- WDIsearch("PPP conversion factor")[2,1]
v.population <- WDIsearch("Population, total")[1]
v.gdp <- WDIsearch("GDP, PPP")[1,1]


#       Make the dataset
branches <- WDI(country = "all",indicator = c(v.bankbranches,v.pricelevel,v.population, v.gdp), start = 2010, end=2012, extra=TRUE)
str(branches)
save(branches, file="bank branches with aggregates.Rda")

branches <- subset(branches, region !="Aggregates")
require(data.table)
branches <- data.table(branches)

#       Drop unused factor levels
branches[,iso3c :=factor(as.character(iso3c))]
branches[,country :=factor(as.character(country))]
branches[,iso2c :=factor(as.character(iso2c))]

names(branches)
setnames(branches, "FB.CBK.BRCH.P5", "branchdensity")
setnames(branches, "PA.NUS.PPPC.RF", "pricelevel")
setnames(branches, "SP.POP.TOTL", "population")
setnames(branches, "NY.GDP.MKTP.PP.CD", "gdp")
branches[,pop_MM := population/10^6]
branches[,banks := branchdensity * pop_MM * 10]

str(branches)

rm(list=c("v.bankbranches","v.population","v.pricelevel", "v.gdp"))


#     2. Calculate CIT per branch in the United States

#       Straight line estimate from 2007 to 2012
CIT.usa <- seq(2225, 2323, length.out=6)
attr(CIT.usa, "var.label") <- "CIT revenues in the United States in 2012, USD millions"

#     Annual observations
CIT.usa <- data.frame(c(2007:2012), CIT.usa)
names(CIT.usa) <- c("year","CIT")

CIT.usa$country <- "United States"
str(CIT.usa)

CIT.usa <- data.table(CIT.usa)
setkey(CIT.usa, country)

# branches[,CIT := NA]
# setkey(branches, country)
# branches["United States",]


#       Merge CIT and USA branches data.
temp <- merge(CIT.usa, branches, all.x=FALSE, by=c("country","year"))


##      How much GDP per bank branch?
lm1 <- lm(gdp/10^6 ~ banks+0, data=temp)
summary(lm1)

##      How much CIT per bank branch?
lm2 <- lm(CIT*10^6 ~ banks+0, data=temp)
summary(lm2)
rm(temp)

##      Globally, predict CIT_i

branches[,CIT_hat := predict(lm2, branches)]
summary(branches$CIT_hat * branches$pricelevel)
attr(branches$CIT_hat, "var.label") <- "National CIT revenues in 2012, USD millions"
branches[,CIT_hat := CIT_hat * pricelevel]

##      Test results
setkey(branches, CIT_hat)
head(branches[!is.na(CIT_hat) & year==2012,], 20)
tail(branches[!is.na(CIT_hat) & year==2012,], 20)


setkey(branches, banks)
head(branches[!is.na(CIT_hat) & year==2012,], 20)
tail(branches[!is.na(CIT_hat) & year==2012,], 20)

summary(branches)
names(branches)

## Don't need the linear models anymore
rm(lm1); rm(lm2)


##      Let's get estimated averages per country
setkey(branches, country, year)
branches[,CITest := mean(CIT_hat, na.rm=T), by=country]
summary(branches)


str(branches)
cashintransit <- branches[!is.na(CITest), .SD[which.max(year), ], by=country]
summary(cashintransit)

table(cashintransit$year)
with(cashintransit, table(is.na(CIT_hat), is.na(CITest)))

##        Some of these variables are distracting
names(cashintransit)
dropme <- list("branchdensity","longitude","latitude","capital")
lapply(dropme, function(x) cashintransit[,paste(x):=NULL])
names(cashintransit)
rm(dropme)


##        Save cashintransit
save(branches, file="Cash in transit multiyear.Rda")
save(cashintransit, file="Cash in transit.Rda")
save.image("CIT.Rdata")

tables()
