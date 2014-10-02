##    Multiple imputation for missing data on cash prices.
##    Ben Mazzotta
##    Cost of Cash Global
##    10/1/2014


# #####
# Goals
# 1. Begin with remittances dataset and financial inclusion covariates.
# 2. Impute prices for cashout transactions where unavailable.
# 3. Impute rates for caashout transactions where unavailable.
# 4. Model national costs of cashout transactions from combined dataset, i.e., imputed and reported.


#    0. Preliminaries
# setwd("../data")
require(Amelia); require(data.table); require(ggplot2)
load("worldbank countries.Rda")

str(worldbank.countries)
setnames(worldbank.countries, "NY.GDP.MKTP.PP.CD", "gdp")
setnames(worldbank.countries, "NY.GDP.MKTP.PP.KD", "gdpreal")
setnames(worldbank.countries, "NY.GDP.PCAP.PP.CD", "gdpcap")
setnames(worldbank.countries, "SP.POP.TOTL", "pop")


incomepop <- worldbank.countries[,c("country","year","iso3c","iso2c","income","region","lending","gdp","gdpreal","gdpcap","pop")]
save(incomepop, file="worldbank income and population.Rda")
rm(worldbank.countries)

str(incomepop)
summary(incomepop)

##      First impute GDP to see whether that works.

impute01 <- amelia(incomepop, ts="year",cs="country", id=c("iso3c","iso2c"), noms = c("region","lending","income"), m=12,logs = c("gdp","gdpreal", "gdpcap"))

# ##        Worked very quickly.
# str(impute01)
# summary(impute01)

##      How did the imputed gdpcap datasets compare to the original?
qplot(1, gdpreal, data=incomepop, geom="boxplot", log="y")+coord_flip()
qplot(1, gdpreal, data=impute01$imputations[[1]], geom="boxplot", log="y")+coord_flip()
qplot(1, gdpreal, data=impute01$imputations[[2]], geom="boxplot", log="y")+coord_flip()


##      Diagnostics
plot(impute01, vars=c("gdp","gdpcap","gdpreal"), log="x")
overimpute(impute01, var = "gdpcap")
