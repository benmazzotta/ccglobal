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

##      First impute GDP 

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


##      Import the remittances data
load(file="remittances.Rda")

wdi <- data.table(incomepop)
# setkey(remit, country, year)
setkey(wdi, country, year)
wdi[,iso3c := as.character(iso3c)]
tables()

remit_imp <- remit[J(wdi[year==2011,])]
tables()

#           What did we find?
summary(remit_imp)

#           How much missing data?
sapply(remit_imp, function (x) sum(is.na(x)))

#           Remove id variables duplicated in merge; income, region, lending, destination, iso2c, iso3c 
remit_imp[, income.1 := NULL]; remit_imp[, region.1 := NULL]; remit_imp[, lending.1 := NULL]; remit_imp[, destination := NULL]; remit_imp[, iso2c.1 := NULL]; remit_imp[, iso3c.1 := NULL]

#           Remove countries missing from the remittances matrix
remit_imp <- remit_imp[!is.na(iso3c),]

#           Remove destpriceavg and cashfees for sparsity
remit_imp[, destpriceavg:=NULL]; remit_imp[, cashfees:=NULL]

#           Take stock
str(remit_imp)


##          Take the plunge
##          Impute all the remittances data... nice try.
# remit_fit <- amelia(remit_imp, cs=c("country"), log=c("gdp", "pop","gdpcap","gdpreal"), noms = NULL,idvars = c("iso2c","iso3c", "year", "region","income","lending"), m = 3)

### That took several minutes and did not converge.


## What about a single variable.
##        NB: Amelia did not like collinear GDP variables.


##          This was no problem ... but the next wasn't.
remit_fit <- amelia(remit_imp[,.SD, .SDcols=c("country","gdpcap","pop","remit_MM","remit_TRX")], log=c("gdpcap","pop"),   idvars=c("country"), m=3)
overimpute(remit_fit, "remit_MM")
overimpute(remit_fit, "remit_TRX")


# ##        Keeps crashing due to failure of inversion matrix.
# ##            I am setting a stopping point.
# save.image('working.Rdata')

##          Pretty much failed to converge with sparse observations of payXrem and cashTRX
##                66 missing payXrem; 85 missing cashTRX; but also 51 with the others...
remit_fit <- amelia(remit_imp[,.SD, .SDcols=c("country","gdpcap","pop", "cashTRX")], log=c("gdpcap","pop"),  idvars=c("country"), m=3)
overimpute(remit_fit, "cashTRX")

remit_fit <- amelia(remit_imp[,.SD, .SDcols=c("country","gdpcap","pop","remit_MM","remit_TRX", "payXrem","cashTRX")],
                    log=c("gdpcap","pop"),
                    idvars=c("country"), m=3, empri=50, parallel="multicore")


# Takeaway: you really can't expect this to work with a large number of simultaneous imputations.

##          Thought experiment: we could include some more fincl covariates and perhaps do better.



### Scratch pad

# summary(remit_imp$destpriceavg)
# qplot(year, destpriceavg, data=remit_imp, geom="boxplot")+coord_flip()
