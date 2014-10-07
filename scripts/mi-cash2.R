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

##      First impute Node 1: parameters nu and rho 

load("national payments received via GFDD.Rda")
str(fincl); summary(fincl)
fincl[,country:= as.character(country)]


### 
fincl[,rec_paymt := rec_paymt/100+.000001]
fincl[,rec_remit := rec_remit/100+.000001]
fincl[,rec_wage := rec_wage/100]
fincl[,acc_active := acc_active/100]


setkey(fincl, country)
tables()

##        Join financial inclusion data to WDI
##          Subset of WDI: year 2011; and no missing gdp/capita 
node1 <- fincl[J(subset(incomepop, year==2011 & !is.na(gdpcap)))]
str(node1); summary(node1)

##        Impute financial inclusion from WDI covariates
node1.mi <- amelia(node1, cs=c("country"), id=c("iso2c","iso3c","wb3c","lending", "gdpreal","gdp","year","payXrem"), noms = c("region","income"), lgstc = c("rec_paymt","rec_remit","rec_wage","acc_active"), logs=c("gdpcap","pop"), empri=5, m=12)

##        This imputed data is the first node in the model. 
##          Directly includes _nu_, noncash payments incidence, and 
##                            _rho_, remittances incidence
save(node1.mi, file="mi_cashnode1_01.Rda")

# #####
# Begin node 2

load(file="national remittances value and volume.Rda")
names(remit)

remit <- remit[,.SD, .SDcols=c("country","year","remit_MM","remit_TRX")]
setkey(remit, country, year) ; setkey(node1, country, year)
node2 <- node1[J(remit)]


# ##          How prevalent is GDP w/o remittances data? Vice versa?
# with(node2, table(is.na(gdp), is.na(remit_MM)))
# with(node2, table(is.na(gdp), is.na(remit_TRX)))

##        Impute for nodes 1 and 2 together
##              I didn't expect this would work simply.
node2.mi <- amelia(node2, cs=c("country"), id=c("iso2c","iso3c","wb3c","lending", "gdpreal","gdp","year","payXrem"), noms = c("region","income"), lgstc = c("rec_paymt","rec_remit","rec_wage","acc_active"), logs=c("gdpcap","pop"), empri=5, m=12)


##      Export to disk
save(node2.mi, file="mi_cashnode2_01.Rda")

unique(node1$country)
unique(node2$country)
setdiff(node2$country, node1$country)

names(node2)
str(node1);summary(node1)
str(node2);summary(node2)

subset(node2, is.na(gdp))


# #####
# Include remittances prices

# New object is rpw11_avgrec
load("C:/Users/bmazzo01/Rstats/ccglobal/data/remittance prices avg by dest 2011.Rda")
names(rpw11_avgrec)

tables()

setkey(node2, iso3c); setkey(rpw11_avgrec, iso3c)
node3 <- rpw11_avgrec[J(node2)]
dim(node3)
names(node3)
node3[,destination:= NULL]
setnames(node3, "destpriceavg","cashprice")

setkey(node3, country, year)

##      As before: keep all the variables. Impute simultaneously.
##          This fails to converge w/o parameter "empri."
##          ...but converges with any positive integer value of empri 
node3.mi <- amelia(node3, cs=c("country"), id=c("iso2c","iso3c","wb3c","lending", "gdpreal","gdp","year","payXrem"), noms = c("region","income"), lgstc = c("rec_paymt","rec_remit","rec_wage","acc_active"), logs=c("gdpcap","pop", "remit_MM","remit_TRX"), empri=1, m=12)

##          Notes on form:
##              non-negative support: remit_TRX, remit_MM, cashprice
##              Need to include logical boundaries for that.
##              Using logs instead of formal zero lower bound.


missmap(node3.mi)

# ###
# Assess 

overimpute(output = node3.mi, var = "remit_TRX")
plot(node3.mi, which.vars=c("remit_MM","remit_TRX", "cashprice"),log="x")

summary(node3.mi)

# #####
# Predict cash fees

##      What's in the node3.mi dataset?
##      rho     rec_remit
##      nu      rec_paymt
##      X_rem   remit_TRX
##      p_cash  cashprice

##      Create feeshat
##            This calculates fees directly from imputed inputs
feeshat <- transform(node3.mi, cashfees = rec_paymt / rec_remit * remit_TRX * cashprice)

##      OK, now you still have to fit a linear model.
##            And it looks like that is done independently for each imputation
fit_node03_01 <- zelig(data=feeshat, cashfees ~ rec_paymt / rec_remit * remit_TRX * cashprice, model="ls")
fit_node03_02 <- zelig(data=feeshat, log(cashfees) ~ log(rec_paymt) -log(rec_remit) + log(remit_TRX) + log(cashprice), 
                       model="ls")

##      Summarize the fit
##            First OLS model includes interaction terms. No statistical power. 
summary(fit_node03_01)
##            Second model (log-log OLS) does not. And it performs perfectly.
##                Not inspiring, given that the data is a noiseless transformation of the data.
summary(fit_node03_02)



save.image("cashfees_miviaAmelia01.Rdata")

# ### 
# Model

#       cashfees = nu / rho * X_rem * p_cash

str(feeshat[12])


### End of script