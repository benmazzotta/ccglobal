## Ben Mazzotta
## 10 year interest rates
## Cost of Cash Global

## 09 December 2014

# #####
# Goals
# 1. Import interest rate data from IFS.
# ~~2. Import interest rate data from World Bank.~~
# 3. Clean and summarize.

setwd("../data")
require(WDI); require(ggplot2); require(data.table)


# ###
# 1. Import IFS data

##        import the data
ifs <- read.delim("IFS interest rates.csv", sep=",", header=T)
str(ifs)

ifs <- data.table(ifs)
str(ifs); summary(ifs)

##        Rename for convenience
oldvars <- c("Interest.Rates..Government.Securities..Government.Bonds", "Interest.Rates..Government.Securities..Government.Bonds..Short.term", "Interest.Rates..Government.Securities..Government.Bonds..Medium..and.Long.term")
oldvars.rename <- c("interest", "int.short","int.long") 
setnames(ifs, oldvars, oldvars.rename)

##        Label variables
setattr(ifs$interest, "var.label", "Interest rate on government bonds, percent") 
setattr(ifs$int.short, "var.label", "Interest rate on government bonds, short term, percent") 
setattr(ifs$int.long, "var.label", "Interest rate on government bonds, medium and long term, percent") 
setattr(ifs, "data.source", "International Financial Statistics (IMF)")

##        Add ISO2C variable
require(countrycode)
ifs[,iso2c := countrycode(Country, origin="country.name", destination="iso2c")]

##        drop some temporary variables
rm(oldvars, oldvars.rename)



# #####
# 3. Clean and summarize

##        Take the average of the most recent three years interest rates
setkey(ifs, Country)

##        Evaluate most recent 3-year average
ifs[Time>2010,int.gov:=mean(interest, na.rm=T), by="Country"]


##        Evaluate period average
ifs[,int.gov2:=mean(interest, na.rm=T), by="Country"]
setattr(ifs@int.gov2, "var.label","Government bond interest rate, ten-year average, percent")

##        When most recent 3yy not available, set to period average
ifs[is.na(int.gov),int.gov:=int.gov2, by="Country"]

##        Surprise! That worked. Expected to see a lot of missing variables.
summary(ifs)

ifs[,int.short:=NULL]
ifs[,int.long:=NULL]
setattr(ifs$int.gov, "var.label", "Interest rate on government bonds, three-year average, percent")
table(ifs$Time)

##          Sort by country and year.
setkey(ifs, Country, Time)

##          Keep only the most recent observation

ifs <- ifs[ unique(ifs[,"Country", with=F])   ,mult="last"]

str(ifs)
summary(ifs)
save(ifs, file="IFS interest rates data.Rda")
  
# #####
# REMARKS
# 66 countries and 10 years reporting
# Full coverage is the norm

# 
# # #####
# # World Bank's 10 year indicator is discontinued.
# 
# intvar <- WDIsearch("10yr")
# intvar
# 
# ##          Import from World Bank
# int.10y <- WDI(indicator=intvar[1], start=2004, end=2013)
# summary(int.10y)
# 
# rm(intvar, int.10y)

# rm(ifs.copy, ifs)



