# #####
# Model HH time for cash transactions
# Ben MAzzotta
# December 2, 2014
# #####


# #####
# Goals.
# 1. Estimate daily time on travel
# 2. Estimate per-transaction time spent
# 3. Estimate national time spent on cash transactions
# 4. Evaluate that time at national median wage
# #####


##      0. Preliminaries
setwd("../data")
require(data.table); require(ggplot2); require(gdata)

load("householdtime.Rda")

##      1. Model daily time

lm.dailytravel <- lm(Travel ~ ATMd + RAInorm, data=hhtime)

# ###          Diagnostics
# par(mfrow=c(2,2))
# plot(lm.dailytravel)
# 
# test <- predict(lm.dailytravel, hhtime)
# str(test)
# table(is.na(test))
# with(hhtime, table(is.na(ATMd), is.na(RAInorm)))

##        Note to self: improve with multiple imputation here
hhtime[,est.day:= predict(lm.dailytravel,hhtime)]
summary(hhtime)

# ###       Outcome
# par(mfrow=c(1,2))
# boxplot(hhtime$est.day, ylim=c(25,70), main="Estimated daily travel", ylab="Minutes per day", sub="Source: Centre for Time Use Research")
# boxplot(hhtime$Travel, ylim=c(25,70), main="Reported daily travel", ylab="Minutes per day", sub="Source: Centre for Time Use Research")
# 
# par(mfrow=c(1,1))

##      2. Model time per transaction from IBGC original data

# ##      Silly toy data
# cashtime <- c(5,15,25)
# cashtime <- data.table(cashtime)
# cashtime[,Country:=c("United States","Mexico","India")]
# cashtime[,iso2c:=countrycode(Country, origin="country.name",destination="iso2c")]
# setkey(cashtime, iso2c)
# rm(cashtime)
# rm(lm.cashtime)

##        These are the mean transaction times from CCH USA, Mexico and India
load("cch-trxtimes.Rda")
setkey(trxtime, iso2c)

##          Merge transaction times from known countries with the rest.
test <- trxtime[hhtime]

##          Estimate the model of cash transaction time from total daily travel time.
lm.trxtime <- lm(trxtime~est.day, data=test)
summary(lm.trxtime)

##          Predict cash transaction times from daily travel time. 
hhtime[,est.ctime:=predict(lm.trxtime, test)]
boxplot(hhtime$est.ctime)
qplot(est.day, est.ctime, data=hhtime, geom="text", label=iso2c)

##          Throw away the training dataset
rm(test)
rm(trxtime)


save.image("working.Rdata")

##      #####
##      3. National transaction volume

load("remittances.Rda")
str(remit)
setkey(remit, iso2c)

##        Join the cash transactions field to the time per transaction
hhtime <- remit[,.SD, .SDcols=c("iso2c","cashTRX")][hhtime,]
summary(hhtime)


##        Now multiply time by transaction volume
hhtime[,timespent:=cashTRX * est.ctime /720]
attr(hhtime$timespent, "var.label") <- "National aggregate consumer time spent per month on cash transactions, (hours, millions)"

hhtime[Region=="MENA"]
hhtime[Region=="ECA"]
hhtime[,timepcap:=timespent/Population*1e6]
attr(hhtime$timepcap, "var.label") <- "National per capita consumer time spent per month on cash transaction, (hours)"

qplot(timespent, timepcap, data=hhtime, log="xy", geom="text", label=iso2c)+labs(title="Time spent on cash transactions", x="Aggregate monthly (millions)",y="Per capita, monthly")
qplot(timespent, timepcap, data=hhtime[Region=="AFR"], log="xy", geom="text", label=iso2c)+labs(title="Time spent on cash transactions \n Africa Region", x="Aggregate monthly (millions)",y="Per capita, monthly")
qplot(timespent, timepcap, data=hhtime[Region=="LAC"], log="xy", geom="text", label=iso2c)+labs(title="Time spent on cash transactions \n Latin America and Caribbean", x="Aggregate monthly (millions)",y="Per capita, monthly")
qplot(timespent, timepcap, data=hhtime[Region=="MENA"], log="xy", geom="text", label=iso2c)+labs(title="Time spent on cash transactions\n Mideast and North Africa", x="Aggregate monthly (millions)",y="Per capita, monthly")
qplot(timespent, timepcap, data=hhtime[Region=="EAP"], log="xy", geom="text", label=iso2c)+labs(title="Time spent on cash transactions\n East Asia and Pacific", x="Aggregate monthly (millions)",y="Per capita, monthly")
qplot(timespent, timepcap, data=hhtime[Region=="SAR"], log="xy", geom="text", label=iso2c)+labs(title="Time spent on cash transactions\n South Asia", x="Aggregate monthly (millions)",y="Per capita, monthly")

levels(hhtime$Region)


##      #####
##      4. Wages

load("wage average and medians.Rda")

tables()

wage.summary <- wage.summary[,.SD, .SDcols=c("iso2c","wageavg", "wagemed")]
setkey(wage.summary, iso2c)

hhtime <- wage.summary[hhtime]

attr(hhtime$wageavg, "var.label") <- "Average monthly wage in LCU"
attr(hhtime$wagemed, "var.label") <- "Median monthly wage in LCU"
attr(hhtime$Travel, "var.label") <- "Daily time spent on non-work travel per capita (minutes)"
attr(hhtime$RAI, "var.label") <- "RAI Rural Access Index"
attr(hhtime$RAInorm, "var.label") <- "Normalized RAI score"
attr(hhtime$est.day, "var.label") <- "Estimated daily time spent on nonwork travel"
attr(hhtime$est.ctime, "var.label") <- "Estimated time spent per cash transaction"
attr(hhtime$iso2c, "var.label") <- "ISO2C standard for two-letter country abbreviations"

save(hhtime, file="household time spent on cash transactions.Rda")

save.image("working.Rdata")
# save.image("archive 20141202.Rdata")

