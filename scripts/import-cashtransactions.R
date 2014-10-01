# Cash transactions by country
# Ben Mazzotta
# 1 October 2014

# #####
# Goals: 
#   Ultimately: Estimate the household cost of cash transactions by country from World Bank Data.
#   ...
#   1. Import financial access data (incidence of payments and remittances)
#   2. Import remittances total transactions
#   3. Derive payments multiplier for remittances
#   4. Model payments received from payment and remittance prevalence, remittance TRX
#   5. Derive per-TRX price of cash access from remittances data
#   6. Estimate aggregate national cost of cash access from modeled cash TRX and fees.


#       0. Preliminaries
require(WDI); require(data.table); require(reshape)

#       1. Import GFDD data from 2011

fincl <- data.table(read.delim("gfdd_active_accounts.csv", sep=",", header=T, na=".."))
str(fincl)

levels(fincl$Series.Name)
setnames(fincl, "X2011","Value")
table(is.na(fincl$Value))


levels(fincl$Series.Name) <- c("rec_paymt", "rec_remit", "rec_wage", "acc_active")

fincl <- cast(data=fincl, id.vars=c("Country.Name","Country.Code"), Country.Name + Country.Code ~ Series.Name)
View(fincl)

#       2. Import remittances received total

var.remittances <- WDIsearch("Remittances and compensation")[2,1]
remit <- WDI(indicator = "BX.TRF.PWKR.CD.DT", country = "all", start = 2011, end=2011, extra=TRUE)
setnames(remit, var.remittances, "remittances")
remit <- subset(remit, region!="Aggregates")
names(remit)
remit <- remit[,c("country","remittances","iso3c","iso2c", "region","income","lending", "year")]
remit <- data.table(remit)


summary(remit)
rownames(remit)

remit[,remit_MM := remittances/10^6]
setkey(remit, remittances, country)
tail(remit, 20)

rm(var.remittances)

##        Median remittance size is $200 in Freund and Spatafora (2008) Journal of Dev Econ

remit[,remit_TRX := remit_MM/200]

summary(remit)

##        Derive payments multiplier

fincl <- data.table(fincl)
names(fincl)
fincl[,payXrem := pmax(rec_paymt, rec_wage)/rec_remit]
summary(fincl)

##        Fincl names must match remit names
setnames(fincl, "Country.Name", "country")
setnames(fincl, "Country.Code", "wb3c")

##      Prepare to merge
setkey(fincl, country); setkey(remit, country)

##      Join fincl data to remit data
remit <- remit[J(fincl[,.SD, .SDcols=c("country","payXrem")]),]
str(remit)
summary(remit)
# ##      Are missing observations identical? Yes.
# identical(fincl[is.na(payXrem), as.character(country)], remit[is.na(payXrem), country])

tables()

##      Estimate cashout transactions

remit[,cashTRX := remit_TRX * payXrem]
summary(remit)

attr(remit$cashTRX, "var.label") <- "Millions of cash-out transactions per annum, modeled from remittances and financial access data."
attr(remit$remit_MM, "var.label") <- "Value of remittances received per annum, millions of current USD"
attr(remit$remit_TRX, "var.label") <- "Volume of remittances received per annum, millions of transactions"
attr(remit$payXrem, "var.label") <- "Multiplier used to estimate cash-out transactions from remittances received"

str(remit)


#       What about cash-out prices? This we really don't have a direct measure of.
#         Putting a finger in the wind.
#         World Bank RPW dataset reports total remittance price including fees and forex.

rpw <- data.table(read.delim(file="rpw_dataset_2011_2014_q2.csv", header=T, sep=","))
rpw[,year:= as.numeric(substr(period, 1,4))]
rpw[,price:= cc1.total.cost../2]
rpw[,iso3c:=countrycode(sourcevar=destination, origin="wb",destination="iso3c")]
rpw[,dyad:=paste(source, destination, sep="_")]

rpw11 <- rpw[year==2011, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]
rpw12 <- rpw[year==2012, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]
rpw13 <- rpw[year==2013, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]
rpw14 <- rpw[year==2014, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]

str(rpw11)

##        Many observations per corridor

rpw11[,destpriceavg := mean(price, na.rm=T), by=destination]

rpw11_avgrec <- rpw11[,.SD, .SDcols=c("destination","destpriceavg","iso3c")]
setkey(rpw11_avgrec,destination)
rpw11_avgrec <- unique(rpw11_avgrec)

tables()

save(rpw, file= "remittance prices worldwide.Rda")
save(rpw11, file= "remittance prices worldwide 2011.Rda")
save(rpw12, file= "remittance prices worldwide 2012.Rda")
save(rpw13, file= "remittance prices worldwide 2013.Rda")
save(rpw14, file= "remittance prices worldwide 2014.Rda")
save(rpw11_avgrec, file="remittance prices avg by dest 2011.Rda")

rm(list=c("rpw", "rpw11", "rpw12","rpw13","rpw14"))

#       Save data
save(fincl, file="national payments received via GFDD.Rda")
save(remit, file="national remittances value and volume.Rda")


##      This is where the join started to break down. Troubleshot everything above.
##          RESUME HERE 2014 10 01

View(rpw11_avgrec)
str(remit); str(rpw11_avgrec)
remit[,newiso3c := as.character(iso3c)]
rpw11_avgrec[,newiso3c := as.character(destination)]

setkey(remit, newiso3c); setkey(rpw11_avgrec, newiso3c)
rm(temp)
temp <- rpw11_avgrec[J(remit)]
setkey(temp, country, year)
str(temp)
View(temp)

View(remit); View(rpw11_avgrec)

save.image("remittances.Rdata")
save.image("archive 20141001.Rdata")
