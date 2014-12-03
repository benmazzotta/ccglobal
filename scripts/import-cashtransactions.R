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

##          Data are in long format with variable names in a column. Replace levels with convenient names.
levels(fincl$Series.Name) <- c("rec_paymt", "rec_remit", "rec_wage", "acc_active")

##          Reshape the data into wide format.
fincl <- cast(data=fincl, id.vars=c("Country.Name","Country.Code"), Country.Name + Country.Code ~ Series.Name)
View(fincl)

#       2. Import remittances received total

##          Search for the remittances variable in WDI
var.remittances <- WDIsearch("Remittances and compensation")[2,1]
remit <- WDI(indicator = "BX.TRF.PWKR.CD.DT", country = "all", start = 2011, end=2011, extra=TRUE)
setnames(remit, var.remittances, "remittances")
remit <- subset(remit, region!="Aggregates")
names(remit)

##          Drop lat/long, capitals from WDI extra fields
remit <- remit[,c("country","remittances","iso3c","iso2c", "region","income","lending", "year")]
remit <- data.table(remit)


summary(remit)
rownames(remit)

##          Convenient to report in millions
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
rpw[,iso3c:=destination]
rpw[,dyad:=paste(source, destination, sep="_")]

##      Which cases matter for cash withdrawal?
str(rpw$pick.up.method)
cashmethods <- grep("Cash", levels(rpw$pick.up.method), value=T)
sum(rpw$pick.up.method %in% cashmethods)

rpw[,cashout:= pick.up.method %in% cashmethods]

rpw11 <- rpw[year==2011 & cashout==T, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]
rpw12 <- rpw[year==2012 & cashout==T, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]
rpw13 <- rpw[year==2013 & cashout==T, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]
rpw14 <- rpw[year==2014 & cashout==T, .SD, .SDcols=c("source","destination","price","iso3c", "dyad")]

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

# View(rpw11_avgrec)
# str(remit); str(rpw11_avgrec)
summary(remit)
summary(fincl)
summary(rpw11_avgrec)

##        Drop Cape Verde observation from remit.dt
remit<- remit["country" !="Cape Verde",]
# 
# ##        Joins failed because "destination" and "iso3c" used different factor variables for the same country codes.
remit[,iso3c := as.character(iso3c)]
rpw11_avgrec[,iso3c := as.character(iso3c)]

sum(duplicated(remit$iso3c))
sum(duplicated(rpw11_avgrec$iso3c))
sum(is.na(remit$iso3c))
sum(is.na(rpw11_avgrec$iso3c))

# remit[is.na(iso3c),]
# View(remit)

setkey(remit, iso3c); setkey(rpw11_avgrec, iso3c)
# rm(temp)

##          Merge remittance prices with transaaction volume estimates
remit <- rpw11_avgrec[remit,]
# summary(remit)


###      You can do the same in {base} but why bother.
# remit <- merge(remit, rpw11_avgrec, by="iso3c", all.x=TRUE)
# setkey(remit, country, year)
# str(remit)

##        Now calculate fees = cashTRX * destpriceavg

remit[,cashfees := cashTRX * destpriceavg]
summary(remit$cashfees)
attr(remit$cashfees, "var.label") <- "Aggregate fees on cash out transactions, millions of USD"

##          But 131 missing obs of 202 possible. Only 114 lack price data and only 74 lack TRX volume;.
summary(remit$cashfees)

cat("\n *** \n These countries are missing transaction VOLUME.")
remit[is.na(cashTRX), unique(country)]

cat("\n *** \n These countries are missing transaction PRICES.")
remit[is.na(destpriceavg), unique(country)]

cat("\n *** \n These countries have complete VOLUME AND PRICES.")
remit[!is.na(cashfees), unique(country)]



qplot(cashTRX, cashfees, data=remit, label=iso3c, geom="text", log="xy")
qplot(payXrem, data=remit, stat="bin")
qplot(cashTRX, data=remit, stat="bin")
qplot(destpriceavg, data=remit, stat="bin")


##        This is a second approach.

##        price.send = average price of cashout transactions conditional on origin country
##        price.rec = average price of cashout transactions conditional on destination country



##        Calculate the average price to send.
##            filter cashout transactions; take the average by source country
rpsend <- rpw[cashout==T,mean(cc1.total.cost.., na.rm=T), by=source]
rpsend[,source:=as.character(source)]
str(rpsend)
##            rename
setnames(rpsend, "source","country.send")
##            DEFINE; price.send is in USD (notional $200, total cost reported in percent)
rpsend <- rpsend[,price.send:=2*V1][,.SD,.SDcols=c("country.send","price.send")]
summary(rpsend)
attr(rpsend$price.send, "var.label") <- "Price to send a $200 transfer including fees and forex, in USD"

cat("\n ##### \n This is good. There are 32 countries that originate transactions cashed out.")
length(unique(rpw[cashout==T,as.character(source)]))



###       Calculate price to receive.
rpdest <- rpw[cashout==T, mean(cc1.total.cost.., na.rm=T), by=destination]
rpdest[,destination:=as.character(destination)]
str(rpdest)
##            rename
setnames(rpdest, "destination","country.dest")
##            DEFINE: price.rec is in USD (notional $200, total costreported in percent)
rpdest <- rpdest[,price.dest:=2*V1][,.SD, .SDcols=c("country.dest", "price.dest")]
summary(rpdest)
attr(rpdest$price.dest, "var.label") <- "Price to receive a $200 transfer including fees and forex, in USD"

##            Check for overlap; there are 9. Prefer the sender's rate.
collisions <- intersect(rpdest$country.dest, rpsend$country.send)
##            Write summaries to disk separately for origin and destination countries.
save(rpsend, file="Remittance price summary by source country.Rda")
save(rpdest, file="Remittance price summary by destination country.Rda")

# ##            Duplicates remain. 
# fixme <- data.table(collisions, rpdest[collisions,], rpsend[collisions,])
# rm(fixme, collisions)


##            Merge the sending and receiving data together with rbind()
setnames(rpsend, names(rpsend), c("iso3c","price"))
setnames(rpdest, names(rpdest), c("iso3c","price"))
# str(rbind(rpsend,rpdest))
remprice <- rbind(rpsend,rpdest[!collisions,])


setkey(remprice,iso3c)
remprice[collisions,]

##            _remprice_ is a data table with cash remittance prices averaged 2011-2014
str(remprice)
save(remprice, file="Remittance prices worldwide global summary.Rda")

##            clean up
rm(rpsend, rpdest, collisions)
rm(rpw, rpw11_avgrec)



##          Merge with the remittances data
remit[,iso3c:=as.character(iso3c)]
setkey(remprice, iso3c); setkey(remit, iso3c)
tables()

remit <- remprice[remit,]


##        There are 171 missing values for destpriceavg. 
##        Need to impute some data for the other cases. 
##        Impute from .... (1) financial access, (2) ATM density, (3) gdp/cap, (4) ppp price level
summary(remit)

save(remit, file="remittances.Rda")

save.image("remittances.Rdata")
# save.image("archive 20141001.Rdata")
# save.image("archive 20141203.Rdata")
