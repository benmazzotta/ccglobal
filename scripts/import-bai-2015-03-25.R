# #####
# Import Yan Bai Seigniorage and Currency Issuance Data 2014 12 09
# Ben Mazzotta
# March 25, 2015
# #####


# #####
# Goals
# 
# 1. Import Yan Bai's data
# 2. Label key variables
# 3. Prepare for merge with Cost of Cash Global Dataset


# ###
# 0. Prelims

require(data.table); require(WDI); require(countrycode)

# ###
# 1. Load the data

seign <- read.csv("bai.2014.12.09.csv", header=TRUE, skip=1, stringsAsFactors=F, na.strings=c("na","NA","#N/A"), fileEncoding="latin1")[1:103,-c(1:2)]
names(seign)
str(seign)
summary(seign)


setDT(seign)
seign[,iso2c:= countrycode(ABB, origin="wb", destination="iso2c")]
table(is.na(seign$iso2c))
seign$iso2c


##    Rename key variables
oldvars <- c("ABB","Countries","EU..y.n.","Interests.applied","CURRIC..MN.","Seigniorage..USD.MN.","Percentage.of.GDP","lg.Seigniorage.","Seinarage.per.capita")
newvars <- c("wb3c","Country","EUMember","iCarry","currcirc","seigniorage","seignGDP","seignLOG","seignCAP")
setnames(seign, oldvars, newvars)


##    Label key variables
setattr(seign$EUMember, "var.label", "Yes if member of the EMU; no otherwise.") 
setattr(seign$iCarry, "var.label", "Interest rate applied to sovereign bond holdings on central bank balance sheet.") 
setattr(seign$currcirc, "var.label", "Currency in circulation as of end FY 2013.") 
setattr(seign$seigniorage, "var.label", "Seigniorage revenue per annum, USD millions.") 
setattr(seign$seignGDP, "var.label", "Seigniorage to GDP ratio.") 
setattr(seign$seignLOG, "var.label", "Natural logarithm of seigniorage revenue.") 
setattr(seign$seignCAP, "var.label", "Per capita seigniorage revenue.") 


seign[,EUMember := EUMember=="y"]
seign[,currcirc := as.numeric(currcirc)]
seign[,MBase..MN. := as.numeric(MBase..MN.)]
seign[,M1..MN. := as.numeric(M1..MN.)]
seign[,Currency.base := as.numeric(Currency.base)]
seign[,Population := as.numeric(Population)]
seign[,Exchange := as.numeric(Exchange)]
seign[,GDP.per.Capita := as.numeric(GDP.per.Capita)]
seign[,Interests_IFS := as.numeric(Interests_IFS)]
seign[,X2013.GDP..USD.MN. := as.numeric(X2013.GDP..USD.MN.)]

## Verify everything worked
str(seign)
summary(seign)

##    Save to disk
save(seign, file="bai.seigniorage.full.Rda")
seign <- seign[,.SD, .SDcols=c("iso2c",newvars)]
save(seign, file="bai.seigniorage.merge.Rda")


##    Clean up
rm(oldvars, newvars)


# ###
# 3. Load Currency Issuance Data

currissue <- read.csv("bai.currissuance.2014.12.29.csv", header=TRUE, fileEncoding="latin1", stringsAsFactors=F)
setDT(currissue)

oldvars <- c("ABB","Countries","EU..y.n.","Banknotes.production.costs..LCUk.","Exchange.rate","Banknotes.production.costs..USDm.","GDP..USDbn.","lg.GDP.","lg.CPC.","GDP.per.capita","lg.GDP.per.Capita.","Currency.base")
newvars <- c("wb3c","Country","EUMember","CurrIssueLCU","ForexRate","CurrIssue","GDPUSD","GDPln","CurrIssueln","GDPCAP","GDPlnCAP","CurrBase")
#   data.frame(oldvars, newvars)

labvars <- c("World Bank Standard Abbreviation","World Bank Standard Name","TRUE if the country belongs to Europe's EMU","Cost of Currency Issuance in LCU","Foreign Exchange Rate","Currency Issuance Cost in USD MM", "GDP in current USD","Logarithm of Currency Issuance Costs (USD)","GDP per capita at PPP","Logarithm of GDP per capita at PPP", "Currency Base")


##    Apply variable labels with description and units
setnames(currissue, oldvars, newvars)
for(i in 1:length(currissue)) {
  setattr(currissue[[i]], "var.label", labvars[i])
}

currissue[,EUMember := EUMember=="y"]
currissue[,CurrBase := as.numeric(CurrBase)]
currissue[,iso2c := countrycode(wb3c, origin="wb",destination="iso2c")]


##    Verify structure and summary data
str(currissue)
summary(currissue)


##    Save the complete dataset
save(currissue, file="bai.currissuance.Rda")

##    Save with only merge-ready columns
mergevars <- c("iso2c","EUMember","CurrIssueLCU","CurrIssue")
currissue <- currissue[,.SD, .SDcols=mergevars]

save(currissue, file="bai.currissuance.merge.Rda")


rm(i, labvars, mergevars, newvars, oldvars)


## End of Script