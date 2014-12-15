# #####
# Yan Bai Seigniorage Data
# Ben Mazzotta
# 2014 12 15
# #####


# ### 
# 1. Import the data

require("data.table"); require("countrycode"); require("WDI"); require("ggplot2")

##        Original data is in CSV
bai <- read.csv(file="seigniorage_bai.csv", header=T, na.strings="na", encoding="latin1", stringsAsFactors=F, nrow=70, row.names=1)
str(bai)
summary(bai)

##        New data table format
bai <- data.table(bai)

##        Rename for convenience
setnames(bai, "Interests_IFS", "interest.ifs")
setnames(bai, "Countries","Country")
bai[,iso2c:=countrycode(Country, origin="country.name",destination="iso2c")]

##        Quote interest rates in percent
bai[,interest.ifs := interest.ifs*100]
bai[,Coupon_Bloomberg := Coupon_Bloomberg*100]
bai[,Interests.applied := Interests.applied*100]
summary(bai)

##        Label variables
varlabs <- c("ISO3C","Country Name","Euro zone member, TRUE if so", "IFS Interest Rate","Bloomberg coupon rate on sovereign debt","Applied interest rate",
             "Monetary base, millions of LCU", "Currency in circulatoin, millions of LCU","M0 base money, millions of LCU",
             "Seigniorage income in millions of LCU", "Exchange rate, LCU per dollar","Seigniorage income in millions of USD")
for (i in c(1:length(varlabs))) {
  setattr(bai[[i]], "var.label", varlabs[i])
  }

str(bai)


save(bai, file="seigniorage_bai.Rda")
save.image("working.Rdata")
# save.image("archive 2014 12 15.Rdata")

# ###
# End of import script