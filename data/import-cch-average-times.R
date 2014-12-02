# #####
# Average Transaction Times in United States, India and Mexico
# Ben Mazzotta 
# 2 December 2014
# #####


# #####
# Goal:
#   1. Estimate queue time per cash transaction in USA, MEX, and IND.
# 


require(data.table)

##      Mexico average is 17.469 minutes per cash transaction
##      USA average is 7.342 minutes per cash transaction

##      India needs to be calculated
load("diary.India.Rda")
india.dt <- diary.dt; rm(diary.dt)
names(india.dt)
indiatime <- c(india.dt$cd1_time, india.dt$cd2_time, india.dt$cd3_time, india.dt$cd4_time, india.dt$cd5_time)
str(indiatime)
mean(indiatime)

trxtime <- c(mean(indiatime), 17.469, 7.342)
trxtime <- data.table(trxtime)
trxtime[,iso2c:=c("IN","MX","US")]

save(trxtime, file="cch-trxtimes.Rda")

