### Learn plyr

## Problem: combine elements of an Amelia MI simulation
##      R-base would suggest transform to array.


# Create an array for fun

temp <- array (c(runif(25,0,1), runif(25,1,2), runif(25,2,3), runif(25,3,4), runif(25,4,5)), c(5,5,5))

# Apply() will sweep out means for every plane, using one single index.
apply(temp, 2, mean)

# Apply() will also sweep out means for any single dimension in the array, using a vector of two indices.
apply(temp, c(2,3), mean)
apply(temp, c(1,3), mean)
apply(temp, c(1,2), mean)

# Just to explain how this works, recall that the index vector for an array is row (d1), column (d2), slice (d3).
temp[,,1]


# So ... can we create an array from Amelia?
##        Crash and burn
##        Would do better just to pick out the columns I need and produce matrix averages

write.amelia(node3.mi, separate=FALSE, file="helloamelia", format="csv")

##    Then write a PivotTable
##    To do the same in R, open the concatenated CSV file and summarize numeric variables by country.
##        Recognizing that the variances are wrong.




## This gets you just the numeric values

temp <- laply(feeshat$imputations, function(x) x[,is.numeric(x), with=F])

indexnum <- sapply(feeshat$imputations[[1]], is.numeric)

temp <- feeshat$imputations$imp1[,.SD, .SDcols=names(feeshat$imputations$imp1)[indexnum]]
str(temp)

# And you can loop over the number of imputations


# This answer from stackexchange
###       It works on (africa) but not on (node3.mi)

require(plyr)
node3.ar <- llply(node3.mi$imputations, function(mydf) mydf[,sapply(mydf, is.numeric)]) 
#           llply(a.out$imputations, function(df) df[,sapply(df, is.numeric)])             
str(node3.ar$imp1)

temp <- node3.mi$imputations$imp1[,sapply(node3.mi$imputations$imp1, is.numeric)]
head(temp)

  ##      Then combine means with Reduce()
mean.mi <- Reduce("+", node3.ar)/length(node3.ar)
names(mean.mi)
str(mean.mi)



## Example from SE

library(Amelia)
data(africa)

# carry out imputations
a.out      = amelia(x = africa, cs = "country", ts = "year", logs = "gdp_pc") 

# extract numeric columns from each element of a.out$impuations  
tst2       = llply(a.out$imputations, function(df) df[,sapply(df, is.numeric)]) 
str(tst2) ; str(a.out$imputations)
# sum them up and divide by length to get mean
mean.right = Reduce("+", tst2)/length(tst2)

# compute fixed columns and cbind with mean.right
left.side  = a.out$imputations[[1]][1:2]
mean0      = cbind(left.side,mean.right)

str(mean.right)
str(left.side)
str(mean0)
