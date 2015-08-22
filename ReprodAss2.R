#load necessary files
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')#load knitr library
library(data.table)#load data table library
library(ggplot2) # we shall use ggplot2 for plotting figures
library(zoo)#for filling in missing values
#read files
file_location <- "C:/Users/PeterYvonne/Documents/ReprodAss2/ReprodAss2/repdata-data-StormData.csv.bz2"
data <- read.csv(file_location)
save(data, file="oridata.saved")#toprevent long loading of original data should there be a need to rollback
data$EVTYPE=toupper(data$EVTYPE) #set column headers to uppercase
head(data)#show first several rows of a dataset
#checks for missing value
is.na(data)#check for missing data
newmeandata=na.aggregate(data)#replace missing data with mean 
is.na(newmeandata)#check for missing data
#checks for injuries
injury <- aggregate(INJURIES ~ EVTYPE, data = data, sum)
actualinjury <- injury[injury$INJURIES > 0, ]#events which caused at least one injury
orderedinjury<-actualinjury[order(actualinjury$INJURIES, decreasing = TRUE), ] 
head(orderedinjury)
#check for injuries using meanfilled data
injury <- aggregate(INJURIES ~ EVTYPE, data = newmeandata, sum)
actualinjury <- injury[injury$INJURIES > 0, ]#events which caused at least one injury
neworderedinjury<-actualinjury[order(actualinjury$INJURIES, decreasing = TRUE), ]
head(neworderedinjury)
#check for deaths
fatality <- aggregate(FATALITIES ~ EVTYPE, data = data, sum)
actualfatality <- fatality[fatality$FATALITIES > 0, ]#events which caused at least one injury
orderedfatality <-actualfatality[order(actualfatality$FATALITIES, decreasing = TRUE), ] 
head(orderedfatality)
#plot 2 panel graph to compare injuries and fatalities
par(mfrow = c(2, 1))
barplot(orderedinjury[1:10, 2], col = rainbow(10), legend.text = orderedinjury[1:10, 
                                                                         1], ylab = "Injuries", main = "10 natural events cause most injuries")
barplot(orderedfatality[1:10, 2], col = rainbow(10), legend.text = orderedfatality[1:10, 
                                                                           1], ylab = "Fatalities", main = "10 natural events that causes the most fatalities")
intersect(orderedfatality[1:10, 1], orderedinjury[1:10, 1])#which event causes the most injuries and fatalities
#scale damages to multiples of 1 USD
data[data$PROPDMGEXP == "K", ]$PROPDMG <- data[data$PROPDMGEXP == "K", ]$PROPDMG * 
  1000
data[data$PROPDMGEXP == "M", ]$PROPDMG <- data[data$PROPDMGEXP == "M", ]$PROPDMG * 
  1e+06
data[data$PROPDMGEXP == "m", ]$PROPDMG <- data[data$PROPDMGEXP == "m", ]$PROPDMG * 
  1e+06
data[data$PROPDMGEXP == "B", ]$PROPDMG <- data[data$PROPDMGEXP == "B", ]$PROPDMG * 
  1e+09
data[data$CROPDMGEXP == "K", ]$CROPDMG <- data[data$CROPDMGEXP == "K", ]$CROPDMG * 
  1000
data[data$CROPDMGEXP == "k", ]$CROPDMG <- data[data$CROPDMGEXP == "k", ]$CROPDMG * 
  1000
data[data$CROPDMGEXP == "M", ]$CROPDMG <- data[data$CROPDMGEXP == "M", ]$CROPDMG * 
  1e+06
data[data$CROPDMGEXP == "m", ]$CROPDMG <- data[data$CROPDMGEXP == "m", ]$CROPDMG * 
  1e+06
data[data$CROPDMGEXP == "B", ]$CROPDMG <- data[data$CROPDMGEXP == "B", ]$CROPDMG * 
  1e+09
#aggregate damage caused in USD to property
damage <- aggregate(PROPDMG ~ EVTYPE, data = data, sum)
actualdamage <- damage[damage$PROPDMG > 0, ]#events which caused at least one USD worth of damage
ordereddamage<-actualdamage[order(actualdamage$PROPDMG, decreasing = TRUE), ] 
head(ordereddamage)
#aggregate damage caused in USD to crops
damagecrop <- aggregate(CROPDMG ~ EVTYPE, data = data, sum)
actualdamagecrop <- damagecrop[damagecrop$CROPDMG > 0, ]#events which caused at least one USD worth of damage
ordereddamagecrop<-actualdamagecrop[order(actualdamagecrop$CROPDMG, decreasing = TRUE), ] 
head(ordereddamagecrop)
#plot 2 panel graph of damages to property and crops
par(mfrow = c(2, 1))
barplot(ordereddamage[1:10, 2], col = rainbow(10), legend.text = ordereddamage[1:10, 
                                                                           1], ylab = "Property damage", main = "10 natural events caused most property damage")
barplot(ordereddamagecrop[1:10, 2], col = rainbow(10), legend.text = ordereddamagecrop[1:10, 
                                                                             1], ylab = "Crop damage", main = "10 natural events caused most crop damage")
#total damage
totaldmg <- merge(ordereddamage, ordereddamagecrop, by = "EVTYPE")
totaldmg$total = totaldmg$PROPDMG + totaldmg$CROPDMG
totaldmgorder <- totaldmg[order(totaldmg$total, decreasing = TRUE), ]
totaldmgorder[1:5, ]
barplot(totaldmgorder[1:5, 2 ], col = rainbow(10), legend.text = totaldmgorder[1:5, 1], ylab = "Total damage", main = "Total damage", xlab = "Events")
