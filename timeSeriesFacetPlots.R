#	TIME-SERIES REGRESSIONS AND FORECASTING: MEAN DAILY SAPFLOW DATA
#	Authored by Sean Hendryx
#	While working at The Univerity of Arizona
#	2016
#########################################################################################################

#Load packages:
packages = c('ggplot2', 'data.table', 'dplyr', 'tools', 'plotly', 'forecast')
lapply(packages, library, character.only = TRUE)

#Set working directory:
setwd("/Users/seanhendryx/Google Drive/THE UNIVERSITY OF ARIZONA (UA)/RESEARCH ASSISTANTSHIP/HR PROJECT HYDRAULIC REDISTRIBUTION UA NSF/Correcting Sap Flux Measurements/Deterministic Sap Velocity Corrections/outputDirectory")
#Read in data:
df <- read.csv("SapVelocityCorrectedForProbeMisalignmentAndWounding.csv")
head(df)

#Make list of ids:
idList <- unique(df$id)
#Extract strings with levels:
idList <- levels(idList)
#idList should be a list of character strings

#Make DateTime Object
df$DateTime <- as.POSIXct(df$DateTime)
df$Date <- as.Date(df$Date)
#Convert dataframe to datatable:
DT <- data.table(df)

#Make datatable of daily means in cm/day:
dailyDT <- DT[, .(VsInCmPerDay, Date), by = .(id, DOY)]

#Compute daily mean for each individual meter and add to new column, meanDailySapVelocity:
dailyDT[, meanDailySapVelocity := mean(na.omit(VsInCmPerDay)), by = .(id, Date)]
dailyDT 

#Trim excess rows since now we only care about the daily mean of each sensor
setkey(dailyDT, Date, id)
dailyDT <- unique(dailyDT)

#Add column signifying if sensor is on tap or lat:
#Note that these variable are hard coded, could be extracted from id by parsing id string
latList <- c("C2lat1", "C3lat1", "C3lat2")
dailyDT[,tapVsLat := ifelse(id %in% latList, "lat", "tap")]

#Compute daily mean for taps and lats and add to new column, tapLatMeanDailySapVelocity:
dailyDT[, tapLatMeanDailySapVelocity := mean(na.omit(meanDailySapVelocity)), by = .(tapVsLat, Date)]
dailyDT[1:7]

#Compute standard error:
stndError <- function(x){
	sd(x)/sqrt(length(x))
}
dailyDT[, SEMeanVs := stndError(na.omit(meanDailySapVelocity)), by = .(tapVsLat, Date)]

#Set key of datatable to retrieve one daily mean and one standard error of the mean for each day and each tapvslat
setkey(dailyDT, Date, tapVsLat)

graphDT <- unique(dailyDT)

#Now remove the columns that don't apply:
graphDT[,c("id","VsInCmPerDay","meanDailySapVelocity") := NULL]

tapDT <- graphDT[tapVsLat =="tap"]
latDT <- graphDT[tapVsLat =="lat"]



alpha <- .4
ggp <- ggplot(data = graphDT, aes(x=Date, y = tapLatMeanDailySapVelocity)) + geom_ribbon(data = tapDT, aes(x = as.Date(Date), ymin=tapLatMeanDailySapVelocity - SEMeanVs, ymax=tapLatMeanDailySapVelocity + SEMeanVs), alpha=alpha) + geom_line(data = tapDT, aes(x = as.Date(Date),y = tapLatMeanDailySapVelocity), colour = "blue")
ggp <- ggp + geom_ribbon(data = latDT, aes(x = as.Date(Date), ymin=tapLatMeanDailySapVelocity - SEMeanVs, ymax=tapLatMeanDailySapVelocity + SEMeanVs), alpha=0.4) + geom_line(data = latDT, aes(x = as.Date(Date),y = tapLatMeanDailySapVelocity), colour = "red") + theme_bw()
ggp <- ggp + labs(y = "Sap Flow Velocity (cm/day)")
ggp

########################################################################################################
#make ARIMA model
#ARIMA models are defined for stationary time series.  Data here is non-stationary, presenting seasonality.
#First, I diff the response data:
numdiffs <- ndiffs(tapDT$tapLatMeanDailySapVelocity)
#output was 1 on 08/23/16

#Stationarize and then plot response data:
tapDT[2:length(tapDT$tapLatMeanDailySapVelocity), VsDiff1 := diff(tapDT$tapLatMeanDailySapVelocity, differences=numdiffs)]
tsggp <- ggplot(data = tapDT, aes(x=Date, y = VsDiff1)) + geom_line() + theme_bw() + labs(y = "Diffed Sap Flow Velocity (cm/day)", title = "Stationary Tap Velocity")
tsggp

#Plot auto correlation:
acf(tapDT[-1,VsDiff1])


#Read in external regressors:
setwd("/Users/seanhendryx/Google Drive/THE UNIVERSITY OF ARIZONA (UA)/RESEARCH ASSISTANTSHIP/HR PROJECT HYDRAULIC REDISTRIBUTION UA NSF/HR Regressions & Forecasting/input")
#Read in data:
envdf <- read.csv("environment.csv", header=TRUE, skip = 2, sep = ",")
head(envdf)


#Combine env and HD root data into single DT for regression & forecasting
#Subset to same dates:
####THIS NEEDS TO BE BETTER
envdf <-envdf[graphDT[1,DOY]:dim(envdf)[1],]
head(envdf)
envDT = as.data.table(envdf)
setkey(envDT, Year, DOY)
#Add date column to envDT:
envDT[,Date := tapDT[,Date]]

#Remove last dates from envDT
envDT = envDT[1:333,]


#Set key for merge:
setkey(envDT, Date)
setkey(tapDT, Date)

#Merge by Date primary key full outer join:
regressDT <- merge(envDT, tapDT)
regressDT

#Diff explanatory variable:
numdiffs <- ndiffs(regressDT$shallowMinusDeep)
numdiffs
#Console output = 1 on 08/30/16
regressDT[2:length(regressDT$shallowMinusDeep), shallowMinusDeepDiff1 := diff(regressDT$shallowMinusDeep, differences = numdiffs)]
#plot diffed delta soil moisture:
tsggp <- ggplot(data = regressDT, aes(x=Date, y = shallowMinusDeepDiff1)) + geom_line() + theme_bw() + labs(y = "Diffed Delta Soil Moisture", title = "Stationary Delta Soil Moisture (Shallow Minus Deep)")
tsggp

#Make a "long" datatable and add rain distinction for Faceting:
#But first, let's try with base graphics:
plot (x=regressDT$Date, y = regressDT$shallowMinusDeepDiff1, type = "l")
plot (x=regressDT$Date, y = regressDT$precip..mm., type = "l", col = "blue")


#Plot cross correlation:
#Ignoring first row since values are NA after diff()
ccf(regressDT[-1,shallowMinusDeepDiff1], regressDT[-1,VsDiff1])
#Quite interesting, changes in Delta soil moisture actually FOLLOW changes in sap velocity

ccfValues <- ccf(regressDT[-1,shallowMinusDeepDiff1], regressDT[-1,VsDiff1])

#and now... MAKE ARIMA MODEL!
arimaModel <- auto.arima(tapDT[1:300,VsDiff1], xreg = regressDT[1:300,shallowMinusDeep])

forecasts <- forecast(arimaModel, h = 30, xreg = regressDT[301:331,shallowMinusDeep])

fggp <- autoplot(forecasts)
fggp
