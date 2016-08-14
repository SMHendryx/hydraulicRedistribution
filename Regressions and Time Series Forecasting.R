#	TIME-SERIES REGRESSIONS AND FORECASTING: MEAN DAILY SAPFLOW DATA
#	Authored by Sean Hendryx
#	While working at The Univerity of Arizona
#	2016
#########################################################################################################

#Load packages:
packages = c('ggplot2', 'data.table', 'dplyr', 'tools', 'plotly')
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



################################################################################################################################################################################################################################################################################################################################
# IID regressions first:
# Regress HD over delta soil moisture 
# read in environmental forcing data (soil moisture, air temp)

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


#how well is tap explained by shallowMinusDeepSoil
model <- lm(regressDT[,tapLatMeanDailySapVelocity] ~ regressDT[,shallowMinusDeep])

plot(regressDT[,shallowMinusDeep], -regressDT[,tapLatMeanDailySapVelocity])

model1 <- lm(regressDT[,tapLatMeanDailySapVelocity] ~ regressDT[,maxAirTemp])

plot(regressDT[,maxAirTemp], -regressDT[,tapLatMeanDailySapVelocity])

mmodel <- lm(regressDT[,tapLatMeanDailySapVelocity] ~ regressDT[,maxAirTemp] + regressDT[,minAirTemp] + regressDT[,shallowMinusDeep]+ regressDT[,precip..mm.])

newDT <- regressDT[,.(Date, Year, DOY.x, precip..mm., shallowMinusDeep, maxAirTemp, minAirTemp, tapLatMeanDailySapVelocity)]


#Change that annoyingly long "y" column name:
names(newDT)[names(newDT)=="tapLatMeanDailySapVelocity"] = "meanTapRootSapVelocity"
write.csv(newDT, "Sap Velocity Data for Predicting HR.csv")





plotDT = newDT

plotDT[, c("Date", "Year", "DOY.x") := NULL]

plot(plotDT)
# No singular variable looks to be a good predictor and it looks like there are some nonlinear relationships
#Since no linear model looks to be present, let's look at a randomForest predictor
#But clearly, there seems to autocorrelation.  So, time-series analysis should be more appropriate

#IID regressions:
#lmodel <- lm(HD ~ shallowMinusDeep)
#plot(shallowMinusDeep, HD)

#Now multiple linear regression:
#new DT for small multiple plots:
#newDT = DT[,.(HD, precip, shallowMinusDeep, maxAirTemp, minAirTemp)]
#newdf <- as.data.frame(newDT)
#plot(newdf)
#mlmodel <- lm(HD, ~ shallowMinusDeep + maxAirTemp + minAirTemp) # add precip? - probably not because precip and shallowMinusDeep are not independent 

#We could also look at how mesquites draw upon stored water:??????
# HL <- 

#Now finally, make ARIMA model
#note: "If your external regressors are causal for yy, but not the other way around and do not cause each other, then ARIMA is definitely appropriate. VAR makes sense if your different time series all depend on each other."
# http://stats.stackexchange.com/questions/122803/arima-time-series-forecast-auto-arima-with-multiple-exogeneous-variables-in-r
#use auto.arima function and shallowMinusDeep




