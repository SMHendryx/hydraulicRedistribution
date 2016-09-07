#	TIME-SERIES PLOTTING AND REGRESSIONS: MEAN DAILY SAPFLOW DATA
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


################################################################################################################################################################################################################################################################################################################################
# Read in environmental forcing data (soil moisture, air temp)

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

#Now make new datatable with the relevant variables:
newDT <- regressDT[,.(Date, Year, DOY.x, precip..mm., shallowMinusDeep, maxAirTemp, minAirTemp, tapLatMeanDailySapVelocity)]

#Read in more environmental forcing data which now includes VPD from Esther:

#Compute mean daily VPD

# Extract daily LAI if Esther's data has it

#Graph VPD simple time series

# Regress Vs over VPD
	#plot

# Diff VPD and Vs since they have autocorrelation:

# ccf() with VPD and Vs

# Regress diffed VPD over Vs (so this shows how changes in VPD relate to changes in Vs)
	#plot

# Multiple linear regression of Vs as explained by VPD and LAI
	#3d scatter plot

# HYPOTHESIS: VPD and phenology control hydraulic descent and list in mesquites in the SRER	
