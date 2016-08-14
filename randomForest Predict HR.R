
#Let's look at variable importance with randomForest:
#Load packages:
packages = c('ggplot2', 'data.table', 'dplyr', 'tools', 'plotly', 'randomForest')
lapply(packages, library, character.only = TRUE)

#Set working directory:
setwd('/Users/seanhendryx/Google Drive/THE UNIVERSITY OF ARIZONA (UA)/RESEARCH ASSISTANTSHIP/HR PROJECT HYDRAULIC REDISTRIBUTION UA NSF/HR Regressions & Forecasting/input')

df <- read.csv("Sap Velocity Data for Predicting HR.csv")
#Make date object:
df$Date <- as.Date(df$Date)
DT = as.data.table(df)

#Change that annoyingly long "y" column name:
names(DT)[names(DT)=="meanTapRootSapVelocity"] = "Vs"

#Remove fields that are not predictors:
rfDT = DT
rfDT[, c("Date", "Year", "DOY.x", "X") := NULL]

fit <- randomForest(rfDT[,Vs] ~ ., data = rfDT)
importance(fit)

varImpPlot(fit, main = "Dotchart of variable importance in predicting HR\n As measured by a Random Forest")

plot(fit)

#Now lets retrain and validate with cross validation (which is already done by randomForest) but just to convince ourselves:
