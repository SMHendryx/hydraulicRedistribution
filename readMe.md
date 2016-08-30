Scripts for time-series analysis and regression in order to develop explanations of the relationships that are controlling hydraulic redistribution by mesquites (prosopis velutina woot.) in the Santa Rita Experimental Range, Arizona.
PROJECT IN DEVELOPMENT
![Vs](outputDirectory/Mean Daily Sap Velocity at SRER 2015 to 2016 with Standard Error.png "Hydraulic Redistribution by tap roots (blue) and lateral roots (red)")
^Lateral roots drawn in red and tap roots in blue.  Negative velocity in the tap (blue) indicates hydraulic descent and positive velocity indicates hydraulic lift.

![smallMultiples](outputDirectory/Scatter Plot Matrix.png "Scatter Plot Matrix")

![Tap Velocity Over Delta Soil Moisture.png](outputDirectory/Tap Velocity Over Delta Soil Moisture.png "Taproot sap velocity over soil moisture delta (shallow minus deep)")

![errorConvergence](outputDirectory/Error Convergence.png "Error convergence from randomForest:")

![dotchart](outputDirectory/Dotchart of variable importance in predicting HR as measured by a Random Forest.png "Variable Importance from randomForest")

Some interesting indications of both autocorellation and multimodality:
![Tap Velocity Over Delta Soil Moisture Colorized and Pathed by Date.png](outputDirectory/Tap Velocity Over Delta Soil Moisture Colorized and Pathed by Date.png "Tap Velocity Over Delta Soil Moisture Colorized and Pathed by Date.png")

Which we also clearly see in the residuals:
![Heteroskedastic Residuals of LM Tap Velocity Over Delta Soil Moisture.png](outputDirectory/Heteroskedastic Residuals of LM Tap Velocity Over Delta Soil Moisture.png "Heteroskedastic Residuals of LM Tap Velocity Over Delta Soil Moisture.png")

Autocorrelation, though statistically significant, does not appear to be strong after differencing, 
![Autocorrelation](outputDirectory/Autocorrelation.png "Autocorrelation")

Especially when compared to cross correlation:
![Changes in the Delta Soil Moisture actually FOLLOW (lag) changes in Tap Velocity](outputDirectory/Changes in the Delta Soil Moisture actually FOLLOW (lag) changes in Tap Velocity.png "Changes in the Delta Soil Moisture actually FOLLOW (lag) changes in Tap Velocity")


Exploratory work with ARIMA Forecasting (NEEDS TO BE UPDATED IN LIGHT OF NEW INFORMATION FROM CCF() AS SHOWN ABOVE):
First make the data stationary, using diff() in R:
(diff returns Value:
If x is a vector of length n and differences = 1, then the computed result is equal to the successive differences x[(1+lag):n] - x[1:(n-lag)].)
![Stationary Tap Velocity.png](outputDirectory/Stationary Tap Velocity.png "Stationary Tap Velocity.png")
And test forecast:
![forecast from arima model.png](outputDirectory/forecast from arima model.png "forecast from arima model.png")
