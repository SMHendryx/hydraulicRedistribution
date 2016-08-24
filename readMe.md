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

Beginning work with ARIMA Forecasting:
First make the data stationary:
![Stationary Tap Velocity.png](outputDirectory/Stationary Tap Velocity.png)"Stationary Tap Velocity.png"
And test forecast:
![forecast from arima model.png](outputDirectory/forecast from arima model.png)"forecast from arima model.png"
