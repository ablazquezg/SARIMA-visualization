# SARIMA-visualization
An implementation for SARIMA model's forecasts visualization in R software. Below is a brief explanation of the code by sections.

## Load data set

Due to privacity, the data employed in the study is not showed. Instead, a similar data set obtained from petolau.github.io/_rmd/DT_4_ind is used. Data are collected every 30 minutes and consist of 5 columns: date_time, value, week, date and type. For our analysis, we only consider the first two columns, i.e., date_time and value (load in kW). As there are different types of energy consumption, we pick only one of them, that is the aggregate consumption of commercial properties during one year.

There are 17520 observations in total, but for simplicity we take only the first 2500 observations in this data set. The last week is left for the validation of the model (test set), and an independent data set that is used neither in the training set nor in the test set is used for the model application.

Note that the main objective of this app is to show a way to visualize SARIMA model forecasts using R software. Data pre-processing has been omitted in the code, outlier analysis is not done in this data set and holidays are not excluded. Therefore, results may be improved. 

## Model adjustment

The model used for the visualization is obtained using genetic algorithms to speed up the estimation. The main goal is not to obtain a perfect forecast, but to obtain a good visualization of the results.

The data set is divided into two: the training set and the test set. Due to double seasonality only working days are considered. 

## Interactive plots

The interactive plots that will be showed in the app are created by the dygraphs package. Absolute and relative error plots are also included. In order to visualize better the results a range selector is enabled. 

## Application in a new data set

The model is applied to a new independent data set that has not been used for model construction. The results will be displayed in another tab in the app.

## Shiny app

The app contains two main tabs and it can be complicated as much as you want. The first tab contains the results of the SARIMA model (forecasts and application in a new data set), and the second one the residuals diagnostic. 
In the SARIMA model tab, the forecasts, the absolute and relative errors, and the accuracy results are shown. There is also an option to select the time horizon one wants to analize (in this example, only one time horizon is displayed).
In the residuals diagnostic tab the residuals plot, the ACF of the residuals and the Box-Ljung test result are shown. In case there were more time horizons, we could select the desired one as in the previous case.

## Packages needed

library(feather)

library(data.table)

library(forecast)

library(ggplot2)

library(timeDate)

library(tseries)

library(GA)

library(xts)

library(dygraphs)


library(shiny)

library(shinydashboard)

library(dplyr)

library(DT)

library(plotly)

library(dygraphs)
