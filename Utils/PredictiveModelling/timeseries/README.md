# R--predictive-modelling
The code in this repo was copied from an obsolete repository of my previous work on different learning algorithms and data modelling in R.

1. tslm_dynamic.R: (Implementation of tslm model for a data with unknown column names(independent variables))

When we try to run a  dynamically choosen data for the tslm in forecast package, conversion of the dependent variable to timeseries and choosing column names to build the regression is implemented here. 

Packages used
Forecast​ : Provides the necessary functionality of converting and evaluating the data for time series analysis. The models that are implemented in the code use the library function provided by this package.
