# Energy Data Business Project
# Project Members: Esteban Ortega Dominguez, Mattia Varagnolo, Sofia Pope Trogu
# 2023-2024


################################# LIBRARIES #############################
# Data manipulation and Analysis
library(dplyr)
library(tsibble)
library(feasts)
library(lubridate)
library(DIMORA)
library(fpp2)
library(fpp3)


# Visualization
library(ggplot2)
library(graphics)
library(GGally)
library(plotly)
library(gridExtra)
library(corrplot)


# Time Series forecasting
library(forecast)
library(qtl2)

# Model and statistical testing
library(lmtest)
library(car)
library(xgboost)
library(caret)
library(timetk)
library(tidyverse)

# Miscellaneousr
library(readxl)
library(reshape2)

############################ Custom functions ##########################
custom_theme <- function() {
  theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

reduce_multicollinearity <- function(model) {
  while(TRUE) {
    # Get the predictors that are part of the model
    predictors <- names(coef(model))[-1]
    
    # Calculate VIF for each variable in the model
    vif_values <- vif(model)
    # Check if the maximum VIF is greater than the threshold (20)
    if(max(vif_values) < 10) {
      break
    }
    
    # Identify the variable with the highest VIF
    max_vif_var <- names(which.max(vif_values))
    # Get rid of that variable
    predictors <- predictors[predictors != max_vif_var]
    # Update our model getting rid of that predictor
    formula_string <- paste("Sales_residential ~", paste(predictors, collapse = " + "))
    # Convert the string to a formula
    formula_object <- as.formula(formula_string)
    # Fit the model using the formula
    model <- lm(formula_object, data=numerical_data, family = gaussian)
  }
  
  return(model)
}

find_correlations <- function(data, threshold = 0.1) {
  # Data subset should be previously given
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe.")
  }
  
  # Select only numeric columns
  numeric_data <- data[sapply(data, is.numeric)]
  
  # Calculate the correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs") # Handling missing values
  
  # Initialize an empty dataframe to store high correlations
  high_corr_df <- data.frame(
    Column1 = character(),
    Column2 = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over the correlation matrix
  for (i in 1:ncol(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      # Check for high correlation and avoid duplicates and self-correlation
      if (i != j && abs(cor_matrix[i, j]) > threshold) {
        high_corr_df <- rbind(high_corr_df, data.frame(
          Column1 = colnames(cor_matrix)[i],
          Column2 = colnames(cor_matrix)[j],
          Correlation = cor_matrix[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(high_corr_df)
}

############################# Data Preparation ############################
options(scipen = 999) # to avoid scientific notation

data = read.csv("data/energy_data.csv", sep = ";", dec = ".")
data$DATE = as.Date(data$DATE, format = "%d/%m/%Y")

data$Petroleum <- abs(data$Petroleum) # converting negative values
data$total_generation_producer <- rowSums(data[, 2:5]) # total by producer
data$total_generation_source <- rowSums(data[, 6:9]) # total by source
data$month <- month(data$DATE, label = TRUE) # each row is a month

# creating tsible object for time series analysis
data_tsbl = data #copy
data_tsbl$DATE = yearmonth(data_tsbl$DATE)
data_tsbl <- as_tsibble(data_tsbl, index = DATE)

train_data_full <- data_tsbl |>
  filter(DATE < yearmonth("2022-01")) 

test_data <- data_tsbl |>
  filter(DATE >= yearmonth("2022-01"))

train_data_split <- data_tsbl |>
  filter(DATE < yearmonth("2022-01")) |>
  filter(DATE > yearmonth("2012-01"))

# representing our data as a time series for regression
ressales_ts <- ts(data$Sales_residential, frequency = 12)

# take a portion of data and fit a linear model with tslm
ressales_ts_10 <- window(ressales_ts, start = 10, end = 22)

# data after 2012
numerical_data_split <- data[data$DATE >= as.Date("2012-01-01"), ]

# full dataset
numerical_data <- data %>%
  select_if(is.numeric)

############################### Pre-Modeling ##################################
p_ressales <- ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "MWh", title = "Residential Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

ggplotly(p_ressales) # Seasonality and increasing trend with a spike in 2015

########### Correlations
# relevant correlations df
correlations_df <- find_correlations(data, threshold = 0.1)
# numbers corresponds to column to include. Choice is based on correlation matrix
data_tsbl[colnames(data_tsbl)[c(12, 2, 3, 4, 5, 6, 7, 8 ,9, 10, 13, 14, 17, 21, 25, 31, 32, 33, 34, 35, 36)]] |>
  GGally::ggpairs()

# Get correlations from previously defined correlations function
corr_df <- find_correlations(data, threshold = 0.01)
# Print relevant correlations
corr_sales <- corr_df[corr_df$Column1 == "Sales_residential", ]
sorted_corr <- corr_sales[order(abs(corr_sales$Correlation), decreasing = TRUE), ]
sorted_corr

# Strongest correlations is with variables related to sales, revenue, number of customer,
# price, which all makes sense since our variable is residential sales of electricty MWh


################# Auto Correlations
Acf(data$Sales_residential, main = "Autocorrelation for Residential Sales")
# Lack of decay at seasonal lags indicates strong and persistent seasonality
# Notable autocorrelation at months 5, 6, 7 reflects changes between winter and summer

Pacf(data$Sales_residential, main = "Partial autocorrelation for Residential Sales")
# Gradual decay in PACF suggests correlations at shorter lags explained by longer lags
# Common in data with a mix of trend and seasonality


################ Seasonal and Trend decomposition using LOESS (STL)
# get components
dcmp <- data_tsbl |>## |> operator pipes the argument into the first function and then from innermost to outermost function in sequential order
  model(
    STL(
      Sales_residential ~ trend(window = 12) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components()

autoplot(dcmp) # trend/season/residuals component + seasonally adjusted.

# comparison between seasonal adjusted and normal.
dcmp_long <- pivot_longer(dcmp, cols = -c(DATE, .model), names_to = "series", values_to = "value")
ggplot(dcmp_long[dcmp_long$series %in% c("Sales_residential", "season_adjust"), ], aes(x = DATE, y = value, color = series)) +
  geom_line() +
  ggtitle("Comparison with seasonally adjusted values") +
  xlab("Time") +
  ylab("Value")


############################################### MODELS ##################################################
#
#
############################################### Simple forecasting methods
# 1. MEAN | Forecast is the mean of observations
# 2. NAIVE | Forecast is the last observed value
# 3. Seasonal NAIVE | Forecast is the last observed value from the same season
# 4. DRIFT | Forecast is a linear extrapolation from the last two observed values

# FIT
sales_fit <- train_data_full |>
  model(
    Mean = MEAN(Sales_residential),
    `Naive` = NAIVE(Sales_residential),
    `Seasonal naive` = SNAIVE(Sales_residential ~ lag("year")),
    Drift = RW(Sales_residential ~ drift())
  )

# FORECAST on full
sales_fc <- sales_fit |>
  forecast(h = "1 years")

# PLOT on TRAIN
sales_fc |>
  autoplot(
    train_data_full,
    level = NULL
  ) +
  labs(
    y = "Sales MWh",
    title = "Forecasts for Sales residential"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# PLOT on TEST
sales_fc |>
  autoplot(
    test_data,
    level = NULL
  ) +
  labs(
    y = "Sales MWh",
    title = "Forecasts for Sales residential"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


# BEST MODEL: Seasonal NAIVE
# ACCURACIES
# The measures calculated are:
# ME: Mean Error
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# MPE: Mean Percentage Error
# MAPE: Mean Absolute Percentage Error
# MASE: Mean Absolute Scaled Error
# ACF1: Autocorrelation of errors at lag 1.

accuracies_test <- accuracy(sales_fc, test_data)


################# RESIDUALS ANALYSIS
fit_mean <- data_tsbl |> model(MEAN(Sales_residential))
fit_naive <- data_tsbl |> model(NAIVE(Sales_residential))
fit_snaive <- data_tsbl |> model(SNAIVE(Sales_residential ~ lag("year")))
fit_drift <- data_tsbl |> model(RW(Sales_residential ~ drift()))

residuals_mean = augment(fit_mean)
residuals_naive = augment(fit_naive)
residuals_snaive = augment(fit_snaive)
residuals_drift = augment(fit_drift)

# Combine residuals into one data frame with a model indicator
residuals_all <- rbind(transform(residuals_mean, Model = "Mean"),
                       transform(residuals_naive, Model = "Naive"),
                       transform(residuals_snaive, Model = "Seasonal Naive"),
                       transform(residuals_drift, Model = "Drift"))

# Create a boxplot to compare residuals across models
ggplot(residuals_all, aes(x = Model, y = .resid)) +
  geom_boxplot() +
  labs(title = "Residuals Comparison Across Models", y = "Residuals")+
  custom_theme()

# shapiro-wilk test to look for normality
shapiro.test(residuals_mean$.resid)
shapiro.test(residuals_naive$.resid)
shapiro.test(residuals_snaive$.resid)
shapiro.test(residuals_drift$.resid)
# all of them has enough p-value to reject null hypothesis so for shapiro-wilk test they're not normal.

######## Autocorrelations of Residuals
acf(residuals_mean$.resid, main='ACF for Mean')
acf(na.omit(residuals_naive$.resid), main="ACF for Naive")
acf(na.omit(residuals_snaive$.resid), main='ACF for Seasonal Naive')  # the only one with lowest acf values.
acf(na.omit(residuals_drift$.resid), main= 'ACF for Drift')


################################################## LINEAR REGRESSION
######### 1. Trend
tslm_trend = tslm(ressales_ts ~ trend)
summary(tslm_trend) # R-squared = 0.2221; F = 74.79 with 262 df; p < 0.0001

######### 2. Trend + Season
tslm_trend_season = tslm(ressales_ts ~ trend + season)
summary(tslm_trend_season) # R-squared = 0.7941; F = 80.65 with 251 df; p < 0.0001

######### 3. Trend + Season on subset
m1 <- tslm(ressales_ts_10 ~ trend + season)
summary(m1) # R-squared = 0.7637; F = 35.55 with ; p<0.0001



########## PLOTTING AND RESIDUALS ANALYSIS
## PLOT VALUES vs FITTED
p_tslm_t <- ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "Residential Sales (MWh)", title = "Real TimeSeries vs Fitted Values TSLM") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme() +
  geom_line(aes(y = fitted(tslm_trend)),
            color = "darkred", linetype = "twodash"
  )

ggplotly(p_tslm_t)

p_tslm_ts <- ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "Residential Sales (MWh)", title = "Real TimeSeries vs Fitted Values TSLM") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme() +
  geom_line(aes(y = fitted(tslm_trend_season)),
            color = "red"
  )

ggplotly(p_tslm_ts)

# Create a sequence of dates from 2010 to 2022
date_sequence <- seq(as.Date("2010-01-01"), as.Date("2022-01-01"), by = "2 year")

autoplot(ressales_ts_10, main = 'Real TimeSeries vs Fitted Values TSLM subset', y = 'Residential Sales (MWh') +
  custom_theme() +
  geom_line(aes(y = ressales_ts_10), color = "black", linetype = "solid") +
  geom_line(aes(y = fitted(m1)), color = "red", linetype = "solid") +
  scale_x_continuous(breaks = seq(10, 22, by = 2), labels = date_sequence)

## RESIDUALS
p_tslm_res <- ggplot(data, aes(x = residuals(tslm_trend))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Histogram of residuals with Trend")+
  custom_theme()
ggplotly(p_tslm_res)

p_tslm_ts_res_hist <- ggplot(data, aes(x = residuals(tslm_trend_season))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Histogram of residuals with Trend and Season")+
  custom_theme()
print(p_tslm_ts_res_hist)

p_tslm_ts_10_res_hist <- ggplot(ressales_ts_10, aes(x = residuals(m1))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Histogram of residuals with Trend and Season on subset")+
  custom_theme()
print(p_tslm_ts_10_res_hist)

## DW TEST (to detect autocorrelations) dw=2 desired, dw<2 positive autocorrelation.
dwtest(tslm_trend) # DW = 1.199, p<0.0001
dwtest(tslm_trend_season) # DW = 1.7889, p-value < 0.05
dwtest(m1) # DW = 1.8668, p-value = 0.2174

###### Autocorrelations
acf(residuals(tslm_trend))
acf(residuals(tslm_trend_season))
acf(residuals(m1))


###### FORECAST
# grey area 80%
# light shaded area 95%
plot(forecast(m1, h=13)) 
plot(forecast(tslm_trend_season))
plot(forecast(tslm_trend))


###### ACCURACIES
m1_acc = as_tibble(accuracy(forecast(m1, h=12), test_data$Sales_residential))
tslm_trend_acc = as_tibble(accuracy(forecast(tslm_trend, h=12), test_data$Sales_residential))
tslm_trend_season_acc = as_tibble(accuracy(forecast(tslm_trend_season, h=12), test_data$Sales_residential))

m1_acc = m1_acc[2,]
m1_acc$.model = 'tslm subset'
m1_acc$.type = 'Test'
m1_acc$MASE= NA
m1_acc$RMSSE = NA
m1_acc$ACF1 = NA
m1_acc = m1_acc[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

tslm_trend_acc = tslm_trend_acc[2,]
tslm_trend_acc$.model = 'tslm trend'
tslm_trend_acc$.type = 'Test'
tslm_trend_acc$MASE= NA
tslm_trend_acc$RMSSE = NA
tslm_trend_acc$ACF1 = NA
tslm_trend_acc = tslm_trend_acc[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

tslm_trend_season_acc = tslm_trend_season_acc[2,]
tslm_trend_season_acc$.model = 'tslm trend + season'
tslm_trend_season_acc$.type = 'Test'
tslm_trend_season_acc$MASE= NA
tslm_trend_season_acc$RMSSE = NA
tslm_trend_season_acc$ACF1 = NA
tslm_trend_season_acc = tslm_trend_season_acc[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracies_test = bind_rows(accuracies_test, m1_acc)
accuracies_test = bind_rows(accuracies_test, tslm_trend_acc)
accuracies_test = bind_rows(accuracies_test, tslm_trend_season_acc)

accuracies_test[accuracies_test$.model == 'tslm subset', 'ACF1'] = ACF1(residuals(m1))
accuracies_test[accuracies_test$.model == 'tslm trend', 'ACF1'] = ACF1(residuals(tslm_trend))
accuracies_test[accuracies_test$.model == 'tslm trend + season', 'ACF1'] = ACF1(residuals(tslm_trend_season))



#################################################### MULTIPLE LINEAR REGRESSION MODELS
# Most important thing: select which predictor we'll use
# stepwise selection
################### FULL DATASET (FROM 2001 TO 2022)
lr_fullModel = lm(Sales_residential ~ ., data=numerical_data, family = gaussian)
summary(lr_fullModel) # Multiple R-squared:  0.9906;	Adjusted R-squared:  0.9893 F = 760.2 on 32 and 231 DF;  p-value: < 0.0000000022
lr_step_aic = step(lr_fullModel, direction="both", trace=0, steps=1000)
summary(lr_step_aic) # Multiple R-squared:  0.9904;	Adjusted R-squared:  0.9897; F:  1259 on 20 and 243 DF;  p-value: < 0.0000000022

################### PARTIAL DATASET (FROM 2012 TO 2022)
lr_splitModel = lm(Sales_residential ~ ., data=numerical_data_split, family = gaussian)
summary(lr_splitModel) # Multiple R-squared: 1; Adjusted R-squared 1; F: 3.704e+10 on 43 and 88 DF,  p-value: < 0.00000000000000022
lr_step_aic_split = step(lr_splitModel, direction="both", trace=0, steps=1000)
summary(lr_step_aic_split) # Multiple R-squared: 1; Adjusted R-squared: 1; F: 7.939e+10 on 21 and 110 DF,  p-value: < 0.00000000000000022



#assess multicollinearity through VIF
vif(lr_step_aic)
vif(lr_step_aic_split)  # curious to see the different VIF between the two models. In general the second has higher VIF.

#reduce collinearity FULL
mlr = reduce_multicollinearity(lr_step_aic)
vif(mlr)
summary(mlr) # Multiple R-squared:  0.9606;	Adjusted R-squared:  0.9586; F:   469 on 13 and 250 DF;  p-value: < 0.00000000000000022
extractAIC(mlr)
# check autocorrelations in residuals
acf(residuals(mlr))

#reduce collinearity SPLIT
mlr_split = reduce_multicollinearity(lr_step_aic_split)
vif(mlr_split)
summary(mlr_split) # Multiple R-squared:  0.9356;	Adjusted R-squared:  0.932; F: 258.5 on 14 and 249 DF;  p-value: < 0.00000000000000022
extractAIC(mlr_split)
# check autocorrelations in residuals
acf(residuals(mlr_split)) # more negative autocorrelation between lag 15 and 20.


# IN GENERAL FULL MODEL IS BETTER.

predict_mlr = predict(mlr, newdata = test_data)
predict_mlr_split = predict(mlr_split, new_data = test_data)

accuracy_mlr = accuracy(predict_mlr, test_data$Sales_residential)
accuracy_mlr_split = accuracy(predict_mlr_split, test_data$Sales_residential)



accuracy_mlr=as_tibble(accuracy(predict_mlr, test_data$Sales_residential))
accuracy_mlr$.model = 'MLR'
accuracy_mlr$.type = 'Test'
accuracy_mlr$MASE= NA
accuracy_mlr$RMSSE = NA
accuracy_mlr$ACF1 = NA
accuracy_mlr = accuracy_mlr[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracy_mlr_split=as_tibble(accuracy(predict_mlr_split, test_data$Sales_residential))
accuracy_mlr_split$.model = 'MLR 2012'
accuracy_mlr_split$.type = 'Test'
accuracy_mlr_split$MASE= NA
accuracy_mlr_split$RMSSE = NA
accuracy_mlr_split$ACF1 = NA
accuracy_mlr_split = accuracy_mlr_split[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracies_test= bind_rows(accuracies_test, accuracy_mlr)
accuracies_test = bind_rows(accuracies_test, accuracy_mlr_split)


accuracies_test[accuracies_test$.model == 'MLR', 'ACF1'] = ACF1(residuals(mlr))
accuracies_test[accuracies_test$.model == 'MLR 2012', 'ACF1'] = ACF1(residuals(mlr_split))



### TODO: i skipped the subjective stepwise because the rsquared is just horrible. let's decide together btw


################################################# Exponential Smoothing
# We want a method that takes into account seasonality as we have seen
# Holt Winters seasonal method

## TRAIN full
hw_fit_full <- train_data_full |>
  model(
    # a) Additive is preferred when seasonal variations are roughly constant
    # through the series
    'hw_additive' = ETS(Sales_residential ~ error("A") + trend("A") +
                          season("A")),
    # b) Multiplicative is preferred when the seasonal variations are changing
    # proportional to the level of the series
    'hw_multiplicative' = ETS(Sales_residential ~ error("M") + trend("A") +
                                season("M"))
  )

# TRAIN split
hw_fit_split <- train_data_split |>
  model(
    # a) Additive is preferred when seasonal variations are roughly constant
    # through the series
    'hw_additive_split' = ETS(Sales_residential ~ error("A") + trend("A") +
                          season("A")),
    # b) Multiplicative is preferred when the seasonal variations are changing
    # proportional to the level of the series
    'hw_multiplicative_split' = ETS(Sales_residential ~ error("M") + trend("A") +
                                season("M"))
  )


## FORECAST full
hw_fc_full <- hw_fit_full |>
  forecast(h= '1 year')

## FORECAST split
hw_fc_split <- hw_fit_split |>
  forecast(h= '1 year')


# Plot forecast full
hw_fc_full |>
  autoplot(test_data, level = NULL) +
  labs(title="Date",
       y="Residential sales MWh") +
  guides(colour = guide_legend(title = "Forecast"))+
  custom_theme()

# Plot forecast split
hw_fc_split |>
  autoplot(test_data, level = NULL) +
  labs(title="Date",
       y="Residential sales MWh") +
  guides(colour = guide_legend(title = "Forecast"))+
  custom_theme()

# Compute accuracies
hw_acc_full <- accuracy(hw_fc_full, test_data) # we could use (complete) data_tsbl and it would detect
hw_acc_split <- accuracy(hw_fc_split, test_data) # we could use (complete) data_tsbl and it would detect
accuracies_test = bind_rows(accuracies_test, hw_acc_full)
accuracies_test = bind_rows(accuracies_test, hw_acc_split)


# TODO: Cross validation


data$DATE <- as.Date(data$DATE, format = "%Y-%m-%d")






###################################### ARIMA Models
# p: autoregresive component (AR): Captures the linear relationship between the current observation and its previous values (lags).
# d: differencing of the time series to achieve stationarity. degrees of differencing needed
# q: MA (Moving Average): Models the short-term, unobserved shocks or random fluctuations in the data.

# Differencing required before running ARIMA:
ggplotly(p_ressales)
ressales_ts_df <- diff(ressales_ts)
tsdisplay(ressales_ts_df)
# Plot differentiated data to check for stationarity:

p_ts_df <- autoplot(ressales_ts_df, xlab = "Time", ylab = "ResidentialSales") +
  ggtitle("Residential Sales (MWh) - Differentiated Series")+
  custom_theme()

ggplotly(p_ts_df)
# Differentiating the series we can see that it seems to be more
# stationary (in term of mean)

# Residuals of differentiated series:
p_acf_df <- ggAcf(ressales_ts_df) +
  ggtitle("Acf Function for Diff Residential Sales")+
  custom_theme()
p_pacf_df <- ggPacf(ressales_ts_df) +
  ggtitle("Partial Acf Function for Diff Residential Sales")+
  custom_theme()

grid.arrange(p_acf_df, p_pacf_df, nrow = 2)
# lag 6, 12, 18, 12 significant (mid-year)

# using auto.arima automatically it finds the best parameters.
auto.arima <- auto.arima(ressales_ts) # AIC=5879.92
auto.arima # ARIMA(1,0,0)(1,1,0)[12] with drift
arima_model = auto.arima(ressales_ts, lambda = "auto", stepwise = TRUE, approximation = FALSE, allowdrift = FALSE) #without drift gives better results
summary(arima_model) # AIC=2534. ARIMA(0,0,3)(0,1,1)[12]

### FORECAST
forecast_arima = forecast(arima_model)
### PLOT
autoplot(forecast_arima, main = 'ARIMA Forecast on sales')+
  custom_theme()
### ACCURACY
arima_accuracy=accuracy(forecast_arima, test_data$Sales_residential)
arima_accuracy = as_tibble_row(arima_accuracy[2,])
arima_accuracy$.model = 'ARIMA'
arima_accuracy$.type = 'Test'
arima_accuracy$RMSSE =NA
arima_accuracy = arima_accuracy[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracies_test = bind_rows(accuracies_test, arima_accuracy)
accuracies_test[accuracies_test$.model == 'ARIMA', 'ACF1'] = ACF1(residuals(arima_model))

### RESIDUALS
p_arima_res <- ggplot(data, aes(x = residuals(arima_model))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Histogram of residuals with Trend")+
  custom_theme()
ggplotly(p_arima_res)

shapiro.test(residuals(arima_model)) # suggest is not normal

### AUTOCORRELATIONS
acf(residuals(arima_model)) # seems fine, very low.




##################################### CHOOSING THE BEST MODEL ###############################

# lets recap.
# This is a summary of all the accuracies that we have right now.

accuracies_test = accuracies_test |> select(-RMSSE,-MASE) # simple models + linerar regression + multi linear regression + exp smoothing + ARIMA


summary(mlr) # Multiple R-squared:  0.9606;	Adjusted R-squared:  0.9586; F:   469 on 13 and 250 DF;  p-value: < 0.00000000000000022
extractAIC(mlr)

summary(mlr_split) # Multiple R-squared:  0.9356;	Adjusted R-squared:  0.932; F: 258.5 on 14 and 249 DF;  p-value: < 0.00000000000000022
extractAIC(mlr_split)


# I would say best model is HW_ADDITIVE (SPLIT VERSION)

# AIC is useless for HW models because is non-parametric.




