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
library(prophet)
library(qtl2)

# Model and statistical testing
library(lmtest)
library(car)
library(xgboost)
library(caret)
library(timetk)
library(tidyverse)
library(tsfknn)

# Miscellaneous
library(readxl)
library(reshape2)
# To create nice tables
library(flextable)


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

reduce_multicollinearity <- function(model, vif_cutoff=7) {
  while(TRUE) {
    # Get the predictors that are part of the model
    predictors <- names(coef(model))[-1]

    # Calculate VIF for each variable in the model
    vif_values <- vif(model)
    # Check if the maximum VIF is greater than the threshold (20)
    if(max(vif_values) < vif_cutoff) {
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
    model <- lm(formula_object, data=numerical_data_train, family = gaussian)
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

set_flextable_defaults(digits = 2)

# Print nice table for report (from accuracies tibble)
print_nice <- function(accs){
  accs |>
    select(.model, ME, RMSE, MAE, MPE, MAPE, ACF1)   |> # select columns
    flextable()  |>
    colformat_double(digits = 2)
}


############################# Data Preparation ############################
options(scipen = 999) # to avoid scientific notation

data = read.csv("data/energy_data.csv", sep = ";", dec = ".")
data$DATE = as.Date(data$DATE, format = "%d/%m/%Y")

data$Petroleum <- abs(data$Petroleum) # converting negative values
data$total_generation_producer <- rowSums(data[, 2:5]) # total by producer
data$total_generation_source <- rowSums(data[, 6:9]) # total by source
data$month <- month(data$DATE, label = TRUE) # each row is a month

# Select only relevant variables
# > relevant_vars
# [1] "DATE"                                            
# [2] "Combined.Heat.and.Power..Commercial.Power"       
# [3] "Combined.Heat.and.Power..Electric.Power"         
# [4] "Electric.Generators..Electric.Utilities"         
# [5] "Electric.Generators..Independent.Power.Producers"
# [6] "Natural.Gas"                                     
# [7] "Other.Biomass"                                   
# [8] "Petroleum"                                       
# [9] "Solar.Thermal.and.Photovoltaic"                  
# [10] "Customers_residential"                           
# [11] "Price_residential"                               
# [12] "Customers_commercial"                            
# [13] "Price_commercial"                                
# [14] "Customers_industrial"                            
# [15] "Price_industrial"                                
# [16] "Customers_transportation"                        
# [17] "Price_transportation"                            
# [18] "Customers_total"                                 
# [19] "Price_total"                                     
# [20] "tavg"                                            
# [21] "tmin"                                            
# [22] "tmax"                                            
# [23] "prcp"                                            
# [24] "wspd"                                            
# [25] "pres"                                            
# [26] "month"   
relevant_vars <- colnames(data)[c(1,2,3,4,5,6,7,8,9,12,13,14,17,18,21,22,25,26,29,30,31,32,33,34,35,36,39)]
data <- data[relevant_vars]

# creating tsible object for time series analysis
data_tsbl = data #copy
data_tsbl$DATE = yearmonth(data_tsbl$DATE)
data_tsbl <- as_tsibble(data_tsbl, index = DATE)

train_data_full <- data_tsbl |>
  filter(DATE < yearmonth("2022-01"))

test_data <- data_tsbl |>
  filter(DATE >= yearmonth("2022-01"))

train_data_split <- data_tsbl |> # so that we don't take petroleum
  filter(DATE < yearmonth("2022-01")) |>
  filter(DATE > yearmonth("2012-01"))


# For arima
ressales_ts_arima_train = ts(train_data_full$Sales_residential, frequency = 12)

# TS full
ressales_ts <- ts(data_tsbl$Sales_residential, frequency = 12)

# TS train
ressales_ts_train <- ts(train_data_full$Sales_residential, frequency = 12)

# TS test
ressales_ts_test <- ts(test_data$Sales_residential, frequency = 12)



numerical_data_train_split <- data %>%
  filter(DATE >= as.Date("2012-01-01") & DATE < as.Date("2022-01-01")) %>%
  select_if(is.numeric)


numerical_data_train <- data %>%
  filter(DATE < as.Date("2022-01-01")) %>%
  select_if(is.numeric)

numerical_test <- data %>%
  filter(DATE >= as.Date("2022-01-01")) %>%
  select_if(is.numeric)


############################### Pre-Modeling ##################################
p_ressales <- ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "MWh", title = "Residential Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

plot(p_ressales) # Seasonality and increasing trend with a spike in 2015

########### Correlations
# relevant correlations df
correlations_df <- find_correlations(data, threshold = 0.1)
# Select the columns that we want to do correlations for
corr_cols <- colnames(numerical_data_train)

data_tsbl[corr_cols] |>
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
accuracies <- accuracy(sales_fc, test_data)



# The measures calculated are:
# ME: Mean Error
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# MPE: Mean Percentage Error
# MAPE: Mean Absolute Percentage Error
# MASE: Mean Absolute Scaled Error
# ACF1: Autocorrelation of errors at lag 1.


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
# all of them has a small enough p-value to reject null hypothesis so for shapiro-wilk test they're not normal.

######## Autocorrelations of Residuals
acf(residuals_mean$.resid, main='ACF for Mean')
acf(na.omit(residuals_naive$.resid), main="ACF for Naive")
acf(na.omit(residuals_snaive$.resid), main='ACF for Seasonal Naive')  # the only one with lowest acf values.
acf(na.omit(residuals_drift$.resid), main= 'ACF for Drift')


################################################## LINEAR REGRESSION
######### 1. Trend
tslm_trend = tslm(ts(train_data_full$Sales_residential, frequency=12) ~ trend)
summary(tslm_trend) # R-squared = 0.2133, F = 67.77 with 250 df; p < 0.0001

######### 2. Trend + Season
tslm_trend_season = tslm(ts(train_data_full$Sales_residential, frequency=12) ~ trend + season)
summary(tslm_trend_season) # R-squared = 0.7858; F = 73.06 with 239 df; p < 0.0001

########## PLOTTING AND RESIDUALS ANALYSIS
## PLOT VALUES vs FITTED
data_pre22 <- filter(data, DATE < as.Date("2022-01-01"))


p_tslm_t <- ggplot(data_pre22, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "Residential Sales (MWh)", title = "Real TimeSeries vs Fitted Values TSLM") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme() +
  geom_line(aes(y = fitted(tslm_trend)),
            color = "darkred", linetype = "twodash"
  )

plot(p_tslm_t)

p_tslm_ts <- ggplot(data_pre22, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "Residential Sales (MWh)", title = "Real TimeSeries vs Fitted Values TSLM") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme() +
  geom_line(aes(y = fitted(tslm_trend_season)),
            color = "red"
  )

plot(p_tslm_ts)


## RESIDUALS
p_tslm_res <- ggplot(data_pre22, aes(x = residuals(tslm_trend))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Histogram of residuals with Trend")+
  custom_theme()
plot(p_tslm_res)

p_tslm_ts_res_hist <- ggplot(data_pre22, aes(x = residuals(tslm_trend_season))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Histogram of residuals with Trend and Season")+
  custom_theme()
print(p_tslm_ts_res_hist)

## DW TEST (to detect autocorrelations) dw=2 desired, dw<2 positive autocorrelation.
dwtest(tslm_trend) # DW = 1.199, p<0.0001
dwtest(tslm_trend_season) # DW = 1.7889, p-value < 0.05


###### Autocorrelations
acf(residuals(tslm_trend))
acf(residuals(tslm_trend_season))


###### FORECAST
# grey area 80%
# light shaded area 95%
# PLOT forecasts
plot(forecast(tslm_trend_season, h =12), main = 'Forecast tslm_trend_season')
plot(forecast(tslm_trend, h =12), main = 'Forecast tslm_trend')

###### ACCURACIES

tslm_trend_acc <- as_tibble(accuracy(forecast(tslm_trend, h=12), test_data$Sales_residential))
tslm_trend_season_acc <- as_tibble(accuracy(forecast(tslm_trend_season, h=12), test_data$Sales_residential))

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

accuracies = bind_rows(accuracies, tslm_trend_acc)
accuracies = bind_rows(accuracies, tslm_trend_season_acc)

accuracies[accuracies$.model == 'tslm trend', 'ACF1'] = ACF1(residuals(tslm_trend))
accuracies[accuracies$.model == 'tslm trend + season', 'ACF1'] = ACF1(residuals(tslm_trend_season))

# seasonal Naive still appears to be outperforming in performance metrics


######################################## MULTIPLE LINEAR REGRESSION MODELS
# Most important thing: select which predictor we'll use
# stepwise selection
################### FULL DATASET (FROM 2001 TO 2022)
lr_fullModel = lm(Sales_residential ~ ., data=numerical_data_train)
summary(lr_fullModel) # Multiple R-squared:  0.9906;	Adjusted R-squared:  0.9893 F = 760.2 on 32 and 231 DF;  p-value: < 0.0000000022
extractAIC(lr_fullModel)
lr_step_aic = step(lr_fullModel, direction="both", trace=0, steps=1000)
summary(lr_step_aic) # Multiple R-squared:  0.9904;	Adjusted R-squared:  0.9897; F:  1259 on 20 and 243 DF;  p-value: < 0.0000000022
extractAIC(lr_step_aic)


################### PARTIAL DATASET (FROM 2012 TO 2022)

# Note: stepwise function getting warnings since fit is already 1
lr_splitModel = lm(Sales_residential ~ ., data=numerical_data_train_split)
summary(lr_splitModel) # Multiple R-squared: 1; Adjusted R-squared 1; F: 3.704e+10 on 43 and 88 DF,  p-value: < 0.00000000000000022
extractAIC(lr_splitModel)
lr_step_aic_split = step(lr_splitModel, direction="both", trace=0, steps=1000)
summary(lr_step_aic_split) # Multiple R-squared: 1; Adjusted R-squared: 1; F: 7.939e+10 on 21 and 110 DF,  p-value: < 0.00000000000000022
extractAIC(lr_step_aic_split)

summary(lr_step_aic_split)



#assess multicollinearity through VIF
vif(lr_step_aic)
vif(lr_step_aic_split)  # curious to see the different VIF between the two models. In general the second has higher VIF.

#reduce collinearity FULL
mlr = reduce_multicollinearity(lr_step_aic)
vif(mlr)
summary(mlr) # Multiple R-squared:  0.9606;	Adjusted R-squared:  0.9586; F:   469 on 13 and 250 DF;  p-value: < 0.00000000000000022
extractAIC(mlr)


##### For report
# Print model table summary variables

# Get summary
model_summary <- summary(mlr)

# Extract coefficients and p-values
coeffs <- model_summary$coefficients
variables_pvalues <- data.frame(
  Variable = rownames(coeffs),
  P_Value = coeffs[, "Pr(>|t|)"]
)

# Create flextable
colformat_double(flextable(variables_pvalues), digits = 6)
####


# check autocorrelations in residuals
acf(residuals(mlr))
acf(residuals(lr_fullModel))


#reduce collinearity SPLIT
mlr_split = reduce_multicollinearity(lr_step_aic_split)
vif(mlr_split)
summary(mlr_split) # Multiple R-squared:  0.9356;	Adjusted R-squared:  0.932; F: 258.5 on 14 and 249 DF;  p-value: < 0.00000000000000022
extractAIC(mlr_split)
# check autocorrelations in residuals
acf(residuals(mlr_split)) # more negative autocorrelation between lag 15 and 20.


# IN GENERAL FULL MODEL IS BETTER

predict_mlr = predict(mlr, newdata = numerical_test)
predict_mlr_split = predict(mlr_split, newdata = numerical_test)

# probably waste code but it works for the plot so leave it.
test_data_mlr = test_data
test_data_mlr$DATE = as.Date(test_data$DATE)

# Plot the actual test data
plot(test_data_mlr$DATE, test_data$Sales_residential, type = "l", col = "black", lwd = 2,
     xlab = "Month", ylab = "Response Variable", main = "MLR Predictions vs Actual",  xaxt = "n")+
  axis(side = 1, at = test_data_mlr$DATE, labels = 1:12)+
  lines(test_data_mlr$DATE, predict_mlr, col = "#ffa8a8", lwd = 2)

legend("topleft", legend = c("Actual", "MLR Predictions"), col = c("black", "red"), lwd = 2)

# Plot the actual test data
plot(test_data_mlr$DATE, test_data$Sales_residential, type = "l", col = "black", lwd = 2, ylim = range(c(test_data$Sales_residential, predict_mlr_split)),
     xlab = "Month", ylab = "Response Variable", main = "MLR Predictions split vs Actual", xaxt="n")+
  axis(side = 1, at = test_data_mlr$DATE, labels = 1:12)+
  lines(test_data_mlr$DATE, predict_mlr_split, col = "#ffa8a8", lwd = 2)

legend("topleft", legend = c("Actual", "MLR Predictions split"), col = c("black", "red"), lwd = 2)


# Accuracies

accuracy_mlr = accuracy(predict_mlr, test_data$Sales_residential)
accuracy_mlr_split = accuracy(predict_mlr_split, test_data$Sales_residential)



accuracy_mlr=as_tibble(accuracy(predict_mlr, test_data$Sales_residential))
accuracy_mlr$.model = 'MLR'
accuracy_mlr$.type = 'Test'
accuracy_mlr$MASE= NA
accuracy_mlr$RMSSE = NA
accuracy_mlr$ACF1 = ACF1(residuals(mlr))
accuracy_mlr = accuracy_mlr[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracy_mlr_split=as_tibble(accuracy(predict_mlr_split, test_data$Sales_residential))
accuracy_mlr_split$.model = 'MLR 2012'
accuracy_mlr_split$.type = 'Test'
accuracy_mlr_split$MASE= NA
accuracy_mlr_split$RMSSE = NA
accuracy_mlr_split$ACF1 = ACF1(residuals(mlr_split))
accuracy_mlr_split = accuracy_mlr_split[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracies= bind_rows(accuracies, accuracy_mlr)
accuracies = bind_rows(accuracies, accuracy_mlr_split)


# Plot correlations of selected variables
variables_sel <- c("Sales_residential", names(mlr$coefficients)[-1]) 
data_tsbl[variables_sel] |>
  GGally::ggpairs()



################################################# Exponential Smoothing

# We want a method that takes into account seasonality as we have seen
# Forecasts produced using exponential smoothing methods are weighted averages
# of past observations, with the weights decaying exponentially as the
# observations get older. In other words, the more recent the observation
# the higher the associated weight. This framework generates reliable 
# forecasts quickly and for a wide range of time series, which is a great
# advantage and of major importance to applications in industry.

# Holt Winters seasonal method
# Holt (1957) and Winters (1960) extended Holt’s method to capture seasonality.
# The Holt-Winters seasonal method comprises the forecast equation and three 
# smoothing equations — one for the level  one for trend and one for seasonal component
# So HW is an extension of exponential smoothing that also works when we have a trend



# hw Returns forecasts and other information for exponential smoothing forecasts applied to y.
# Additive
hw1<- hw(ressales_ts_train, seasonal="additive", h = 12)
# Multiplicative
hw2<- hw(ressales_ts_train, seasonal="multiplicative", h = 12)

#Plot forecast ADDITIVE
autoplot(ressales_ts)+
  autolayer(hw1, series="HW additive", PI=F) + 
  theme(legend.position = c(0.8, 0.2))

#Plot forecast MULTIPLICATIVE
autoplot(ressales_ts)+
  autolayer(hw2, series="HW multiplicative", PI=F)+ 
  theme(legend.position = c(0.8, 0.2))

summary(hw1) # AIC: 6488.702
summary(hw2) # AIC: 6482.330


# Compute accuracies
hw1_acc <- accuracy(hw1, test_data$Sales_residential) # additive performs better
hw1_acc = as_tibble_row(hw1_acc[2,])
hw1_acc$.model = 'HW additive'
hw1_acc$.type = 'Test'
hw1_acc$RMSSE = NA
hw1_acc$ACF1 = ACF1(residuals(hw1))
hw1_acc = hw1_acc[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

hw2_acc <- accuracy(hw2, test_data$Sales_residential)
hw2_acc = as_tibble_row(hw2_acc[2,])
hw2_acc$.model = 'HW multiplicative'
hw2_acc$.type = 'Test'
hw2_acc$RMSSE = NA
hw2_acc$ACF1 = ACF1(residuals(hw2))
hw2_acc = hw2_acc[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]


accuracies = bind_rows(accuracies, hw1_acc)
accuracies = bind_rows(accuracies, hw2_acc)

# The ETS models are a family of time series models with an underlying 
# state space model consisting of a level component, a trend component 
# (T), a seasonal component (S), and an error term (E). i.e. Exponential smoothing


###################################### ARIMA Models
# While exponential smoothing models are based on a description of the trend
# and seasonality in the data, ARIMA models aim to describe the autocorrelations
# in the data.

# p: autoregresive component (AR): Captures the linear relationship between the current observation and its previous values (lags).
# d: differencing of the time series to achieve stationarity. degrees of differencing needed
# q: MA (Moving Average): Models the short-term, unobserved shocks or random fluctuations in the data.

# Differencing required before running ARIMA:
plot(p_ressales)
Acf(data_tsbl$Sales_residential)

# One way to make a non-stationary time series stationary — 
#compute the differences between consecutive observations. 
#This is known as differencing.
ressales_ts_df <- diff(ressales_ts,  lag = 1)
tsdisplay(ressales_ts_df)

# After differencing ther is still seasonality present, we can difference a second 
# time

ressales_ts_df_2 <- diff(ressales_ts_df,  lag = 1)
tsdisplay(ressales_ts_df_2)

# Plot differentiated data to check for stationarity:

p_ts_df <- autoplot(ressales_ts_df, xlab = "Time", ylab = "ResidentialSales") +
  ggtitle("Residential Sales (MWh) - Differentiated Series")+
  custom_theme()

plot(p_ts_df)
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
arima_model = auto.arima(train_data_full$Sales_residential,
                         lambda = "auto",
                         stepwise = TRUE,
                         seasonal = TRUE,
                         trace = TRUE,
                         approximation = FALSE,
                         allowdrift = TRUE,
                         nmodels = 500,
                         max.p = 12,
                         max.q = 12,
                         max.P = 12,
                         max.Q = 12,
                         max.order = 6,
                         max.d = 2,
                         max.D = 2,)
summary(arima_model) # AIC=378.89. ARIMA(5,1,0)

# let's try to use this model but to model the seasonality which apparently 
# is not considered by auto.arima.


# MANUAL SARIMA
s

# order	
# A specification of the non-seasonal part of the ARIMA model: the three integer components 
# (p,d,q) are the AR order, the degree of differencing, and the MA order.

# seasonal	
# A specification of the seasonal part of the ARIMA model, plus the period
# (which defaults to frequency(x)). This may be a list with components order 
# and period, or just a numeric vector of length 3 which specifies the seasonal
# order. In the latter case the default period is used.


### FORECAST
forecast_arima = forecast(arima_model, h=12)
forecast_arima0 = forecast(arimamodel0, h=12)
### PLOT AUTO ARIMA
autoplot(forecast_arima, main = 'ARIMA Forecast on sales')+
  custom_theme()

### PLOT MANUAL ARIMA
autoplot(forecast_arima0, main = 'ARIMA Forecast on sales')+
  custom_theme()


### ACCURACY
arima_accuracy=accuracy(forecast_arima, test_data$Sales_residential)
arima_accuracy = as_tibble_row(arima_accuracy[2,])
arima_accuracy$.model = 'ARIMA'
arima_accuracy$.type = 'Test'
arima_accuracy$RMSSE =NA
arima_accuracy$ACF1 = ACF1(residuals(arima_model))
arima_accuracy = arima_accuracy[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]
arima_accuracy

print_nice(arima_accuracy)

accuracies= bind_rows(accuracies, arima_accuracy)



arima_accuracy0=accuracy(forecast_arima0, test_data$Sales_residential)
arima_accuracy0 = as_tibble_row(arima_accuracy0[2,])
arima_accuracy0$.model = 'ARIMA_manual'
arima_accuracy0$.type = 'Test'
arima_accuracy0$RMSSE =NA
arima_accuracy0$ACF1 = ACF1(residuals(arimamodel0))
arima_accuracy0 = arima_accuracy0[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]
arima_accuracy0

print_nice(arima_accuracy0)

accuracies= bind_rows(accuracies, arima_accuracy0)


### RESIDUALS
p_arima_res <- ggplot(ressales_ts_arima_train, aes(x = residuals(arima_model))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Reisduals auto arima")+
  custom_theme()

p_arima_res0 <- ggplot(ressales_ts_arima_train, aes(x = residuals(arimamodel0))) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(x = "Value", y = "Frequency", title = "Residuals manual arima")+
  custom_theme()

plot(p_arima_res)
plot(p_arima_res0)


shapiro.test(residuals(arima_model)) # suggest is not normal
shapiro.test(residuals(arimamodel0)) # suggest is not normal

### AUTOCORRELATIONS


ggAcf(arima_model$residuals) # still some seasonality
Pacf(residuals(arima_model))

ggAcf(arimamodel0$residuals) # MUCH better. values are all under the threshold, even if seasonality still present.
Pacf(arimamodel0$residuals) # very good indeed, even if some values are over the threshold.

#  IMPORTANT: We can ignore one significant spike in each plot if it is just 
# outside the limits, and not in the first few lags.





############################ KNN

pred <- knn_forecasting(train_data_full$Sales_residential, h = 12, lags = 1:12, k = 2, msas = "MIMO", transform = 'additive')
autoplot(pred, highlight = "neighbors", faceting = FALSE)+
  custom_theme()


# The prediction is the average of the target vectors of the two nearest neighbors.
#  As can be observed, we have chosen to see all the nearest neighbors in the same plot.
# Because we are working with a monthly time series, we have thought that lags 1-12 are a suitable choice for selecting the features of the examples.

ro <- rolling_origin(pred, h = 12, rolling=FALSE)

ro$global_accu




knn_acc = as_tibble_row(ro$global_accu)
knn_acc$.model = 'KNN'
knn_acc$.type = 'Test'
knn_acc$RMSSE =NA
knn_acc$ME = NA
knn_acc$MPE = NA
knn_acc$ACF1 = NA
knn_acc$MASE = NA
knn_acc = knn_acc[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracies= bind_rows(accuracies, knn_acc)

print_nice(filter(accuracies, .model == 'KNN'))

################################## Generalized Additive Model (GAM)
# In statistics, a generalized additive model (GAM) is a generalized
# linear model in which the linear response variable depends linearly
# on unknown smooth functions of some predictor variables, and interest
# focuses on inference about these smooth functions.
library(gam)


# First pull our baseline linear regression model
# Create a time variable to index the time points in the series
tt <- 1:length(train_data_full$Sales_residential)
# Create a seasonal factor representing the months of the year
seas <- factor(rep(1:12, length.out = length(train_data_full$Sales_residential)))


g1 <- lm(train_data_full$Sales_residential ~ tt+seas) # baseline linear regression model
# seas is included as a predictor, allowing the model to account for seasonal variations.
summary(g1) # Multiple R-squared:  0.7941,	Adjusted R-squared:  0.7842 F-statistic: 80.65 on 12 and 251 DF,  p-value: < 0.00000000000000022
AIC(g1) # 6062.793
plot(as.numeric(ressales_ts), type="l")
lines(fitted(g1), col ="red") # fit according to GAM (g1)

# add smoothing spline for trend
g2 <- gam(train_data_full$Sales_residential ~ s(tt)+seas)
# s(tt) specifies a smooth term for the time variable tt. The s() function 
#is used to fit a non-linear spline to the time variable, capturing 
#non-linear trends in the data.
summary(g2) # have ANOVA for parametric and non-parametric effects: both effects are significant
# time has a non-linear effect
plot(g2, se=T) # diagnostic plot called "Partial Residuals vs. Fitted Values."
AIC(g2) # 6054.869

####try another option with loess (lo)
g3<- gam(train_data_full$Sales_residential ~ lo(tt)+seas)
# lo() Specify a loess fit in a GAM formula
summary(g3) # simmilar results to g2
plot(g3, se=T)
AIC(g3) # 6054.832

# Residual Analysis
tsdisplay(residuals(g3)) # non significant residuals

# Plot better later
plot(as.numeric(ressales_ts), type="l")
lines(fitted(g3), col ="red") # fit according to GAM (g1)

# Next, try more complicated gam with gam.scope for stepwise selection

numeric_train_data <- train_data_full |>
  select_if(is.numeric)

# removed ind and transportation customers since they had less than 3 unique values
g4 <- gam(Sales_residential~.-Customers_industrial-Customers_transportation-DATE, data=numeric_train_data)

#Show the linear effects
par(mfrow=c(3,5))
plot(g4, se=T)
summary(g4)

#Perform stepwise selection procedure using gam scope
#Values for df should be greater than 1, with df=1 implying a linear fit
#determines set of variables that need to be inserted in final model

sc = gam.scope(numeric_train_data,
               response=which(names(numeric_train_data) == "Sales_residential"),
               arg=c("df=2","df=3","df=4"), smoother='s')
#Builds a GAM model in a step-wise fashion. For each "term" there is an ordered
#list of alternatives, and the function traverses these in a greedy fashion.
g5<- step.Gam(g4, scope=sc, trace=T) # use first model g4 as starting point and then add scope

summary(g5) # sig effects for both parametetric and nonparametric

AIC(g4) #5045.061
AIC(g5) #4716.624

par(mfrow=c(3,5))
plot(g5, se=T)

# if we want to see better some plots
par(mfrow=c(1,1))
#plot(g5, se=T, ask=t)

#Prediction
p.gam <- predict(g5, newdata=test_data)
gam_accuracy <- accuracy(p.gam, test_data$Sales_residential)
gam_accuracy 

# TRY ON SPLIT DATASET
numeric_train_split_data <- train_data_split |>
  select_if(is.numeric)

g4_split <- gam(Sales_residential~.-Customers_industrial-Customers_transportation-DATE-Electric.Generators..Electric.Utilities , data=numeric_train_split_data)

par(mfrow=c(3,5))
plot(g4_split, se=T)
summary(g4_split)

sc_split = gam.scope(numeric_train_data,
                     response=which(names(numeric_train_data) == "Sales_residential")
                     , arg=c("df=2","df=3","df=4"))
g5_split<- step.Gam(g4_split, scope=sc_split, trace=T)

summary(g5_split) # not significant for non-parametric
# Warning message:
# In anova.lm(object.lm, ...) :
#   ANOVA F-tests on an essentially perfect fit are unreliable

AIC(g4_split) #159.7907
AIC(g5_split) #141.1391

#compare to full
AIC(g4) #5045.061
AIC(g5) #4716.624

#Prediction
p.gam.split <- predict(g5_split,newdata=test_data)
gam_split_accuracy <- accuracy(p.gam.split, test_data$Sales_residential)




accuracy_gam = accuracy(p.gam, test_data$Sales_residential)
accuracy_gam = as_tibble(accuracy_gam)
accuracy_gam$.model = 'GAM complete'
accuracy_gam$.type = 'Test'
accuracy_gam$RMSSE =NA
accuracy_gam$ACF1 = NA
accuracy_gam$MASE = NA
accuracy_gam = accuracy_gam[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracy_gam_split = accuracy(p.gam.split, test_data$Sales_residential)
accuracy_gam_split = as_tibble(accuracy_gam_split)
accuracy_gam_split$.model = 'GAM splitted'
accuracy_gam_split$.type = 'Test'
accuracy_gam_split$RMSSE =NA
accuracy_gam_split$ACF1 = NA
accuracy_gam_split$MASE = NA
accuracy_gam_split = accuracy_gam_split[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]


accuracies = bind_rows(accuracies, accuracy_gam)
accuracies = bind_rows(accuracies, accuracy_gam_split)
##################################






################## GRADIENT BOOSTING

library(xgboost)
library(gbm)

train_gbm = train_data_full[, !names(train_data_full) %in% "DATE", drop = FALSE] #it gave errors with the DATE variable.

# Generalized Boosted Regression Modeling (GBM)
model_gbm = gbm(Sales_residential ~.,
                data = train_gbm,
                distribution = "gaussian",
                cv.folds = 5,
                shrinkage = .001,
                n.minobsinnode = 10,
                n.trees = 20000)


#i've tried several parameters and those seems the best.

############# ACCURACIES

accuracy(model_gbm$fit, train_data_full$Sales_residential) # on train
pred_y = predict.gbm(model_gbm, test_data)
accuracy_gb = accuracy(pred_y, test_data$Sales_residential) # on test, no overfitting and good results
accuracy_gb

print_nice(accuracy_gb)

accuracy_gb = as_tibble(accuracy_gb)
accuracy_gb$.model = 'Gradient Boosting'
accuracy_gb$.type = 'Test'
accuracy_gb$RMSSE =NA
accuracy_gb$ACF1 = NA
accuracy_gb$MASE = NA
accuracy_gb = accuracy_gb[, c('.model','.type','ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE','RMSSE','ACF1')]

accuracies= bind_rows(accuracies, accuracy_gb)

# PLOTTING
x_ax = 1:length(pred_y)
plot(x_ax, test_data$Sales_residential, col="blue", pch=20,
     cex=.9, type='l', xlab="Month", ylab="Sales_residential")
lines(x_ax, pred_y, col="red", pch=20, cex=.9)
legend("topright",            # Position of the legend
       legend=c("Data", "GBM Prediction"), # Text
       col=c("blue", "red"),  # Colors
       pch=20,               # Type of point
       lty=1)                # Type of line
title(main="Boosting Prediction")


x_ax_train = 1:length(train_data_full$Sales_residential)
plot(x_ax_train, train_data_full$Sales_residential, col="black", pch=20,
     cex=.9, type='l', xlab="Month", ylab="Sales_residential")
lines(x_ax_train, model_gbm$fit, col="red", pch=20, cex=.9)
legend("topright",            # Position of the legend
       legend=c("Data", "GBM fit"), # Text
       col=c("black", "red"),  # Colors
       pch=20,               # Type of point
       lty=1)                # Type of line
title(main="Boosting fit")


##################################### CHOOSING THE BEST MODEL ###############################

# lets recap.
# This is a summary of all the accuracies that we have right now.


# let's consider RMSE as our main measure and print it ordered by it.
final_acc = accuracies |>
  select(-MASE, -RMSSE)|>
  arrange(RMSE) # simple models + Multiple linear regression + exp smoothing + ARIMA + KNN


summary(mlr) # Multiple R-squared:  0.9606;	Adjusted R-squared:  0.9586; F:   469 on 13 and 250 DF;  p-value: < 0.00000000000000022
extractAIC(mlr)

summary(mlr_split) # Multiple R-squared:  0.9356;	Adjusted R-squared:  0.932; F: 258.5 on 14 and 249 DF;  p-value: < 0.00000000000000022
extractAIC(mlr_split)


# I would say best model is among HW_ADDITIVE (SPLIT VERSION)

# AIC is useless for HW models because is non-parametric.
# Nice table for report
print_nice(final_acc)



