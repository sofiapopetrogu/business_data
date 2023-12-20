# Energy Data Business Project
# Project Members: Esteban Ortega Dominguez, Mattia Varagnolo, Sofia Pope Trogu
# 2023-2024

# R Libraries

library(ggplot2)
library(dplyr)
# model libraries
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(graphics)
library(prophet)
library(lubridate)


# read data
data <- read.csv("data/energy_data.csv", sep = ';', dec = '.')

# Explore data structure and summary
str(data)
summary(data)

# Convert DATE to Date type
data$DATE <- as.Date(data$DATE, format = "%d/%m/%Y")
str(data)


# Initialize an empty data frame for numerical columns
numerical_data <- data.frame()

# Create a new data frame with only numerical columns
numerical_data <- data %>%
  select_if(is.numeric)


# Set a seed for reproducibility
set.seed(123)

# Create an index for splitting the data into training and testing sets
index <- sample(seq_len(nrow(numerical_data)), size = 0.8 * nrow(numerical_data))

# Create the training set
train_set <- numerical_data[index, ]

# Create the testing set
test_set <- numerical_data[-index, ]
 
##################################### Linear Regression and GAM with 1 feature
library(mgcv)

mod_lm = gam(Price_total ~ Customers_total, data=data)

summary(mod_lm)


mod_gam1 = gam(Price_total ~ s(Customers_total, bs='cr'), data=data)
summary(mod_gam1)

plot(mod_gam1)

AIC(mod_lm)

summary(mod_lm)$sp.criterion
summary(mod_lm)$r.sq

summary(mod_gam1)$sp.criterion
summary(mod_gam1)$r.sq

anova(mod_lm, mod_gam1, test='Chisq')

################################# Linear Regression and GAM with multiple features

mod_lm2 = gam(Price_total ~ Customers_total + tavg + prcp , data=data)
summary(mod_lm2)


mod_gam2= gam(Price_total ~ s(Customers_total) + s(tavg) +s(prcp), data=data) 
summary(mod_gam2)

library(ggeffects)
library(gratia)

gratia::draw(mod_gam2)


mod_gamdef = lm(Price_total ~ ., data=train_set)
summary(mod_gamdef)                 


mod_tslm = tslm(Price_total ~ trend+season+data$Customers_total, data=data)
                
        summary(mod_tslm)

