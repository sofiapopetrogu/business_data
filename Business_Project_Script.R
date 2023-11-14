
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

# read data
data <- read_delim("energy_data.csv", delim = ";")

# Explore data structure and summary
str(data)
summary(data)

# Did data load correctly?

# EXAMPLE CODE BELOW

# Plot data
##Identify Date variable
year_month <- data$date
##create variable for generation_mwh
gmwh <- data$generation_mwh

##make a plot
plot(year_month, gmwh, xlab="Year-Month", ylab="Energy Generation")

##Plot generation by energy source
plot_data <- generation_data %>%
  group_by(date, energy_source) %>%
  summarise(gen_by_energy = sum(generation_mwh))

## Assess Autocorrelations 

##acf of variable "gmwh"

Acf(gmwh)

# Models

## Linear Regression
##fit a linear regression model 
fit1 <- fitts<- tslm(gmwh~year_month)
summary(fit1)

##plot of the linear model
plot(year_month, gmwh, xlab="Year-Month", ylab="Energy Generation")
abline(fit1, col=3)

##Check residuals of model and see if there are autocorrelated
dwtest(fit1)
##check the residuals
resfit1<- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals" )

mac.ts<-ts(imac, frequency=4)
