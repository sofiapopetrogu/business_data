
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

# read data
data <- read.csv("data/energy_data.csv", sep = ';', dec = '.')

# Explore data structure and summary
str(data)
summary(data)

# Convert DATE to Date type
data$DATE <- as.Date(data$DATE, format = "%d/%m/%Y")


#################### PLOTTING ###################

# Plot time series for Solar, thermal and photovoltaic
plot(data$DATE, data$Solar.Thermal.and.Photovoltaic, type = "l", col= 'orange', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series of Solar | Thermal | Photovoltaic Generation", ylim = c(0, 5000))
legend('topright', legend = 'Solar | Theremal | Photovoltaic', col = 'orange', lty=1)

# Plot time series for Petroleum
plot(data$DATE, data$Petroleum, type = "l", col= 'black', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series of Petroleum generation", ylim = c(0, 100000))
legend('topright', legend = 'Petroleum', col = 'black', lty=1)

# Plot time series for natural gas
plot(data$DATE, data$Natural.Gas, type = "l", col= 'green', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series of Natural Gas", ylim = c(0, 15000))
legend('topright', legend = 'Natural Gas', col = 'green', lty=1)

# Plot time series for other biomass
plot(data$DATE, data$Other.Biomass, type = "l", col= 'red', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series of Biomass", ylim = c(0, 6000))
 legend('topright', legend = 'Biomass', col = 'red', lty=1)

 
# OVERALL PLOT

# Melt the data for ggplot
library(reshape2)
energy_data_melted <- melt(data, id.vars = "DATE", measure.vars = c('Natural.Gas','Other.Biomass','Petroleum','Solar.Thermal.and.Photovoltaic'))

# Create a bar plot
ggplot(energy_data_melted, aes(x = DATE, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(title = "Energy Generation Over Time",
       x = "Date",
       y = "Energy Generated (MWh)",
       fill = "Energy Source") +
  theme_minimal()

#PLOT BY ENERGY PRODUCERS

# Plot time series Combined Heat and Power. Commercial Power
plot(data$DATE, data$Combined.Heat.and.Power..Commercial.Power, type = "l", col= 'orange', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000))
legend('topright', legend = 'Solar | Theremal | Photovoltaic', col = 'orange', lty=1)

#TODO: check the correlation with commercial customers

# Plot time series Combined Heat and Power. Commercial Power
plot(data$DATE, data$Combined.Heat.and.Power..Electric.Power, type = "l", col= 'orange', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000))
legend('topright', legend = 'Generation of electric power', col = 'orange', lty=1)




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
