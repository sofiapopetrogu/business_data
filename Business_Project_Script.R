
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

# Add total generation columns


# negative values in generation indicate independent power producer
# example: solar power producers relying on sunlight might require independent power sources/producers
# on non-sunny days

data$Petroleum <- abs(data$Petroleum)
data$total_generation_producer <- rowSums(data[, 2:5])
data$total_generation_source <- rowSums(data[, 6:9])

head(data)

# Predicting on Petroleum no longer useful since conversion to renewable energy after 2010
# Maybe could do some prediction on later generation after the switch happens

#################### PLOTTING ###################
# Remove missing or non-finite values
valid_indices <- complete.cases(data$DATE)
data <- data[valid_indices, ]

summary(data)

# Plot time series for total generation columns: source
plot(data$DATE, data$total_generation_source, type = "l", col = "black", pch = 16, xlab = "Data", ylab = "Generation",
     main = "Time Series of Total Energy Generation from Producers", ylim = c(0, 100000))

# Since data has a wide range, apply logarithmic scale to total column
data$ltotal_generation_source <- log(data$total_generation_source)

#Plot data with log transformation
# since log 0 is undefined, you have missing values
plot(data$DATE, data$ltotal_generation_source, type = "l", col = "black", pch = 16, xlab = "Data", ylab = "Log Generation",
     main = "Time Series of Log Total Energy Generation from Producers", ylim = c(0, 20))

# We can see switch from petroleum/non-renewable energy production 
# to renewable sources starting in 2010

library(lubridate)

data$month <- month(data$DATE, label = TRUE)

ggplot(data, aes(x = data$month, y = data$total_generation_source)) +
  geom_line() +
  labs(title = "Absolute Total Generation Source by Month",
       x = "Month",
       y = "Absolute Total Generation Source")

############################################################## Average per month

head(data)

unique(data$month)
head(data$DATE)

summary(data$total_generation_source)
hist(data$total_generation_source)

avg_data <- data %>%
  group_by(data$month) %>%
  summarise(avg_abs_total_generation_source = mean(data$total_generation_source, na.rm = TRUE))

print(avg_data)
table(data$month)

# Filter the dataset for rows where Petroleum is greater than 0
petroleum_data <- subset(data, Petroleum > 0)

# Find the date and value of the maximum point
max_point <- petroleum_data[which.max(petroleum_data$Petroleum), ]

# Set the x-axis limits to go up to the year 2015
ggplot(petroleum_data, aes(x = DATE, y = Petroleum, color = "Petroleum")) +
  geom_line(size = 1.5, color = 'lightblue') +  # Adjust line width and color here
  geom_point(data = max_point, aes(x = DATE, y = Petroleum), color = 'red', size = 5) +  # Add red point at the max value
  geom_text(data = max_point, aes(x = DATE, y = Petroleum, label = round(Petroleum, 2)),
            vjust = -1, hjust = -0.5, color = 'red', size = 5, family='serif') +  # Display the value of the max point
  labs(title = "Petroleum Energy Generation",
       x = "Date",
       y = "MWh") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date('2001-01-01'), as.Date("2012-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +  # Adjust x-axis limits and labels
  theme(
    panel.grid.major = element_line(color = "white", size = 0.5),  # Adjust major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Adjust title font size
    axis.text = element_text(size = 12, family = "serif"),  # Adjust axis text font size and family
    axis.title = element_text(size = 14, family = "serif", angle = 0),  # Adjust axis title font size and set angle to 0 for horizontal
    axis.line = element_line(color = "black", size = 0.8),  # Adjust axis line color and size
    axis.ticks = element_line(color = "black")  # Adjust tick marks color
  ) +
  scale_color_manual(name = "Energy Source", values = c("Petroleum" = "blue"))


# do the same for renewables
renewable = data$total_renew_source


ggplot(data, aes(x = DATE, y = total_renew_source, color = "Petroleum")) +
  geom_line(size = 1.5, color = 'lightblue') +  # Adjust line width and color here
  geom_point(data = max_point, aes(x = DATE, y = total_renew_source), color = 'red', size = 5) +  # Add red point at the max value
  geom_text(data = max_point, aes(x = DATE, y = total_renew_source, label = round(total_renew_source, 2)),
            vjust = -1, hjust = -0.5, color = 'red', size = 5, family='serif') +  # Display the value of the max point
  labs(title = "Renewable Energy Generation",
       x = "Date",
       y = "MWh") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date('2011-01-01'), as.Date("2022-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +  # Adjust x-axis limits and labels
  theme(
    panel.grid.major = element_line(color = "white", size = 0.5),  # Adjust major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Adjust title font size
    axis.text = element_text(size = 12, family = "serif"),  # Adjust axis text font size and family
    axis.title = element_text(size = 14, family = "serif", angle = 0),  # Adjust axis title font size and set angle to 0 for horizontal
    axis.line = element_line(color = "black", size = 0.8),  # Adjust axis line color and size
    axis.ticks = element_line(color = "black")  # Adjust tick marks color
  ) +
  scale_color_manual(name = "Energy Source", values = c("Petroleum" = "blue"))



# Filter the dataset for rows where any of the renewable sources is greater than 0
renewable <- subset(data, Natural.Gas > 0 | Other.Biomass > 0 | Solar.Thermal.and.Photovoltaic > 0)

# Compute the max value for each renewable source
max_value_natural_gas <- max(renewable$Natural.Gas)
max_value_other_biomass <- max(renewable$Other.Biomass)
max_value_solar <- max(renewable$Solar.Thermal.and.Photovoltaic)

# Find the date and value of the maximum point for each renewable source
max_point_natural_gas <- renewable[which.max(renewable$Natural.Gas), ]
max_point_other_biomass <- renewable[which.max(renewable$Other.Biomass), ]
max_point_solar <- renewable[which.max(renewable$Solar.Thermal.and.Photovoltaic), ]

# Set the x-axis limits to go from 2011 to 2022
ggplot(renewable, aes(x = DATE, color = "Energy Source")) +
  geom_line(aes(y = Natural.Gas, linetype = "Natural Gas"), size = 1.5, color = alpha("red", 0.4)) +
  geom_line(aes(y = Other.Biomass, linetype = "Other Biomass"), size = 1.5, color = alpha("lightblue", 0.8)) +
  geom_line(aes(y = Solar.Thermal.and.Photovoltaic, linetype = "Solar"), size = 1.5, color = alpha("orange", 0.4)) +
  geom_point(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, color = "Natural Gas"), size = 5) +
  geom_point(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, color = "Other Biomass"), size = 5) +
  geom_point(data = max_point_solar, aes(x = DATE, y = max_value_solar, color = "Solar"), size = 5) +
  geom_text(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, label = round(max_value_natural_gas, 2)), vjust = -1, hjust = -0.5, size = 5, family='serif', color = "red") +
  geom_text(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, label = round(max_value_other_biomass, 2)), vjust = -1, hjust = -0.5, size = 5, family='serif', color = "blue") +
  geom_text(data = max_point_solar, aes(x = DATE, y = max_value_solar, label = round(max_value_solar, 2)), vjust = -1, hjust = -0.5, size = 5, family='serif', color = "orange") +
  labs(title = "Renewable Energy Generation",
       x = "Date",
       y = "MWh") +
  scale_x_date(limits = c(as.Date('2011-01-01'), as.Date("2022-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text = element_text(size = 12, family = "serif"),
    axis.title = element_text(size = 14, family = "serif", angle = 0),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(
    name = "Energy Source",
    values = c("Natural Gas" = alpha("red", 1), "Other Biomass" = alpha("blue", 0.8), "Solar" = alpha("orange", 1)),
    breaks = c("Natural Gas", "Other Biomass", "Solar"),
    labels = c("Natural Gas", "Other Biomass", "Solar"),
    guide = "legend"
  ) +
  scale_linetype_manual(
    name = "Energy Source",
    values = c("Natural Gas" = "solid", "Other Biomass" = "solid", "Solar" = "solid"),
    breaks = c("Natural Gas", "Other Biomass", "Solar"),
    labels = c("Natural Gas", "Other Biomass", "Solar"),
    guide = "legend"
  )
 
############################################################################################# OVERALL PLOT

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

############################################################################################# PLOT BY ENERGY PRODUCERS

# Plot time series Combined Heat and Power. Commercial Power
plot(data$DATE, data$Combined.Heat.and.Power..Commercial.Power, type = "l", col= 'orange', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000))
legend('topright', legend = 'Solar | Theremal | Photovoltaic', col = 'orange', lty=1)

#TODO: check the correlation with commercial customers

# Plot time series Combined Heat and Power. Commercial Power
plot(data$DATE, data$Combined.Heat.and.Power..Electric.Power, type = "l", col= 'orange', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000))
legend('topright', legend = 'Generation of electric power', col = 'orange', lty=1)

# plot electric generators from electric utilities

plot(data$DATE, data$Electric.Generators..Electric.Utilities, type = "l", col= 'orange', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series Electric Generators | electric utilities", ylim = c(0, 10000))
legend('topright', legend = 'Generation of electric power', col = 'orange', lty=1)

# plot electric generators from indipendent producers

plot(data$DATE, data$Electric.Generators..Independent.Power.Producers, type = "l", col= 'orange', pch=16 ,xlab = "Date", ylab = "Generation",
     main = "Time Series Electric Generators | electric utilities", ylim = c(0, 100000))
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
