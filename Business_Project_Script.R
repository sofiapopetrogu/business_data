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

library(qtl2)
library(readxl)
library(plotly)
library(gridExtra)

# TS analysis specific libraries
# tsibble for time series objects
library(tsibble)
library(feasts)
library(lubridate)
library(GGally)

# From the Hyndman time series book
library(fpp3)

# for VIF
library(car)


options(scipen = 999)
# read data
data <- read.csv("data/energy_data.csv", sep = ";", dec = ".")

# Explore data structure and summary
str(data)
summary(data)

# Convert DATE to Date type
data$DATE <- as.Date(data$DATE, format = "%d/%m/%Y")
str(data)

head(data)

####### Descriptions of columns found in the data dictionary

# Add total generation columns
# negative values in generation found only in independent power producer
# example: solar power producers relying on sunlight might require independent power sources/producers
# on non-sunny days

data$Petroleum <- abs(data$Petroleum)
data$total_generation_producer <- rowSums(data[, 2:5])
data$total_generation_source <- rowSums(data[, 6:9])
data$month <- month(data$DATE, label = TRUE)

summary(data)
head(data)

# Predicting on Petroleum no longer useful since conversion to renewable energy after 2010
# Maybe could do some prediction on later generation after the switch happens

#################### PLOTTING ###################
# Remove missing or non-finite values
valid_indices <- complete.cases(data$DATE)
data <- data[valid_indices, ]

summary(data)


# FIRST, WE PLOT THE OVERALL TOTAL GENERATION BY SOURCE
# Plot time series for total generation columns: source
ggplot(data, aes(x = DATE, y = total_generation_source)) +
  geom_line(color = "darkblue", size = 0.55, ) +
  labs(
    x = "Time",
    y = "Total Energy Generation (MWh)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(min(data$DATE), max(data$DATE), by = "year"),
    labels = format(seq(min(data$DATE), max(data$DATE), by = "year"), "%Y"),
    expand = c(0, 0)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# NOW WE PLOT DATA BEFORE 2011 TO SEE PETROLEUM
# Plot data pre 2011 for Petroleum Dominant Market

# Subset data for dates before 2011
pre2011_subset_data <- data[data$DATE < as.Date("2011-01-01"), ]

# Plot the subset of data before 2011
ggplot(pre2011_subset_data, aes(x = DATE, y = total_generation_source)) +
  geom_line(color = "darkblue", size = 0.6) +
  geom_smooth(method = "loess", color = "red", size = 1, span = 0.2) + # Adjust span
  labs(
    x = "Time",
    y = "Total Energy Generation (MWh)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(min(pre2011_subset_data$DATE), max(pre2011_subset_data$DATE), by = "year"),
    labels = format(seq(min(pre2011_subset_data$DATE), max(pre2011_subset_data$DATE), by = "year"), "%Y"),
    expand = c(0, 0)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# NOW WE PLOT DATA AFTER 2011 TO SEE THE SWITCH TO GAS AND RENEWABLES
# Petroleum completely stops in mid 2012

# Subset data for dates starting from 2011
post2011_subset_data <- data[data$DATE >= as.Date("2011-01-01"), ]

# Plot the subset of data
ggplot(post2011_subset_data, aes(x = DATE, y = total_generation_source)) +
  geom_line(color = "darkblue") +
  geom_smooth(method = "loess", color = "red", size = 1, span = 0.25) + # Adjust span
  labs(
    x = "Time",
    y = "Total Energy Generation (MWh)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(min(post2011_subset_data$DATE), max(post2011_subset_data$DATE), by = "year"),
    labels = format(seq(min(post2011_subset_data$DATE), max(post2011_subset_data$DATE), by = "year"), "%Y"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, 90000), breaks = seq(0, 90000, by = 5000)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Since data has a wide range, apply logarithmic scale to total column
# data$ltotal_generation_source <- log(data$total_generation_source)

# Plot data with log transformation
# since log 0 is undefined, you have missing values
# plot(data$DATE, data$ltotal_generation_source, type = "l", col = "black", pch = 16, xlab = "Data", ylab = "Log Generation",
#     main = "Time Series of Log Total Energy Generation from Producers", ylim = c(0, 20))

# We can see switch from petroleum/non-renewable energy production
# to renewable sources starting in 2010


# ggplot(data, aes(x = month, y = total_generation_source)) +
#   geom_line() +
#   labs(title = "Absolute Total Generation Source by Month",
#        x = "Month",
#        y = "Absolute Total Generation Source") +
#   theme(panel.grid = element_blank())
#
# # pre 2011 subset
# ggplot(pre2011_subset_data, aes(x = month, y = total_generation_source)) +
#   geom_line() +
#   labs(title = "Absolute Total Generation Source by Month",
#        x = "Month",
#        y = "Absolute Total Generation Source")
#
# # post 2011 subset
# ggplot(post2011_subset_data, aes(x = month, y = total_generation_source)) +
#   geom_line() +
#   labs(title = "Absolute Total Generation Source by Month",
#        x = "Month",
#        y = "Absolute Total Generation Source")

############################################################## Average per month

summary(data$total_generation_source)
hist(data$total_generation_source)


# Average monthly energy generation
avg_data <- data %>%
  group_by(month = factor(month)) %>%
  summarise(avg_generation = mean(total_generation_source, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 10000, TRUE, FALSE))

avg_data$energysource <- "All"

ggplot(avg_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Generation") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Other seasons", "Summer")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = "Helvetica Neue"),
    title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000)) +
  geom_text(
    data = filter(avg_data, month == "lug"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  )






# Average monthly energy generation from petroleum pre 2011
pre2011_avg_pet_data <- pre2011_subset_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Petroleum, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 10000, TRUE, FALSE))

pre2011_avg_pet_data$energysource <- "Petroleum"

ggplot(pre2011_avg_pet_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Petroleum usage") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Other seasons", "Summer")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = "Helvetica Neue"),
    title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000)) +
  geom_text(
    data = filter(pre2011_avg_pet_data, month == "ago"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  )

# Average monthly energy generation from renewables sources post 2011
post2011_avg_ren_data <- post2011_subset_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(total_renew_source, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 3500, TRUE, FALSE))

post2011_avg_ren_data$energysource <- "Renewables"


ggplot(post2011_avg_ren_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Renewables usage") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Normal", "Highest")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    text = element_text(hjust = 1, family = "Helvetica Neue"),
    title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 1000)) +
  geom_text(
    data = filter(post2011_avg_ren_data, month == "dec"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  )

# Average monthly energy generation from solar
# solar data (starting in 2019)
solar_data <- subset(data, Solar.Thermal.and.Photovoltaic > 0)

avg_solar_data <- solar_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Solar.Thermal.and.Photovoltaic, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 1600, TRUE, FALSE))

ggplot(avg_solar_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Renewables usage") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Normal", "Highest")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = "Helvetica Neue"),
    title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 2000)) +
  geom_text(
    data = filter(avg_solar_data, month == "mag"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  )





gas_data <- subset(data, Natural.Gas > 0)

avg_gas_data <- gas_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Natural.Gas, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 6500, TRUE, FALSE))

avg_gas_data$energysource <- "Gas"


ggplot(avg_gas_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Natural Gas usage") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Normal", "Highest")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = "Helvetica Neue"),
    title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 2000)) +
  geom_text(
    data = filter(avg_gas_data, month == "lug"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  )




######################## COMBINED DATA PLOT


combined_data <- rbind(avg_data, pre2011_avg_pet_data)
combined_data <- rbind(combined_data, post2011_avg_ren_data)
combined_data <- rbind(combined_data, avg_gas_data)
print(n = 30, combined_data)

ggplot(
  combined_data,
  aes(fill = energysource, y = avg_generation, x = month)
) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("All" = "white", "Petroleum" = "#6d93d1", "Renewables" = "#32558c", "Gas" = "#b1c3e0")) +
  labs(
    title = "Average Generation by Source",
    x = "Month",
    y = "Average Generation (MWh)",
    fill = "Energy Source"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Helvetica Neue"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", family = "Helvetica Neue"),
    axis.text.y = element_text(hjust = 1, family = "Helvetica Neue")
  ) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Filter the dataset for rows where Petroleum is greater than 0
petroleum_data <- subset(data, Petroleum > 0)

# Find the date and value of the maximum point
max_point <- petroleum_data[which.max(petroleum_data$Petroleum), ]

# Set the x-axis limits to go up to the year 2015
ggplot(petroleum_data, aes(x = DATE, y = Petroleum, color = "Petroleum")) +
  geom_line(size = 1, color = "darkblue") + # Adjust line width and color here
  geom_point(data = max_point, aes(x = DATE, y = Petroleum), color = "red", size = 3) +
  geom_text(
    data = max_point, aes(x = DATE, y = Petroleum, label = paste("max: ", round(Petroleum, 2))),
    vjust = 0.2, hjust = -0.3, color = "black", size = 5, family = "serif"
  ) + # Display the value of the max point
  labs(
    title = "Petroleum Energy Generation",
    x = "Time",
    y = "MWh"
  ) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2001-01-01"), as.Date("2015-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") + # Adjust x-axis limits and labels
  theme(
    panel.grid.major = element_line(color = "white", size = 0.5), # Adjust major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16), # Adjust title font size
    axis.text = element_text(size = 12, family = "serif"), # Adjust axis text font size and family
    axis.title = element_text(size = 14, family = "serif", angle = 0), # Adjust axis title font size and set angle to 0 for horizontal
    axis.line = element_line(color = "black", size = 0.8), # Adjust axis line color and size
    axis.ticks = element_line(color = "black"), # Adjust tick marks color
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels at a 45-degree angle
  ) +
  scale_color_manual(name = "Energy Source", values = c("Petroleum" = "blue"))

# do the same for renewables
renewable <- data$total_renew_source # total_renew_source combines solar and biomass

ggplot(data, aes(x = DATE, y = total_renew_source, color = "Renewable")) +
  geom_line(size = 1, color = "darkblue") + # Adjust line width and color here
  labs(
    title = "Renewable Energy Generation",
    x = "Time",
    y = "MWh"
  ) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2011-01-01"), as.Date("2022-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") + # Adjust x-axis limits and labels
  theme(
    panel.grid.major = element_line(color = "white", size = 0.5), # Adjust major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16), # Adjust title font size
    axis.text = element_text(size = 12, family = "serif"), # Adjust axis text font size and family
    axis.title = element_text(size = 14, family = "serif", angle = 0), # Adjust axis title font size and set angle to 0 for horizontal
    axis.line = element_line(color = "black", size = 0.8), # Adjust axis line color and size
    axis.ticks = element_line(color = "black"), # Adjust tick marks color
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels at a 45-degree angle
  ) +
  scale_color_manual(name = "Energy Source", values = c("Renewable" = "blue"))




# Filter the dataset for rows where any of the renewable sources is greater than 0
renewable <- subset(data, Natural.Gas > 0 | Other.Biomass > 0 | Solar.Thermal.and.Photovoltaic > 0)
# why?

# Compute the max value for each renewable source
max_value_natural_gas <- max(renewable$Natural.Gas)
max_value_other_biomass <- max(renewable$Other.Biomass)
max_value_solar <- max(renewable$Solar.Thermal.and.Photovoltaic)

# Find the date and value of the maximum point for each renewable source
max_point_natural_gas <- renewable[which.max(renewable$Natural.Gas), ]
max_point_other_biomass <- renewable[which.max(renewable$Other.Biomass), ]
max_point_solar <- renewable[which.max(renewable$Solar.Thermal.and.Photovoltaic), ]


# NOT SUPER READABLE
# Set the x-axis limits to go from 2011 to 2022
ggplot(renewable, aes(x = DATE, fill = "Energy Source")) +
  geom_bar(aes(y = Natural.Gas), stat = "identity", color = alpha("red", 0.4)) +
  geom_bar(aes(y = Other.Biomass), stat = "identity", color = alpha("lightblue", 0.8)) +
  geom_bar(aes(y = Solar.Thermal.and.Photovoltaic), stat = "identity", color = alpha("orange", 0.4)) +
  geom_point(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, color = "Natural Gas"), size = 5) +
  geom_point(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, color = "Other Biomass"), size = 5) +
  geom_point(data = max_point_solar, aes(x = DATE, y = max_value_solar, color = "Solar"), size = 5) +
  geom_text(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, label = round(max_value_natural_gas, 2)), vjust = -1, hjust = -0.5, size = 5, family = "serif", color = "red") +
  geom_text(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, label = round(max_value_other_biomass, 2)), vjust = -1, hjust = -0.5, size = 5, family = "serif", color = "blue") +
  geom_text(data = max_point_solar, aes(x = DATE, y = max_value_solar, label = round(max_value_solar, 2)), vjust = -1, hjust = -0.5, size = 5, family = "serif", color = "orange") +
  labs(
    title = "Current  Energy Generation",
    x = "Time",
    y = "MWh"
  ) +
  scale_x_date(limits = c(as.Date("2011-01-01"), as.Date("2022-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +
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
  scale_fill_manual(
    name = "Energy Source",
    values = c("Natural Gas" = alpha("red", 1), "Other Biomass" = alpha("blue", 0.8), "Solar" = alpha("orange", 1)),
    breaks = c("Natural Gas", "Other Biomass", "Solar"),
    labels = c("Natural Gas", "Other Biomass", "Solar"),
    guide = "legend"
  )

# Line version of the previous plot
# Set the x-axis limits to go from 2011 to 2022
ggplot(renewable, aes(x = DATE, color = "Energy Source")) +
  geom_line(aes(y = Natural.Gas, linetype = "Natural Gas"), size = 1.5, color = alpha("red", 0.4)) +
  geom_line(aes(y = Other.Biomass, linetype = "Biomass"), size = 1.5, color = alpha("lightblue", 0.8)) +
  geom_line(aes(y = Solar.Thermal.and.Photovoltaic, linetype = "Solar"), size = 1.5, color = alpha("orange", 0.4)) +
  geom_point(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, color = "Natural Gas"), size = 5) +
  geom_point(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, color = "Biomass"), size = 5) +
  geom_point(data = max_point_solar, aes(x = DATE, y = max_value_solar, color = "Solar"), size = 5) +
  geom_text(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, label = paste("gas max: ", round(max_value_natural_gas, 2))), vjust = 0, hjust = 1.2, size = 4, family = "serif", color = "red") +
  geom_text(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, label = paste("biomass max: ", round(max_value_other_biomass, 2))), vjust = -0.9, hjust = 1.2, size = 4, family = "serif", color = "blue") +
  geom_text(data = max_point_solar, aes(x = DATE, y = max_value_solar, label = paste("solar max: ", round(max_value_solar, 2))), vjust = 0, hjust = 1.1, size = 4, family = "serif", color = "orange") +
  labs(
    title = "Current  Energy Generation",
    x = "Time",
    y = "MWh"
  ) +
  scale_x_date(limits = c(as.Date("2011-01-01"), as.Date("2022-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +
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
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels at a 45-degree angle
  ) +
  scale_color_manual(
    name = "Energy Source",
    values = c("Natural Gas" = alpha("red", 1), "Biomass" = alpha("blue", 0.8), "Solar" = alpha("orange", 1)),
    breaks = c("Natural Gas", "Biomass", "Solar"),
    labels = c("Natural Gas", "Biomass", "Solar"),
    guide = "legend"
  ) +
  scale_linetype_manual(
    name = "Energy Source",
    values = c("Natural Gas" = "solid", "Biomass" = "solid", "Solar" = "solid"),
    breaks = c("Natural Gas", "Biomass", "Solar"),
    labels = c("Natural Gas", "Biomass", "Solar"),
    guide = "legend"
  ) +
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 2500))

############################################################## OVERALL PLOT

# Melt the data for ggplot
library(reshape2)
energy_data_melted <- melt(data, id.vars = "DATE", measure.vars = c("Natural.Gas", "Other.Biomass", "Petroleum", "Solar.Thermal.and.Photovoltaic"))

# Create a bar plot
ggplot(energy_data_melted, aes(x = DATE, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(
    title = "Energy Generation Over Time",
    x = "Time",
    y = "Energy Generated (MWh)",
    fill = "Energy Source"
  ) +
  theme_minimal()



################################################### PLOT BY ENERGY PRODUCERS

# Plot time series Combined Heat and Power. Commercial Power
plot(data$DATE, data$Combined.Heat.and.Power..Commercial.Power,
  type = "l", col = "orange", pch = 16, xlab = "Date", ylab = "Generation",
  main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000)
)
legend("topright", legend = "Solar | Thermal | Photovoltaic", col = "orange", lty = 1)

# TODO: check the correlation with commercial customers

# Plot time series Combined Heat and Power. Commercial Power
plot(data$DATE, data$Combined.Heat.and.Power..Electric.Power,
  type = "l", col = "orange", pch = 16, xlab = "Date", ylab = "Generation",
  main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000)
)
legend("topright", legend = "Generation of electric power", col = "orange", lty = 1)







#################################################### CUSTOMERS

# Note about customers: when customers = 0, this doesn't make sense. Likely
# data was not being accuractely collected then
# Starting in 2007, data collection begins to become more reliable
filtered_residential <- data[data$Customers_residential > 0, ]
filtered_commercial <- data[data$Customers_commercial > 0, ]
filtered_industrial <- data[data$Customers_industrial > 0, ]
filtered_transportation <- data[data$Customers_transportation > 0, ]
filtered_total <- data[data$Customers_total > 0, ]

# Function to create a common theme
custom_theme <- function() {
  theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
}

# option to remove scientific notation
options(scipen = 999)
# Plot for Customers_residential (houses)
ggplot(filtered_residential, aes(x = DATE, y = Customers_residential)) +
  geom_line(colour = "orange") +
  labs(x = "Time", y = "Number of customers", title = "Residential's customers") +
  scale_y_continuous(limits = c(0, max(filtered_residential$Customers_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Customers_commercial (malls, businesses)
ggplot(filtered_commercial, aes(x = DATE, y = Customers_commercial)) +
  geom_line(colour = "blue") +
  labs(x = "Time", y = "Number of customers", title = "Commercial's customers") +
  scale_y_continuous(limits = c(0, max(filtered_commercial$Customers_commercial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Customers_industrial (factory)
ggplot(filtered_industrial, aes(x = DATE, y = Customers_industrial)) +
  geom_line(colour = "green") +
  labs(x = "Time", y = "Number of customers", title = "Industrial's customers") +
  scale_y_continuous(limits = c(0, max(filtered_industrial$Customers_industrial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Customers_transportation (cars, trucks, trains, planes, and boats)
ggplot(filtered_transportation, aes(x = DATE, y = Customers_transportation)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "Number of customers", title = "Transportation's customers") +
  scale_y_continuous(limits = c(0, max(filtered_transportation$Customers_transportation))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Total
ggplot(filtered_total, aes(x = DATE, y = Customers_total)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "Number of customers", title = "Transportation's customers") +
  scale_y_continuous(limits = c(0, max(filtered_total$Customers_total))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()



####################################################### SALES AND MONEYYY

# Plot for Residential
# seasonality and residential consumption has upward trend with variability
ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "MWh", title = "Residential consumption") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# note to look for monthly or annual population data in DC


# Plot for Commercial Sales
ggplot(data, aes(x = DATE, y = Sales_commercial)) +
  geom_line(colour = "blue") +
  labs(x = "Time", y = "MWh", title = "Commercial Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_commercial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Industrial Sales
ggplot(data, aes(x = DATE, y = Sales_industrial)) +
  geom_line(colour = "green") +
  labs(x = "Time", y = "MWh", title = "Industrial Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_industrial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Transportation Sales
ggplot(data, aes(x = DATE, y = Sales_transportation)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "MWh", title = "Transportation Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_transportation))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()


# Plot for Total Sales
ggplot(data, aes(x = DATE, y = Sales_total)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "MWh", title = "Total Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_total))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Average x month TOTAL
sales_data <- subset(data, Sales_total > 0)

avg_sales_month <- sales_data %>%
  group_by(month) %>%
  summarise(avg_sales = mean(Sales_total, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_sales > 100500, TRUE, FALSE))


ggplot(avg_sales_month, aes(x = factor(month), y = avg_sales, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Sales", fill = "Sales") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Normal", "Highest")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = "Helvetica Neue"),
    title = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 1500000), breaks = seq(0, max(avg_sales_month$avg_sales) + 10000, by = 100000)) +
  geom_text(
    data = filter(avg_sales_month, month == "lug"),
    aes(x = month, y = avg_sales, label = round(avg_sales)),
    vjust = -0.5, color = "black", size = 6
  )


# TODO Avg Sales for residential and commercial sales after 2012 (customers column filled)


########################### PRICE plotting

ggplot(data, aes(x = DATE, y = Price_total)) +
  geom_line(colour = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2005-01-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2013-01-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype = "dashed", color = "blue") +
  labs(x = "Time", y = "Average Price (cents/kWh)", title = "Price over time") +
  scale_y_continuous(limits = c(0, max(data$Price_total)), breaks = seq(0, max(data$Price_total), by = 2)) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  custom_theme()


ggplot(data, aes(x = DATE, y = Price_residential)) +
  geom_line(colour = "darkgreen") +
  geom_vline(xintercept = as.numeric(as.Date("2005-01-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2013-01-01")), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype = "dashed", color = "blue") +
  labs(x = "Time", y = "Average Price (cents/kWh)", title = "Price over time") +
  scale_y_continuous(limits = c(0, max(data$Price_total)), breaks = seq(0, max(data$Price_total), by = 2)) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  custom_theme()


############################ Assess Autocorrelations



acf(data$Sales_total, main = "Autocorrelation for sales")
acf(data$total_generation_source, main = "Autocorrelation for energy generation")
acf(data[data$Solar.Thermal.and.Photovoltaic > 0, ]$Solar.Thermal.and.Photovoltaic) # WEIRD#
acf(data$Sales_transportation) # trend
acf(data$Sales_residential) # Seasonal
acf(data$Price_total)


########################### Correlation matrices
# Initialize an empty data frame for numerical columns
# numerical data is already a split after 2012
numerical_data <- data[data$DATE >= as.Date("2012-01-01"), ]

str(numerical_data)

# Create a new data frame with only numerical columns
numerical_data <- data %>%
  select_if(is.numeric)

# Print the new data frame with numerical columns
print(numerical_data)

cor_matrix <- cor(numerical_data)

library(corrplot)
color_palette <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_matrix, method = "number", tl.cex = 0.7, cl.cex = 0.8, number.cex = 0.3)

# Do the correlation plot without full titles to see better (optional)
numerical_data_i <- numerical_data
colnames(numerical_data_i) <- seq_along(colnames(numerical_data_i))
cor_matrix_i <- cor(numerical_data_i)
corrplot(cor_matrix_i, method = "color", tl.cex = 0.7, cl.cex = 0.8, number.cex = 0.3)


### Correlations df
# Lets make a correlations function that inputs data frame and outputs correlations df
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

# Print relevant correlations df
correlations_df <- find_correlations(data, threshold = 0.1)


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
# ANALYZE SERIES OF RESIDENTIAL (MWh) SALES IN DC
# 1. EDA plots: Residential sales

# Plotting time series:
p_ressales <- ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "MWh", title = "Residential Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

ggplotly(p_ressales)

# Observations:
# appears to be prominent seasonality
# slightly increasing trend with a spike in Jan 2015

### Create tsible object (specialized object for time series analysis)
# Copy data
data_tsbl <- data
# change date format
data_tsbl$DATE <- yearmonth(data_tsbl$DATE)
# do we want to filter for data only  post 2012?
data_tsbl <- as_tsibble(data_tsbl, index = DATE)

# Yearly seasonal plots
gg_season(data_tsbl, y = Sales_residential)
# Different view: years grouped by month
gg_subseries(data_tsbl, y = Sales_residential)
# Clear seasonality for summer peaks and winter


#### CORRELATIONS
# Columns to include in the correlations graph:
# colnames(data_tsbl)
# [1] "DATE"
# [2] "Combined.Heat.and.Power..Commercial.Power"  yes
# [3] "Combined.Heat.and.Power..Electric.Power"   yes
# [4] "Electric.Generators..Electric.Utilities"   yes
# [5] "Electric.Generators..Independent.Power.Producers" yes
# [6] "Natural.Gas"    yes
# [7] "Other.Biomass"   yes
# [8] "Petroleum"       yes
# [9] "Solar.Thermal.and.Photovoltaic" yes
# [10] "total_renew_source"             yes
# [11] "Revenue_residential"
# [12] "Sales_residential"            yes
# [13] "Customers_residential" yes
# [14] "Price_residential" yes
# [15] "Revenue_commercial"
# [16] "Sales_commercial"
# [17] "Customers_commercial" yes
# [18] "Price_commercial"
# [19] "Revenue_industrial"
# [20] "Sales_industrial"
# [21] "Customers_industrial" yes
# [22] "Price_industrial"
# [23] "Revenue_transportation"
# [24] "Sales_transportation"
# [25] "Customers_transportation" yes
# [26] "Price_transportation"
# [27] "Revenue_total"
# [28] "Sales_total"
# [29] "Customers_total"
# [30] "Price_total"
# [31] "tavg" yes
# [32] "tmin" yes
# [33] "tmax" yes
# [34] "prcp" yes
# [35] "wspd" yes
# [36] "pres" yes
# [37] "total_generation_producer"
# [38] "total_generation_source"
# [39] "month"
# data_tsbl[colnames(data_tsbl)[c(12, 2, 3, 4, 5, 6, 7, 8,  9, 10, 31, 32, 34, 35)]]
# Plot correlations with graphs
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


### LAG PLOTS
data_tsbl |>
  gg_lag(Sales_residential, geom = "point", lags = 1:12) +
  labs(x = "lag(Sales_residential, k)")
# we see a strongly positive relationship at lags 1, 6, 12 and a somewhat
# negative one at lags 3, 9

### AUTO CORRELATIONS
# Run Acf:
Acf(data$Sales_residential, main = "Autocorrelation for Residential Sales")
# The lack of decay in the autocorrelation at the seasonal lags
# may suggest a strong and persistent seasonality in the data
# We can see a very strong autocorrelation at months 5,6,7 which reflects the
# changes between winter - summer

# Run pAcf:
Pacf(data$Sales_residential, main = "Partial autocorrelation for Residential Sales")
# more gradual decay in the partial autocorrelation indicates that
# some of the observed correlation at shorter lags can be explained
# by the correlations at longer lags.
# This is typical in the presence of a combination of trend and seasonality


### Seasonal and Trend decomposition using Loess (STL)
# Get components
dcmp <- data_tsbl |>
  model(
    STL(
      Sales_residential ~ trend(window = 12) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components()
# Syntax equivalent to: components(model(data_tsbl, STL(Sales_residential ~ trend(window = 12) + season(window = "periodic"), robust = TRUE)))
## |> operator pipes the argument into the first function and
# then from innermost to outermost function in sequential order

autoplot(dcmp) # The season_adjust values are the seasonally adjusted values
# i.e. trend value + remainder (true - trend - seasonal)
# dcmp has the trend component, the season, the remainder and the seasonally adjusted values

# Plot raw and seasonally adjusted values
# Pivot dable into long format for one graph
dcmp_long <- pivot_longer(dcmp, cols = -c(DATE, .model), names_to = "series", values_to = "value")

ggplot(dcmp_long[dcmp_long$series %in% c("Sales_residential", "season_adjust"), ], aes(x = DATE, y = value, color = series)) +
  geom_line() +
  ggtitle("Comparison with seasonally adjusted values") +
  xlab("Time") +
  ylab("Value")

###########################################################
# MODELS

#########
# Simple forecasting methods (used for benchmarking)
# 1. MEAN forecasting
# forecast is the mean of observations
fit_mean <- data_tsbl |> model(MEAN(Sales_residential))
# Plot
fit_mean |>
  forecast(h = "2 years") |>
  autoplot(data_tsbl)

# 2. Naive method
# we set all forecasts to be the value fo the last observation
fit_naive <- data_tsbl |> model(NAIVE(Sales_residential))
# Plot
fit_naive |>
  forecast(h = "2 years") |>
  autoplot(data_tsbl)

# 3. Seasonal naive method
# we set the forecast values to be the same of the last season i.e. month
fit_snaive <- data_tsbl |> model(SNAIVE(Sales_residential ~ lag("year")))
# Plot
fit_snaive |>
  forecast(h = "2 years") |>
  autoplot(data_tsbl)

# Store residuals
aug <- augment(fit_snaive)

# Residuals plotting
autoplot(aug, .innov)

# We can also do a simple scatter plot
plot(aug$.innov, main = "Scatter Plot")

# Plot histogram of residuals: normal distribution of residuals
aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")
# Residuals appear to be somewhat normally distributed

# Plot autocorrelation of residuals: correlation of residuals
aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from drift method")
# Autocorrelation at lag 12

# 4. Drift method
# variation of naive method that allows increase or decrease over time
fit_rw <- data_tsbl |> model(RW(Sales_residential ~ drift()))
# Plot
fit_rw |>
  forecast(h = "2 years") |>
  autoplot(data_tsbl)

# Store residuals
aug <- augment(fit_rw)

# Residuals plotting
autoplot(aug, .innov)

# We can also do a simple scatter plot
plot(aug$.innov, main = "Scatter Plot")

# Plot histogram of residuals: normal distribution of residuals
aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")
# Residuals appear to be somewhat normally distributed

# Plot ACF of residuals: correlation of residuals
aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from drift method")
# There still is autocorrelation on residuals after applying the model

# Residual diagnostics: a good forecasting model will have:
# 1. Uncorrelated innovation residuals (innovation residuals are residuals on the transformed data)
# 2. Innovation residuals have zero mean if not forecast is biased
# 3. The innovation residuals have constant variance. This is known as “homoscedasticity”.
# 4. The innovation residuals are normally distributed.

# Forecasting with seasonally adjusted values
# Fit model with seasonaly adjusted values except last year which takes seasonality
fit_dcmp <- data_tsbl |>
  model(stlf = decomposition_model(
    STL(Sales_residential ~ trend(window = 12), robust = TRUE),
    NAIVE(season_adjust)
  ))
# Plot model with forecast
fit_dcmp |>
  forecast() |>
  autoplot(data_tsbl)

### Evaluating our simple methods
# Evaluation of a forecasting method should be done on a test set, separate from training
# Dataset splitting
recent_data <- data_tsbl |>
  filter(DATE >= yearmonth("2022-01"))

older_data <- data_tsbl |>
  filter(DATE < yearmonth("2022-01")) 

# Fit data to older data with our 4 simple models
sales_fit <- older_data |>
  model(
    Mean = MEAN(Sales_residential),
    `Naive` = NAIVE(Sales_residential),
    `Seasonal naive` = SNAIVE(Sales_residential),
    Drift = RW(Sales_residential ~ drift())
  )

# Generate the forecast
sales_fc <- sales_fit |>
  forecast(h = "1 years")

#Plot
sales_fc |>
  autoplot(
    data_tsbl,
    level = NULL
  ) +
  labs(
    y = "Sales MWh",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Graphically we see that the best simple model is the Seasonal naive
# Lets compute our metrics
  # The accuracy() function will automatically extract the relevant periods from 
  # the data (recent_production in this example) to match the forecasts when 
  # computing the various accuracy measures.
accuracy(sales_fc, recent_data)
  # The measures calculated are:
  # ME: Mean Error
  # RMSE: Root Mean Squared Error
  # MAE: Mean Absolute Error
  # MPE: Mean Percentage Error
  # MAPE: Mean Absolute Percentage Error
  # MASE: Mean Absolute Scaled Error
  # ACF1: Autocorrelation of errors at lag 1.


############
# 1. Linear Regression

# represent our data as a time series:
ressales_ts <- ts(data$Sales_residential, frequency = 12)

# TREND ONLY
# First model with only the trend:
tslm_t <- tslm(ressales_ts ~ trend)
summary(tslm_t)

# Trend is significant but R2 is bad
# R-squared = 0.2221; F = 74.79 with 262 df; p < 0.0001

# plot real values against fitted values
p_tslm_t <- ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "Residential Sales (MWh)", title = "Real TimeSeries vs Fitted Values TSLM") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme() +
  geom_line(aes(y = fitted(tslm_t)),
    color = "darkred", linetype = "twodash"
  )

ggplotly(p_tslm_t)

# Plot residuals
p_tslm_res <- ggplot(data, aes(x = DATE, y = residuals(tslm_t))) +
  geom_point() +
  labs(x = "Time", y = "Residuals", title = "Residuals of TSLM with Trend")

ggplotly(p_tslm_res)

# As we can see, importance of seasonality and that outlier spike in 2015 emerges

# TREND + SEASON
# Second model with the trend and the seasonality:
tslm_ts <- tslm(ressales_ts ~ trend + season)
summary(tslm_ts)
# Trend is significant and R2 has drastically improved
# R-squared = 0.7941; F = 80.65 with 251 df; p < 0.0001

# plot real values against fitted values and also residuals
p_tslm_ts <- ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line() +
  labs(x = "Time", y = "Residential Sales (MWh)", title = "Real TimeSeries vs Fitted Values TSLM") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme() +
  geom_line(aes(y = fitted(tslm_ts)),
    color = "darkred", linetype = "twodash"
  )

ggplotly(p_tslm_ts)

p_tslm_ts_res <- ggplot(data, aes(x = DATE, y = residuals(tslm_ts))) +
  geom_point() +
  labs(x = "Time", y = "Residuals", title = "Residuals of TSLM with Trend and Season")

ggplotly(p_tslm_ts_res)
# residuals much closer to 0

# tslm with trend and season seems to capture trend fairly well and is a good starting point

# Run DW Test on timeseries:
dwtest(tslm_t) # DW = 1.199, p<0.0001
dwtest(tslm_ts) # DW = 1.7889, p-value < 0.05

# DW statistic ranges from 0 to 4. Test detects presences of autocorrelation
# in residuals (errors) of a model. Autocorrelation in residuals indicates
# systematic pattern in unexplained variation in data points, which violates
# assumptions of independence
# DW = 2: no sig autocorrelation - desired
# DW < 2: pos autocorrelation in residuals, consecutive residuals tend to have similar values


# TREND + SEASON on subset
# Forecasting on tslm trend + season model
# take a portion of data and fit a linear model with tslm
ressales_ts_10 <- window(ressales_ts, start = 10, end = 22)
plot(ressales_ts_10)
m1 <- tslm(ressales_ts_10 ~ trend + season)
summary(m1)
# R2=0.7637, F-stat=35.55, p<0.0001
fit <- fitted(m1)

plot(ressales_ts_10)
lines(fitted(m1), col = 2)

# forecasts from regression model for residential sales
# The dark shaded region shows 80% prediction intervals and
# the light shaded 95% prediction intervals (range of values the random variable could take with relatively high probability).
fore <- forecast(m1)
plot(fore)

res <- residuals(m1)
plot(res)
# the form of residuals seems to indicate the presence of negative autocorrelation
# white noise residuals indicate good candidate for forecasting
Acf(res)

### QUESTION: DOES IT MAKE SENSE TO FIT A BASS MODEL HERE?
# R: does not make sense sinse bass models decribes the process of how  new
# products get adopted in a population
# The assumption of the Bass Model is product growth
# TODO: Try Bass Models

################
# 2. Multiple linear regression model
# Key we have to select which predictors we will use
# We can do this via stepwise selection

# We make the following assumptions regarding error terms
# ("1, ..., "N)
# 1. errors have mean zero
# 2. errors are uncorrelated
# 3. errors are uncorrelated with Xj,i

# We will start with the full model that takes all variables into account
# lets take only data after 2012 for the multiple linear regression
# as petroleum has some spikes earlier
# numerical_data is already only post 2012


##########
# OPTION 1: STEPWISE REDUCTION WITH ALL VARIABLES (NO SUBJECTIVE SELECTION)
lr_fullModel = lm(Sales_residential ~ ., data=numerical_data, family = gaussian)
summary(lr_fullModel)
lr_step_aic = step(lr_fullModel, direction="both", trace=0, steps=1000)
summary(lr_step_aic)
# Compute Variance Inflation factor to assess multicollinearity
vif(lr_step_aic)
# We see that many predictors have very high VIFs

# Multicolinearity reduction function
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
# Reduce colinearity
new_model_1 <- reduce_multicollinearity(lr_step_aic)
# Summary
vif(new_model_1)
summary(new_model_1)
extractAIC(new_model_1)
# Very high fit but also has Revenue_residential and Sales_total which
# has a correlation value of 0.89 as its a function of sales, we should remove
# it
acf(residuals(new_model_1))

# We should not use option 1

####
# OPTION 2: SUBJECTIVE FEATURE SELECTION as first step (using logic)
#Lets remove those predictors that are a function of others, example revenue
# [1] "Combined.Heat.and.Power..Electric.Power" Y        
# [2] "Electric.Generators..Independent.Power.Producers" Y
# [3] "Natural.Gas" Y                                     
# [4] "Other.Biomass" Y                                  
# [5] "Petroleum"   Y                                    
# [6] "Solar.Thermal.and.Photovoltaic" Y                 
# [7] "Revenue_residential"   N                          
# [8] "Customers_residential"   Y                        
# [9] "Price_residential"    Y                           
# [10] "Revenue_commercial"    N                          
# [11] "Sales_commercial"     N                           
# [12] "Customers_commercial"  N because is colinear with customers resdential                          
# [13] "Price_commercial"     Y                           
# [14] "Revenue_industrial"   N                           
# [15] "Revenue_transportation"  N                        
# [16] "Customers_transportation"  N                      
# [17] "Revenue_total"   N                                
# [18] "Sales_total"     N                                
# [19] "Price_total"     N                                
# [20] "tmax" Y
# These are our regressors:
predictors <- names(coef(lr_step_aic))[-1]
predictors
# Variable selection:
new_predictors <- predictors[c(1,2,3,4,5,6,8,9,13,20)]
new_predictors
# Fit model
formula_string <- paste("Sales_residential ~", paste(new_predictors, collapse = " + "))
# Convert the string to a formula
formula_object <- as.formula(formula_string)
# Fit the model using the formula
new_model_2 <- lm(formula_object, data=numerical_data)
# Stepwise selection of variables
new_model_2_step = step(new_model_2, direction="both", trace=0, steps=1000)
# Reduce colinearity
new_model_2_step <- reduce_multicollinearity(new_model_2)
vif(new_model_2_step)
summary(new_model_2_step)
extractAIC(new_model_2_step)
acf(residuals(new_model_2_step))

# In summary: Multiple linear regression  not a good option!!


############
# ARIMA Models

ggplotly(p_ressales)

# Differencing required before running ARIMA:
ressales_ts_df <- diff(ressales_ts)
tsdisplay(ressales_ts_df)
# Plot differentiated data to check for stationarity:

p_ts_df <- autoplot(ressales_ts_df, xlab = "Time", ylab = "ResidentialSales") +
  ggtitle("Residential Sales (MWh) - Differentiated Series")

ggplotly(p_ts_df)
# Differentiating the series we can see that it seems to be more
# stationary (in term of mean)

# Residuals of differentiated series:
p_acf_df <- ggAcf(ressales_ts_df) +
  ggtitle("Acf Function for Diff Residential Sales")
p_pacf_df <- ggPacf(ressales_ts_df) +
  ggtitle("Partial Acf Function for Diff Residential Sales")

grid.arrange(p_acf_df, p_pacf_df, nrow = 2)
# lag 6, 12, 18, 12 significant (mid-year)

# Build first ARIMA models, testing diff combos for p, d, q:
# p: autoregresive component (AR): Captures the linear relationship between the current observation and its previous values (lags).
# d: differencing of the time series to achieve stationarity. degrees of differencing needed
# q: MA (Moving Average): Models the short-term, unobserved shocks or random fluctuations in the data.

arima0 <- Arima(ressales_ts, order = c(0, 1, 2), seasonal = c(0, 1, 0)) # AIC=5918.56
arima1 <- Arima(ressales_ts, order = c(0, 1, 2), seasonal = c(0, 1, 2)) # AIC=5799.4
arima2 <- Arima(ressales_ts, order = c(0, 1, 0), seasonal = c(0, 0, 2)) # AIC=6327.95
arima3 <- Arima(ressales_ts, order = c(2, 1, 0), seasonal = c(0, 0, 2)) # AIC=6304.43

# best ARIMA
arima1

p_arima <- p_ressales +
  geom_line(aes(y = fitted(arima1)),
    color = "darkred", linetype = "twodash"
  ) +
  ggtitle("Real TimeSeries vs Fitted Values with Arima") +
  xlab("Time") +
  ylab("Residential Sales (MWh)")


ggplotly(p_arima)

# We now take a look of the residuals obtained by the arima model

p_arima_res <- ggplot(data, aes(x = DATE, y = residuals(arima1))) +
  geom_point() +
  labs(x = "Time", y = "Residuals", title = "Residuals of ARIMA with Trend and Season")

ggplotly(p_arima_res)
# randomly distributed residuals with peak at 2015

# The we look at a more complete information about them:
checkresiduals(arima1)

# Try auto arima
auto.arima <- auto.arima(ressales_ts) # AIC=5879.92
auto.arima # ARIMA(1,0,0)(1,1,0)[12] with drift

# Forecast with ARIMA
# Randomly pick starting index for training set: start_index =  sample(1:500, 1)
# Create training set: fit_size = 60 # 5 years /n train_ts = car_ts[start_index:(start_index+fit_size-1)]
# Create test set: test_size = 10 /n test_ts = car_ts[(start_index+fit_size):(start_index+fit_size+test_size-1)]
# Re-fit the previously found model on the defined window of data
# Forecast with fitted model: pred_arima_partial = forecast(arima_partial, h = test_size)
# Plot predictions: c(arima_partial$fitted, pred_arima_partial$mean)

# Lagged Regressors

# Overall Schematic of Project
# 1. Describe story of energy generation and consumption for electricity in DC
# 2. Analyze series of residential consumption (sales) in DC
# 3. Add other explanatory variables: residential customers, avg price, generation and modify time series with sales
# 4. Focus on timeframe from 2012-2022 since data is more reliable then and there is the transition from Petroleum to alternatives and renewables
# 5. Add other series for more information: temperature data
# 6. analyze correlations with all the variables
# 7. improve tslm models
# 8. Models with Lagged Regressors
# 9. Determine Final Model (could be tslm, GBM, etc...)
# 10. Forecasting using ARIMA and exponential smoothing
# 11. Find correlations and optimal lags for forecasting
# 12. Try to make forecasts with the Final Model

# Models to test:
# Seasonal-Trend decomposition using LOESS
# Seasonal Autoregressive Integrated Moving Average (SARIMA)
# Seasonal Exponential Smoothing (ETS)
# XGBoost and LightGBM Boosting Algorithms
