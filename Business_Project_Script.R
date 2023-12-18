
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

head(data)
# Add total generation columns


# negative values in generation indicate independent power producer
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


#FIRST, WE PLOT THE OVERALL TOTAL GENERATION BY SOURCE
# Plot time series for total generation columns: source
ggplot(data, aes(x = DATE, y = total_generation_source)) +
  geom_line(color = "darkblue", size=0.55,) +
  labs(x = "Time",
       y = "Total Energy Generation (MWh)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(data$DATE), max(data$DATE), by = "year"),
                     labels = format(seq(min(data$DATE), max(data$DATE), by = "year"),  "%Y"),
                     expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# NOW WE PLOT DATA BEFORE 2011 TO SEE PETROLEUM
# Plot data pre 2011 for Petroleum Dominant Market

# Subset data for dates before 2011
pre2011_subset_data <- data[data$DATE < as.Date("2011-01-01"), ]

# Plot the subset of data before 2011
ggplot(pre2011_subset_data, aes(x = DATE, y = total_generation_source)) +
  geom_line(color = "darkblue", size=0.6) +
  geom_smooth(method = "loess", color = "red", size = 1, span = 0.2) +  # Adjust span
  labs(x = "Time",
       y = "Total Energy Generation (MWh)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(pre2011_subset_data$DATE), max(pre2011_subset_data$DATE), by = "year"),
                     labels = format(seq(min(pre2011_subset_data$DATE), max(pre2011_subset_data$DATE), by = "year"),  "%Y"),
                     expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# NOW WE PLOT DATA AFTER 2011 TO SEE THE SWITCH TO GAS AND RENEWABLES
# Petroleum completely stops in mid 2012

# Subset data for dates starting from 2011
post2011_subset_data <- data[data$DATE >= as.Date("2011-01-01"), ]

# Plot the subset of data
ggplot(post2011_subset_data, aes(x = DATE, y = total_generation_source)) +
  geom_line(color = "darkblue")+
  geom_smooth(method = "loess", color = "red", size = 1, span = 0.25) +  # Adjust span
  labs(x = "Time",
       y = "Total Energy Generation (MWh)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(post2011_subset_data$DATE), max(post2011_subset_data$DATE), by = "year"),
                     labels = format(seq(min(post2011_subset_data$DATE), max(post2011_subset_data$DATE), by = "year"),  "%Y"),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 90000), breaks = seq(0, 90000, by = 5000))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Since data has a wide range, apply logarithmic scale to total column
#data$ltotal_generation_source <- log(data$total_generation_source)

#Plot data with log transformation
# since log 0 is undefined, you have missing values
#plot(data$DATE, data$ltotal_generation_source, type = "l", col = "black", pch = 16, xlab = "Data", ylab = "Log Generation",
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

avg_data$energysource = 'All'

ggplot(avg_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Generation") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = '#3e6fff'), labels = c('Other seasons', 'Summer')) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = 'Helvetica Neue'),
    title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000))+
  geom_text(data = filter(avg_data, month == "lug"),
            aes(x = month, y = avg_generation, label = round(avg_generation)),
            vjust = -0.5, color = "black", size = 6)






# Average monthly energy generation from petroleum pre 2011
pre2011_avg_pet_data <- pre2011_subset_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Petroleum, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 10000, TRUE, FALSE))

pre2011_avg_pet_data$energysource = 'Petroleum'

ggplot(pre2011_avg_pet_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Petroleum usage") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = '#3e6fff'), labels = c('Other seasons', 'Summer')) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = 'Helvetica Neue'),
    title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000))+
  geom_text(data = filter(pre2011_avg_pet_data, month == "ago"),
            aes(x = month, y = avg_generation, label = round(avg_generation)),
            vjust = -0.5, color = "black", size = 6)

# Average monthly energy generation from renewables sources post 2011
post2011_avg_ren_data <- post2011_subset_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(total_renew_source, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 3500, TRUE, FALSE))

post2011_avg_ren_data$energysource = 'Renewables'


ggplot(post2011_avg_ren_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Renewables usage") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = '#3e6fff'), labels = c('Normal', 'Highest')) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    text = element_text(hjust = 1, family = 'Helvetica Neue'),
    title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 1000))+
  geom_text(data = filter(post2011_avg_ren_data, month == "dec"),
            aes(x = month, y = avg_generation, label = round(avg_generation)),
            vjust = -0.5, color = "black", size = 6)

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
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = '#3e6fff'), labels = c('Normal', 'Highest')) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = 'Helvetica Neue'),
    title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 2000))+
  geom_text(data = filter(avg_solar_data, month == "mag"),
            aes(x = month, y = avg_generation, label = round(avg_generation)),
            vjust = -0.5, color = "black", size = 6)





gas_data <- subset(data, Natural.Gas > 0)

avg_gas_data <- gas_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Natural.Gas, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 6500, TRUE, FALSE))

avg_gas_data$energysource = 'Gas'


ggplot(avg_gas_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue")+
  
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Natural Gas usage") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = '#3e6fff'), labels = c('Normal', 'Highest')) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = 'Helvetica Neue'),
    title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 2000))+
  geom_text(data = filter(avg_gas_data, month == "lug"),
            aes(x = month, y = avg_generation, label = round(avg_generation)),
            vjust = -0.5, color = "black", size = 6)




######################## COMBINED DATA PLOT


combined_data = rbind(avg_data, pre2011_avg_pet_data)
combined_data = rbind(combined_data, post2011_avg_ren_data)
combined_data = rbind(combined_data, avg_gas_data)
print(n=30, combined_data)

ggplot(combined_data, 
       aes(fill = energysource, y = avg_generation, x = month)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  scale_fill_manual(values = c("All" = "white", "Petroleum" = "#6d93d1", "Renewables" = "#32558c", 'Gas'="#b1c3e0")) +
  labs(
    title = "Average Generation by Source",
    x = "Month",
    y = "Average Generation (MWh)",
    fill = "Energy Source"
    ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = 'Helvetica Neue'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", family = 'Helvetica Neue'),
    axis.text.y = element_text(hjust = 1, family = 'Helvetica Neue')
  ) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000))

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
# Filter the dataset for rows where Petroleum is greater than 0
petroleum_data <- subset(data, Petroleum > 0)

# Find the date and value of the maximum point
max_point <- petroleum_data[which.max(petroleum_data$Petroleum), ]

# Set the x-axis limits to go up to the year 2015
ggplot(petroleum_data, aes(x = DATE, y = Petroleum, color = "Petroleum")) +
  geom_line(size = 1, color = 'darkblue') +  # Adjust line width and color here
  geom_point(data = max_point, aes(x = DATE, y = Petroleum), color = 'red', size = 3) +  
  geom_text(data = max_point, aes(x = DATE, y = Petroleum, label = paste("max: ", round(Petroleum, 2))),
            vjust = 0.2, hjust = -0.3, color = 'black', size = 5, family='serif') +  # Display the value of the max point
  labs(title = "Petroleum Energy Generation",
       x = "Time",
       y = "MWh") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date('2001-01-01'), as.Date("2015-12-31")), expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +  # Adjust x-axis limits and labels
  theme(
    panel.grid.major = element_line(color = "white", size = 0.5),  # Adjust major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Adjust title font size
    axis.text = element_text(size = 12, family = "serif"),  # Adjust axis text font size and family
    axis.title = element_text(size = 14, family = "serif", angle = 0),  # Adjust axis title font size and set angle to 0 for horizontal
    axis.line = element_line(color = "black", size = 0.8),  # Adjust axis line color and size
    axis.ticks = element_line(color = "black"),  # Adjust tick marks color
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels at a 45-degree angle
  ) +
  scale_color_manual(name = "Energy Source", values = c("Petroleum" = "blue"))

# do the same for renewables
renewable = data$total_renew_source

ggplot(data, aes(x = DATE, y = total_renew_source, color = "Renewable")) +
  geom_line(size = 1, color = 'darkblue') +  # Adjust line width and color here
   labs(title = "Renewable Energy Generation",
       x = "Time",
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
    axis.ticks = element_line(color = "black"),  # Adjust tick marks color
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels at a 45-degree angle
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
  geom_text(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, label = round(max_value_natural_gas, 2)), vjust = -1, hjust = -0.5, size = 5, family='serif', color = "red") +
  geom_text(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, label = round(max_value_other_biomass, 2)), vjust = -1, hjust = -0.5, size = 5, family='serif', color = "blue") +
  geom_text(data = max_point_solar, aes(x = DATE, y = max_value_solar, label = round(max_value_solar, 2)), vjust = -1, hjust = -0.5, size = 5, family='serif', color = "orange") +
  labs(title = "Current  Energy Generation",
       x = "Time",
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
  geom_text(data = max_point_natural_gas, aes(x = DATE, y = max_value_natural_gas, label = paste("gas max: ", round(max_value_natural_gas, 2))), vjust = 0, hjust = 1.2, size = 4, family='serif', color = "red") +
  geom_text(data = max_point_other_biomass, aes(x = DATE, y = max_value_other_biomass, label = paste("biomass max: ", round(max_value_other_biomass, 2))), vjust = -0.9, hjust = 1.2, size = 4, family='serif', color = "blue") +
  geom_text(data = max_point_solar, aes(x = DATE, y = max_value_solar, label = paste("solar max: ", round(max_value_solar, 2))), vjust = 0, hjust = 1.1, size = 4, family='serif', color = "orange") +
  labs(title = "Current  Energy Generation",
       x = "Time",
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
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels at a 45-degree angle
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
     main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000))+
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




#################################################### CUSTOMERS

filtered_residential <- data[data$Customers_residential > 0, ]
filtered_commercial <- data[data$Customers_commercial > 0, ]
filtered_industrial <- data[data$Customers_industrial > 0, ]
filtered_transportation <- data[data$Customers_transportation > 0, ]
filtered_total <- data[data$Customers_total > 0, ]

# Function to create a common theme
custom_theme <- function() {
  theme_minimal() +
    theme(legend.position = 'bottom',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black'))
}


# Plot for Customers_residential
ggplot(filtered_residential, aes(x = DATE, y = Customers_residential)) +
  geom_line(colour = 'orange') +
  labs(x = "Date", y = "Number of customers", title = "Residential's customers") +
  scale_y_continuous(limits = c(0, max(filtered_residential$Customers_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Customers_commercial
ggplot(filtered_commercial, aes(x = DATE, y = Customers_commercial)) +
  geom_line(colour = 'blue') +
  labs(x = "Date", y = "Number of customers", title = "Commercial's customers") +
  scale_y_continuous(limits = c(0, max(filtered_commercial$Customers_commercial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Customers_industrial
ggplot(filtered_industrial, aes(x = DATE, y = Customers_industrial)) +
  geom_line(colour = 'green') +
  labs(x = "Date", y = "Number of customers", title = "Industrial's customers") +
  scale_y_continuous(limits = c(0, max(filtered_industrial$Customers_industrial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Customers_transportation
ggplot(filtered_transportation, aes(x = DATE, y = Customers_transportation)) +
  geom_line(colour = 'red') +
  labs(x = "Date", y = "Number of customers", title = "Transportation's customers") +
  scale_y_continuous(limits = c(0, max(filtered_transportation$Customers_transportation))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Customers_transportation
ggplot(filtered_total, aes(x = DATE, y = Customers_total)) +
  geom_line(colour = 'red') +
  labs(x = "Date", y = "Number of customers", title = "Transportation's customers") +
  scale_y_continuous(limits = c(0, max(filtered_total$Customers_total))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()





#######################################################à SALES AND MONEYYY

# Plot for Residential
ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line(colour = 'red') +
  labs(x = "Date", y = "Number of customers", title = "Transportation's customers") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Commercial Sales
ggplot(data, aes(x = DATE, y = Sales_commercial)) +
  geom_line(colour = 'blue') +
  labs(x = "Date", y = "Sales", title = "Commercial Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_commercial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Industrial Sales
ggplot(data, aes(x = DATE, y = Sales_industrial)) +
  geom_line(colour = 'green') +
  labs(x = "Date", y = "Sales", title = "Industrial Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_industrial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Transportation Sales
ggplot(data, aes(x = DATE, y = Sales_transportation)) +
  geom_line(colour = 'red') +
  labs(x = "Date", y = "Sales", title = "Transportation Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_transportation))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()



# Plot for Total Sales
ggplot(data, aes(x = DATE, y = Sales_total)) +
  geom_line(colour = 'red') +
  labs(x = "Date", y = "Sales", title = "Total Sales") +
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
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c('Normal', 'Highest')) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(hjust = 1, family = 'Helvetica Neue'),
    title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0, 1500000), breaks = seq(0, max(avg_sales_month$avg_sales) + 10000, by = 100000)) +
  geom_text(data = filter(avg_sales_month, month == "lug"),
            aes(x = month, y = avg_sales, label = round(avg_sales)),
            vjust = -0.5, color = "black", size = 6)



########################### PRICE plotting

ggplot(data, aes(x = DATE, y = Price_total)) +
  geom_line(colour = 'darkgreen') +
  geom_vline(xintercept = as.numeric(as.Date("2005-01-01")), linetype='dashed',color='blue')+
  geom_vline(xintercept = as.numeric(as.Date("2013-01-01")), linetype='dashed',color='blue')+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype='dashed',color='blue')+
  labs(x = "Date", y = "Price", title = "Price over time") +
  scale_y_continuous(limits = c(0, max(data$Price_total)), breaks = seq(0, max(data$Price_total), by = 2)) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  custom_theme()


############################ Assess Autocorrelations 

acf(data$Sales_total, main='Autocorrelation for sales')
acf(data$total_generation_source, main='Autocorrelation for energy generation')
acf(data$Solar.Thermal.and.Photovoltaic)  #WEIRD#
acf(data$Sales_transportation) # trend
acf(data$Sales_residential) # Seasonal
acf(data$Price_total)


########################### Correlation matrices
# Initialize an empty data frame for numerical columns
numerical_data <- data.frame()

# Create a new data frame with only numerical columns
numerical_data <- data %>%
  select_if(is.numeric)

# Print the new data frame with numerical columns
print(numerical_data)

cor_matrix = cor(numerical_data)

library(corrplot)

color_palette = colorRampPalette(c("#BB4444","#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor_matrix, method = "circle", tl.cex = 0.7, cl.cex = 0.7)



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

