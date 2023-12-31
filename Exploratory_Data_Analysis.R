# Script for Exploratory Analysis of Energy Data

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

# Explore data structure and summary
str(data) # 36 total variables. With exception of DATE variable, all are numeric
summary(data)

# Convert DATE to Date type
data$DATE = as.Date(data$DATE, format = "%d/%m/%Y")

# REFORMAT OTHER COLUMNS

# Negative values in generation found only in independent power producer
# example: solar power producers relying on sunlight might require independent power sources/producers
# on non-sunny days
data$Petroleum <- abs(data$Petroleum) # converting negative values since it's still generated energy being used

# Add total generation columns
data$total_generation_producer <- rowSums(data[, 2:5]) # total by producer
data$total_generation_source <- rowSums(data[, 6:9]) # total by source

# Extract month from dates
data$month <- month(data$DATE, label = TRUE) # each row is a month

summary(data)
head(data)

# Predicting on Petroleum no longer useful since conversion to renewable energy after 2010
# Maybe could do some prediction on later generation after the switch happens

################################ Plotting #################################

# Initially, began our project with an investigation of the generation data

# FIRST, PLOT THE OVERALL TOTAL GENERATION BY SOURCE

ggplot(data, aes(x = DATE, y = total_generation_source)) +
  geom_line(color = "darkblue", size = 0.55, ) +
  labs(
    x = "Time",
    y = "Total Energy Generation (MWh)"
  ) +
  scale_x_continuous(
    breaks = seq(min(data$DATE), max(data$DATE), by = "year"),
    labels = format(seq(min(data$DATE), max(data$DATE), by = "year"), "%Y"),
    expand = c(0, 0)
  ) +
  custom_theme()
# volatile generation pre 2012 with a somewhat steady increasing trend after

# NOW, PLOT DATA BEFORE 2011 TO SEE PETROLEUM
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
  scale_x_continuous(
    breaks = seq(min(pre2011_subset_data$DATE), max(pre2011_subset_data$DATE), by = "year"),
    labels = format(seq(min(pre2011_subset_data$DATE), max(pre2011_subset_data$DATE), by = "year"), "%Y"),
    expand = c(0, 0)
  ) +
  custom_theme()
# apply smoothing on data pre 2012 to understand general trend
# still pretty volatile with smoothing

# NOW, PLOT DATA AFTER 2011 TO SEE THE SWITCH TO GAS AND RENEWABLES
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
  scale_x_continuous(
    breaks = seq(min(post2011_subset_data$DATE), max(post2011_subset_data$DATE), by = "year"),
    labels = format(seq(min(post2011_subset_data$DATE), max(post2011_subset_data$DATE), by = "year"), "%Y"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, 90000), breaks = seq(0, 90000, by = 5000)) +
  custom_theme()
# data post 2011 with smoothing indicates slightly increasing trend with a dip in 2022

# PLOTTING OF AVERAGE GENERATION DATA BY MONTH ACROSS YEARS TO CHECK FOR SEASONALITY

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
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000)) +
  geom_text(
    data = filter(avg_data, month == "lug"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  ) +
  custom_theme()
# clear spike in summer - DC summers are very hot and use of ACs is high

# Average monthly energy generation from petroleum pre 2011
pre2011_avg_pet_data <- pre2011_subset_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Petroleum, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 10000, TRUE, FALSE))

pre2011_avg_pet_data$energysource <- "Petroleum"

ggplot(pre2011_avg_pet_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Petroleum") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Other seasons", "Summer")) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000)) +
  geom_text(
    data = filter(pre2011_avg_pet_data, month == "ago"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  ) +
  custom_theme()
# clear spike in summer - DC summers are very hot and use of ACs is high

# Subset data for dates starting from 2011
post2011_subset_data <- data[data$DATE >= as.Date("2011-01-01"), ]

# Average monthly energy generation from "renewable" sources post 2011
post2011_avg_ren_data <- post2011_subset_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(total_renew_source, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 3500, TRUE, FALSE))

post2011_avg_ren_data$energysource <- "Renewables"

ggplot(post2011_avg_ren_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Renewables") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Normal", "Highest")) +
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 1000)) +
  geom_text(
    data = filter(post2011_avg_ren_data, month == "dec"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  ) +
  custom_theme()
# not as much seasonality with generation from "renewable sources"

# Average monthly energy generation from solar
# solar data (starting in 2019)
solar_data <- subset(data, Solar.Thermal.and.Photovoltaic > 0)

avg_solar_data <- solar_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Solar.Thermal.and.Photovoltaic, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 1600, TRUE, FALSE))

ggplot(avg_solar_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Renewables") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Normal", "Highest")) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 2000)) +
  geom_text(
    data = filter(avg_solar_data, month == "mag"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  ) +
  custom_theme()
# low generation of solar but still see slight seasonality in summer

# Average gas data
gas_data <- subset(data, Natural.Gas > 0)

avg_gas_data <- gas_data %>%
  group_by(month) %>%
  summarise(avg_generation = mean(Natural.Gas, na.rm = TRUE)) %>%
  mutate(highlight_flag = ifelse(avg_generation > 6500, TRUE, FALSE))

avg_gas_data$energysource <- "Gas"

ggplot(avg_gas_data, aes(x = factor(month), y = avg_generation, fill = as.character(highlight_flag))) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "Month", y = "Avg. Generation (MWh)", fill = "Natural Gas") +
  scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "#3e6fff"), labels = c("Normal", "Highest")) +
  scale_y_continuous(limits = c(0, 12000), breaks = seq(0, 12000, by = 2000)) +
  geom_text(
    data = filter(avg_gas_data, month == "lug"),
    aes(x = month, y = avg_generation, label = round(avg_generation)),
    vjust = -0.5, color = "black", size = 6
  ) +
  custom_theme()
# highest generation in july

# Combined Dataset for Average Generation Data 
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
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 5000)) +
  custom_theme()
# grouped bar plot indicates summer seasonality with much higher average gen. for petroleum

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
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 2500)) +
  custom_theme()
# post petroleum era is a mix of sources
# growing solar, natural gas, and steady biomass usage

# PLOT BY ENERGY PRODUCERS

# Plot time series Combined Heat and Power. Commercial Power
plot(data$DATE, data$Combined.Heat.and.Power..Commercial.Power,
     type = "l", col = "orange", pch = 16, xlab = "Date", ylab = "Generation",
     main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000)
)
legend("topright", legend = "Solar | Thermal | Photovoltaic", col = "orange", lty = 1)
# after 2011, almost steady increase with high volatility

# Plot time series Combined.Heat.and.Power..Electric.Power
plot(data$DATE, data$Combined.Heat.and.Power..Electric.Power,
     type = "l", col = "orange", pch = 16, xlab = "Date", ylab = "Generation",
     main = "Time Series Combined Heat and Power. Commercial Power", ylim = c(0, 20000)
)
legend("topright", legend = "Generation of electric power", col = "orange", lty = 1)
# just a little blip after 2015

# PLOT BY CUSTOMERS 

# Note about customers: when customers = 0, this doesn't make sense. Likely
# data was not being accuractely collected then
# Starting in 2007, data collection begins to become more reliable
filtered_residential <- data[data$Customers_residential > 0, ]
filtered_commercial <- data[data$Customers_commercial > 0, ]
filtered_industrial <- data[data$Customers_industrial > 0, ]
filtered_transportation <- data[data$Customers_transportation > 0, ]
filtered_total <- data[data$Customers_total > 0, ]

# Plot for Customers_residential (houses)
ggplot(filtered_residential, aes(x = DATE, y = Customers_residential)) +
  geom_line(colour = "orange") +
  labs(x = "Time", y = "Number of customers", title = "Residential's customers") +
  scale_y_continuous(limits = c(0, max(filtered_residential$Customers_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# steady linear increase in residential customers - likely pop growth a factor

# Plot for Customers_commercial (malls, businesses)
ggplot(filtered_commercial, aes(x = DATE, y = Customers_commercial)) +
  geom_line(colour = "blue") +
  labs(x = "Time", y = "Number of customers", title = "Commercial's customers") +
  scale_y_continuous(limits = c(0, max(filtered_commercial$Customers_commercial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# pretty constant trend for commercial customers

# Plot for Customers_industrial (factory)
ggplot(filtered_industrial, aes(x = DATE, y = Customers_industrial)) +
  geom_line(colour = "green") +
  labs(x = "Time", y = "Number of customers", title = "Industrial's customers") +
  scale_y_continuous(limits = c(0, max(filtered_industrial$Customers_industrial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# except for 2008, only 1 factory in DC

# Plot for Customers_transportation (cars, trucks, trains, planes, and boats)
ggplot(filtered_transportation, aes(x = DATE, y = Customers_transportation)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "Number of customers", title = "Transportation's customers") +
  scale_y_continuous(limits = c(0, max(filtered_transportation$Customers_transportation))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# range of 1-3 transportation customers

# Plot for Total
ggplot(filtered_total, aes(x = DATE, y = Customers_total)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "Number of customers", title = "All customers") +
  scale_y_continuous(limits = c(0, max(filtered_total$Customers_total))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# steady increase driven by residential and commercial trends

# PLOT BY SALES (CONSUMPTION) AND REVENUE 

# Plot for Residential
# seasonality and residential consumption has upward trend with some variability
# This could be a nice target variable
ggplot(data, aes(x = DATE, y = Sales_residential)) +
  geom_line(colour = "red") +
  labs(x = "Time", y = "MWh", title = "Residential consumption") +
  scale_y_continuous(limits = c(0, max(data$Sales_residential))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()

# Plot for Commercial Sales
ggplot(data, aes(x = DATE, y = Sales_commercial)) +
  geom_line(colour = "blue") +
  labs(x = "Time", y = "MWh", title = "Commercial Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_commercial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# weird drop in 2005 - linked to housing bubble pre-market crash?

# Plot for Industrial Sales
ggplot(data, aes(x = DATE, y = Sales_industrial)) +
  geom_line(colour = "green") +
  labs(x = "Time", y = "MWh", title = "Industrial Sales") +
  scale_y_continuous(limits = c(0, max(data$Sales_industrial))) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  custom_theme()
# industrial also impacted by drop

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
# driven by residential and commercial consumption that have the most customers

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
  scale_y_continuous(limits = c(0, 1500000), breaks = seq(0, max(avg_sales_month$avg_sales) + 10000, by = 100000)) +
  geom_text(
    data = filter(avg_sales_month, month == "lug"),
    aes(x = month, y = avg_sales, label = round(avg_sales)),
    vjust = -0.5, color = "black", size = 6
  ) +
  custom_theme()
# increase in summer and winter months indicating seasonality

# PLOT AVERAGE PRICE

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
# Price shows interesting increasing trend
# Price known to be strongly impacted by regulation and less by consumer market
# in that sense, not a good candidate for target variable


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

############## Initial Plotting for Residential Sales (target) ###############

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

# appears to have prominent seasonality
# increasing trend with a spike in Jan 2015

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

############## Assess Autocorrelations and Correlation Matrix ################

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


### Lag plots
data_tsbl |>
  gg_lag(Sales_residential, geom = "point", lags = 1:12) +
  labs(x = "lag(Sales_residential, k)") +
  custom_theme()
# we see a strongly positive relationship at lags 1, 6, 12 and a somewhat
# negative one at lags 3, 9

### Autocorrelations
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
  ylab("Value") +
  custom_theme()

# Exploratory Data Analysis complete - on to the modeling!

