# Energy Data Business Project

### Project Overview
This project focuses on the analysis and forecasting of energy consumption in Washington D.C. using time series models. It aims to examine energy sales trends by both source and producer, detect transitions toward renewable energy, and generate predictions for future energy output.

### Project Members
- Esteban Ortega Dominguez
- Mattia Varagnolo
- Sofia Pope Trogu

### Timeline
2023-2024

### Key Features
- **Time Series Analysis**: Implemented using `fpp3`, `tsibble`, and `forecast` packages.
- **Data Visualization**: Plots and graphical analysis are created with `ggplot2` and `plotly` to explore trends in energy generation.
- **Modeling**: Includes models from `lmtest` and `forecast` for various forecasting methods.
- **Data Processing**: Utilizes `dplyr` and `lubridate` to clean and structure the data for analysis.

### Data Description
The project uses energy consumption data from various sources (e.g., Petroleum, Solar, Wind). Key steps include:
1. Converting date formats.
2. Summing generation by producer and energy source.
3. Removing missing or non-finite values for more accurate analysis.

### Predictive Modeling
The project includes forecasting models for energy consumption trends, particularly focusing on shifts from petroleum to renewable sources post-2010.

### Libraries Used
- `ggplot2`, `plotly`: Data visualization.
- `dplyr`, `lubridate`: Data manipulation and time handling.
- `forecast`, `prophet`, `fpp3`: Time series analysis and forecasting.
- `car`: For Variance Inflation Factor (VIF) analysis.

### Visualizations
The project includes time series plots for total energy sales by source over time, showcasing trends and forecasting future energy outputs.
