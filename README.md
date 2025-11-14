# soil temp analysis

This project explores relationships between soil temperature and other environmental variables (air temperature, precipitation, snow depth) using R.

## Data
- Soil temperature : Agriculture and Agri-Food Canada RISMA Station data
- Air temperature and precipitation: Environment Canada , Ottawa Intl Station data

## Methods
- Data cleaning and aggregation (hourly → daily)
- Correlation and visualization
- Exploratory plots with ggplot2 and corrplot

## Results
- Soil temperature strongly correlated with air temperature (r = 0.89)
- Snow depth shows moderate negative correlation

## Tools
R (tidyverse, dplyr, corrplot, ggplot2)
