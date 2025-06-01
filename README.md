# Traffic Data Visualization Using Shiny

A Shiny app to explore Uber ride data from April–September 2014 using maps, histograms, and interactive plots.

## Overview

This interactive dashboard allows you to explore Uber ride patterns in New York City from April through September 2014. The application provides various visualization tools including interactive maps, time-series analysis, and statistical summaries of ride data.

## Features

- **Interactive Maps**: Visualize ride locations using Leaflet maps
- **Time Analysis**: Explore ride patterns by hour, day, and month
- **Statistical Plots**: Generate histograms and other statistical visualizations
- **Interactive Plots**: Powered by Plotly for enhanced user interaction

## Data

The app uses Uber ride data from April–September 2014, including:
- Pickup locations (latitude/longitude)
- Pickup times and dates  
- Base station information

## Technologies Used

- **R**: Core programming language
- **Shiny**: Web application framework
- **Leaflet**: Interactive mapping
- **Plotly**: Interactive plotting
- **Data manipulation**: dplyr, lubridate

## Getting Started

1. Clone this repository
2. Install required R packages:
   ```r
   install.packages(c("shiny", "leaflet", "plotly", "dplyr", "lubridate"))
   ```
3. Run the application:
   ```r
   shiny::runApp()
   ```

## Tags

#R #Shiny #DataVisualization #Plotly #Leaflet #UberData
