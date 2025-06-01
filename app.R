library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)
library(lubridate) # Load the lubridate package

source("uber_analysis.R")

tolerance <- 3000 # Max number to plot and perform k means clustering on
min_cl_num <- 5 # Minimum number of clusters
max_cl_num <- 20 # Maximum number of clusters

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Traffic Data Analysis"),
  
  dashboardSidebar(
    disable = FALSE,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Scatter Plot", tabName = "scatter_plot", icon = icon("line-chart")),
      menuItem("Bar Chart", tabName = "bar_chart", icon = icon("bar-chart"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              sidebarPanel(
                h4("Data Varying over the date"),
                sliderInput("slider", "Date Index:",
                            value = 1, min = 1, max = 184),
                sliderInput("kmc", "Number of Clusters:",
                            value = min_cl_num, min = min_cl_num, max = max_cl_num),
                plotOutput("histhr")
              ),
              mainPanel(
                leafletOutput("map")
              ),
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
      ),
      tabItem(tabName = "scatter_plot",
              fluidPage(
                sidebarPanel(
                  h4("Scatter Plot"),
                  sliderInput("time_series_slider", "Date Index:",
                              value = 1, min = 1, max = 184)
                ),
                mainPanel(
                  plotlyOutput("time_series_plot")
                )
              )
      ),
      tabItem(tabName = "bar_chart",
              fluidPage(
                selectInput("bar_chart_dropdown", "Granularity:", 
                            choices = c("Day", "Week", "Month"),
                            selected = "Day"),
                plotlyOutput("collective_bar_chart_plot")
              )
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  p <- apr_to_sep_dat
  
  pal <- colorFactor(
    palette = "Dark2",
    domain = factor(p$hr_map)
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Layout
      addCircleMarkers(data = p[first_indices[input$slider]:(first_indices[input$slider]+
                                                               min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),],
                       lat = ~ Lat, lng = ~ Lon,
                       fillOpacity = 1, 
                       radius = 0.5,
                       color = ~pal(hr_map)) %>%
      addMarkers(data = data.frame(kmeans(p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                           min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),c("Lat","Lon")],
                                          input$kmc)$centers),
                 lat = ~Lat, lng = ~ Lon,
                 label = ~ paste("Latitude:", round(Lat, 3), "Longitude", round(Lon, 3))) %>%
      addLegend("bottomright", pal = pal, values = p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                                    min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),"hr_map"],
                title = paste("Date:", p[first_indices[input$slider],"Date"]),
                opacity = 1) %>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 12) # Sets zoom to be in NYC
  })
  
  output$histhr <- renderPlot({
    ggplot(data = data.frame(count(p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                    min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),"hr_map"])), aes(x=x, y=freq, fill=I("green"), col=I("blue"))) +  
      geom_bar(stat="identity", position="dodge", width=0.9, alpha=0.3) +
      theme(text = element_text(size = 11), 
            axis.line = element_line(colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.background = element_rect(fill = "grey")) +
      ggtitle(paste("Histogram of ride frequency for date:", p[first_indices[input$slider],"Date"]))
  })
  
  output$time_series_plot <- renderPlotly({
    selected_data <- p[first_indices[input$time_series_slider]:(first_indices[input$time_series_slider] +
                                                                  min(first_indices[(input$time_series_slider + 1)] - first_indices[(input$time_series_slider)], tolerance)), ]
    
    # Create an interactive scatter plot using plotly
    plot_ly(data = selected_data, x = ~Lon, y = ~Lat, mode = "markers", text = ~paste("Time: ", hr, "<br>Date: ", Date)) %>%
      layout(
        title = paste("Interactive Scatter Plot of Rides: Latitude vs Longitude"),
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
  })
  
  collective_bar_chart_data <- reactive({
    selected_granularity <- input$bar_chart_dropdown
    if (selected_granularity == "Day") {
      p$Granularity <- as.Date(p$Date)
    } else if (selected_granularity == "Week") {
      p$Granularity <- as.Date(cut(p$Date, breaks = "week"))
    } else if (selected_granularity == "Month") {
      p$Granularity <- as.Date(cut(p$Date, breaks = "month"))
    }
    
    aggregated_data <- aggregate(Lat ~ Granularity, data = p, FUN = length)
    colnames(aggregated_data) <- c("Granularity", "Count")
    aggregated_data
  })
  
  output$collective_bar_chart_plot <- renderPlotly({
    data <- collective_bar_chart_data()
    
    # Create an interactive collective bar chart using plotly
    plot_ly(data = data, x = ~Granularity, y = ~Count, type = "bar") %>%
      layout(
        title = "Collective Bar Chart",
        xaxis = list(title = "Granularity"),
        yaxis = list(title = "Count")
      )
  })
})

runApp(shinyApp(ui, server), launch.browser = TRUE, port = 8080, host = "0.0.0.0")
