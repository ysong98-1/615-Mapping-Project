
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)


climate_data <- data.frame(
month = factor(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                   'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                 levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
  
month_num = 1:12,
  
# Average maximum temperature (degrees Celsius)
max_temp = c(21.8, 21.8, 20.3, 17.4, 14.6, 12.1, 11.8, 13.2, 15.2, 17.0, 18.8, 20.4),
  
# Average minimum temperature (degrees Celsius)
min_temp = c(12.1, 12.1, 11.0, 9.0, 7.0, 5.2, 4.6, 5.3, 6.5, 7.8, 9.4, 10.9),
  
# Average rainfall (millimeters)
rainfall = c(46.5, 38.8, 44.1, 49.4, 47.4, 53.6, 51.9, 54.0, 53.0, 62.0, 54.0, 56.7),
  
# Average daily sunshine (hours per day)
sunshine = c(8.0, 7.3, 6.4, 5.3, 4.2, 3.9, 4.4, 5.0, 5.9, 6.5, 6.9, 7.4)
)

#season session
climate_data <- climate_data %>%
  mutate(season = case_when(
    month_num %in% c(12, 1, 2) ~ "Summer",   # Dec, Jan, Feb
    month_num %in% c(3, 4, 5) ~ "Autumn",    # Mar, Apr, May
    month_num %in% c(6, 7, 8) ~ "Winter",    # Jun, Jul, Aug
    month_num %in% c(9, 10, 11) ~ "Spring"   # Sep, Oct, Nov
  ))


ui <- navbarPage(
  title = "Tasmania Climate Dashboard",
  
#Overview tab:
  
  tabPanel("Overview",
    h2("Tasmania Climate Overview"),
    h4("Hobart, Tasmania, Australia"),
    hr(),
    
    fluidRow(
      column(6,
        h3("Location Map"),
        p("Interactive map showing the weather station location in Hobart, Tasmania."),
        leafletOutput("location_map", height = "400px"),
        br(),
        p(em("You can zoom in/out and click on the marker for more information."))
      ),
      column(6,
        h3("Location Information"),
        p(strong("City:"), "Hobart"),
        p(strong("State:"), "Tasmania"),
        p(strong("Country:"), "Australia"),
        p(strong("Weather Station:"), "Hobart (Ellerslie Road)"),
        p(strong("Station Number:"), "094029"),
        p(strong("Latitude:"), "42.89°S"),
        p(strong("Longitude:"), "147.33°E"),
        p(strong("Elevation:"), "51 meters"),
        br(),
        
        h3("Climate Summary"),
        p(strong("Annual Average Maximum Temperature:"), "17.0°C"),
        p(strong("Annual Average Minimum Temperature:"), "8.4°C"),
        p(strong("Annual Rainfall:"), "611 mm"),
        p(strong("Average Daily Sunshine:"), "5.9 hours"),
        br(),
        
        h3("Climate Type"),
        p(strong("Temperate Oceanic (Cfb)")),
        p("• Mild summers (20-22°C)"),
        p("• Cool winters (5-12°C)"),
        p("• Year-round rainfall"),
        p("• No extreme temperatures")
      )
    ),
    
    hr(),
    
# Climate overview chart
    fluidRow(
      column(12,
        h3("Temperature and Rainfall Overview"),
        plotlyOutput("overview_plot", height = "350px")
      )
    ),
    
    br(),

    fluidRow(
      column(12,
        h4("Best Time to Visit: December - February (Summer)"),
        p("Summer offers the warmest temperatures (20-22°C), most sunshine (7-8 hours/day), 
          and lower rainfall. This makes it ideal for outdoor activities and sightseeing.")
      )
    )
  ),
  
#Tempeature:
  
  tabPanel("Temperature",
    h2("Temperature Analysis"),
    hr(),
    
    fluidRow(
      column(4,
        h3("Temperature Statistics"),
        p(strong("Warmest Month:"), "January/February (21.8°C)"),
        p(strong("Coolest Month:"), "July (4.6°C)"),
        p(strong("Annual Range:"), "17.2°C"),
        p(strong("Annual Average:"), "12.7°C"),
        br(),
        
        h3("Seasonal Averages"),
        p(strong("Summer (Dec-Feb):"), "Max 21.4°C, Min 11.7°C"),
        p(strong("Autumn (Mar-May):"), "Max 17.4°C, Min 9.0°C"),
        p(strong("Winter (Jun-Aug):"), "Max 12.4°C, Min 5.0°C"),
        p(strong("Spring (Sep-Nov):"), "Max 17.0°C, Min 7.9°C"),
        br(),
        
        h3("About Tasmania's Climate"),
        p("Tasmania has a moderate climate because it is surrounded by ocean. 
          The ocean keeps temperatures from getting too hot or too cold."),
        p("Temperatures are comfortable throughout the year with no extreme heat or cold.")
      ),
      
      column(8,
        h3("Monthly Temperature Pattern"),
        plotlyOutput("temp_plot", height = "350px"),
        br(),
        
        h3("Seasonal Temperature Comparison"),
        plotlyOutput("seasonal_temp_plot", height = "300px")
      )
    )
  ),
  
#Rainfall and sunshine:

  tabPanel("Rainfall & Sunshine",
    h2("Rainfall and Sunshine Analysis"),
    hr(),
    
    fluidRow(
      column(4,
        h3("Rainfall Statistics"),
        p(strong("Annual Total:"), "611 mm"),
        p(strong("Wettest Month:"), "October (62.0 mm)"),
        p(strong("Driest Month:"), "February (38.8 mm)"),
        p(strong("Monthly Average:"), "50.9 mm"),
        br(),
        
        h3("Seasonal Rainfall"),
        p(strong("Summer:"), "142 mm total"),
        p(strong("Autumn:"), "141 mm total"),
        p(strong("Winter:"), "160 mm total"),
        p(strong("Spring:"), "169 mm total"),
        br(),
        h3("Sunshine Statistics"),
        p(strong("Annual Average:"), "5.9 hours per day"),
        p(strong("Most Sunshine:"), "January (8.0 hours/day)"),
        p(strong("Least Sunshine:"), "June (3.9 hours/day)"),
        br(),
        h3("Climate Patterns"),
        p("Tasmania receives rainfall throughout the year with no dry season. 
          Spring months (September-November) tend to be the wettest."),
        p("Summer has about twice as much sunshine as winter, 
          giving long days for outdoor activities.")
      ),
      column(8,
        h3("Monthly Rainfall Distribution"),
        plotlyOutput("rainfall_plot", height = "350px"),
        br(),
        h3("Daily Sunshine Hours"),
        plotlyOutput("sunshine_plot", height = "350px")
      )
    )
  )
)


server <- function(input, output, session) {
  
#Interactive map
  
output$location_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 147.33, lat = -42.89, zoom = 10) %>%
      addMarkers(
        lng = 147.33, 
        lat = -42.89,
        popup = "<strong>Hobart Weather Station</strong><br>
                 Station #094029<br>
                 Hobart (Ellerslie Road)<br>
                 Elevation: 51m<br>
                 Coordinates: 42.89°S, 147.33°E"
      ) %>%
      addCircleMarkers(
        lng = 147.33,
        lat = -42.89,
        radius = 15,
        color = "red",
        fillColor = "red",
        fillOpacity = 0.3,
        popup = "Weather Station Location"
      )
  })

#Overview:

output$overview_plot <- renderPlotly({
    p <- ggplot(climate_data, aes(x = month)) +
      geom_line(aes(y = max_temp, group = 1, color = "Max Temperature"), size = 1) +
      geom_point(aes(y = max_temp, color = "Max Temperature"), size = 2) +
      geom_line(aes(y = min_temp, group = 1, color = "Min Temperature"), size = 1) +
      geom_point(aes(y = min_temp, color = "Min Temperature"), size = 2) +
      geom_col(aes(y = rainfall/5, fill = "Rainfall (÷5)"), alpha = 0.3) +
      scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue")) +
      scale_fill_manual(values = c("Rainfall (÷5)" = "green")) +
      scale_y_continuous(
        name = "Temperature (°C)",
        sec.axis = sec_axis(~.*5, name = "Rainfall (mm)")
      ) +
      labs(x = "Month", color = "", fill = "") +
      theme_minimal()
    ggplotly(p)
  })
  
#Temperature charts:
  
  output$temp_plot <- renderPlotly({
    p <- ggplot(climate_data, aes(x = month)) +
      geom_line(aes(y = max_temp, group = 1, color = "Maximum"), size = 1) +
      geom_point(aes(y = max_temp, color = "Maximum"), size = 2) +
      geom_line(aes(y = min_temp, group = 1, color = "Minimum"), size = 1) +
      geom_point(aes(y = min_temp, color = "Minimum"), size = 2) +
      scale_color_manual(values = c("Maximum" = "red", "Minimum" = "blue")) +
      labs(x = "Month", y = "Temperature (°C)", color = "") +
      theme_minimal()
    ggplotly(p)
  })
  
#seasonal temperature:
  
  output$seasonal_temp_plot <- renderPlotly({
    seasonal_data <- climate_data %>%
      group_by(season) %>%
      summarise(
        avg_max = mean(max_temp),
        avg_min = mean(min_temp),
        .groups = 'drop'
      ) %>%
      mutate(season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring")))
    p <- ggplot(seasonal_data, aes(x = season)) +
      geom_col(aes(y = avg_max, fill = "Maximum"), position = "dodge") +
      geom_col(aes(y = avg_min, fill = "Minimum"), position = "dodge") +
      scale_fill_manual(values = c("Maximum" = "red", "Minimum" = "blue")) +
      labs(x = "Season", y = "Temperature (°C)", fill = "") +
      theme_minimal()
    ggplotly(p)
  })
  
#Rainfall chart:
  
  output$rainfall_plot <- renderPlotly({
    p <- ggplot(climate_data, aes(x = month, y = rainfall, fill = season)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Summer" = "orange",
        "Autumn" = "brown",
        "Winter" = "lightblue",
        "Spring" = "green"
      )) +
      labs(x = "Month", y = "Rainfall (mm)", fill = "Season") +
      theme_minimal()
    ggplotly(p)
  })
  
#Sunshine chart:
  output$sunshine_plot <- renderPlotly({
    p <- ggplot(climate_data, aes(x = month, y = sunshine, group = 1)) +
      geom_line(color = "orange", size = 1) +
      geom_point(color = "orange", size = 2) +
      labs(x = "Month", y = "Sunshine (hours/day)") +
      theme_minimal()
    ggplotly(p)
  })
}


shinyApp(ui = ui, server = server)
