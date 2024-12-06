library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)

#get the data

laps <- readr::read_csv("lap_times.csv") %>% 
  select("raceId", "driverId", "lap", "position")
drivers <- readr::read_csv("drivers.csv") %>% 
  select("driverId", "driverRef", "code")
races <- readr::read_csv("races.csv") %>% 
  select("raceId", "year", "round", "name")
#teams <- readr::read_csv("constructors.csv")

#Joins
positions <- laps %>% 
  dplyr::left_join(y = races, by = "raceId") %>% 
  dplyr::left_join(y = drivers, by = "driverId") %>% 
  dplyr::filter(year == 2023) %>% 
  dplyr::select("round", "GP_name" = "name", "lap", "dcode" = "code", "driverRef", "position")


# Define UI for application that draws a histogram
ui <- page_sidebar(
  sidebar = sidebar(
    selectInput(inputId = "GP",
                label = "Grand Prix",
                choices = dplyr::distinct(positions,GP_name),
                selected = "Bahrain Grand Prix"
    ),
    
    selectInput(inputId = "driver",
                label = "Driver",
                choices = dplyr::distinct(positions,driverRef),
                selected = "hamilton"
    )
  ),
  # Output: Show scatterplot
  card(plotOutput(outputId = "scatterplot")) 
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$scatterplot <- renderPlot({ 
    
    ggplot(data = dplyr::filter(.data = positions, GP_name == input$GP & driverRef == input$driver), aes_string(x = "lap", y = "position", color = "driverRef")) +
      geom_point()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
