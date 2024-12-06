####Shiny app which displays driver positions for each lap of each race of the 2023 F1 season####

#Load required packages
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)

#Load data - could this be done in a helper file?

laps <- readr::read_csv("lap_times.csv") %>% 
  select("raceId", "driverId", "lap", "position")
drivers <- readr::read_csv("drivers.csv") %>% 
  select("driverId", "driverRef", "code")
races <- readr::read_csv("races.csv") %>% 
  select("raceId", "year", "round", "name")
#teams <- readr::read_csv("constructors.csv")

#Joins --see above
positions <- laps %>% 
  dplyr::left_join(y = races, by = "raceId") %>% 
  dplyr::left_join(y = drivers, by = "driverId") %>% 
  dplyr::filter(year == 2023) %>% 
  dplyr::select("round", "GP_name" = "name", "lap", "dcode" = "code", "driverRef", "position")


# Define UI
ui <- page_sidebar(
  sidebar = sidebar(
    selectInput(inputId = "GP",
                label = "Grand Prix",
                choices = dplyr::distinct(positions,GP_name),
                selected = "Bahrain Grand Prix"
    ),
    
    checkboxGroupInput(inputId = "driver",
                label = "Drivers",
                choices = pull(dplyr::distinct(positions,driverRef),driverRef),
                selected = "hamilton"
    )
  ),
  # Output: Show scatterplot
  card(plotOutput(outputId = "scatterplot")) 
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$scatterplot <- renderPlot({ 
    
    ggplot(data = dplyr::filter(.data = positions, GP_name == input$GP & driverRef %in% input$driver), aes_string(x = "lap", y = "position", color = "driverRef")) +
      geom_path()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
