# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  sidebar = sidebar(
    # Select variable for y-axis
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c(
        "IMDB rating" = "imdb_rating",
        "IMDB number of votes" = "imdb_num_votes",
        "Critics score" = "critics_score",
        "Audience score" = "audience_score",
        "Runtime" = "runtime"
      ),
      selected = "audience_score"
    ),
    
    # Select variable for x-axis
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = c(
        "IMDB rating" = "imdb_rating",
        "IMDB number of votes" = "imdb_num_votes",
        "Critics score" = "critics_score",
        "Audience score" = "audience_score",
        "Runtime" = "runtime"
      ),
      selected = "critics_score"
    ),
    
    # Select variable for color
    selectInput(
      inputId = "z",
      label = "Color by:",
      choices = c(
        "Title type" = "title_type",
        "Genre" = "genre",
        "MPAA rating" = "mpaa_rating",
        "Critics rating" = "critics_rating",
        "Audience rating" = "audience_rating"
      ),
      selected = "mpaa_rating"
    )
  ),
  
  # Output: Show scatterplot
  card(plotOutput(outputId = "scatterplot")) 
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({ ##should be renderPlot
    
    ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point()
    
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)