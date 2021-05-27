library(shiny)
library(tidyr)
library(ggplot2)
library(DT)

load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))
source(file = names.R)

ui <- fluidPage(
  titlePanel("Movies ratings analysis"),

  sidebarLayout(

    sidebarPanel(
      selectInput("x", "X-axis",
        c("runtime", "IMDB rating", "iIMDB - number of votes", "critics score", "audience score"),
        selected = "runtime"),

      selectInput("y", "Y-axis",
        c("runtime", "IMDB rating", "IMDB - number of votes", "critics score", "audience score"),
        selected = "IMBD rating"),

      selectInput("title_type", "Type",
        c("Documentary", "Feature Film", "TV Movie")),

      selectInput("genre", "Genre",
          c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
            "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
            "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
            "Short", "Sport", "Thriller", "War", "Western")),

      sliderInput("thtr_rel_year", "Year released", 1970, 2014, value = c(1970,2014), step = 1),

      sliderInput("runtime", "Length",65,267, value = c(65,267), step = 1),


    ),
    # Output --------------------------------------------------------
    mainPanel(

      # Show scatterplot --------------------------------------------
      plotOutput(outputId = "scatterplot"),

      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {

  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y)) +
    geom_point()
  })

  # Print data table if checked -------------------------------------
  output$moviestable <- DT::renderDataTable({
    DT::datatable(data = movies[, 1:4],
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)