library(shiny)
library(tidyr)
library(ggplot2)
library(DT)

load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

ui <- fluidPage(
  titlePanel("Movies ratings analysis"),

  sidebarLayout(

    sidebarPanel(

      selectInput("title_type", "Type",
        c("Documentary", "Feature Film", "TV Movie")),

      selectInput("genre", "Genre",
          c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
            "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
            "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
            "Short", "Sport", "Thriller", "War", "Western")),

      sliderInput("thtr_rel_year", "Year released", 1970, 2014, value = c(1970,2014), step = 1),

      sliderInput("runtime", "Length",65,267, value = c(65,267), step = 1),

      selectInput("x", "X-axis",
        c("runtime", "imdb_rating", "imdb_num_votes", "critics_score", "audience_score"),
        selected = "runtime"),

      selectInput("y", "Y-axis",
        c("runtime", "imdb_rating", "imdb_num_votes", "critics_score", "audience_score"),
        selected = "imdb_rating")
    ),

    # Output --------------------------------------------------------
    mainPanel(
      tabsetPanel(
            tabPanel('Plot', plotOutput("scatterplot"),
            tabPanel( "Table", dataTableOutput("moviestable"))
        )
    )
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
  output$moviestable <- renderDataTable({
    datatable(data = movies[, 1:4],
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)