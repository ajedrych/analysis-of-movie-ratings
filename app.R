#Loading libraries
library(shiny)
library(tidyr)
library(ggplot2)
library(DT)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(ggvis)

#Loading data from URL
load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

#Removing rows with missing values from table
drop_na(data = movies)

#User interface
ui <- fluidPage(

  #Title
  titlePanel("Analysis of movie ratings"),

  #Creating tabs
  tabsetPanel(

          #Tab1 - displaying scatterplot with reactive elements
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(

                        selectInput("genre", "Genre",
                                    c("All", "Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "History", "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi", "Short", "Sport", "Thriller", "War", "Western")),

                        sliderInput("thtr_rel_year", "Release year", 1970, 2014, value = c(1970,2014), step = 1, round = TRUE, sep=""),

                        sliderInput("runtime", "Movie length",65,267, value = c(65,267), step = 1, round = TRUE, sep=""),

                        selectInput("x", "X-axis",
                                    c("runtime", "imdb_rating", "imdb_num_votes", "critics_score", "audience_score"),
                                    selected = "runtime"),

                        selectInput("y", "Y-axis",
                                    c("runtime", "imdb_rating", "imdb_num_votes", "critics_score", "audience_score"),
                                    selected = "imdb_rating")
                      ),

                      mainPanel(plotOutput("scatterplot"),
                                span("Number of movies selected:", textOutput("n_movies"))
                      )
                    )
           ),

           #Tab2 - dispalying data table
           tabPanel("Data", dataTableOutput("moviestable"))

  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {

  movies_a <- reactive({
    minyear <- input$thtr_rel_year[1]
    maxyear <- input$thtr_rel_year[2]
    minruntime <- input$runtime[1]
    maxruntime <- input$runtime[2]

  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(m, aes_string(x = input$x, y = input$y)) +
    geom_point()
  })

  m <- movies %>%
      filter(
        thtr_rel_year >= minyear,
        thtr_rel_year <= maxyear,
        runtime >= minruntime,
        runtime <= maxruntime
      ) %>%
    arrange(runtime)

    if (input$genre != "All") {
      genre <- paste0(input$genre)
      m <- m %>% filter(genre %like% genre)
    }

    m <- as.data.frame(m)
 })

  # Print data table if checked -------------------------------------)
  output$moviestable <- renderDataTable({
    datatable(data = movies_table,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })

   output$n_movies <- renderText({ nrow(movies_a()) })
}

shinyApp(ui = ui, server = server)