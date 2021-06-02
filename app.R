#Loading libraries
library(shiny)
library(tidyr)
library(ggplot2)
library(DT)
library(dplyr)
library(rsconnect)
library(shinythemes)



#Load data from URL
load("movies.Rdata")

#Remove rows with missing values from table
drop_na(data = movies)

#Define user interface
ui <- fluidPage(theme = shinytheme("spacelab"),

  #Title
  titlePanel("Analysis of movie ratings"),

  #Create tabs
  tabsetPanel(

          #Tab1 - display scatterplot with reactive elements
           tabPanel("Plot",

                    sidebarLayout(

                      sidebarPanel(

                        sliderInput("thtr_rel_year", "Release year", 1970, 2014, value = c(1970,2014), step = 1, round = TRUE, sep=""),

                        sliderInput("runtime", "Movie length",39,267, value = c(39,267), step = 1, round = TRUE, sep=""),

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

           #Tab2 - display data table
           tabPanel("Data", dataTableOutput("moviestable")),

           tabPanel("Documentation",
                    p("Author: Aleksandra Jedrych"), br(),
                    a(href = "https://github.com/ajedrych", "GitHub"))
  )
)

# Define server function
server <- function(input, output) {

  #Filter movies for properly working of scatterplot
  movies_a <- reactive({
    minyear <- input$thtr_rel_year [1]
    maxyear <- input$thtr_rel_year[2]
    minruntime <- input$runtime[1]
    maxruntime <- input$runtime[2]

    #Create scatterplot using ggplot library
    output$scatterplot <- renderPlot({
    ggplot(m, aes_string(x = input$x, y = input$y)) +
      geom_point() +
      stat_smooth(method = glm, col = "red")
  })

    # Apply filters for release year and runtime
    m <- movies %>%
      filter(
        thtr_rel_year >= minyear,
        thtr_rel_year <= maxyear,
        runtime >= minruntime,
        runtime <= maxruntime
      ) %>%
    arrange(runtime)

 })

  #Delete unnecessary columns
  movies$title_type <- NULL
  movies$mpaa_rating <- NULL
  movies$thtr_rel_month <- NULL
  movies$thtr_rel_day <- NULL
  movies$dvd_rel_day <- NULL
  movies$dvd_rel_month <- NULL
  movies$dvd_rel_year <- NULL
  movies$best_pic_nom <- NULL
  movies$best_pic_win <- NULL
  movies$best_actor_win <- NULL
  movies$best_actress_win <- NULL
  movies$best_dir_win <- NULL
  movies$top200_box <- NULL
  movies$critics_rating <- NULL
  movies$audience_rating <- NULL

  #Print data table
  output$moviestable <- renderDataTable({
    datatable(data = movies,
              options = list(pageLength = 10),
              rownames = FALSE,
              class = "cell-border stripe")
  })

  #Print number of movies
  output$n_movies <- renderText({
      nrow(movies_a())
  })
}

#ShinyApp
shinyApp(ui = ui, server = server)