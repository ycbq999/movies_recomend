
#setwd("D:/My Data/UIUC/CS598_Practical_Statistical_Learning/Project4")
#install.packages("shiny")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ShinyRatingInput)


# source('D:/My Data/UIUC/CS598_Practical_Statistical_Learning/Project4/functions/helpers.R')
source('functions/helpers.R')
#path = 'D:/My Data/UIUC/CS598_Practical_Statistical_Learning/Project4/'
#path = '/'

genre_list = c("Action",	"Adventure",	"Animation",	"Children.s",	"Comedy",	
               "Crime",	"Documentary",	"Drama",	"Fantasy",	"Film.Noir",	"Horror",	
               "Musical",	"Mystery",	"Romance",	"Sci.Fi",	"Thriller",	"War",	"Western")  


header <- dashboardHeader(title = "Basic dashboard")


sidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Recommender by Genre", tabName = "genre", icon = icon("film")),
    menuItem("Recommender by Rating", tabName = "rating", icon = icon("star"))
  )
)


body <- dashboardBody(includeCSS("css/movies.css"),
  tabItems(
    tabItem( tabName = "genre",
             
             fluidRow(
               box(width = 12, title = "Step 1: select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                   selectInput("genre_select", h5("Select a single genre from the dropdown menu"), 
                               choices = genre_list))
             ),
             fluidRow(
               useShinyjs(),
               box(width = 12, title = "Step 2: Discover movies you might like", status = "info", solidHeader = TRUE, collapsible = TRUE,
                   br(),
                   withBusyIndicatorUI(
                     actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                     ),
                   br(),
                   tableOutput("results")
                   )
             )
    ),
    
    
    
    tabItem( tabName = "rating",
             #h5 ("rating tab content")  start here
             
             fluidRow(
               box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                   div(class = "rateitems",
                       uiOutput('ratings')
                   )
               )
             ),
             
             fluidRow(
               useShinyjs(),
               box(
                 width = 12, status = "info", solidHeader = TRUE,
                 title = "Step 2: Discover books you might like",
                 br(),
                 withBusyIndicatorUI(
                   actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                 ),
                 br(),#,
                 #tableOutput("results2")
                 uiOutput('results2')# for testing
               )
             )
             
             
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

