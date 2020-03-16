library(shiny)
library(here)
library(readr)
library(shinythemes)
library(tidyverse)

lessons <- read_csv(here("rladieslesson.csv")) %>%
  select(City, topic, language, html_url)

websites <- read_csv(here("websites.csv")) 

repos <- read_csv(here("repos.csv")) 
  

ui <- fluidPage(
  titlePanel("R-Ladies Meetups Materials"),
  p(strong("This Shiny App was generated for the celebration of International Women's Day 2020.The Twitter campaign can be followed in the @rladies_iwd account. If you missed a tweet, here is all the material we share during the 8M"), style = "color:purple"),
  p(strong("The topic and language tags were automatically generated so we may have errors, if you find one, please report an issue in the Shiny App repository here:",style = "color:purple")), 
  tags$a(href="https://github.com/rladies/IWD/tree/master/RLadiesLesson","https://github.com/rladies/IWD/tree/master/RLadiesLesson"),
  br(),
  br(),
  navlistPanel(
    
    "Resourses",
    tabPanel("Material by Topic",
             h3("Material by Topic", style = "color:purple"),
             # Create a new Row in the UI for selectInputs
             fluidRow(
               column(4,
                      selectInput("city",
                                  "City:",
                                  c("All",
                                    unique(as.character(lessons$City))))
               ),
               column(4,
                      selectInput("topic",
                                  "Topic:",
                                  c("All",
                                    unique(as.character(lessons$topic))))
               ),
               column(4,
                      selectInput("lang",
                                  "Language:",
                                  c("All",
                                    unique(as.character(lessons$language))))
               )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("table_lessons")
    ),
    tabPanel("Chapters WebSites",
             h3("Chapters WebSites", style = "color:purple"),
             # Create a new Row in the UI for selectInputs
             fluidRow(
               column(4,
                      selectInput("city_wb",
                                  "City:",
                                  c("All",
                                    unique(as.character(websites$City))))
               ),
               column(4,
                      selectInput("country",
                                  "Country:",
                                  c("All",
                                    unique(as.character(websites$Country))))
               ),
               column(4,
                      selectInput("email",
                                  "Email:",
                                  c("All",
                                    unique(as.character(websites$Email))))
               )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("table_website")
    ),
    tabPanel("Chapters GitHub Repos",
             h3("Chapters GitHub Repos", style = "color:purple"),
             fluidRow(
               column(4,
                      selectInput("city_gr",
                                  "City:",
                                  c("All",
                                    unique(as.character(websites$City))))
               ),
               column(4,
                      selectInput("country_gr",
                                  "Country:",
                                  c("All",
                                    unique(as.character(websites$Country))))
               ),
               column(4,
                      selectInput("email_gr",
                                  "Email:",
                                  c("All",
                                    unique(as.character(websites$Email))))
               )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("table_github")
    )
  ),img(src = "R-LadiesGlobal.png", height = 140, width = 140)
   
  )

server <- function(input, output) {
  
  # Filter data based on selections
  output$table_lessons <- DT::renderDataTable(DT::datatable({
    data <- lessons
    
    data$html_url <- paste0('<a href="', data$html_url,'" target="_blank">', data$html_url ,'</a>')
    
    if (input$city != "All") {
      data <- data[data$City == input$city,]
    }
    if (input$topic != "All") {
      data <- data[data$topic == input$topic,]
    }
    if (input$lang != "All") {
      data <- data[data$language == input$lang,]
    }
    data
  }, escape = FALSE))
  
 
  
  output$table_website <- DT::renderDataTable(DT::datatable({
    data <- websites
    data$Website <- paste0('<a href="', data$Website,'" target="_blank">', data$Website ,'</a>')
    data$Meetup <- paste0('<a href="', data$Meetup,'" target="_blank">', data$Meetup ,'</a>')
    
    if (input$city_wb != "All") {
      data <- data[data$City == input$city_wb,]
    }
    if (input$topic != "All") {
      data <- data[data$Country == input$Country,]
    }
    if (input$lang != "All") {
      data <- data[data$Email == input$email,]
    }
    data
  }, escape = FALSE))
  
  output$table_github <- DT::renderDataTable(DT::datatable({
    data <- repos
    data$Meetup <- paste0('<a href="', data$Meetup,'" target="_blank">', data$Meetup ,'</a>')
    data$GitHub <- paste0('<a href="', data$GitHub,'" target="_blank">', data$GitHub ,'</a>')
    
    if (input$city_gr != "All") {
      data <- data[data$City == input$city_gr,]
    }
    if (input$country_gr != "All") {
      data <- data[data$Country == input$country_gr,]
    }
    if (input$email_gr != "All") {
      data <- data[data$Email == input$email_gr,]
    }
    data
  }, escape = FALSE))
}

shinyApp(ui, server)