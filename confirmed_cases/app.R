#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(callr)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(rsconnect)
library(shinythemes)
library(DT)
remove(list=ls())

##Read Data
confirmed_cases<-read.csv("confirmed_pivot.csv")

#Filter to Tracked Cases
confirmed_cases_copy <- confirmed_cases
confirmed_cases_copy <- confirmed_cases_copy %>%
  filter(location =="East Moline" |
           location == "Jacksonville" |
           location == "Menard" | 
           location == "Logan" |
           location == "Hill")
  



# UI
ui <- fluidPage(theme = shinytheme("united"),
      ##Confirmed Cases  
  navbarPage(collapsible = TRUE,
             "Confirmed Cases at IDOC", id="nav",
             
             tabPanel("Cases at Tracked Facilities",
                      div(class="outer",
   fluidRow(             
             sidebarLayout(sidebarPanel(selectInput("location", "Facility", choices = unique(confirmed_cases_copy$location))),
                           mainPanel(
                             plotOutput("cases_tracked")
                           )                
              ))),
   ),
   ##dataset
  tabPanel("Data",
           div(class="outer",
               DT::dataTableOutput("dataset")
               ),
           
             downloadButton('downloadData', 'Download data')
           )))

  
# Server
server <- function(input, output) {
  
      output$cases_tracked <- renderPlot({
          ggplot(
            #filtered_data(),
            subset(confirmed_cases_copy, location == input$location),
          ) +
          geom_line(aes(x = Day, y = prisoner_confirmed, color = "Confirmed"), size = 2) +
          geom_line(aes(x = Day, y = prisoner_recovered, color = "Recovered"), size = 2) +
          scale_color_brewer(palette = "Spectral") + 
          labs(title = "COVID-19 Cases Among Prisoners From August 7 to August 30", 
               caption = "Source: Illinois Department of Corrections", x = "Day", y = "", color = "")

         
      })
      
      output$dataset = DT::renderDataTable({confirmed_cases})
      
      output$downloadData <- downloadHandler(
        filename = function() { 
          paste("dataset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(confirmed_cases, file)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
