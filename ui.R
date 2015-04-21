
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  #Header
  dashboardHeader(title = "Basic dashboard"),
  
  #Sidebar
  dashboardSidebar(
    selectInput(inputId = "nationInput",
                label = "Nation",
                choices = c("Choose..." = "","Canada" = "CAN","United States" = "USA"),
                selected = NULL,
                selectize = TRUE),
    selectInput(inputId = "nameInput",
                label = "Name",
                choices = "",
                selected = NULL,
                selectize = TRUE),
    hr(),
    helpText("Joran Elias - statisticalskier.com"),
    hr(),
    helpText("Some generic help.")),
  
  #Body
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plotDst1", height = 350),title = "Distance"),
      box(plotOutput("plotSpr1",height = 350),title = "Sprint")
    ),
    fluidRow(
      box(plotOutput("plotDst2",height = 350),title = "Distance"),
      box(plotOutput("plotSpr2",height = 350),title = "Sprint")
    )
  )
))
