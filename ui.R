
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
source("load_nations.R")

shinyUI(dashboardPage(
  #Header
  dashboardHeader(title = "Statistical Skier"),
  
  #Sidebar
  dashboardSidebar(
    selectInput(inputId = "nationInput",
                label = "Nation",
                choices = c("Choose..." = "",NATIONS),
                selected = NULL,
                selectize = TRUE),
    selectInput(inputId = "nameInput",
                label = "Name",
                choices = "",
                selected = NULL,
                selectize = TRUE),
    menuItem(
      menuSubItem(text = "Summary",tabName= "athSummary"),
      menuSubItem(text = "All FIS Results",tabName = "allFIS"),
      menuSubItem(text = "Major International Results",tabName = "majInt"),
      text = "Charts",
      icon = icon("bar-chart")
      ),
    hr(),
    helpText("Joran Elias - statisticalskier.com"),
    hr(),
    helpText("Choose a nation and then an individual from that nation. Click on one of the sidebar options 
             to see the results."),
    helpText("These plots show only one athlete at a time. Plots with small amounts of data may be omitted.")),
  
  #Body
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "athSummary",
        fluidRow(
          column(width = 6,
                 box(tableOutput("wjcTable"),
                     width = NULL,
                     title = "World Junior Results",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     status = "primary"),
                 box(tableOutput("u23Table"),
                     width = NULL,
                     title = "U23 Results",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     status = "primary")
          ),
          column(width = 6,
                 box(tableOutput("majTable"),
                     width = NULL,
                     title = "Major Results",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     status = "success")
          )  
        )
      ),
      tabItem(tabName = "allFIS",
              fluidRow(
                column(width = 6,
                       box(plotOutput("plotDst4"),
                           width = NULL,
                           title = "Distance by FIS points",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary")
                ),
                
                column(width = 6,
                       box(plotOutput("plotSpr3"),
                           width = NULL,
                           title = "Sprint by FIS points",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "success"),
                       box(plotOutput("plotSpr4"),
                           width = NULL,
                           title = "Sprint qualification rates",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "success")
                )
              )
      ),
      tabItem(tabName = "majInt",
              fluidRow(
                column(width = 6,
                       box(plotOutput("plotDst3"),
                           width = NULL,
                           title = "Distance by Stand % Back from Median",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary"),
                       box(plotOutput("plotDst1"),
                           width = NULL,
                           title = "Distance by FIS points",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary"),
                       box(plotOutput("plotDst2"),
                           width = NULL,
                           title = "Distance by finishing place",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary")
                ),
                
                column(width = 6,
                       box(plotOutput("plotSpr1"),
                           width = NULL,
                           title = "Sprint by finishing place",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "success"),
                       box(plotOutput("plotSpr2"),
                           width = NULL,
                           title = "Sprint qualification rates",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "success")
                )
            )
        )
    )
  )
))
