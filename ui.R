library(shiny)
library(shinydashboard)
source("load_nations.R")

header <- dashboardHeader(title = "Statistical Skier")
sidebar <- dashboardSidebar(
  selectInput(inputId = "nationInput",
              label = "Nation",
              choices = c("Start typing..." = "",NATIONS),
              selected = NULL,
              selectize = TRUE),
  selectInput(inputId = "nameInput",
              label = "Name",
              choices = "",
              selected = NULL,
              selectize = TRUE),
  hr(),
  sidebarMenu(
    menuItem(text = "Summary",tabName= "athSummary",icon = icon("list")),
    menuItem(text = "All FIS Results",tabName = "allFIS",icon = icon("bar-chart")),
    menuItem(text = "Major International Results",tabName = "majInt",icon = icon("bar-chart"))
  ),
  hr(),
  helpText("Joran Elias - statisticalskier.com"),
  hr(),
  helpText("Choose a nation and then an individual from that nation. Click on one of the sidebar options 
             to see the results."),
  hr(),
  helpText("These plots show only one athlete at a time. Plots with small amounts of data may be omitted."),
  helpText("Only shows athletes who have competed at the World Cup, 
             Olympic, World Championship or Tour de Ski level. These events are referred to as 'Major'."),
  hr(),
  helpText("Pursuit refers to what FIS now calls Skiathlon; Handicap refers to what FIS now calls Pursuit and
             Pursuit Break refers to the older one or two day pursuits.")
  )

body <- dashboardBody(
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
                   status = "success"),
               box(tableOutput("startTechTable"),
                   width = NULL,
                   title = "Median Distance Results (Major)",
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

dashboardPage(header,sidebar,body)