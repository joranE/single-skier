library(shiny)
library(shinydashboard)
source("helpers.R")
source("load_data.R")
source("load_nations.R")

shinyServer(function(input, output, session) {
  
  #Update name selections based on nation selection
  observe({
    selected_nation <- input$nationInput
    updateSelectInput(session,inputId = "nameInput",choices = c("",sort(unique(DATA$name[DATA$nation == selected_nation]))))
  })
  
  #Create plots
  output$plotDst1 <- renderPlot({
    plot_dst(nm = input$nameInput,type = "points")
  })
  
  output$plotDst2 <- renderPlot({
    plot_dst(nm = input$nameInput,type = "rank")
  })
  
  output$plotSpr1 <- renderPlot({
    plot_spr(nm = input$nameInput)
  })
  
  output$plotSpr2 <- renderPlot({
    plot_spr_bar(nm = input$nameInput,byTech = TRUE)
  })
}
)
