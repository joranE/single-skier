library(shiny)
library(shinydashboard)
source("helpers.R")
source("load_data.R")
source("load_nations.R")

shinyServer(function(input, output, session) {
  
  #Update name selections based on nation selection
  observe({
    selected_nation <- input$nationInput
    updateSelectInput(session,
                      inputId = "nameInput",
                      choices = c("",sort(unique(DATA$name[DATA$nation == selected_nation]))))
  })
  
  #Create plots
  
  #Major international plots
  output$plotDst1 <- renderPlot({
    plot_dst(nm = input$nameInput,type = "points")
  })
  
  output$plotDst2 <- renderPlot({
    plot_dst(nm = input$nameInput,type = "rank")
  })
  
  output$plotDst3 <- renderPlot({
    plot_dst(nm = input$nameInput,type = "mpb")
  })
  
  output$plotSpr1 <- renderPlot({
    plot_spr(nm = input$nameInput,type = "rank")
  })
  
  output$plotSpr2 <- renderPlot({
    plot_spr_bar(nm = input$nameInput,byTech = TRUE)
  })
  
  #General FIS plots
  output$plotDst4 <- renderPlot({
    plot_dst(nm = input$nameInput,type = "points",maj_int = FALSE)
  })
  output$plotSpr3 <- renderPlot({
    plot_spr(nm = input$nameInput,type = "points",maj_int = FALSE)
  })
  
  output$plotSpr4 <- renderPlot({
    plot_spr_bar(nm = input$nameInput,byTech = TRUE,maj_int = FALSE)
  })
  
  gen_tables <- reactive({ath_summary(nm = input$nameInput)})
  output$wjcTable <- renderTable({gen_tables()$ath_wjc})
  output$u23Table <- renderTable({gen_tables()$ath_u23})
  output$majTable <- renderTable({gen_tables()$ath_maj})
}
)
