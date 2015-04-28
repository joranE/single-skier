library(shiny)
library(shinydashboard)
#library(Cairo)
#options(shiny.usecairo = TRUE)
source("helpers.R")
source("load_data.R")
source("load_nations.R")

function(input, output, session) {
  
  #Update name selections based on nation selection
  observe({
    possible_names <- DATA %>% 
      filter(nation == input$nationInput) %>%
      select(name) %>%
      collect() %>%
      unique() %>%
      arrange(name) %>%
      magrittr::extract2("name")
      
    updateSelectInput(session,
                      inputId = "nameInput",
                      choices = c("",possible_names))
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
  
  #Generate summary tables
  wjc_table <- reactive({ath_wjc(nm = input$nameInput)})
  u23_table <- reactive({ath_u23(nm = input$nameInput)})
  maj_table <- reactive({ath_maj(nm = input$nameInput)})
  start_tech_table <- reactive({start_tech(nm = input$nameInput)})
  
  #Render summary tables
  output$wjcTable <- renderTable({wjc_table()},include.rownames = FALSE)
  output$u23Table <- renderTable({u23_table()},include.rownames = FALSE)
  output$majTable <- renderTable({maj_table()},include.rownames = FALSE)
  output$startTechTable <- renderTable({start_tech_table()},include.rownames = FALSE)
}

