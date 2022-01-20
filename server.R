
library(shiny)
library(tidytext)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(plotly)
library(tools)


shinyServer(function(input, output) {

    output$downloadData <- downloadHandler(
        filename = function() { "PMSpeech.csv" },
        content = function(file) {
            write.csv(readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/PM%20speech%202014.txt'), file, row.names=F, col.names=F)
        })

    
    
    dataset <- reactive({
        if (is.null(input$file)) {return(NULL)}
        else {
            
            if(file_ext(input$file$datapath)=="txt"){
                Document = readLines(input$file$datapath)
                return(Document)}
            else{
                Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
                return(Document)
            }
            
        }
    })     
    
    dataset2 <- reactive({
      if (is.null(input$file2)) {return(NULL)}
      else {
        
        if(file_ext(input$file2$datapath)=="txt"){
          Document = readLines(input$file2$datapath)
          return(Document)}
        else{
          Document = read.csv(input$file2$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
          return(Document)
        }
        
      }
    })
    
    
    senti_df = read.csv("https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/sentiments.csv")
    
    afinn_senti = senti_df %>% filter(., lexicon == "AFINN") %>%
        select(X, word, score) %>% rename("value"="score")
    
    nrc_senti = senti_df %>% filter(., lexicon == "nrc") %>%
        select(X, word, sentiment) 
    
    nrc_emots = table(nrc_senti$sentiment) %>% names() # list of 10
    
    token0 = eventReactive(input$apply,input$TokenType)
    lexicon0 = eventReactive(input$apply,input$Lexicon)
    emots0 = eventReactive(input$apply,input$nrc_emots)
    
    
    a00 = reactive({calc_senti_score(dataset(), token0(), lexicon0(), afinn_senti, nrc_senti, emots0())})
    a01 = reactive({calc_senti_score(dataset2(), token0(), lexicon0(), afinn_senti, nrc_senti, emots0())})
    
    output$Senti_Table <-renderTable({as.data.frame(a00()[1])})
    
    
    output$ComparisonData <- renderTable(as.data.frame(a01()[1]))
    
    a00_dtm <- reactive({dtm_build(a00()[[1]]$senti_tooltip)})
    a00_count <- reactive({dtm.word.count(a00_dtm())})
    
    a01_dtm <- reactive({dtm_build(a01()[[1]]$senti_tooltip)})
    a01_count <- reactive({dtm.word.count(a01_dtm())})
    
    output$WordCloud1 <- renderPlot(
      dtm.word.cloud(a00_count(), max.words = 100, title = "Word Cloud for Data 1")
    )
    
    
    output$WordCloud2 <- renderPlot(
      dtm.word.cloud(a01_count(), max.words = 100, title = "Word Cloud for Data 2")
    )
    
    output$Plot <- renderPlotly({a00()[[2]]})
    
    output$VersusPlot <- renderPlotly({a01()[[2]]})
#        if (is.null(input$file)|input$apply==0) {return(NULL)}
#        else{
            
  #          a = a00()[[2]]
   #         a #}
   # )

    
    
    
    })
