#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

shinyUI(fluidPage(
    
    headerPanel('Viewing Sentiments Chronologically'),
    titlePanel(title = div(img(src="logo.png",align='right')), "Data-fy Sentiment"),
    
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload Input data:", placeholder = "NM_Speech.csv"),
            
            selectInput('TokenType','Select level of tokenization :',c('sentences','lines','paragraphs')),
            
            selectInput('Lexicon','Select Sentiment Dictionary :',c('afinn','nrc')),
            
            selectInput('nrc_emots',"Select emotion (NRC)", nrc_emots, selected = "joy"),
            
            sliderInput('num_chars','Enter number of characters to display',10,400,150),
            
            actionButton(inputId = "apply",label = "Apply Changes", icon("refresh"))
            
        ),
        
        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        tabPanel("Overview",
                                 h4(p("Visualizing Sentiment as Time Series")),
                                 h4(p("Download Sample Input File")),
                                 downloadButton('downloadData', 'Download Example file')
                                 
                                 
                        ),
                        
                        tabPanel("Data",
                                 h3("Divided into tokens like above, it gives the following sentiment scores:"),
                                 tableOutput('Senti_Table')
                        ),
                        
                        tabPanel("Sentiment Plot",
                                 plotlyOutput('Plot')
                        ),
                        
                        tabPanel("Document Level Analysis"
                                 
                        )
            )
        ))))
