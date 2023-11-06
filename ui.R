
library(shiny)
library(plotly)

shinyUI(fluidPage(
    
    headerPanel('Viewing Sentiments Chronologically'),
    titlePanel(title = div(img(src="logo.png",align='right')), "Data-fy Sentiment"),
    
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload Input data:", placeholder = "NM_Speech.csv"),
            fileInput("file2", "Upload Input data for comparison :", placeholder = "NM_Speech.csv"),
            
            #selectInput('TokenType','Select level of tokenization :',c('lines','words')),
            
            selectInput('Lexicon','Select Sentiment Dictionary :',c('afinn','nrc')),
            
            selectInput('nrc_emots',"Select emotion (NRC)", nrc_emots, selected = "joy"),
            
            #sliderInput('num_chars','Enter number of characters to display',10,400,150),
            
            actionButton(inputId = "apply",label = "Apply Changes", icon("refresh"))
            
        ),
        
        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        tabPanel("Overview",
                                 h4(p("Visualizing Sentiment as Time Series")),
                                 p("This app takes long text(s) as input and plots the sentiment reflected by each line or sentence from the start to end. 
                                    The first two tabs show how the back-end breaks the data into usable inputs for sentiment identification, the words that contribute
                                    to the sentiment and the score for that sentence. The third tab plots these sentiment scores on a graph for visual comparison. 
                                    The final tab shows the wordcloud for the sentiment creating words."), br(),
                                 p("Please note that this application is a work in development so some errors may arise during WordCloud generation."),
                                 h4(p("Download Sample Input File")),
                                 downloadButton('downloadData', 'Download Example file')
                                 
                                 
                        ),
                        
                        tabPanel("Data",
                                 h3("Divided into tokens like above, it gives the following sentiment scores:"),
                                 tableOutput('Senti_Table')
                        ),
                        
                        tabPanel("Comparison Data",
                                 h3("The data input for comparison appears like this :"),
                                 tableOutput('ComparisonData')),
                        
                        tabPanel("Sentiment Plot",
                                 h3('First Dataset'),
                                 
                                 plotlyOutput('Plot'),
                                 h3('Comparison Dataset'),
                                 plotlyOutput('VersusPlot')
                        ),
                        tabPanel("Word Clouds",
                                 plotOutput('WordCloud1',height = 700, width = 700),
                                 plotOutput('WordCloud2',height = 700, width = 700))
            )
        ))))
