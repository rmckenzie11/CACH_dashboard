## Load in libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)
library(gridExtra)
library(grid)
library(ggplotify)
library(quanteda)

## Load in tidy words by meeting
tidy_words <- readRDS("data/tidy_words.RDS")

## Load in sentiment plot by meeting
sent_plot_meetings <- readRDS("data/sent_plot_meetings.rds")

## Load in sentiment analysis of all comments
sent_all_comments <- readRDS("data/sent_all_comments.rds")

## Load in Topic Modelling
lda <- readRDS("data/LDA.rds")

## Load in just MP text file
just_mp <- readRDS("data/just_mp.rds")

## Funcion (fluidPage) that defines UI
ui <- fluidPage(
    
    ## ShinyThemes, remember to experiment later
    theme = shinytheme("cosmo"),
    
    # Application title
    titlePanel("Analysis of CACN Meetings, January 20 - March 9"),
    checkboxInput("witness", "Include Witnesses, Clerk, and Chair?"),
    # Sidebar, MP Focus.  
    sidebarLayout(
        
        ## Only one panel, nice and simple
        sidebarPanel(
        
            h2("MP Focus"),
            ## Allow user to select an MP to see their specific analysis
            selectInput("select", "Speaker:", 
                        choices = unique(just_mp$name), selected = "Mr. Richard Oliphant"),
            hr(),
            
            helpText("Data from ourcommons.ca."),
            
            ## 1st Output, topics
            h4("Topics Mentioned Most by MP"),
            plotOutput("mpTop"),
            
            ## 2nd Output Table: the most frequent words, they used, remember to work on 
            h4("Most frequent words used by MP"),
            DT::dataTableOutput("mpTable"),
            
            #3rd output table, kwic
            h4("KWIC"),
            textInput("keyword", "Keyword in Context:", value = "", width = NULL,
                      placeholder = NULL),
            tableOutput("kwic")
            
        ),
        
        ## Main Panel: Overall Meeting 
        mainPanel(
            
            h2("Meeting Overview"),
            
            ## Tabs for each meeting
            tabsetPanel(type = "tabs",
                        tabPanel("Home",
                                 h4("Welcome!"),
                                 p("This webapp analyzes transcripts from CACN meetings, Jan 20 - Mar 9. In that time, over 100,000 words have been spoken in committee. On the sidebar, you can see an MPs most used words, topics, and find individual comments by searching for a keyword."),
                                 p("On the main panel, you can investigate who spoke the most in committee, what topics were mentioned most frequently, and the words that were most 'important' to the meeting theme, using a technique called term-frequency inverse document frequency. "),
                                 p("Switch through the tabs to see different meetings, and click the checkbox in the top left if you'd like witness statements included in the visualizations."),
                                 p("Future updates: Sentiment analyisis of topics, expanded topic dictionaries, + who knows?"),
                                 img(src = "flags.png")
                                ),
                        tabPanel("Meeting 1",
                                
                                
                                 conditionalPanel(
                                     condition = "input.witness == 1",
                                 
                                 h4("Words Spoken by MP/Witness"),
                                 ## First Plot is Bar Graph of Words
                                 plotOutput("tplot1"),
                                 
                                 h4("Topics mentioned in Meeting"),
                                 ## Second Plotly is Topics!
                                 plotOutput("toplot1")),
                                 
                                 conditionalPanel(
                                     condition = "input.witness == 0",
                                     
                                     ## First Plot is Bar Graph of Words
                                     h4("Words Spoken by MP"),
                                     plotOutput("tplot1mp"),
                                     h4("Topics mentioned in Meeting"),
                                     plotOutput("toplot1mp")),
                                 
                                 ## Last plot is TF-IDF
                                 
                                 h4("Most important terms"),
                                 plotOutput("tfplot1"),
                                     
                                 ),
                        
                        tabPanel("Meeting 3", 

                                 conditionalPanel(
                                     condition = "input.witness == 1",
                                     
                                     h4("Words Spoken by MP/Witness"),
                                     ## First Plot is Bar Graph of Words
                                     plotOutput("tplot3"),
                                     
                                     h4("Topics mentioned in Meeting"),
                                     ## Second Plotly is Topics!
                                     plotOutput("toplot3")),
                                 
                                 conditionalPanel(
                                     condition = "input.witness == 0",
                                     
                                     ## First Plot is Bar Graph of Words
                                     h4("Words Spoken by MP"),
                                     plotOutput("tplot3mp"),
                                     h4("Topics mentioned in Meeting"),
                                     plotOutput("toplot3mp")),
                                 
                                 ## Last plot is TF-IDF
                                 
                                 h4("Most important terms"),
                                 plotOutput("tfplot3"),
                                 
                        ),
        

                        tabPanel("Meeting 4",
                                 conditionalPanel(
                                     condition = "input.witness == 1",
                                     
                                     h4("Words Spoken by MP/Witness"),
                                     ## First Plot is Bar Graph of Words
                                     plotOutput("tplot4"),
                                     
                                     h4("Topics mentioned in Meeting"),
                                     ## Second Plotly is Topics!
                                     plotOutput("toplot4")),
                                 
                                 conditionalPanel(
                                     condition = "input.witness == 0",
                                     
                                     ## First Plot is Bar Graph of Words
                                     h4("Words Spoken by MP"),
                                     plotOutput("tplot4mp"),
                                     h4("Topics mentioned in Meeting"),
                                     plotOutput("toplot4mp")),
                                 
                                 ## Last plot is TF-IDF
                                 
                                 h4("Most important terms"),
                                 plotOutput("tfplot4"),
                                 
                        ),
                        
                        tabPanel("Meeting 5", 
                                 conditionalPanel(
                                     condition = "input.witness == 1",
                                     
                                     h4("Words Spoken by MP/Witness"),
                                     ## First Plot is Bar Graph of Words
                                     plotOutput("tplot5"),
                                     
                                     h4("Topics mentioned in Meeting"),
                                     ## Second Plotly is Topics!
                                     plotOutput("toplot5")),
                                 
                                 conditionalPanel(
                                     condition = "input.witness == 0",
                                     
                                     ## First Plot is Bar Graph of Words
                                     h4("Words Spoken by MP"),
                                     plotOutput("tplot5mp"),
                                     h4("Topics mentioned in Meeting"),
                                     plotOutput("toplot5mp")),
                                 
                                 ## Last plot is TF-IDF
                                 
                                 h4("Most important terms"),
                                 plotOutput("tfplot5"),
                                 
                        ),
    
                        tabPanel("Meeting 7", 
                                 conditionalPanel(
                                     condition = "input.witness == 1",
                                     
                                     h4("Words Spoken by MP/Witness"),
                                     ## First Plot is Bar Graph of Words
                                     plotOutput("tplot7"),
                                     
                                     h4("Topics mentioned in Meeting"),
                                     ## Second Plotly is Topics!
                                     plotOutput("toplot7")),
                                 
                                 conditionalPanel(
                                     condition = "input.witness == 0",
                                     
                                     ## First Plot is Bar Graph of Words
                                     h4("Words Spoken by MP"),
                                     plotOutput("tplot7mp"),
                                     h4("Topics mentioned in Meeting"),
                                     plotOutput("toplot7mp")),
                                 
                                 ## Last plot is TF-IDF
                                 
                                 h4("Most important terms"),
                                 plotOutput("tfplot7"),
                                 
                        ),
                        tabPanel("Meeting 8", 
                                 conditionalPanel(
                                     condition = "input.witness == 1",
                                     
                                     h4("Words Spoken by MP/Witness"),
                                     ## First Plot is Bar Graph of Words
                                     plotOutput("tplot8"),
                                     
                                     h4("Topics mentioned in Meeting"),
                                     ## Second Plotly is Topics!
                                     plotOutput("toplot8")),
                                 
                                 conditionalPanel(
                                     condition = "input.witness == 0",
                                     
                                     ## First Plot is Bar Graph of Words
                                     h4("Words Spoken by MP"),
                                     plotOutput("tplot8mp"),
                                     h4("Topics mentioned in Meeting"),
                                     plotOutput("toplot8mp")),
                                 
                                 ## Last plot is TF-IDF
                                 
                                 h4("Most important terms"),
                                 plotOutput("tfplot8"),
                                 
                        ),
                        tabPanel("LDA Topic Modelling",
                                 HTML(
                                     paste(
                                         p("This tab displayes Latent Dirichlet Allocation from Meetings 1-8. These were the words LDA determined most clearly identified as the 'topic' of the meeting.")
                                     )
                                 ),
                                 plotOutput("ldaplot"))
                        
            ),
            
        )
    )
)