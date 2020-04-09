## Load in libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)

mp <- readRDS("sent_MP.rds") %>%
    mutate(emote = positive + negative) %>%
    filter(n > 30)
tidy_words <- readRDS("tidy_words.RDS")
sent_plot_meetings <- readRDS("sent_plot_meetings.RDS")

ui <- fluidPage(
    theme = shinytheme("cosmo"),
    # Application title
    titlePanel("Sentiment Analysis of CACN Meeting 1"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("MP Focus"),
            selectInput("slider", "MP:", 
                        choices = mp$name, selected = "Mr. Richard Oliphant"),
            hr(),
            helpText("Data from ourcommons.ca."),
            plotOutput("mpPlot_tot"),
            plotOutput("mpPlot_sent"),
            h6("Most frequent words used by MP"),
            tableOutput("mpTable")
        ),
        mainPanel(
            h3("Meeting Overview"),
            tabsetPanel(type = "tabs",
                        tabPanel("Meeting 1", 
                                 HTML(
                                     paste(
                                         p("There were 13,473 words spoken during the 1st meeting of the Special Committee on Canada-China Relations. This is an analysis conducted on the transcript of that meeting using the SentimentR framework, which evaluates the tone of a sentence as either positive or negative.")
                                     )
                                 ),
                                 plotOutput("plot1")),
                        tabPanel("Meeting 3", 
                                 HTML(
                                    paste(
                                        p("There were 15,807 words spoken during the 3rd meeting of the Special Committee on Canada-China Relations. This is an analysis conducted on the transcript of that meeting using the SentimentR framework, which evaluates the tone of a sentence as either positive or negative.")
                                    )
                                ),
                                plotOutput("plot3")),
                        tabPanel("Meeting 4",
                                 HTML(
                                    paste(
                                        p("There were 16,297 words spoken during the 4th meeting of the Special Committee on Canada-China Relations. This is an analysis conducted on the transcript of that meeting using the SentimentR framework, which evaluates the tone of a sentence as either positive or negative.")
                                    )
                                ),
                                plotOutput("plot4")),
                        tabPanel("Meeting 5", 
                                 HTML(
                                     paste(
                                         p("There were 16,128 words spoken during the 5th meeting of the Special Committee on Canada-China Relations. This is an analysis conducted on the transcript of that meeting using the SentimentR framework, which evaluates the tone of a sentence as either positive or negative.")
                                     )
                                 ),
                                 plotOutput("plot5")),
                        tabPanel("Meeting 7", 
                                 HTML(
                                     paste(
                                         p("There were 21,110 words spoken during the 7th meeting of the Special Committee on Canada-China Relations. This is an analysis conducted on the transcript of that meeting using the SentimentR framework, which evaluates the tone of a sentence as either positive or negative.")
                                     )
                                 ),
                                 plotOutput("plot7")),
                        tabPanel("Meeting 8", 
                                 HTML(
                                     paste(
                                         p("There were 22,309 words spoken during the 8th meeting of the Special Committee on Canada-China Relations. This is an analysis conducted on the transcript of that meeting using the SentimentR framework, which evaluates the tone of a sentence as either positive or negative.")
                                     )
                                 ),
                                 plotOutput("plot8"))
            )
        )
    )
)