## Load in libraries
library(shiny)
library(tidyverse)
library(stringr)

## Load in tidy words by meeting
tidy_words <- readRDS("tidy_words.RDS")

## Load in sentiment plot by meeting
sent_plot_meetings <- readRDS("sent_plot_meetings.RDS")

## Define server logic 
shinyServer(function(input, output) {

## Meeting Sentiment Plots
    output$plot1 <- renderPlot({
            sent_plot_meetings[[1]]
    })
    output$plot3 <- renderPlot({
        sent_plot_meetings[[2]]
    })
    output$plot4 <- renderPlot({
        sent_plot_meetings[[3]]
    })
    output$plot5 <- renderPlot({
        sent_plot_meetings[[4]]
    })
    output$plot7 <- renderPlot({
        sent_plot_meetings[[5]]
    })
    output$plot8 <- renderPlot({
        sent_plot_meetings[[6]]
    })

})
