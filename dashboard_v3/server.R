## Load in libraries
library(shiny)
library(tidyverse)
library(stringr)
library(gridExtra)
library(grid)
library(ggplotify)
library(DT)

##Animation
anim1 <- readRDS("anim1.rds")

## Load in topic dictionaries
topic <- readRDS("topics_by_meeting.rds")

mp_topic <- readRDS("topics_by_mp.rds")

## Load in tidy words by meeting
tidy_words <- readRDS("tidy_words.RDS")

## Load in sentiment plot by meeting
sent_plot_meetings <- readRDS("sent_plot_meetings.rds")

## Load in sentiment analysis of all comments
sent_all_comments <- readRDS("sent_all_comments.rds")

## Load in word count plots by meeting
tplot <- readRDS("talk_plot_meetings.rds")

## Load in Topic Modelling
lda <- readRDS("LDA.rds")

## Define server logic 
shinyServer(function(input, output) {
    
## MP Word Count Table
    output$mpTable <- renderDT({
        tidy_words <- bind_rows(tidy_words, .id = "meeting") %>%
            filter(name == input$select,
                   str_length(word) > 5,
                   !word %in% c("committee","subcommittee","meeting","members","chair","opposition", "government", "motion", "canada","canadians","important","minutes","briefings","parties","committees")) %>%
            group_by(word) %>%
            tally() %>%
            arrange(desc(n))
        
        tidy_words
    })
    
## MP Topic Table
    output$mpTop <- renderDT({
        df <- mp_topic[input$select]
        
        df <- data.frame(table(unlist(df))) %>%
            arrange(desc(Freq))
        names(df) <- c("Topic", "Freq")
        
        df
    })

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

## Meeting Word Count Plots
    output$tplot1 <- renderPlot({
        tplot[[1]]
    })
    output$tplot3 <- renderPlot({
        tplot[[2]]
    })
    output$tplot4 <- renderPlot({
        tplot[[3]]
    })
    output$tplot5 <- renderPlot({
        tplot[[4]]
    })
    output$tplot7 <- renderPlot({
        tplot[[5]]
    })
    output$tplot8 <- renderPlot({
        tplot[[6]]
    })
    
## Meeting TOpic Count Plots
    output$toplot1 <- renderPlotly({
        ggplotly(topic[[1]], tooltip = "count")
    })
    output$toplot3 <- renderPlot({
        topic[[2]]
    })
    output$toplot4 <- renderPlot({
        topic[[3]]
    })
    output$toplot5 <- renderPlot({
        topic[[4]]
    })
    output$toplot7 <- renderPlot({
        topi[[5]]
    })
    output$toplot8 <- renderPlot({
        topic[[6]]
    })
    
    
    output$ldaplot <- renderPlot({
        lda +
            labs(title = "LD Term Allocation of Meetings 1-8")
    })
})
