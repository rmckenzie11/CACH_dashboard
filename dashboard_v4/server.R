## Load in libraries
library(shiny)
library(tidyverse)
library(stringr)
library(gridExtra)
library(grid)
library(ggplotify)
library(DT)
library(corpus)
library(quanteda)
library(treemapify)

##Animation
anim1 <- readRDS("data/anim1.rds")

## tf-idf
tf <- readRDS("data/tf_idf_plots.rds")


## Load in topic dictionaries
topic <- readRDS("data/topics_by_meeting.rds")

## Load in topic dictionaries (just mp)
topic_plots_mp <- readRDS("data/topics_by_meeting_mp.rds")

## Load in mp top topics
mp_topic <- readRDS("data/topics_by_mp.rds")

## Load in tidy words by meeting
tidy_words <- readRDS("data/tidy_words.RDS")

## Load in sentiment plot by meeting
sent_plot_meetings <- readRDS("data/sent_plot_meetings.rds")

## Load in sentiment analysis of all comments
sent_all_comments <- readRDS("data/sent_all_comments.rds")

## Load in word count plots by meeting
tplot <- readRDS("data/talk_plot_meetings.rds")

## Load in word count plots (just mp)
tplot_mp <- readRDS("data/talk_plots_mp.rds")

## Load in Topic Modelling
lda <- readRDS("data/LDA.rds")

## Load in Tokenized Comments
toke_comm <- readRDS("data/tokenized_comments.rds")

## Define server logic 
shinyServer(function(input, output) {
    
    ## MP Topic Table
    output$mpTop <- renderPlot({
        df <- mp_topic[input$select]
        df <- data.frame(table(unlist(df))) %>%
            arrange(desc(Freq))
        names(df) <- c("Topic", "Freq")
        
        df <- df %>%
            mutate(fill = case_when(Topic == "trade" ~ "#606c38",
                                Topic == "health" ~ "#768A5F",
                                Topic == "rights" ~ "#DA9587",
                                Topic == "free press" ~ "#dda15e",
                                Topic == "consular" ~ "#bc6c25",
                                Topic == "Taiwan" ~ "#5e6472",
                                Topic == "Hong Kong" ~ "#9a8c98",
                                TRUE ~ "#a8dadc"))
        
        ggplot(df, aes(area = Freq, fill = fill, label = toupper(Topic))) +
            geom_treemap() +
            geom_treemap_text(fontface = "italic", place = "centre") +
            scale_fill_identity() 
    })

## MP Word Count Table
    output$mpTable <- renderDT({
        
        check <- input$select

        df <- toke_comm %>%
            filter(name == check,
                   str_length(word) > 5) %>%
            drop_na() %>%
            group_by(word) %>%
            tally() %>%
            arrange(desc(n)) 
        df
    })
    
    
## MP KWIC
    output$kwic <- renderTable({
        
        check <- input$select
        
        df <- sent_all_comments %>%
            filter(name == check) 
        temp <- as.data.frame(kwic(df$text, input$keyword, window = 1000))
        temp$meeting <- df$column_label[as.integer(gsub("[a-z]", "",temp$docname))]
        temp$comment <- gsub('.{1}$', '', paste(temp$pre, temp$keyword, temp$post))
        temp$comment <- gsub("\\\\ n", "", temp$comment)
        temp %>%
            select(meeting, comment) %>%
            distinct()
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
    
    ## Meeting Word Count Plots - just MPs
    output$tplot1mp <- renderPlot({
        tplot_mp[[1]]
    })
    output$tplot3mp <- renderPlot({
        tplot_mp[[2]]
    })
    output$tplot4mp <- renderPlot({
        tplot_mp[[3]]
    })
    output$tplot5mp <- renderPlot({
        tplot_mp[[4]]
    })
    output$tplot7mp <- renderPlot({
        tplot_mp[[5]]
    })
    output$tplot8mp <- renderPlot({
        tplot_mp[[6]]
    })
    
## Meeting TOpic Count Plots
    output$toplot1 <- renderPlot({
        topic[[1]]
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
        topic[[5]]
    })
    output$toplot8 <- renderPlot({
        topic[[6]]
    })
    
## Meeting TOpic Count Plots (just MP)
    output$toplot1mp <- renderPlot({
        topic_plots_mp[[1]]
    })
    output$toplot3mp <- renderPlot({
        topic_plots_mp[[2]]
    })
    output$toplot4mp <- renderPlot({
        topic_plots_mp[[3]]
    })
    output$toplot5mp <- renderPlot({
        topic_plots_mp[[4]]
    })
    output$toplot7mp <- renderPlot({
        topic_plots_mp[[5]]
    })
    output$toplot8mp <- renderPlot({
        topic_plots_mp[[6]]
    })
## Meeting TF_IDF  Plots
    output$tfplot1 <- renderPlot({
        tf[[1]]
    })
    output$tfplot3 <- renderPlot({
        tf[[2]]
    })
    output$tfplot4 <- renderPlot({
        tf[[3]]
    })
    output$tfplot5 <- renderPlot({
        tf[[4]]
    })
    output$tfplot7 <- renderPlot({
        tf[[5]]
    })
    output$tfplot8 <- renderPlot({
        tf[[6]]
    })
    
    output$ldaplot <- renderPlot({
        lda +
            labs(title = "LD Term Allocation of Meetings 1-8")
    })
})
