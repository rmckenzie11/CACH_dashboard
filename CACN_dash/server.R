## Load in libraries
library(shiny)
library(tidyverse)
library(stringr)
library(gridExtra)
library(grid)
library(ggplotify)
library(kableExtra)

## Load in tidy words by meeting
tidy_words <- readRDS("tidy_words.RDS")

## Load in sentiment plot by meeting
sent_plot_meetings <- readRDS("sent_plot_meetings.RDS")

## Load in sentiment analysis of all comments
sent_all_comments <- readRDS("sent_all_comments.rds")

## Load in word count plots by meeting
tplot <- readRDS("talk_plot_meetings.rds")

## Define server logic 
shinyServer(function(input, output) {
    
## MP Sentiment Plots
    output$mpPlot_sent <- renderPlot({
        sent_all_comments <-sent_all_comments %>%
            filter(name == input$select)
        sent_all_comments <- split(sent_all_comments , f = sent_all_comments$column_label)
        
        dat <- list()
        mps <- list()
        
        for(i in 1:length(sent_all_comments)){
            dat[[i]] <- with(density(sent_all_comments[[i]]$ave_sentiment), data.frame(x, y))
            mps[[i]] <- ggplot(dat[[i]], aes(x = x, y = y)) +
            geom_line() +
            geom_area(mapping = aes(x = ifelse(x >=0 & x<=1 , x, 0)), fill = "green") +
            geom_area(mapping = aes(x = ifelse(x <=0 & x>=-1 , x, 0)), fill = "red") +
            scale_y_continuous(limits = c(0,7.5)) +
            theme_minimal() +
            labs(x = "Sentiment", 
                 y = "",
                 title = paste("Meeting ", case_when(i == 1 ~ 1,
                                                     i == 2 ~ 3,
                                                     i == 3 ~ 4,
                                                     i == 4 ~ 5,
                                                     i == 5 ~ 7,
                                                     i == 6 ~ 8)))
        }
        
        grid.arrange(grobs = mps,ncol = 2, newpage = F, top = textGrob(paste(input$select,"'s Sentiments"), gp=gpar(fontface="bold")))
    })
    
## MP Word Count Table
    output$mpTable <- renderTable({
        tidy_words <- bind_rows(tidy_words, .id = "meeting") %>%
            filter(name == "Mr. Dan Albas",
                   str_length(word) > 5,
                   !word %in% c("committee","subcommittee","meeting","members","chair","opposition", "government")) %>%
            group_by(word) %>%
            tally() %>%
            arrange(desc(n)) %>%
            head(15)
        
        kable(tidy_words)
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
})
