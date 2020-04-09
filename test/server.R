#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        tr_per <- c(round(tr$negative/tr$emote,3), round(tr$positive/tr$emote,3))
        labs <- c(paste(tr_per[1],"%"), paste(tr_per[2], "%"))
        pie(c(tr$negative, tr$positive), labels = labs, main = "Analysis of Meeting", col = c("red","green"))

    })

})
