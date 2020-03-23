function(input, output, session) {
  
  d <- reactive({
    MP_words <- tidy_words %>%
      filter(name == input$slider) %>%
      count(word) %>%
      filter(str_length(word) > 4) %>%
      filter(!word %in% common_words) %>%
      arrange(desc(n)) %>%
      head()
    return(MP_words)
  })
  
  output$trPlot <- renderPlot({
    tr_per <- c(round(tr$negative/tr$emote,3), round(tr$positive/tr$emote,3))
    labs <- c(paste("Negative", tr_per[1],"%"), paste("Positive", tr_per[2], "%"))
    pie(c(tr$negative, tr$positive), labels = labs, main = "Analysis of Meeting", col = c("red","green"))
  })
  output$mpPlot_tot <- renderPlot({
    pie(c(dat$n, tot), labels = c(input$slider, "Everyone else"), main = "How much MP spoke", col = c("blue","yellow"))
  })
  output$mpPlot_emote <- renderPlot({
    dat <- mp %>%
      filter(name == input$slider)
    pie(c(dat$emote, emote), labels = c(input$slider, "Everyone Else"), main = "How many 'sentimental' words MP spoke", col = c("red","green"))
  })
  output$mpPlot_sent <- renderPlot({
    dat <- mp %>%
      filter(name == input$slider)
    pie(c(dat$negative, dat$positive), labels = c("Negative", "Positive"), main = "Sentiments of MPs language", col = c("red","green"))
  })
  output$mpTable <- renderTable({
    d()
  })
}
