
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sentiment Analysis of CACN Meeting 1"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4("MP Focus"),
                 selectInput("slider", "MP:", 
                             choices = mp$name, selected = "Mr. Richard Oliphant"),
                 hr(),
                 helpText("Data from ourcommons.ca."),
    plotOutput(outputId = "mpPlot_tot"),
    plotOutput(outputId = "mpPlot_emote"),
    plotOutput(outputId = "mpPlot_sent"),
    tableOutput(outputId = "mpTable")
    ),
    mainPanel(
      h3("Meeting Overview"),
              HTML(
                paste(
                  p("There were 13,309 words spoken during the first meeting of the Special Committee on Canada-China Relations. This is an analysis conducted on the transcript of that meeting using the Bing sentiment framework, which evaluates the tone of a word as either positive or negative.")
                )
              ),
              plotOutput(outputId = "trPlot")
              )
  )
)