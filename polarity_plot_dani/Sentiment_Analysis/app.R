library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

library(tidyverse)
library(syuzhet)
library(ggplot2)
data <- read.csv("/Users/dani/MASDS/STATS 418/Final Project/dataset/output.csv")
data <- data %>%
  mutate(sentiment_scores = get_nrc_sentiment(as.character(filtered_reviewText)))
data <- data %>%
  mutate(polarity = sentiment_scores$positive - sentiment_scores$negative) %>%
  mutate(index = row_number())

# Define the header of the dashboard
header <- dashboardHeader(title = "Amazon Reviews Sentiment Analysis")

# Define the sidebar of the dashboard
sidebar <- dashboardSidebar(
  sliderInput("numReviews", 
              "Number of Reviews:",
              min = 1, 
              max = nrow(data),  # Assuming data is your dataset
              value = 1000)
)

# Define the body of the dashboard
body <- dashboardBody(
  plotlyOutput("sentimentPlot")
)

# Define UI using the dashboardPage layout
ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Define server logic
server <- function(input, output) {
  output$sentimentPlot <- renderPlotly({
    # Filter data based on slider input
    filtered_data <- data %>%
      slice(1:input$numReviews)
    
    # Generate the specified plot
    plot_ly() %>%
      add_trace(data = filtered_data, x = ~index, y = ~polarity, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#909090'), marker = list(color = '#2c3e50', size = 4),
                name = 'Data Points and Line',
                text = ~paste("Index:", index, "<br>Polarity:", polarity), hoverinfo = "text") %>%
      add_trace(data = filtered_data, x = ~index, y = ~fitted(loess(polarity ~ index)), type = 'scatter', mode = 'lines',
                line = list(color = '#FFCD14', width = 2),
                name = 'LOESS Smoothing Line') %>%
      layout(title = "Sentiment Polarity Interactive Plot",
             xaxis = list(title = "Amazon User Index"),
             yaxis = list(title = "Sentiment Polarity"),
             hovermode = "closest",
             plot_bgcolor = "#e5ecf6",
             legend = list(orientation = "h", x = 0.5, y = -0.3, xanchor = 'center'))
  })
}

# Run the dashboard application 
shinyApp(ui, server)
