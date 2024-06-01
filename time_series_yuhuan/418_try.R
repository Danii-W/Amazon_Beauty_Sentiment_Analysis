library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(plotly)
library(tidyverse)
library(syuzhet)

# Define the UI
ui <- dashboardPage(
  skin = "midnight",
  header = dashboardHeader(title = "Amazon Beauty Reviews"),
  sidebar = dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Time Series", tabName = "timeSeries", icon = icon("line-chart")),
                menuItem("Polarity", tabName = "polarity", icon = icon("dashboard")),
                menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud")),
                conditionalPanel(
                  condition = "input.tabs === 'wordcloud'",
                  selectInput("year", "Year:", choices = 2008:2010),
                  selectInput("category", "Category:", choices = c("User Type", "Word Cloud Type"))
                )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "timeSeries",
              fluidRow(
                box(title = "Sentiment Over Time", width = 12, 
                    plotOutput("sentimentPlot"),
                    selectInput("sentimentType", "Choose Sentiment Analysis:",
                                choices = c("General" = "general",
                                            "Anger" = "anger",
                                            "Anticipation" = "anticipation",
                                            "Disgust" = "disgust",
                                            "Fear" = "fear",
                                            "Joy" = "joy",
                                            "Sadness" = "sadness",
                                            "Surprise" = "surprise",
                                            "Trust" = "trust"),
                                selected = "general")
                )
              )
      ),
      tabItem(tabName = "polarity",
              fluidRow(
                box(width = 12,
                    sliderInput("numReviews", "Number of Reviews:", min = 1, max = 2000, value = 1000),
                    plotlyOutput("polarityPlot")
                )
              )
      ),
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(title = "Word Cloud", width = 12,
                    imageOutput("wordCloudImage"),
                    actionButton("toggleImage", "Toggle View"),
                    uiOutput("missingImageMessage")  # Proper placement of the message UI
                )
              )
      )
    )
  ),
  title = "Amazon Beauty Reviews"
)

# Define server logic
server <- function(input, output, session) {
  # Load and preprocess the data
  df <- read_csv("review_data.csv")
  df_tibble <- as_tibble(df)
  nrc_lexicon <- get_sentiments("nrc")
  
  # Reactive data for sentiment and emotion analysis
  sentiment_data <- reactive({
    selected_sentiment <- if (input$sentimentType == "general") {
      c("positive", "negative")
    } else {
      input$sentimentType
    }
    
    df_tibble %>%
      unnest_tokens(word, filtered_reviewText) %>%
      inner_join(nrc_lexicon %>% filter(sentiment %in% selected_sentiment), by = "word") %>%
      count(year, sentiment) %>%
      group_by(year, sentiment) %>%
      summarize(count = sum(n), .groups = 'drop')
  })
  
  # Plot for sentiment and emotions
  output$sentimentPlot <- renderPlot({
    data <- sentiment_data()
    ggplot(data, aes(x = year, y = count, color = sentiment, group = sentiment)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Sentiment Over Time:", input$sentimentType),
           x = "Year", y = "Count",
           subtitle = paste("Analysis of", input$sentimentType, "Sentiment")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16))
  })
  
  output$polarityPlot <- renderPlotly({
    data <- read.csv("processed_review_data.csv")
    filtered_data <- data %>%
      slice(1:input$numReviews)
    
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
  # Define reactive value for toggling, initializing with the first option (e.g., "verified" or "positive")
  imageType <- reactiveVal("verified")
  
  # Toggle button to switch images
  observeEvent(input$toggleImage, {
    # Toggle between verified/unverified for User Type, or positive/negative for Word Cloud Type
    if (input$category == "User Type") {
      if (imageType() == "verified") {
        imageType("unverified")
      } else {
        imageType("verified")
      }
    } else if (input$category == "Word Cloud Type") {
      if (imageType() == "positive") {
        imageType("negative")
      } else {
        imageType("positive")
      }
    }
  })
  
  # Image rendering based on user input
  output$wordCloudImage <- renderImage({
    # Build the image path based on current selections
    img_path <- paste0("www/wordcloud_", input$year, "_", imageType(), ".png")
    
    # Check if the image exists
    if (file.exists(img_path)) {
      list(src = img_path,
           contentType = 'image/png',
           alt = "This is a word cloud image.")
    } else {
      # Return a placeholder or message if the image does not exist
      list(src = "www/image_not_available.png",
           contentType = 'image/png',
           alt = "Image not available.")
    }
  }, deleteFile = FALSE)
  
}

# Run the application
shinyApp(ui, server)
