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
library(markdown)


# Define the UI
ui <- navbarPage(
  title = tagList(
    span(class = "logo-lg", "Amazon Beauty Reviews"),
    icon("leaf")
  ),
  theme = shinythemes::shinytheme("flatly"),
  
  tabPanel("Welcome",
           fluidRow(
             box(
               title = "Welcome to Amazon Beauty Reviews Dashboard", 
               status = "primary", 
               solidHeader = TRUE,
               width = 12,
               includeMarkdown("welcome.md")
             )
           ),
           div(style = "height: 200px;") # Add a spacer div at the bottom
  ),
  
  tabPanel("Time Series",
           fluidRow(
             box(
               title = "Sentiment Over Time", 
               status = "primary", 
               solidHeader = TRUE,
               width = 12, 
               height = "700px",
               plotlyOutput("sentimentPlot"),
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
           ),
           fluidRow(
             box(
               title = "Explanation",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               textOutput("timeSeriesExplanationText")
             )
           ),
           div(style = "height: 200px;") # Add a spacer div at the bottom
  ),
  
  tabPanel("Polarity",
           fluidRow(
             box(
               title = "Polarity",
               width = 12,
               status = "primary", 
               solidHeader = TRUE,
               sliderInput("numReviews", "Number of Reviews:", min = 1, max = 2000, value = 1000, width = '100%'),
               plotlyOutput("polarityPlot")
             )
           ),
           fluidRow(
             box(
               title = "Explanation",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               textOutput("polarityExplanationText")
             )
           ),
           div(style = "height: 200px;") # Add a spacer div at the bottom
  ),
  
  tabPanel("Word Cloud - User Type",
           fluidRow(
             box(
               title = "Word Cloud - User Type", 
               status = "primary", 
               solidHeader = TRUE,
               width = 12,
               height = "700px",
               selectInput("yearUsers", "Year:", choices = 2008:2018, width = '100%'),
               radioButtons("wordCloudTypeUsers", "Select Type:", choices = c("Verified", "Unverified"), selected = "Verified"),
               imageOutput("wordCloudImageUsers", height = "600px"),
               uiOutput("missingImageMessageUsers")
             )
           ),
           fluidRow(
             box(
               title = "Explanation",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               textOutput("explanationTextUsers")
             )
           ),
           div(style = "height: 200px;") # Add a spacer div at the bottom
  ),
  
  tabPanel("Word Cloud - Sentiment Polarity",
           fluidRow(
             box(
               title = "Word Cloud - Sentiment Polarity", 
               status = "primary", 
               solidHeader = TRUE,
               width = 12,
               height = "700px",
               selectInput("yearReviews", "Year:", choices = 2008:2018, width = '100%'),
               radioButtons("wordCloudTypeReviews", "Select Sentiment:", choices = c("Positive", "Negative"), selected = "Positive"),
               imageOutput("wordCloudImageReviews", height = "600px"),
               uiOutput("missingImageMessageReviews")
             )
           ),
           fluidRow(
             box(
               title = "Explanation",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               textOutput("explanationTextReviews")
             )
           ),
           div(style = "height: 200px;") # Add a spacer div at the bottom
  ),
  
  tabPanel("Dataset",
           fluidRow(
             box(
               title = "Download Dataset", 
               status = "primary", 
               solidHeader = TRUE,
               width = 12,
               downloadButton("downloadData", "Download Dataset")
             )
           ),
           fluidRow(
             box(
               title = "About the Dataset", 
               status = "primary", 
               solidHeader = TRUE,
               width = 12,
               includeMarkdown("dataset.md")
             )
           ),
           fluidRow(
             box(
               title = "View Dataset", 
               status = "primary", 
               solidHeader = TRUE,
               width = 12,
               selectInput("numRows", "Number of Rows to Display:", choices = c(5, 10, 15, 20, 25), selected = 5, width = '100%'),
               tableOutput("dataTable")
             )
           ),
           div(style = "height: 200px;") # Add a spacer div at the bottom
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load and preprocess the data
  df <- read_csv("review_data.csv")
  df_tibble <- as_tibble(df)
  nrc_lexicon <- read_csv("nrc_lexicon.csv")  # Load the lexicon from the local file
  
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
  output$sentimentPlot <- renderPlotly({
    data <- sentiment_data()
    p <- ggplot(data, aes(x = year, y = count, color = sentiment, group = sentiment, text = paste("Year:", year, "<br>Count:", count, "<br>Sentiment:", sentiment))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(title = paste("Sentiment Over Time:", input$sentimentType),
           x = "Year", y = "Count",
           subtitle = paste("Analysis of", input$sentimentType, "Sentiment")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "plain"),
            axis.title = element_text(face = "plain"))
    ggplotly(p, tooltip = "text")
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
  
  # Image rendering based on user input for Users
  output$wordCloudImageUsers <- renderImage({
    img_type <- ifelse(input$wordCloudTypeUsers == "Verified", "verified", "unverified")
    img_path <- paste0("www/wordcloud_", input$yearUsers, "_", img_type, ".png")
    
    # Check if the image exists
    if (file.exists(img_path)) {
      list(src = img_path,
           contentType = 'image/png',
           alt = "This is a word cloud image.",
           width = "100%",
           height = "650px")
    } else {
      # Return a placeholder or message if the image does not exist
      list(src = "www/image_not_available.png",
           contentType = 'image/png',
           alt = "Image not available.",
           width = "100%",
           height = "650px")
    }
  }, deleteFile = FALSE)
  
  # Image rendering based on user input for Reviews
  output$wordCloudImageReviews <- renderImage({
    img_type <- ifelse(input$wordCloudTypeReviews == "Positive", "positive", "negative")
    img_path <- paste0("www/wordcloud_", input$yearReviews, "_", img_type, ".png")
    
    # Check if the image exists
    if (file.exists(img_path)) {
      list(src = img_path,
           contentType = 'image/png',
           alt = "This is a word cloud image.",
           width = "100%",
           height = "650px")
    } else {
      # Return a placeholder or message if the image does not exist
      list(src = "www/image_not_available.png",
           contentType = 'image/png',
           alt = "Image not available.",
           width = "100%",
           height = "650px")
    }
  }, deleteFile = FALSE)
  
  # Explanation text for wordcloud Users
  output$explanationTextUsers <- renderText({
    "This word cloud represents the most common words found in the reviews for the selected year and user type. The size of each word indicates its frequency in the reviews. Larger words appear more frequently in the text, giving a visual representation of key themes and sentiments expressed by customers. For certain years, the plot for the selected year does not exist due to an unidentified reason."
  })
  
  # Explanation text for wordcloud Reviews
  output$explanationTextReviews <- renderText({
    "This word cloud represents the most common words found in the reviews for the selected year and review type. The size of each word indicates its frequency in the reviews. Larger words appear more frequently in the text, giving a visual representation of key themes and sentiments expressed by customers. For certain years, the plot for the selected year does not exist due to an abundance of non-polarizing reviews."
  })
  
  # Explanation text for time series
  output$timeSeriesExplanationText <- renderText({
    "This time series chart shows the sentiment over time for the selected sentiment type. 'Count' on the y-axis represents the number of reviews expressing the sentiment, while the x-axis represents the year. It allows you to observe trends and changes in sentiment across different years."
  })
  
  # Explanation text for polarity
  output$polarityExplanationText <- renderText({
    "This chart displays the sentiment polarity for Amazon reviews. 'Index' on the x-axis represents the position of each review in the dataset. 'Sentiment polarity' on the y-axis indicates the sentiment score, with positive values representing positive sentiment and negative values representing negative sentiment. Each point on the plot represents an individual review's sentiment score."
  })
  
  # Render the table based on the number of rows selected
  output$dataTable <- renderTable({
    head(df_tibble, n = as.numeric(input$numRows))
  })
  
  # Download handler for dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("review_data", ".csv", sep = "")
    },
    content = function(file) {
      file.copy("www/review_data.csv", file)
    }
  )
}

# Run the application
shinyApp(ui, server)
