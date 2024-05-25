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
  skin = "blue",
  dashboardHeader(title = tagList(
    span(class = "logo-lg", "Amazon Beauty Reviews"),
    icon("leaf")
  )),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Welcome", tabName = "welcome", icon = icon("home")),
                menuItem("Time Series", tabName = "timeSeries", icon = icon("line-chart")),
                menuItem("Polarity", tabName = "polarity", icon = icon("dashboard")),
                menuItem("Word Cloud - Users", tabName = "wordcloudUsers", icon = icon("cloud")),
                menuItem("Word Cloud - Reviews", tabName = "wordcloudReviews", icon = icon("cloud")),
                menuItem("Dataset", tabName = "dataset", icon = icon("database")),
                menuItem("Team", tabName = "team", icon = icon("users")),
                conditionalPanel(
                  condition = "input.tabs === 'wordcloudUsers'",
                  selectInput("yearUsers", "Year:", choices = 2008:2018)
                ),
                conditionalPanel(
                  condition = "input.tabs === 'wordcloudReviews'",
                  selectInput("yearReviews", "Year:", choices = 2008:2018)
                ),
                conditionalPanel(
                  condition = "input.tabs === 'timeSeries' || input.tabs === 'polarity'",
                  div(style = "margin-top: 20px;")
                )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
        .sidebar-mini.sidebar-collapse .shiny-input-container {
          display: block;
          width: 100%;
        }
        .sidebar-mini.sidebar-collapse .selectize-input {
          width: 100%;
        }
      ")),
      tags$script(HTML('
        $(document).on("shiny:inputchanged", function(event) {
          if (event.name === "toggleExplanation") {
            var wordCloudBox = $("#wordCloudBox");
            var explanationBox = $("#explanationBox");
            if (event.value) {
              wordCloudBox.hide();
              explanationBox.show();
            } else {
              wordCloudBox.show();
              explanationBox.hide();
            }
          }
        });
      '))
    ),
    tabItems(
      tabItem(tabName = "welcome",
              fluidRow(
                box(
                  title = "Welcome to Amazon Beauty Reviews Dashboard", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  includeMarkdown("welcome.md")
                )
              ),
              fluidRow(
                box(
                  title = "Interesting Findings", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  # Add some example content here, such as text, images, or charts
                  p("Here you can add some interesting findings or highlights from the analysis."),
                  img(src = "interesting_finding.png", height = "300px")
                )
              )
      ),
      tabItem(tabName = "timeSeries",
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
                  collapsible = TRUE,
                  collapsed = TRUE,
                  id = "timeSeriesExplanationBox",
                  textOutput("timeSeriesExplanationText")
                )
              )
      ),
      tabItem(tabName = "polarity",
              fluidRow(
                box(
                  title = "Polarity",
                  width = 12,
                  status = "primary", 
                  solidHeader = TRUE,
                  sliderInput("numReviews", "Number of Reviews:", min = 1, max = 2000, value = 1000),
                  plotlyOutput("polarityPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Explanation",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  id = "polarityExplanationBox",
                  textOutput("polarityExplanationText")
                )
              )
      ),
      tabItem(tabName = "wordcloudUsers",
              fluidRow(
                box(
                  title = "Word Cloud - Users", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  height = "700px",  # Adjust the height to make the box larger
                  id = "wordCloudBoxUsers",  # Add an ID for toggling
                  imageOutput("wordCloudImageUsers", height = "600px"),  # Ensure the image respects the height
                  div(class = "wordcloud-toggle", actionButton("toggleImageUsers", "Toggle View")),
                  uiOutput("missingImageMessageUsers")
                )
              ),
              fluidRow(
                box(
                  title = "Explanation",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  id = "explanationBoxUsers",  # Add an ID for toggling
                  textOutput("explanationTextUsers")
                )
              )
      ),
      tabItem(tabName = "wordcloudReviews",
              fluidRow(
                box(
                  title = "Word Cloud - Reviews", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  height = "700px",  # Adjust the height to make the box larger
                  id = "wordCloudBoxReviews",  # Add an ID for toggling
                  imageOutput("wordCloudImageReviews", height = "600px"),  # Ensure the image respects the height
                  div(class = "wordcloud-toggle", actionButton("toggleImageReviews", "Toggle View")),
                  uiOutput("missingImageMessageReviews")
                )
              ),
              fluidRow(
                box(
                  title = "Explanation",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  id = "explanationBoxReviews",  # Add an ID for toggling
                  textOutput("explanationTextReviews")
                )
              )
      ),
      tabItem(tabName = "dataset",
              fluidRow(
                box(
                  title = "About the Dataset", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  includeMarkdown("dataset.md"),
                  downloadButton("downloadData", "Download Dataset")
                )
              )
      ),
      tabItem(tabName = "team",
              fluidRow(
                box(
                  title = "About the Team", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  includeMarkdown("team.md")
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
  output$sentimentPlot <- renderPlotly({
    data <- sentiment_data()
    p <- ggplot(data, aes(x = year, y = count, color = sentiment, group = sentiment)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(title = paste("Sentiment Over Time:", input$sentimentType),
           x = "Year", y = "Count",
           subtitle = paste("Analysis of", input$sentimentType, "Sentiment")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(face = "bold"))
    ggplotly(p)
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
  
  # Define reactive values for toggling images
  imageTypeUsers <- reactiveVal("verified")
  imageTypeReviews <- reactiveVal("positive")
  
  # Toggle button to switch images for Users
  observeEvent(input$toggleImageUsers, {
    if (imageTypeUsers() == "verified") {
      imageTypeUsers("unverified")
    } else {
      imageTypeUsers("verified")
    }
  })
  
  # Toggle button to switch images for Reviews
  observeEvent(input$toggleImageReviews, {
    if (imageTypeReviews() == "positive") {
      imageTypeReviews("negative")
    } else {
      imageTypeReviews("positive")
    }
  })
  
  # Image rendering based on user input for Users
  output$wordCloudImageUsers <- renderImage({
    img_path <- paste0("www/wordcloud_", input$yearUsers, "_", imageTypeUsers(), ".png")
    
    # Check if the image exists
    if (file.exists(img_path)) {
      list(src = img_path,
           contentType = 'image/png',
           alt = "This is a word cloud image.",
           width = "100%",   # Set explicit width
           height = "600px")  # Set explicit height
    } else {
      # Return a placeholder or message if the image does not exist
      list(src = "www/image_not_available.png",
           contentType = 'image/png',
           alt = "Image not available.",
           width = "100%",   # Set explicit width
           height = "600px")  # Set explicit height
    }
  }, deleteFile = FALSE)
  
  # Image rendering based on user input for Reviews
  output$wordCloudImageReviews <- renderImage({
    img_path <- paste0("www/wordcloud_", input$yearReviews, "_", imageTypeReviews(), ".png")
    
    # Check if the image exists
    if (file.exists(img_path)) {
      list(src = img_path,
           contentType = 'image/png',
           alt = "This is a word cloud image.",
           width = "100%",   # Set explicit width
           height = "600px")  # Set explicit height
    } else {
      # Return a placeholder or message if the image does not exist
      list(src = "www/image_not_available.png",
           contentType = 'image/png',
           alt = "Image not available.",
           width = "100%",   # Set explicit width
           height = "600px")  # Set explicit height
    }
  }, deleteFile = FALSE)
  
  # Explanation text for wordcloud Users
  output$explanationTextUsers <- renderText({
    "This word cloud represents the most common words found in the reviews for the selected year and user type. The size of each word indicates its frequency in the reviews. Larger words appear more frequently in the text, giving a visual representation of key themes and sentiments expressed by customers."
  })
  
  # Explanation text for wordcloud Reviews
  output$explanationTextReviews <- renderText({
    "This word cloud represents the most common words found in the reviews for the selected year and review type. The size of each word indicates its frequency in the reviews. Larger words appear more frequently in the text, giving a visual representation of key themes and sentiments expressed by customers."
  })
  
  # Explanation text for time series
  output$timeSeriesExplanationText <- renderText({
    "This time series chart shows the sentiment over time for the selected sentiment type. It allows you to observe trends and changes in sentiment across different years."
  })
  
  # Explanation text for polarity
  output$polarityExplanationText <- renderText({
    "This chart displays the sentiment polarity for Amazon reviews. It helps visualize the positive and negative sentiments expressed in the reviews over time."
  })
  
  # Toggle explanation visibility
  observeEvent(input$toggleExplanation, {
    updateActionButton(session, "toggleExplanation", 
                       label = ifelse(input$toggleExplanation %% 2 == 0, "Show Explanation", "Hide Explanation"))
    session$sendCustomMessage("toggleExplanation", input$toggleExplanation %% 2 == 1)
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
