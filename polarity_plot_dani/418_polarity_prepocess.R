library(dplyr)
library(tidytext)
library(syuzhet)
library(readr)

##This is preprocessing for Polarity Plot
# Load data
data <- read.csv("review_data.csv")

# Process sentiment scores
data <- data %>%
  mutate(sentiment_scores = get_nrc_sentiment(as.character(filtered_reviewText)))

# If the structure is a list of data frames where each data frame has multiple sentiment columns:
data <- data %>%
  unnest(sentiment_scores)

# Now calculate polarity
data <- data %>%
  mutate(polarity = positive - negative,
         index = row_number())

# Save to CSV
write_csv(data, "processed_review_data.csv")
