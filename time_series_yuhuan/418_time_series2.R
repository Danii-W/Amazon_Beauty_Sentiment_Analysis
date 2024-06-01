


df <- read.csv("review_data.csv")
# Check the data
head(df)


library(tidytext)
library(dplyr)
library(tibble)


# Convert data frame to tibble for better handling in tidytext
df_tibble <- as_tibble(df)


# Load NRC lexicon for all emotional dimensions
nrc_lexicon <- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"))

# Perform sentiment analysis incorporating all emotions
df_tibble <- as_tibble(read.csv("review_data.csv"))  # assuming df is loaded and converted to tibble

# Tokenize and analyze sentiment based on NRC emotional dimensions
emotion_data <- df_tibble %>%
  unnest_tokens(word, filtered_reviewText) %>%
  inner_join(nrc_lexicon, by = c("word" = "word")) %>%
  group_by(reviewerID, productID, year) %>%
  count(sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0))  # Pivoting to wide format for each emotion

print(emotion_data)



library(ggplot2)

# Convert data from wide to long format for easier plotting
long_emotion_data <- emotion_data %>%
  pivot_longer(cols = c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"), 
               names_to = "emotion", values_to = "count")

# Plotting with facets for each emotion
ggplot(long_emotion_data, aes(x = year, y = count, color = emotion, group = emotion)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ emotion, scales = "free_y") +
  labs(title = "Emotional Sentiment Over Time by Type",
       x = "Year",
       y = "Count of Emotionally Coded Words",
       caption = "Data source: https://nijianmo.github.io/amazon/") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend if faceting

