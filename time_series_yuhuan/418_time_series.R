


df <- read.csv("review_data.csv")
# Check the data
head(df)


library(tidytext)
library(dplyr)
library(tibble)

# Assuming your dataframe is named df
# Convert data frame to tibble for better handling in tidytext
df_tibble <- as_tibble(df)


# Load NRC lexicon
nrc_lexicon <- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative"))

# Perform sentiment analysis with NRC
sentiment_data <- df_tibble %>%
  unnest_tokens(word, filtered_reviewText) %>%
  inner_join(nrc_lexicon) %>%
  group_by(reviewerID, productID, year) %>%
  summarize(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative"),
    .groups = 'drop'
  )


print(sentiment_data)



library(ggplot2)
library(tidyr)
library(ggthemes)  # for additional themes

# Convert data from wide to long format for easier plotting
long_sentiment_data <- sentiment_data %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "sentiment_type", values_to = "count")


# Enhanced plotting with jittering and alpha transparency
ggplot(long_sentiment_data, aes(x = year, y = count, color = sentiment_type, group = sentiment_type)) +
  geom_line(linewidth = 1.2) +  # Keep lines thicker
  geom_point(aes(shape = sentiment_type), size = 3, position = position_jitter(width = 0.2, height = 0), alpha = 0.6) +  # Jitter points horizontally
  scale_shape_manual(values = c(16, 17)) +  # Manual shapes for different sentiment types
  labs(title = "Sentiment Over Time by Type",
       subtitle = "Analysis of Positive vs Negative Sentiments",
       x = "Year",
       y = "Sentiment Count",
       caption = "Data source: https://nijianmo.github.io/amazon/") +
  scale_color_manual(values = c("positive" = "dodgerblue", "negative" = "firebrick1")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "italic"),
        plot.caption = element_text(color = "gray40")) 
 


product_sentiment <- sentiment_data %>%
  filter(productID == "B000FI4S1E")

# Convert data from wide to long format
long_product_sentiment <- product_sentiment %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "sentiment_type", values_to = "count")

# Enhanced plotting with jittering and alpha transparency
ggplot(long_product_sentiment, aes(x = year, y = count, color = sentiment_type, group = sentiment_type)) +
  geom_line(linewidth = 1.2) +  # Updated from size to linewidth
  geom_point(aes(shape = sentiment_type), size = 3, position = position_jitter(width = 0.2, height = 0), alpha = 0.6) +  # Jitter points horizontally
  scale_shape_manual(values = c(16, 17)) +  # Manual shapes for different sentiment types
  labs(title = "Sentiment Over Time for Product B000FI4S1E",
       subtitle = "Analysis of Positive vs Negative Sentiments",
       x = "Year",
       y = "Sentiment Count",
       caption = "Data source: https://nijianmo.github.io/amazon/") +
  scale_color_manual(values = c("positive" = "dodgerblue", "negative" = "firebrick1")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "italic"),
        plot.caption = element_text(color = "gray40"))









# Plotting
#ggplot(long_sentiment_data, aes(x = year, y = count, color = sentiment_type, group = sentiment_type)) +
#geom_line() +
#geom_point() +
#labs(title = "Sentiment Over Time by Type",
#x = "Year",
#y = "Sentiment Count") +
#theme_minimal() +
#scale_color_manual(values = c("positive" = "blue", "negative" = "red"))

