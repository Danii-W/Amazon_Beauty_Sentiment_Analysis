# Amazon Beauty Sentiment Analysis Dashboard

## Description
This project provides an interactive dashboard for analyzing Amazon beauty product reviews. It allows users to explore various metrics such as sentiment over time, polarity, and word clouds based on user types and sentiment polarity. The goal is to help companies quickly understand customer sentiment and predict customer churn.

## Features
- **Welcome Page**: Introduction to the dashboard with key findings.
- **Time Series Analysis**: Sentiment over time for different emotions and sentiments.
- **Polarity Analysis**: Sentiment polarity plot for Amazon reviews.
- **Word Cloud - User Type**: Word clouds for verified and unverified user reviews.
- **Word Cloud - Sentiment Polarity**: Word clouds for positive and negative reviews.
- **Dataset**: Download and view the dataset used in the analysis.

## Dashboard Link
Access the dashboard [here](https://mas418final.shinyapps.io/418_shiny/).

## Installation Instructions
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/Amazon_Beauty_Sentiment_Analysis.git

2. Navigate to the 'my_shiny_app' directory:
   ```bash
   cd Amazon_Beauty_Sentiment_Analysis/my_shiny_app

3. Install the required R packages:
   ```bash
   install.packages(c("shiny", "shinydashboard", "shinydashboardPlus", "tidytext", "dplyr", "tidyr", "ggplot2", "readr", "plotly", "tidyverse", "syuzhet", "shinythemes", "markdown"))
   ```

## Usage
1. Run the Shiny app:
   ```bash
   library(shiny)
   runApp('418_shiny.R')
   
2. Access the dashboard in your web browser.

## File Structure

```bash
Amazon_Beauty_Sentiment_Analysis/
├── data_cleaning/
├── dataset/
├── my_shiny_app/
│   ├── www/
│   │   ├── custom.css
│   │   ├── image_not_available.png
│   │   ├── review_data.csv
│   │   ├── wordcloud_*.png
│   ├── 418_shiny.R
│   ├── dataset.md
│   ├── nrc_lexicon.csv
│   ├── processed_review_data.csv
│   ├── review_data.csv
│   ├── welcome.md
│   └── rsconnect/
├── polarity_plot_dani/
├── time_series_yuhuan/
├── wordcloud_positive_negative_anum/
├── wordcloud_cindy/
└── README.md
```


##  Contributors
- **Yuhuan He**
- **Dan Wu**
- **Anum Damani**
- **Cindy Xu**


## Acknowledgments
- **TA Kaiwen Jiang** for layout and interactivity suggestions.
- **shinydashboardPlus Documentation** [shinydashboardPlus official website.](https://rinterface.github.io/shinydashboardPlus/index.html)



This `README.md` provides a comprehensive guide to understanding, installing, and using the project. You can replace placeholders such as `yourusername` with actual names as needed.



Note: This is a private student project. Please do not use it for any personal or commercial purposes.





