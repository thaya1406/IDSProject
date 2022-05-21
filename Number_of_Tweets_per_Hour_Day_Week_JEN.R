# 1.0 LIBRARIES ----

##TEXT
library(tidytext)
library(textdata)

##Visualization
library(plotly)
library(ggwordcloud)

##Text
library(tidyverse)
library(tidyquant)


# 1.0 ACCOUNT SETUP --------------
library(rtweet)
token <- create_token(
  app = "immigration_trend", 
  consumer_key = "Hgj9nhsN2FPhruxxpwhttnBOS", 
  consumer_secret = "IQXwprbhJYzBCqhEIpkJLcuPSPQaYTMWadj3BMg3nWrcnBIpwd",
  access_token = "1068608387334766593-BpB8hUTe09InPeeGrAqZF9Rk2SEomb",
  access_secret = "phFEUzi8xfvjaS7XKi5i8VNjXzoDUh326mcJ6SczheBH6"
)

# 2.0 SEARCH TWEETS --------------


rt = search_tweets(
  input$text,                ##search query
  n = 180000,             ##Number of results
  include_rts = FALSE,   ## Dont include retweets if want unique tweets
  geocode = "3.14032,101.69466,93.5mi"
)                  

#SAVING THE TWEETS
saveRDS(rt, "Data/raw.rds")
tweeets = readRDS("Data/raw.rds")



# 2.0 TIDY TEXT ----
tweetsForSentiment = readRDS("Data/raw.rds")

##Tidy Data
tweets_tokenized <- tweetsForSentiment %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word,text)

# Counting frequency of words
tweets_tokenized %>% count(word,sort=TRUE) 

# 3.0 SENTIMENT ANALYSIS ----

##  3.1 Sentiment Dictionaries
get_sentiments(lexicon = "bing")  # Categorical Positive & Negative
get_sentiments(lexicon = "afinn") # Assigns polarity

##  3.2 Joining Sentiment Dict with Tokenized Text
sentiment_bing <- tweets_tokenized %>% inner_join(get_sentiments("bing"))

##  3.3 Measuring Sentiment

### Overall Sentiment
sentiment_bing %>% count(sentiment)

### Sentiment by user
sentiment_by_row_id <- sentiment_bing %>%
  select(-word) %>% 
  count(rowid, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill =list(n=0)) %>%
  mutate(sentiment= positive-negative)%>%
  left_join(
    tweetsForSentiment %>% select(screen_name, text) %>% rowid_to_column()
  ) 

##### FROM MY UNDERSTANDING YOU WONT HAVE TO DEAL WITH THE SENTIMENT , JUST THE RAW DATA WHICH IS THE TWEETS , BUT IM STILL PUTTING IT ABOVE INCASE YOU WOULD LIKE TO EXPLORE  ####

# -------- YOUR CODE STARTS HERE ---------
library(ggplot2)

ts_plot(tweeets, by = "hours", ) +
  labs(x = NULL, y = NULL,
       title = "Number of tweets per hour",
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()

ts_plot(tweeets, by ="days") +
  labs(x = NULL, y = NULL,
       title = "Number of tweets per day",
       subtitle = paste0(format(min(tweeets$created_at), "%d %B"), " to ", format(max(tweeets$created_at),"%d %B")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()

ts_plot(tweeets, by ="weeks") +
  labs(x = NULL, y = NULL,
       title = "Number of tweets per week",
       subtitle = paste0(format(min(tweeets$created_at), "%d %B"), " to ", format(max(tweeets$created_at),"%d %B")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()


TOPIC <- list(
  # Name of the conference or topic, for use in descriptive text
  name             = "najib",
  # Name of the full Twitter community, for use in descriptive text
  full_community   = "#najib",
  # Terms related to the topic that must be included in topical tweet text
  terms            = c("rstudioconf", "rstudio conf", "rstudio::conf", "rstudiconf", "rstduioconf"),
  # Hashtags to exclude from the Top 10 Hashtags list (because they're implied by the topic)
  hashtag_exclude  = "rstudio?conf|rstduioconf|rstats|rstudio conf",
  # Words to exclude from the Top 10 Words list (because they're implied by the topic)
  wordlist_exclude = "rstudio|conf|rstats"
)


ADMINLTE_COLORS <- list(
  "light-blue" = "#6699CC",
  "green"      = "#99C794",
  "red"        = "#EC5f67",
  "purple"     = "#C594C5",
  "aqua"       = "#a3c1e0",
  "yellow"     = "#FAC863",
  "navy"       = "#343D46",
  "olive"      = "#588b8b",
  "blue"       = "#4080bf",
  "orange"     = "#F99157",
  "teal"       = "#5FB3B3",
  "fuchsia"    = "#aa62aa",
  "lime"       = "#b0d4b0",
  "maroon"     = "#AB7967",
  "black"      = "#1B2B34",
  "gray-lte"   = "#D8DEE9",
  "primary"    = "#6699CC",
  "success"    = "#99C794",
  "danger"     = "#EC5f67",
  "info"       = "#a3c1e0",
  "warning"    = "#FAC863"
)




tweeets %>%
    tweets_just(created_at, is_topic) %>%
    group_by(is_topic) %>%
    tweets_volume() %>%
    mutate(topic = if_else(is_topic, "topic", "all")) %>%
    ungroup() %>%
    rename(Date = by_time) %>%
    select(-is_topic) %>%
    spread(topic, n, fill = 0) %>%
    plot_ly(x = ~ Date) %>%
    add_lines(y = ~topic, name = TOPIC$name, color = I(ADMINLTE_COLORS$teal)) %>%
    {
      if (!is.null(TOPIC$full_community)) {
        add_lines(., y = ~all, name = TOPIC$full_community, color = I(ADMINLTE_COLORS$purple))
      } else .
    }%>%
    config(displayModeBar = FALSE) %>%
    layout(
      xaxis = list(
        range = c(now(tz_global()) - days(7), now(tz_global())),
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "Today",
              step = "day",
              stepmode = "todate"),
            list(
              count = 1,
              label = "Yesterday",
              step = "day",
              stepmode = "backward"),
            list(
              count = 7,
              label = "Week",
              step = "day",
              stepmode = "backward"),
            list(step = "all", label = "All"))),
        rangeslider = list(type = "date")),
      yaxis = list(title = "Tweets"),
      legend = list(orientation = 'h', x = 0.05, y = 0.9),
      hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
    ) %>%
    config(collaborate = FALSE, cloud = FALSE, mathjax = NULL)

