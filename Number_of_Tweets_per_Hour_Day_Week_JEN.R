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



