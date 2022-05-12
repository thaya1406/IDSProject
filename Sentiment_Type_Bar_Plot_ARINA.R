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
  "Bonia",                ##search query
  n = 180000,             ##Number of results
  include_rts = FALSE,   ## Dont include retweets if want unique tweets
  geocode = "3.14032,101.69466,93.5mi"
)                  

#SAVING THE TWEETS
saveRDS(rt, "data/raw.rds")
tweeets = readRDS("data/raw.rds")



# 2.0 TIDY TEXT ----
tweetsForSentiment = readRDS("data/raw.rds")

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


###### AT THIS SENTIMENT HAVE BEEN IDENTIFIED AND ASSIGNED ALREDY. SO TO UNDERSTAND THE DATA BETTER PLEASE VIEW IT FIRST BEFORE YOU BEGIN TO WORK ON IT ####
##### ALL YOU HAVE TO DO IS BASED ON THE SENTIMENT IDENTIFIED DATA MANIPULATE IT FOR YOUR OUTPUT ########

# -------- YOUR CODE STARTS HERE ---------

## Table: Sentiment per tweet + retweet_count
sentiment_by_tweets <- sentiment_by_row_id %>%
  mutate(Sentiment = if_else(sentiment < 0, "Negative", 
                             if_else(sentiment == 0, "Neutral", 
                                     "Positive")) ) %>%
  left_join(
    tweetsForSentiment %>% select(retweet_count) %>% rowid_to_column()
  )%>%
  select(-rowid, -positive, -negative, -sentiment)


## Rename columns in sentiment_by_tweets
colnames(sentiment_by_tweets) <- c("Username", "Tweets", "Sentiment", "Retweets")

## Formatting table
sentiment_by_tweets <- sentiment_by_tweets %>%
  mutate(
    sentiment_box_color = dplyr::case_when(
      Sentiment == "Positive" ~ "lightgreen",
      Sentiment == "Negative" ~ 'tomato',
      Sentiment == "Neutral" ~ "skyblue",
      TRUE ~ 'grey')
  )%>%
  mutate(
    sentiment_text_color = dplyr::case_when(
      Sentiment == "Positive" ~ "darkgreen",
      Sentiment == "Negative" ~ 'darkred',
      Sentiment == "Neutral" ~ "#39568CFF",
      TRUE ~ 'grey')
  )

n_positive <- length(which(sentiment_by_tweets$Sentiment == "Positive")) 
n_neutral <- length(which(sentiment_by_tweets$Sentiment == "Neutral"))
n_negative <- length(which(sentiment_by_tweets$Sentiment == "Negative")) 
Count <- c(n_positive, n_neutral, n_negative)
Sentiment <- c("Positive","Neutral","Negative")
result <- data.frame(Sentiment, Count)
result$Sentiment <- factor(result$Sentiment, levels = Sentiment)

ggplot(result, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  scale_fill_manual("Sentiment", values = c("Positive" = "#06d6a0", "Neutral" = "#64b5f6", "Negative" = "#f38375"))


