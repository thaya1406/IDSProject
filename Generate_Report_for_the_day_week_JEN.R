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
  "najib",                ##search query
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
  mutate(sentiment= positive - negative)%>%
  left_join(
    tweetsForSentiment %>% select(created_at, screen_name, text) %>% rowid_to_column()
  )


###### AT THIS SENTIMENT HAVE BEEN IDENTIFIED AND ASSIGNED ALREDY. SO TO UNDERSTAND THE DATA BETTER PLEASE VIEW IT FIRST BEFORE YOU BEGIN TO WORK ON IT ####
##### ALL YOU HAVE TO DO IS BASED ON THE SENTIMENT IDENTIFIED DATA MANIPULATE IT FOR YOUR OUTPUT ########

# -------- YOUR CODE STARTS HERE ---------

library(rmarkdown)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(syuzhet)
library(ggplot2)
library(tidyr)
library(janeaustenr)
library(dplyr)
library(stringr)
library(texter)

dates <- as.Date(sentiment_by_row_id$created_at) #sort out dates from tweets
dates_cleaned <- dates[!duplicated(dates)] #remove duplicates
dates_sorted <- sort(dates_cleaned) #sort the dates in ascending order

sentiment_dates_only <- as.Date(sentiment_by_row_id$created_at)
totalsentiment <- integer(length(dates_sorted))

resultString = rep.int("", length(dates_sorted)) 
day_sentiment <- 1:length(dates_sorted)

for(i in 1:length(sentiment_by_row_id$text)) # loop over tweets
{
  for (j in 1:length(dates_sorted)) # loop over different dates
  {
    if (sentiment_dates_only[i] == dates_sorted[j]) # concatenate to resultString if dates match
    {
      resultString[j] = paste(resultString[j], sentiment_by_row_id$text[i]) #combine tweets in the same day together
      totalsentiment[j] = totalsentiment[j] + sentiment_by_row_id$sentiment[i] # calculate the total sentiment for the day
      if (totalsentiment[j]>0){
        day_sentiment[j] = "positive"
      } else if (totalsentiment[j]<0) {
        day_sentiment[j] = "negative"
      } else{
        day_sentiment[j] = "neutral"
      }
    }
  }
}


result = data.frame(date=dates_sorted, tweetsByDate=resultString, sentimentmark=totalsentiment, sentiment= as.character(day_sentiment)) #data of tweets with total sentiments for the day

for(i in 1:length(result$date)){
  if(dates_sorted[i] == Sys.Date())
    cat(paste("The overall sentiment is ", result$sentiment[i] ," for today"))
}

tweet_text <- result$tweetsByDate[result$date=="2022-05-24"]
gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweet_text)
docs <- Corpus(VectorSource(tweet_text))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, random.color = TRUE,
          scale=c(2,.5), colors=brewer.pal(12, "Paired"))

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# It also counts the number of positive and negative emotions found in each row
today_sentiment_text <-get_nrc_sentiment(tweet_text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (today_sentiment_text,10)

#transpose
td<-data.frame(t(today_sentiment_text))
#Transformation and cleaning
names(td)[1] <- "count"
td <- cbind("sentiment" = rownames(td), td)
rownames(td) <- NULL
td_new<-td[1:8,]
#Plot One - count of words associated with each sentiment
plotting <- quickplot(sentiment, data=td_new, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Sentiments Analysis")+coord_flip()

top_words_list <- strsplit(tweet_text, split = " ")
top_words <- as.data.frame(top_words_list)
colnames(top_words) <- c("word")

bing_word_counts <- top_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() 

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)