

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

# 2.0 TIDY TEXT ----


rt = search_tweets(
  "vaksin",                ##search query
  n = 180000,             ##Number of results
  include_rts = FALSE,   ## Dont include retweets if want unique tweets
  geocode = "3.14032,101.69466,93.5mi"
)  
saveRDS(rt, "Data/raw.rds")

tweetsForSentiment = readRDS("Data/raw.rds")


print(dim(tweetsForSentiment))
##Tidy Data
tweets_tokenized <- tweetsForSentiment %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word,text)

tweets_tokenized %>% count(word,sort=TRUE) # Counting frequency of words

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


  
# 4.0 POLARITY VISUALIZATION ----
  
label_wrap <- label_wrap_gen(width = 60)

data_formatted <- sentiment_by_row_id %>%
        mutate(text_formatted= str_glue("Row ID: {rowid}
                                         Screen Name: (screen_name)
                                         Text:
                                         {label_wrap(text)}"))
g <- data_formatted %>%
  ggplot(aes(rowid, sentiment))+
  geom_line(color="#2c3e50", alpha=0.5)+
  geom_point(aes(text=text_formatted), color="#2c3e50")+
  geom_smooth(method="loess", span=0.25, se=FALSE, color="blue")+
  geom_hline(aes(yintercept=mean(sentiment)),color="blue")+
  geom_hline(aes(yintercept=median(sentiment)+ 1.96*IQR(sentiment)), color="red")+
  geom_hline(aes(yintercept=median(sentiment)-1.96*IQR(sentiment)), color="red") +
  theme_tq()+
  labs(title="Sentiment Polarity", x ="Twitter User", y="Sentiment")
                

ggplotly(g, tooltip="text") %>%
  layout(
      xaxis = list(
          rangeslider=list(type="date") 
      )
  )





  



#### top 10 ############
topwords <-  tweetsForSentiment[,1:16] %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = gsub("rt", "", text)) %>% 
  mutate(text = gsub("https","", text)) %>% 
  mutate(text = gsub("t.co", "", text)) %>% 
  mutate(text = gsub("covid", "", text)) %>% 
  mutate(text = removeNumbers(text)) %>% 
  mutate(text = gsub("19", "", text)) %>% 
  mutate(text = gsub("ppl", "people", text)) %>% 
  mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
  mutate(text = gsub("en", "", text)) %>% 
  mutate(rowmumber = row_number()) %>%#mutate row numbers
  mutate(text = str_remove(text, "rt")) %>% 
  unnest_tokens(word, text) %>%  #unnest words
  anti_join(stop_words) %>% #removes stop words
  count(word, sort = T) %>%#count most occuring words
  top_n(10) #select top 10

ggplot(topwords, aes(reorder(word, n), n, fill = word)) + #piped into ggplot
  geom_bar(stat = "identity", show.legend = F) + coord_flip() +
  labs(x = "Word", y = "count") + theme_minimal() +
  theme(axis.title.x = element_text(face ="bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text = element_text(face = "bold"))