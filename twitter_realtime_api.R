library(tmaptools)
library(leaflet)
library(tidyverse)
library(twitteR)
library(base64enc)
library(tm)
library(rsconnect)   
library(base64enc)
library(shiny)
library(rtweet)
library(reactable)
library(scales)
library(reshape2)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(shinydashboard)
library(twitteR)
library(ROAuth)
library(plotly)
library(glue)
library(twitteR)
library(rvest)
library(wordcloud2)
library(textdata)


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

#geocode = "usa"

                       
saveRDS(rt, "Data/raw.rds")
tweeets = readRDS("Data/raw.rds")


#3.0 RESULTS --------------

tweeets  %>% glimpse()


#4.0 STREAMING TWEETS (REALTIME) --------------
##HOW IT WORKS ? - run for five seconds 
##tweeets <- stream_tweets(timeout = 5)
##tweeets  %>% glimpse()



#5.0 GEOCODING (REALTIME) --------------

##Geocoding Coordinates
###lookup_coords("usa")
##tweeets <- stream_tweets(lookup_coords("usa"),timeout = 5)

lookup_coords("uk")

#6.0 MAP

?leafet()

tweeets %>%
          select(screen_name, text, coords_coords) %>%
          unnest_wider (coords_coords) %>%
          filter(!is.na(...1)) %>%
          set_names(c("screen_name", "text", "lon", "lat")) %>%
          leaflet()   %>%
          addTiles()   %>%
          addMarkers(~lon, ~lat, popup = ~as.character(text), label = ~as.character(screen_name))


