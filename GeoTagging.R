# 1.0 LIBRARIES ----
library(tm)
library(rsconnect)   
library(base64enc)
library(shiny)
library(rtweet)
library(reactable)
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




#GeoTagging
library(tidyverse)
library(sf)
library(leaflet)
library(tigris)
library(rtweet)
library(tidytext)





# 2.0 TIDY TEXT ----


tweetsForGeoTagging = search_tweets(
  "najib",                ##search query
  n = 18000,             ##Number of results
  include_rts = FALSE,   ## Dont include retweets if want unique tweets
  geocode = lookup_coords("Malaysia")
)  

saveRDS(rt, "Data/raw.rds")
data = readRDS("Data/raw.rds")


tweetsForGeoTagging %>% 
  filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
  count(place_full_name, sort = TRUE) %>% 
  slice(1:10)


tweetsForGeoTagging %>%
  count(place_full_name, sort = TRUE) %>%
  mutate(location = reorder(place_full_name,n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Place",
       y = "Count",
       title = "Unique locations ")


##Extracting Tweet Geographic Coordinates

tweetsForGeoTagging <- lat_lng(tweetsForGeoTagging)

tweetsForGeoTagging.geo <- tweetsForGeoTagging %>%
  filter(is.na(lat) == FALSE & is.na(lng) == FALSE)


#Mapping Tweets
tweetsForGeoTagging.geo.sf <- st_as_sf(tweetsForGeoTagging.geo, coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircles(data = tweetsForGeoTagging.geo.sf, color = "red",radius =10)

