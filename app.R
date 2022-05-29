library(tm)
library(rsconnect)   
library(base64enc)
library(shiny)
library(rtweet)
library(reactable)
library(reshape2)
library(RColorBrewer)
library(shinydashboard)
library(ROAuth)
library(plotly)
library(glue)
library(twitteR)
library(rvest)
library("openssl")
library("httpuv")

##TEXT
library(tidytext)
library(textdata)
library(tidyverse)
library(tidyquant)

##Visualization
library(plotly)
library(ggplot2)
library(plotly)
library(ggwordcloud)


#Tweet Table
library(reactablefmtr)
library(DT)


#GeoTagging
library(sf)
library(leaflet)
library(tigris)
library(rtweet)


#HIGHSCORE
library(forcats)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(dplyr)
library(shinycssloaders)

#Emoji
library("devtools")
#devtools :: install_github("hadley/emo", force = TRUE)

#Documentation
library(knitr)
library(markdown)
library(rmarkdown)
library(shinyjs)

#GenerateReport
library(wordcloud)
library(SnowballC)
library(syuzhet)
library(janeaustenr)

# Google Map Query
Sys.setenv(GOOGLE_MAPS_KEY= "AIzaSyCZbE_A1CI2PlHD2KZdu15qVTU7U5wXujI")

# Run.txt
progressGroup = function(text, value, min = 0, max = value, color = "aqua") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    progressBar(round(value / max * 100), color = color, size = "sm")
  )
}

progressBar_v = function(x, colors) {
  if (length(colors) > length(x)) {
    colors <- rep(colors, ceiling(length(x)/length(colors)))
  }
  x <- purrr::map2(x, colors[seq_along(x)], ~ progressBar(.x, color = .y))
  map_chr(x, paste)
}

progressBar = function(
    value = 0,
    label = FALSE,
    color = "aqua",
    size = NULL,
    striped = FALSE,
    active = FALSE,
    vertical = FALSE
) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  # if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
  #   stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "2em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}

#Emoji function
twemoji = function(runes, width = "20px") {
  runes <- tolower(runes)
  runes <- gsub(" ", "-", runes)
  runes <- sub("-fe0f$", "", runes) # seems to cause problems with twemoji :shrug:
  emojis <- glue::glue("https://cdnjs.cloudflare.com/ajax/libs/twemoji/11.2.0/2/svg/{runes}.svg")
  emojis <- glue::glue('<img src="{emojis}" width = "{width}">')
  paste(emojis)
}

# Rmd file
rmdfiles <- c("documentation.Rmd")
sapply(rmdfiles, knit, quiet = T)
    
# Files needed
source("IDSProject/functions.R") ## Make sure the working directory is the same as this file
source("IDSProject/queen.R") ## Make sure the working directory is the same as this file

#### --- 0.0 UI ----
ui <- dashboardPage(
  dashboardHeader(title = h2(HTML("Brand Analyzer"),          
                             style = "font-weight:bold"), 
                  titleWidth = 250,
                  disable = FALSE),
  
  dashboardSidebar(
    sidebarPanel(
      id = "sidebar",
##This one is the number of tweets to be downloaded
      h5(style="color:#006d77", sliderInput("Tweets_to_Download",
                                            "No of Tweets to Download:",
                                            min = 500,
                                            max = 18000,
                                            value = 500,
                                            step = 500)),

##This one is for the user to key in the value
      fluidPage(style="color:#006d77",
                textInput("caption", "Key-in your word or Hashtags", "najib"),
                verbatimTextOutput("value")), 
      
      fluidPage(style="color:#006d77",
                selectInput("state", "Location",
                            list(`North America` = list("United States of America", "Canada", "Mexico", "Greenland", "Cuba"),
                                 `South America` = list("Brazil", "Argentina", "Colombia", "Peru", "Chile", "Venezuela"),
                                 `Europe` = list("United Kingdom", "Spain", "Germany", "Italy", "France", "Netherlands"),
                                 `Asia` = list("Malaysia", "Singapore", "China", "Japan", "South Korea", "Indonesia", "Afghanistan"),
                                 `Africa` = list("South Africa", "Nigeria", "Kenya", "Ghana", "Morocco"),
                                 `Oceania` = list("New Zealand", "Australia", "Fiji", "Papua New Guinea"))
                ),
                textOutput("result")
      ),
      width = 250)
  ), 


  

  dashboardBody(
    useShinyjs(),
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML("

        .skin-blue .main-header .logo {
          background-color: #c9e4de;
          color:#006d77;
        }
        
        .skin-blue .main-header .logo:hover {
          background-color: #c9e4de;
        }
        
        .skin-blue .main-header .navbar {
          background-color: #c9e4de;
        }
        
        /* toggle button when hovered  */
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
        background-color: #0081a7;
        }
        
        .main-sidebar {
            background-color: #c9e4de !important;
        }
        
        #sidebar{
            background-color: #c9e4de;
        }
        
        #tabset{
          color: #0081a7;
        }
        
        .tabbable > .nav > li > a {
          color:#0081a7;
        }
          
        .content-wrapper, .right-side {
          background-color: #c9e4de;
        }
        
        .box.box-solid.box-primary>.box-header {
          background-color: #006d77;
        }

        .box.box-solid.box-primary{
          background:#edf6f9
        }
  
        
        "))),
    
    tabsetPanel(
      id = "tabset",
      tabPanel(title = "Sentiment Analysis",
               fluidRow(
                 valueBoxOutput("value1"),
                 valueBoxOutput("value2"),
                 valueBoxOutput("value3")),            
               fluidRow(  
                 box(
                   title = "WordCloud"
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                   ,withSpinner(plotOutput("wordcloud", height = "300px"))
                 ),
                 box(
                   title = "Summary of Sentiments"
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,withSpinner(plotOutput("sentimenttype", height = "300px"))
                 )),
               fluidRow(
                 box(
                   title = "Top Positive and Negative Words",
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   withSpinner(plotOutput("bing", height = "300px"))
                 ),
                 box(
                   title = "Types of Sentiments Found",
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   withSpinner(plotOutput("NRC", height = "300px"))
                 )),
               fluidRow(
                 box(
                   title = "Sentiment Polarity",
                   status = "primary",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   withSpinner(plotlyOutput("polarity", height = "300px"))
                   
                 )),
           
      ),
      tabPanel(title = "Overview of Tweets",
               fluidRow(  
                 box(
                   title = "Tweets on Map"
                   ,width = 12
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                   ,withSpinner(leafletOutput("geoTaggedTweets"))

                 )),
               fluidRow(  
                 box(
                   title = "Table of Tweets"
                   ,helpText("Tips : Click the column header to sort a column.")
                   ,width = 12
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE
                   ,withSpinner(reactableOutput("tweettable"))
                 )),
      ),
      tabPanel(title = "Number of tweets",
               fluidRow(
                 box(
                   title = "Number of tweets per hour",
                   width = 12,
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   withSpinner(plotOutput("No_of_tweets_hour", height = "300px"))
                 )),
               fluidRow(
                 box(
                   title = "Number of tweet per day",
                   width = 12,
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   withSpinner(plotOutput("No_of_tweets_day", height = "300px"))
                 )),
               fluidRow(
                 box(
                   title = "Number of tweets per week",
                   width = 12,
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   withSpinner(plotOutput("No_of_tweets_week", height = "300px"))
                 )),
               
      ),
      
      tabPanel(
        "High Scores",
        fluidRow(
          box(
            width = "6 col-lg-3",
            status = "info",
            title = "Top Tweeters",
            tags$div(
              class = "scroll-overflow-x",
              withSpinner(uiOutput("top_tweeters"))
            )
          ),
          box(
            width = "6 col-lg-3",
            status = "danger",
            title = "Top Hashtags",
            withSpinner(uiOutput("top_hashtags"))
          ),
          box(
            width = "6 col-lg-3",
            status = "warning",
            title = "Top Words",
            withSpinner(uiOutput("top_tweet_words"))
          ),
          box(
            width = "6 col-lg-3",
            status = "success",
            title = "Top Emoji",
            withSpinner(uiOutput("top_emojis"))
          )
        )
        ),
      tabPanel(title = "Report of the day",
               fluidRow(
                 valueBoxOutput("today_sentiment_value",width = 12)
               ),
               fluidRow(  
                 box(
                   title = "WordCloud of the Day"
                   ,status = "primary"
                   ,solidHeader = FALSE 
                   ,collapsible = TRUE
                   ,withSpinner(plotOutput("day_wordcloud", height = "300px"))
                 ),
                 box(
                   title = "Sentiments Found Today"
                   ,status = "warning"
                   ,solidHeader = FALSE 
                   ,collapsible = TRUE 
                   ,withSpinner(plotOutput("day_sentimenttype", height = "300px"))
                 )),
               fluidRow(
                 box(
                   title = "Today's Top Positive and Negative Words"
                   ,status = "success"
                   ,solidHeader = FALSE
                   ,collapsible = TRUE
                   ,width = 12
                   ,withSpinner(plotOutput("Top_PosNeg_Today", height = "300px"))
                 )),
               
      ),
      tabPanel(
        "Tweet Wall",
        class = "text-center",
        tags$h1("Tweets about", TOPIC$name),
        # Tweet Wall - twitter.js and masonry.css - start --------------------
        # twitter.js has to be loaded after the page is loaded (divs exist and jquery is loaded)
        tags$head(HTML(
          '
        <script>
        document.addEventListener("DOMContentLoaded", function(event) {
          var script = document.createElement("script");
          script.type = "text/javascript";
          script.src  = "twitter.js";
          document.getElementsByTagName("head")[0].appendChild(script);
        });
        </script>
        ')),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "masonry.css")),
        # Tweet Wall - twitter.js and masonry.css - end ----------------------
        fluidRow(
          column(
            # Tweet Wall - Controls - start -------------------------------------------
            12,
            class = "col-md-8 col-md-offset-2 col-lg-6 col-lg-offset-3",
            tags$form(
              class = "form-inline",
              tags$div(
                class = "form-group",
                tags$div(
                  class = "btn-toolbar btn-group-sm",
                  dateRangeInput("tweet_wall_daterange", "",
                                 start = today(tz_global()), end = today(tz_global()),
                                 min = "2019-01-01", max = today(tz_global()),
                                 weekstart = 1, separator = " to "),
                  shinyThings::dropdownButtonUI("tweet_wall_date_presets",
                                                TWEET_WALL_DATE_INPUTS,
                                                class = "btn-default")
                )
              )
            )
            # Tweet Wall - Controls - end ---------------------------------------------
          ),
          shinyThings::paginationUI("tweet_wall_pager", width = 12, offset = 0)
        ),
        withSpinner(uiOutput("tweet_wall_tweets"), type = 3),
        shinyThings::pagerUI("tweet_wall_pager", centered = TRUE),
        tabItem(
          "tab_pic_tweets",
          class = "text-center",
          tags$h1(HTML("Tweets with", twemoji("1F5BC", width = "42px"))),
          shinyThings::paginationUI("pic_tweets", width = 12, offset = 0),
          withSpinner(uiOutput("pic_tweets_wall"), type = 3),
          shinyThings::pagerUI("pic_tweets", centered = TRUE)
        )
      ),
      
      tabPanel(title = "About App",
               fluidPage(
                 style="padding-top: 40px;",
                 downloadButton("download", "Download documentation"),
                 withMathJax(includeMarkdown("documentation.md"))
               )
      )#tabPanel
    )#tabsetPanel
  ),#dashboardBody
)#dashboardPage

# Define server logic 
server <- function(input, output,session) {
  #### --- 1.0 SET UP TWITTER ----
  
  # Hide sidebar when input$tabset == "About App"
  { observe({
    if (input$tabset == "About App") {
      hide(selector = "body > div.wrapper > header > nav > div:nth-child(4) > ul")
      addClass(selector = "body", class = "sidebar-collapse")
      removeClass(selector = "body", class = "control-sidebar-open") 
    }
    else {
      show(selector = "body > div.wrapper > header > nav > div:nth-child(4) > ul")
      removeClass(selector = "body", class = "sidebar-collapse")
    }
  })}
  
  # Define Twitter keys
  consumer_key = "Hgj9nhsN2FPhruxxpwhttnBOS"
  consumer_secret = "IQXwprbhJYzBCqhEIpkJLcuPSPQaYTMWadj3BMg3nWrcnBIpwd"
  access_token = "1068608387334766593-BpB8hUTe09InPeeGrAqZF9Rk2SEomb"
  access_secret = "phFEUzi8xfvjaS7XKi5i8VNjXzoDUh326mcJ6SczheBH6"
  
  # Set up Twiter Authentication 
  setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
  
  
  
  #### --- 2.0 DEFINE REACTIVE FUNCTIONS TO GET DATA ----
  ## Reactive function get raw tweets 
   tweets_r <- reactive({
    
    rt = search_tweets(
      input$caption,                #search query
      n = input$Tweets_to_Download, #Number of results
      include_rts = TRUE,           #Dont include retweets if want unique tweets
    #  geocode = "3.14032,101.69466,93.5mi"
     geocode = lookup_coords(input$state)
    )    
    saveRDS(rt, "Data/raw.rds")
    tweeets = readRDS("Data/raw.rds")
    return(tweeets)
    
  })
   
   ## Reactive function get formatted tweets for sentiment analysis
   dataformatted_r <- reactive({
    
    rt = search_tweets(
      input$caption,                  #search query
      n = input$Tweets_to_Download,   #Number of results
      include_rts = TRUE,            #Dont include retweets if want unique tweets
     # geocode = "3.14032,101.69466,93.5mi",
      geocode = lookup_coords(input$state)
    )    
    saveRDS(rt, "Data/raw.rds")
    tweeets = readRDS("Data/raw.rds")
    
    
    
    label_wrap <- label_wrap_gen(width = 60)
    ##Tidy Data
    tweets_tokenized <- tweeets %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word,text)
    
    tweets_tokenized %>% count(word,sort=TRUE) # Counting frequency of words
    
    ##2.1 SENTIMENT ANALYSIS 
    
    #Sentiment Dictionaries
    get_sentiments(lexicon = "bing")  # Categorical Positive & Negative
    get_sentiments(lexicon = "afinn") # Assigns polarity
    
    # Joining Sentiment Dict with Tokenized Text
    sentiment_bing <- tweets_tokenized %>% inner_join(get_sentiments("bing"))
    
    # Overall Sentiment
    sentiment_bing %>% count(sentiment)
    
    # Sentiment by user
    sentiment_by_row_id <- sentiment_bing %>%
      select(-word) %>% 
      count(rowid, sentiment) %>% 
      pivot_wider(names_from = sentiment, values_from = n, values_fill =list(n=0)) %>%
      mutate(sentiment= positive-negative)%>%
      left_join(
        tweeets %>% select(screen_name, text) %>% rowid_to_column()
      )
    
    # XXXXXXXXXXX
    data_formattedd <- sentiment_by_row_id %>%
      mutate(text_formatted= str_glue("Row ID: {rowid}
                                         Screen Name: (screen_name)
                                         Text:
                                         {label_wrap(text)}"))
    return(data_formattedd)
  })
  
  
   
   
   
   
      ###### === TAB 1 : SENTIMENT ANALAYSIS === ######
  #### --- 3.0 BUILDING VALUE BOXES ---  #####
  ## Value 1 holds Number of Positive Tweets
  output$value1 <- renderValueBox({
    n <- tweets_r() %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      group_by(word, sentiment) %>% 
      count(word, sentiment, sort = T) %>% 
      ungroup() %>% 
      group_by(sentiment) %>% 
      summarise(n = sum(n)) %>% 
      mutate(n = round(n/sum(n), 2)) %>% 
      filter(sentiment == "positive")
    
    n <- n[,2]
    
    
    valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
             icon = icon("smile", lib ="font-awesome" ), color = "purple")
  })
   
   ## Value 2 holds Number of Negative Tweets
  output$value2 <- renderValueBox({
    n <-tweets_r()[,1:16] %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      group_by(word, sentiment) %>% 
      count(word, sentiment, sort = T) %>% 
      ungroup() %>% 
      group_by(sentiment) %>% 
      summarise(n = sum(n)) %>% 
      mutate(n = round(n/sum(n), 2)) %>% 
      filter(sentiment == "negative")
    
    n <- n[,2]
    valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
             icon = icon("angry", lib ="font-awesome" ), color = "maroon")
  })
  
  ## Value 3  holds Total Tweets obtained
  output$value3 <- renderValueBox({
    tweets_count <- tweets_r() %>% 
      nrow()
    valueBox(tweets_count, subtitle = "Total Tweets", icon = icon("chart-bar", lib ="font-awesome" ), color = "yellow")
  })
  
  
  
  #### --- 4.0 BUILDING SENTIMENT ANALYSIS CHARTS/GRAPHS  --- ####
  ## 4.1 WORDCLOUD ####
  output$wordcloud <- renderPlot({
    
    tweets_cLOUD <- tweets_r()

    tweets_tokenized <- tweets_cLOUD %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word,text)
    
    get_sentiments(lexicon = "bing")  # Categorical Positive & Negative
    get_sentiments(lexicon = "afinn") # Assigns polarity
    
    sentiment_bing <- tweets_tokenized %>% inner_join(get_sentiments("bing"))
    sentiment_bing %>% count(sentiment)
    
    tweets_tokenized <- tweets_cLOUD %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word,text)
    
    sentiment_bing <-  tweets_tokenized %>% inner_join(get_sentiments("bing"))
    sentiment_by_word <-  sentiment_bing %>% count(word, sentiment, sort=TRUE)
    sentiment_by_word %>%
      slice(1:100) %>%
      mutate(sentiment=factor(sentiment, levels=c("positive", "negative"))) %>%
      ggplot(aes(label=word, color=sentiment, size=n))+
      geom_text_wordcloud_area()+
      facet_wrap(~ sentiment, ncol=2)+
      theme_tq()+
      scale_color_tq(theme = "dark")+
      scale_size_area(max_size=16)+
      labs(tittle ="Sentiment Word Frequency")
  })


  ## 4.2 SUMMARY OF SENTIMENT TYPE ####
    output$sentimenttype <- renderPlot ({
    n_positive <- length(which(tweettable_r()$Sentiment == "Positive")) 
    n_neutral <- length(which(tweettable_r()$Sentiment == "Neutral"))
    n_negative <- length(which(tweettable_r()$Sentiment == "Negative")) 
    Count <- c(n_positive, n_neutral, n_negative)
    Sentiment <- c("Positive","Neutral","Negative")
    result <- data.frame(Sentiment, Count)
    result$Sentiment <- factor(result$Sentiment, levels = Sentiment)
    ggplot(result, aes(x=Sentiment,y=Count))+
      geom_bar(stat = "identity", aes(fill = Sentiment))+
      scale_fill_manual("Sentiment", values = c("Positive" = "#06d6a0", "Neutral" = "#64b5f6", "Negative" = "#f38375"))
    
  })
  
  ## 4.3 Top Positive and Negative Word ####
    output$bing <- renderPlot({
    pos_vs_neg <- tweets_r()[,1:16] %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      mutate(text = gsub("fidelity", " ", text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      group_by(word, sentiment) %>% 
      count(word, sentiment, sort = T) %>% 
      ungroup() %>% 
      group_by(sentiment) %>% 
      top_n(10)
    
    ggplot(pos_vs_neg, aes(reorder(word, n), n, fill = word)) +
      geom_col(show.legend = F) +
      facet_wrap(~sentiment, scales = "free_y") +
      coord_flip() + 
      labs(y = "Count", x = "Words") +
      theme_gray()
  })
  

  ## 4.4 TYPES OF SENTIMENT FOUND ####
    output$NRC <- renderPlot({
    
    nr <- read.csv("nrc.csv")  
    #nrc tweet analysis
    nrc <- tweets_r() %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(nr) %>% 
      group_by(sentiment) %>% 
      count(sentiment, sort = T)
    
    ggplot(nrc, aes(reorder(sentiment, n), n, fill = sentiment)) + 
      geom_bar(stat = "identity", show.legend = F) + coord_flip() +
      theme_minimal() + labs(x = "Sentiments", y = "Count") +
      theme(axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(face = "bold"))
  })
  
  
  
  ## 4.5 SENTIMENT POLARITY ####
      output$polarity <- renderPlotly({
    
    g <- dataformatted_r() %>%
      ggplot(aes(rowid, sentiment))+
      geom_line(color="#2c3e50", alpha=0.5)+
      geom_point(aes(text=text_formatted), color="#2c3e50")+
      geom_smooth(method="loess", span=0.25, se=FALSE, color="blue")+
      geom_hline(aes(yintercept=mean(sentiment)),color="blue")+
      geom_hline(aes(yintercept=median(sentiment)+ 1.96*IQR(sentiment)), color="red")+
      geom_hline(aes(yintercept=median(sentiment)-1.96*IQR(sentiment)), color="red") +
      theme_tq()+
      labs(x ="Twitter User", y="Sentiment")
    
    
    ggplotly(g, tooltip="text") %>%
      layout(
        xaxis = list(
          rangeslider=list(type="date") 
        )
      )
    
  })
  
  
  
  
    
    
  
  
  
      ###### === TAB 2 : OVERVIEW OF TWEETS === ######
  ## 5.0 DISPLAYING THE RAW TWEES####
  output$geoTaggedTweets <- renderLeaflet ({
    
    tweetsForGeoTagging <- tweets_r()
    
    tweetsForGeoTagging %>% 
      filter(is.na(place_full_name) == FALSE & place_full_name != "") %>% 
      count(place_full_name, sort = TRUE) %>% 
      slice(1:10)
    
    ##Extracting Tweet Geographic Coordinates
    
    tweetsForGeoTagging <- lat_lng(tweetsForGeoTagging)
    
    tweetsForGeoTagging.geo <- tweetsForGeoTagging %>%
      filter(is.na(lat) == FALSE & is.na(lng) == FALSE)
    
    
    #Mapping Tweets
    tweetsForGeoTagging.geo.sf <- st_as_sf(tweetsForGeoTagging.geo, coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")
    
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addCircles(data = tweetsForGeoTagging.geo.sf, color = "red",radius =10)
  })
  
  ## 5.1 BUILDING A MAP THAT SHOWS THE LOCATION OF TWEETS ####
  # Reactive function get modify the raw tweets for table display
  tweettable_r <- reactive({
    
    rt = search_tweets(
      input$caption,                ##search query
      n = input$Tweets_to_Download,             ##Number of results
      include_rts = FALSE,   ## Dont include retweets if want unique tweets
      geocode = lookup_coords(input$state)
    )    
    saveRDS(rt, "Data/raw.rds")
    tweeets = readRDS("Data/raw.rds")
    
    
    
    label_wrap <- label_wrap_gen(width = 60)
    ##Tidy Data
    tweets_tokenized <- tweeets %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word,text)
    
    tweets_tokenized %>% count(word,sort=TRUE) # Counting frequency of words
  
    get_sentiments(lexicon = "bing")  # Categorical Positive & Negative
    get_sentiments(lexicon = "afinn") # Assigns polarity
    
    sentiment_bing <- tweets_tokenized %>% inner_join(get_sentiments("bing"))

    sentiment_bing %>% count(sentiment)
    
    sentiment_by_row_id <- sentiment_bing %>%
      select(-word) %>% 
      count(rowid, sentiment) %>% 
      pivot_wider(names_from = sentiment, values_from = n, values_fill =list(n=0)) %>%
      mutate(sentiment= positive-negative)%>%
      left_join(
        tweeets %>% select(screen_name, text) %>% rowid_to_column()
      )
    
    # Start modification:
    # Table: Sentiment per tweet + retweet_count
    sentiment_by_tweets <- sentiment_by_row_id %>%
      mutate(Sentiment = if_else(sentiment < 0, "Negative", 
                                 if_else(sentiment == 0, "Neutral", 
                                         "Positive")) ) %>%
      left_join(
        tweets_r() %>% select(retweet_count) %>% rowid_to_column()
      )%>%
      select(-rowid, -positive, -negative, -sentiment)
    
    
    # Rename columns in sentiment_by_tweets
    colnames(sentiment_by_tweets) <- c("Username", "Tweets", "Sentiment", "Retweets")
    
    # Formatting table
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
    
    return(sentiment_by_tweets)
  })
  
  output$tweettable <- renderReactable({
    tweettable_r() %>%
      reactable(
        columns = list(
          sentiment_box_color = colDef(show = FALSE),
          sentiment_text_color = colDef(show = FALSE),
          Username = colDef(
            style = list(color = "grey", fontFamily = "Menlo")
          ),
          Sentiment = colDef(
            cell = color_tiles(., color_ref = "sentiment_box_color",text_color_ref = "sentiment_text_color", opacity = 0.5)
          )
        ),
        searchable = TRUE
      )
  })
  
  
  
  
  
      ###### === TAB 3 : NUMBER  OF TWEETS === ######
  #### --- 6.0 Time series Graphs  --- ####
  ## 6.1 Number of tweets per HOUR ####
  
  output$No_of_tweets_hour <- renderPlot({
    tweeets <- tweets_r()
    
    ts_plot(tweeets, by = "hours",xtime ="%F %H:%S") +
      labs(x = "Date", y = "Count",
           caption = "Data collected from Twitter's REST API via rtweet") +
      theme_gray()
  })
  
  ## 6.2 Number of tweets per DAY ####
  
  output$No_of_tweets_day <- renderPlot({
    tweeets <- tweets_r()
    
    ts_plot(tweeets, by ="days") +
      labs(x = "Date", y = "Count",
           subtitle = paste0(format(min(tweeets$created_at), "%d %B"), " to ", format(max(tweeets$created_at),"%d %B")),
           caption = "Data collected from Twitter's REST API via rtweet") +
      theme_gray()
  })
  
  ## 6.3 Number of tweets per WEEK ####
  output$No_of_tweets_week <- renderPlot({
    tweeets <- tweets_r()
    
    ts_plot(tweeets, by ="weeks") +
      labs(x = "Date", y = "Count",
           subtitle = paste0(format(min(tweeets$created_at), "%d %B"), " to ", format(max(tweeets$created_at),"%d %B")),
           caption = "Data collected from Twitter's REST API via rtweet") +
      theme_gray()
  })
  
      ###### === TAB 4 : HIGH SCORES === ######
  #### --- 7.0 DISPLAYING ALL HIGH SCORES OF THE TWEETS  --- ####
  # Color Helpers 
  BASIC_COLORS <- c("primary", "info", "success", "danger", "warning")
  
  ## 7.1 HIGH SCORES FOR ENGAGEMENT ####
  output$top_tweeters <- renderUI({
    tweets_r() %>%
      group_by(screen_name, profile_url, profile_image_url) %>%
      summarize(engagement = (sum(retweet_count) * 2 + sum(favorite_count)) / n()) %>%
      arrange(desc(engagement)) %>%
      ungroup() %>%
      slice(1:10) %>%
      mutate(
        engagement = scale(engagement, center = FALSE),
        engagement = engagement / max(engagement) * 100,
        profile_image = map_chr(profile_image_url, cache_profile_image),
        profile_image_url = glue::glue('<div class="center-block"><img class="img-responsive img-circle" src="{profile_image}" alt={screen_name} style="max-height: 25px; min-width: 20px;"></div>'),
        profile_url = if_else(is.na(profile_url), glue::glue("https://twitter.com/{screen_name}"), profile_url),
        screen_name = glue::glue('<a href="{profile_url}" target="_blank">@{screen_name}</a>'),
        engagement = progressBar_v(engagement, rep(BASIC_COLORS[1:5], 2))
      ) %>%
      select(profile_image_url, screen_name, engagement) %>%
      knitr::kable(
        format = "html",
        escape = FALSE,
        align = "cll",
        col.names = c("", "Screen Name", "Engagement/Tweet "),
        table.attr = 'class = "table"'
      ) %>%
      HTML()
  })
  
  ## 7.2 HIGH SCORES FOR HASHTAGS ####
  output$top_hashtags <- renderUI({
    twh <-
      tweets_r() %>%
      select(hashtags) %>%
      unnest() %>%
      count(hashtags, sort = TRUE) %>%
      filter(!is.na(hashtags)) %>%
      mutate(hashtags = paste0("#", hashtags))
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(twh))), ~ {
        progressGroup(twh$hashtags[[.]], twh$n[[.]], max = max(twh$n), color = colors[.])
      })
    )
  })
  
  ## 7.3 HIGH SCORES FOR WORDS FREQUENCY ####
  output$top_tweet_words <- renderUI({
    tw <- tweets_r() %>%
      select(text) %>%
      mutate(
        text = str_remove_all(text, "@[[:alnum:]_]+\\b"),
        text = str_remove_all(text, "&\\w+;")
      ) %>%
      tidytext::unnest_tokens(word, text) %>%
      filter(
        !word %in% c("http", "https", "t.co"),
        nchar(word) >= 3
      ) %>%
      anti_join(tidytext::stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      slice(1:10)
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(tw))), ~ {
        progressGroup(tw$word[[.]], tw$n[[.]], max = max(tw$n), color = colors[.])
      })
    )
  })
  
  ## 7.4 HIGH SCORES FOR EMOJI ####
  output$top_emojis <- renderUI({
    emoji_regex <- "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF\u2600-\u27ff]+"
    
    twe <- tweets_r() %>%
      select(text) %>%
      tidytext::unnest_tokens(text, text, token = "tweets") %>%
      filter(str_detect(text, emoji_regex)) %>%
      mutate(text = str_remove_all(text, "\\w")) %>%
      tidytext::unnest_tokens(text, text, token = "characters") %>%
      count(text, sort = TRUE) %>%
      inner_join(emo::jis %>% select(runes, emoji, name), by = c("text" = "emoji")) %>%
      filter(!str_detect(runes, "^1F3F[B-F]$")) %>%
      slice(1:10) %>%
      mutate(
        b = n,
        # use twemoji
        runes = str_replace(tolower(runes), " ", "-"),
        runes = twemoji(runes)
      )
    
    colors <- rep(BASIC_COLORS[1:5], 2)
    
    tags$div(
      map(seq_len(min(10, nrow(twe))), ~ {
        progressGroup(HTML(twe$runes[[.x]]), twe$n[[.x]], max = max(twe$n), color = colors[.x])
      })
    )
  })
 
  ###### === TAB 5 : TWEET WALL === ######
  #### --- 8.0 DISPLAYING ALL TWEETS BASED ON TWITTER UI  --- ####

  ## GLOBAL REACTIVES NEEDED FOR TWEET WALL ##
  tweets_all <- reactiveFileReader(1 * 60 * 1000, session, "Data/tweets.rds", function(file) { 
    x <- readRDS(file)
    x
  })
  
  tweets <- reactive({
    req(tweets_all())
    tweets_all()
  })
  
  tweets_hourly_topic_count <- reactive({
    req(tweets())
    tweets() 
  })
  
  tweets_simple <- reactive({
    req(tweets())
    tweets() 
  })
  
  tweets_simple_today <- reactive({
    req(tweets_simple())
    tweets_simple() %>%
      tweets_today()
  })
  
  ## SERVER FOR TWEET WALL ##
  
  tweets_wall <- reactive({
    tweets_simple() %>%
      filter(
        created_at >= input$tweet_wall_daterange[1],
        created_at < input$tweet_wall_daterange[2] + 1
      )
  })
  
  tweet_wall_page_break = 20
  tweet_wall_n_items <- reactive({ nrow(tweets_wall()) })
  tweet_wall_page <- shinyThings::pager("tweet_wall_pager",
                                        n_items = tweet_wall_n_items,
                                        page_break = tweet_wall_page_break)
  
  output$tweet_wall_tweets <- renderUI({
    s_page_items <- tweet_wall_page() %||% 1L
    
    validate(need(
      nrow(tweets_wall()) > 0,
      "No tweets in selected date range. Try another set of dates."
    ))
    
    tweets_wall() %>%
      slice(s_page_items) %>%
      masonify_tweets()
  })
  
  tweet_wall_date_preset <- shinyThings::dropdownButton("tweet_wall_date_presets",
                                                        options = TWEET_WALL_DATE_INPUTS)
  
  observe({
    req(tweet_wall_date_preset())
    update_dates <- TWEET_WALL_DATE_RANGE(tweet_wall_date_preset())
    if (any(is.na(update_dates))) return(NULL)
    update_dates <- strftime(update_dates, "%F", tz = tz_global(), usetz = TRUE) %>% unname()
    updateDateRangeInput(session, "tweet_wall_daterange", start = update_dates[1], end = update_dates[2], max = now(tz_global()))
  })
  
  # Picture Tweet Wall ------------------------------------------------------
  pic_tweets_page_break <- 20
  tweets_pictures <- reactive({
    tweets() %>%
      select(created_at, status_id, screen_name, media_url) %>%
      filter(!map_lgl(media_url, ~ length(.) > 1 || is.na(.)))
  })
  
  pic_tweets_n_items <- reactive({ nrow(tweets_pictures()) })
  pic_tweets_page <- shinyThings::pager("pic_tweets", pic_tweets_n_items, pic_tweets_page_break)
  
  output$pic_tweets_wall <- renderUI({
    s_page_items <- pic_tweets_page() %||% 1L
    
    validate(need(
      nrow(tweets_pictures()) > 0,
      "No media tweets yet. Check back again soon."
    ))
    
    tweets_pictures() %>%
      slice(s_page_items) %>%
      masonify_tweets()
  })
  
  ###### === TAB 6 : DOCUMENTATION/GUIDE === ######
  #### --- 9.0 DISPLAYING GUIDES TO USER  --- ####
  
  output$download <- downloadHandler(
    filename = "Documentation.pdf",
    
    content = function(file) {
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      file.copy("documentation.pdf", file) 
    }
  )
  
  #### @JEN - BACKEND FOR Generate Report for the day/week ####
  report <- dataformatted_r() %>%
    dates <- as.Date(sentiment_by_row_id$created_at) %>% #sort out dates from tweets 
    dates_cleaned <- dates[!duplicated(dates)] %>% #remove duplicates
    dates_sorted <- sort(dates_cleaned)%>% #sort the dates in ascending order
    
    sentiment_dates_only <- as.Date(sentiment_by_row_id$created_at)%>%
    totalsentiment <- integer(length(dates_sorted))%>%
    
    resultString = rep.int("", length(dates_sorted)) %>%
    day_sentiment <- 1:length(dates_sorted)%>%
    
    for(i in 1:length(sentiment_by_row_id$text)) # loop over tweets
    {
      for (j in 1:length(dates_sorted))  # loop over different dates
      {
        if (sentiment_dates_only[i] == dates_sorted[j]) # concatenate to resultString if dates match
        {
          resultString[j] = paste(resultString[j], sentiment_by_row_id$text[i])%>% #combine tweets in the same day together
            totalsentiment[j] = totalsentiment[j] + sentiment_by_row_id$sentiment[i]%>% # calculate the total sentiment for the day
              if (totalsentiment[j]>0){
                day_sentiment[j] = "positive" } 
            else if (totalsentiment[j]<0) {
              day_sentiment[j] = "negative" }
            else{
              day_sentiment[j] = "neutral" }
        } 
      }
    } %>%
    
    
    result_by_date = data.frame(date=dates_sorted, tweetsByDate=resultString, sentimentmark=totalsentiment, sentiment= as.character(day_sentiment)) %>% #data of tweets with total sentiments for the day
    
    for(i in 1:length(result_by_date$date)){
      if(dates_sorted[i] == Sys.Date())
        output$today_sentiment_value <- renderValueBox({
          valueBox(paste("Sentiment Report"), 
                   subtitle = tags$p(paste("The overall sentiment for today is ", result_by_date$sentiment[i], "!"),style = "font-size: 170%;"), 
                   width = 12, 
                   icon = icon("users", "fa-1.5x", lib = "font-awesome" ), 
                   color = "teal")
        })
    }
  
  tweet_text <- result_by_date$tweetsByDate[result_by_date$date==Sys.Date()] %>%
    docs <- Corpus(VectorSource(tweet_text)) %>%
    
    inspect(docs) %>%
    
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) %>%
    docs <- tm_map(docs, toSpace, "/") %>%
    docs <- tm_map(docs, toSpace, "@") %>%
    docs <- tm_map(docs, toSpace, "\\|") %>%
    
    
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))%>%
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)%>%
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))%>%
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) %>%
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)%>%
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)%>%
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    dtm <- TermDocumentMatrix(docs)%>%
    m <- as.matrix(dtm)%>%
    v <- sort(rowSums(m),decreasing=TRUE)%>%
    d <- data.frame(word = names(v),freq=v)%>%
    
    output$day_wordcloud <- renderPlot({
      set.seed(1234)
      wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=100, random.order=FALSE, random.color = TRUE,
                scale=c(2,.5), colors=brewer.pal(12, "Paired"))
      
    })
  
  output$day_sentimenttype <- renderPlot({
    # run nrc sentiment analysis to return data frame with each row classified as one of the following
    # It also counts the number of positive and negative emotions found in each row
    today_sentiment_text <-get_nrc_sentiment(tweet_text)%>%
      # head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
      head (today_sentiment_text,10)%>%
      
      #transpose
      td<-data.frame(t(today_sentiment_text))%>%
        #Transformation and cleaning
        names(td)[1] <- "count"%>%
          td <- cbind("sentiment" = rownames(td), td)%>%
            rownames(td) <- NULL%>%
              td_new<-td[1:8,]%>%
                #Plot One - count of words associated with each sentiment
                quickplot(sentiment, data=td_new, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Sentiments Analysis")+coord_flip()
              
  })
  
  top_words_list <- strsplit(tweet_text, split = " ")%>%
    top_words <- as.data.frame(top_words_list)%>%
    colnames(top_words) <- c("word")%>%
    
    bing_word_counts <- top_words %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  
  output$Top_PosNeg_Today <- renderPlot({
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
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)



