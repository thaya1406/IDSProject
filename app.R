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
##TEXT
library(tidytext)
library(textdata)

##Visualization
library(plotly)
library(ggwordcloud)

##Text
library(tidyverse)
library(tidyquant)

#value boxes


ui <- dashboardPage(
  dashboardHeader(title = h4(HTML("Twitter Covid'19 <br/>Sentiment Analysis")), titleWidth = 230,
                  disable = FALSE),
  
#### @SAMUEL YOU WILL FOCUS HERE FOR CREATING THE INPUT ####
  dashboardSidebar(
    sidebarPanel(
      h5(style="color:#cc4c02", sliderInput("Tweets_to_Download",
                                            "No of Tweets to Download:",
                                            min = 500,
                                            max = 18000,
                                            value = 500,
                                            step = 500)),
      h6(style = "color:#006d2c", textInput("text", h3("Text input"), 
                                            value = "Enter text..."))
      , width = 0.3)
  ),
  
#### @SAMUEL YOUR FOCUS HERE FOR CREATING THE INPUT ENDS HERE ####




  dashboardBody(
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
        .main-sidebar {
            background-color: skinblue !important;
          }
  
          
        "))),
    
    ## @ARINA @ JEN @THAYA You have to organise the tab nicely ,later figure out which will go which tab and so on !!
    tabsetPanel(
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
                   ,wordcloud2Output("wordcloud", height = "300px")
                 ),
                 box(
                   title = "Top 10 words"
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("top10", height = "300px")
                 )),
               fluidRow(
                 box(
                   title = "Top Positive and Negative Words",
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("bing", height = "300px")
                 ),
                 box(
                   title = "NRC Sentiments",
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotOutput("NRC", height = "300px")
                 )),
               fluidRow(
                 box(
                   title = "Sentiment Polarity",
                   width = 12,
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotlyOutput("polarity", height = "300px")
                   
                 )),
      ),
      tabPanel(title = "Tweet Table",
               fluidRow(
                 valueBoxOutput("value4"),
                 valueBoxOutput("value5"),
                 valueBoxOutput("value6")),
               fluidRow(
                 valueBoxOutput("value7"),
                 valueBoxOutput("value8"),
                 valueBoxOutput("value9")),
               fluidRow(reactableOutput("tweet_table"))),
      tabPanel(title = "About App",
               tags$div( id = 'ci_intel_by_hs_hstable' ,
                         fluidRow( h3( style="color:#cc4c02",HTML("<b>Twitter Sentiment Analysis Shiny App</b>")),
                                   h5(HTML("Using Twittter API, this Dashboard collects recent tweets.
                                          The Number of Tweets and preferred hashtag can be used to retrieve tweets with a range between
                                          500 & 18000 tweets at a time. There are also a range of preferred hashtags that can be used to
                                          guide the search. The tweet table presented using reactable, contains options to search either 
                                                a specific column or the entire table. </br>
                                            The Sentiment Polarity Graph, shows the extreme positivity or negativity of all tweets collected. 
                                                An interactive plot, that presents the guiding statement, tweet author and link to the page of twitter itself.
                                                The <b>Tweet table</b> contains the details
                                                of each tweet and <b>>></b> is the tweet link")))
               ))
    )))






# Define server logic 
server <- function(input, output) {

  token <- create_token(
    app = "immigration_trend", 
    consumer_key = "Hgj9nhsN2FPhruxxpwhttnBOS", 
    consumer_secret = "IQXwprbhJYzBCqhEIpkJLcuPSPQaYTMWadj3BMg3nWrcnBIpwd",
    access_token = "1068608387334766593-BpB8hUTe09InPeeGrAqZF9Rk2SEomb",
    access_secret = "phFEUzi8xfvjaS7XKi5i8VNjXzoDUh326mcJ6SczheBH6"
  )
  
  #### @SAMUEL YOU WILL HAVE TO MANIPULATE HERE SINCE THE INPUT YOU CREATED ABOVE HAVE TO BE REFLECTED HERE IN THE SEARCH TERM ####
  
  dataInput <- reactive({searchTwitter(input$text, n = input$Tweets_to_Download) })
  
  
  saveRDS(rt, "Data/raw.rds")
  tweetsForSentiment <-  readRDS("Data/raw.rds")
  
  
  
  
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
  
  
  
  
  
  #### @THAYA - BACKEND FOR WORDCLOUD ####
  
  
  
  
  
  #Create reactive word cloud
  output$wordcloud <- renderPlot({
    
    ##Tidy Data
    tweets_tokenized <- tweetsForSentiment %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word,text)
    
    
    ##  3.2 Joining Sentiment Dict with Tokenized Text
    sentiment_bing <-  tweets_tokenized %>% inner_join(get_sentiments("bing"))
    
    
    sentiment_by_word <-  sentiment_bing %>% count(word, sentiment, sort=TRUE)
    
    sentiment_by_word %>%
      slice(1:100) %>%
      mutate(sentiment=factor(sentiment, levels=c("positive", "negative"))) %>%
      ggplot(aes(label=word, color=sentiment, size=n))+
      geom_text_wordcloud_area()+
      facet_wrap(~ sentiment, ncol=2)+
      theme_tq()+
      scale_color_tq()+
      scale_size_area(max_size=16)+
      labs(tittle ="Sentiment Word Frequency")
    
    
  })
  
  
  
  
  #### @THAYA - BACKEND FOR SENTIMENT POLARITY ####
  output$polarity <- renderPlotly({
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
    
  })
  
  
  
  #### @JEN - BACKEND FOR Number of tweets per ####
  #### @JEN - BACKEND FOR Generate Report for the day/week ####
  
  
  #### @ARINA - BACKEND FOR Table of Tweets ####
  
  tweet_table_data <- reactive({
    req(tweeets)
    tweeets %>% 
      rename(user_id = id,
             created_at = created,
             screen_name= screenName,
             favorite_count = favoriteCount,
             retweet_count = retweetCount,
             urls_expanded_url = statusSource) %>% 
      mutate(status_id = user_id) %>% 
      select(user_id, created_at, status_id, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })
  
  output$tweet_table <- renderReactable({
    reactable(tweet_table_data(), 
              filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
              showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 50, 75, 100, 200), 
              columns = list(
                DateTime = colDef(defaultSortOrder = "asc"),
                User = colDef(defaultSortOrder = "asc"),
                Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                URLs = colDef(html = TRUE)
              )
    )
  })
  #### @ARINA - BACKEND FOR Sentiment Type Bar Plot  ####
  
  
  ## @ARINA @JEN Once you have done the backend , make sure you go up to the UI and make sure its reflected on  the UI and it should be reactive ##
  
  
  
  output$top10 <- renderPlot({
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
  })
  
  
  
  
  output$bing <- renderPlot({
    pos_vs_neg <- tweetsForSentiment[,1:16] %>% 
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
      theme_bw()
    
    
  })
  
  
  
  output$NRC <- renderPlot({
    
    nr <- read.csv("nrc.csv")  
    #nrc tweet analysis
    nrc <- tweeets %>% 
      mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
      mutate(text = tolower(text)) %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      inner_join(nr) %>% 
      group_by(sentiment) %>% 
      count(sentiment, sort = T)
    
    ggplot(nrc, aes(reorder(sentiment, n), n, fill = sentiment)) + 
      geom_bar(stat = "identity", show.legend = F) + coord_flip() +
      theme_minimal() + labs(x = "Sentiments", y = "n") +
      theme(axis.title.x = element_text(face ="bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15),
            axis.text = element_text(face = "bold"))
  })
  
  #Build value box
  output$value1 <- renderValueBox({
    n <- tweeets %>% 
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
             icon = icon("smile", lib ="font-awesome" ), color = "aqua")
  })
  
  output$value2 <- renderValueBox({
    n <-tweeets[,1:16] %>% 
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
             icon = icon("angry", lib ="font-awesome" ), color = "green")
  })

  
  output$value3 <- renderValueBox({
    
    tweets_count <- tweeets %>% 
      nrow()
    
    
    valueBox(tweets_count, subtitle = "Total Tweets", 
             icon = icon("chart-bar", lib ="font-awesome" ), color = "orange")
  })
  
  
}



####SAM



# Run the application 
shinyApp(ui = ui, server = server)



