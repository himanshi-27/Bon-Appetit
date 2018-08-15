library(shiny)
library(leaflet)
library(dplyr)
library(reshape2)
library(wordcloud)
library(DT)
library(tidytext)
library(recommenderlab)

final <- read.csv("final.csv")
review_words <- read.csv("reviews.csv")
rating <- read.csv("rating.csv")
idf_city <- read.csv("idf_city.csv")

business_id <- unique(as.character(review_words$business_id))
business_id <- c("0", business_id)


review_words$business_id <- as.character(review_words$business_id)
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

# AFINN

bing <- sentiments %>%
  filter(lexicon %in% c("nrc", "bing")) %>%
  group_by(lexicon, sentiment) %>%
  summarise(noOfWords = n())

# bing


nrc_lexicon <- get_sentiments("nrc")

reviews_sentiment <- review_words %>%
  inner_join(AFINN, by = "word")
reviews_sentiment$afinn_score <- as.double(reviews_sentiment$afinn_score)

#Positive percentage
reviews_sentiment_positive <- reviews_sentiment %>% filter(afinn_score > 0) %>%
  group_by(business_id) %>%
  summarize(sentiment = mean(afinn_score))

reviews_sentiment_negative <- reviews_sentiment %>% filter(afinn_score < 0) %>%
  group_by(business_id) %>%
  summarize(sentiment = mean(afinn_score*-1))

# Recommenation
# rating_mx <- sparseMatrix(
#   i =  rating$user_No,
#   j =  rating$restaurant_No,
#   x = rating$stars.x,
#   dimnames = list(levels(rating$user_No), levels(rating$restaurant_No))
# )
# 
# mx <- as(rating_mx,"realRatingMatrix")
# colnames(mx) <- paste("R", 1:190, sep = "")
# as(mx[1,1:10],"list")
# 
# rownames(mx) <- paste("U", 1:2297, sep = "")
# as(mx[1,1:10], "list")
# 
# 
# mx_n <- normalize(mx)
# 
# 
# table(mx@data@x[] > 5)
# table(mx@data@x[] < 1)
# 
# mx@data@x[mx@data@x[] > 5] <- 5
# 
# mx_r <- mx[rowCounts(mx) > 20,]
# 
# set.seed(1)
# e <- evaluationScheme(mx_r, method = "split",train = 0.8, given = 5, goodRating = 3, k=5)
# 
# r_ubcf <- Recommender(getData(e, "train"), method ="UBCF", parameter = list(method = "cosine", normalize = "Z-score", nn=25))
# 
# gc()
# 
# memory.limit(size=700000)
# names(getModel(r_ubcf))
# results <- evaluate(e, method="UBCF", type = "ratings", n=c(1,3,5,10,15,20))



# User Interface
ui <- fluidPage(
  
  # Application title
  titlePanel("Bon Appetit"),
  
  # Sidebar  
  sidebarLayout(
    
    # inputs go here 
    sidebarPanel(
      selectInput('cuisine', 'Cuisine', c('Indian','Noodles',
                                          'Salad','Seafood','African','American','Arabian',
                                          'Asian','Bar','Breakfast','British',
                                          'Central America','Coffee Shop','Dessert',
                                          'Dietary Restriction','European','Lunch',
                                          'Mediterranean','Pacific','Pub','Russian',
                                          'South American'), selected = 'American',
                  multiple = TRUE),
      sliderInput('rating', 'Star Rating', min = 1, max = 5, value = c(1,5), step = .5),
      textInput('zip', 'Zip Code', value = "85003"),
      sliderInput('price', 'Price Range $-$$$$', min = 1, max = 4, value = c(1,4), step = 1),
      selectInput('b_id', "Business ID", c(business_id)),
      selectInput('u_id', "User ID", c(1:10))
    ),    
    
    # outputs go here
    mainPanel(
      tabsetPanel(
         tabPanel("Endusers",leafletOutput('myMap')),
         #tabPanel("Enduser Recommendation", leafletOutput('reMap')),
         tabPanel("Managers", h4("Percentage positive reviews for you"),
                  textOutput("percentagePos"),
                  h4("Percentage negative reviews for you"),
                  textOutput("percentageNeg"),
                  plotOutput("plot1"),
                  plotOutput("plot2"),
                  h2("Top 10 common words"),
                  DT::dataTableOutput("mostCommon"),
                  h2("Top 10 Postive and Negative words"),
                  DT::dataTableOutput("mostCommonPosNeg"))
              )
          )
    )
)

#App Server
server <- function(input, output) {
  ##EndUser
  indian <- reactive({if('Indian' %in% input$cuisine){
    final %>%
      filter(Indian == 1)
  }})
  
  salad <- reactive({if('Salad' %in% input$cuisine){
    final %>%
      filter(Salad == 1)
  }})
  
  noodles <- reactive({if('Noodles' %in% input$cuisine){
    final %>%
      filter(Noodles == 1)
  }})
  
  seafood <- reactive({if('Seafood' %in% input$cuisine){
    final %>%
      filter(Seafood == 1)
  }})
  
  african <- reactive({if('African' %in% input$cuisine){
    final %>%
      filter(African == 1)
  }})
  
  american <- reactive({if('American' %in% input$cuisine){
    final %>%
      filter(American == 1)
  }})
  
  arabian <- reactive({if('Arabian' %in% input$cuisine){
    final %>%
      filter(Arabian == 1)
  }})
  
  asian <- reactive({if('Asian' %in% input$cuisine){
    final %>%
      filter(Asian == 1)
  }})
  
  bar <- reactive({if('Bar' %in% input$cuisine){
    final %>%
      filter(Bar == 1)
  }})
  
  breakfast <- reactive({if('Breakfast' %in% input$cuisine){
    final %>%
      filter(Breakfast == 1)
  }})
  
  british <- reactive({if('British' %in% input$cuisine){
    final %>%
      filter(British == 1)
  }})
  
  central <- reactive({if('Central America' %in% input$cuisine){
    final %>%
      filter(Central_America == 1)
  }})
  
  coffee <- reactive({if('Coffee Shop' %in% input$cuisine){
    final %>%
      filter(Coffee_Shop == 1)
  }})
  
  dessert <- reactive({if('Dessert' %in% input$cuisine){
    final %>%
      filter(Dessert == 1)
  }})
  
  diet <- reactive({if('Dietary Restriction' %in% input$cuisine){
    final %>%
      filter(Dietary_Restriction == 1)
  }})
  
  europe <- reactive({if('European' %in% input$cuisine){
    final %>%
      filter(European == 1)
  }})
  
  lunch <- reactive({if('Lunch' %in% input$cuisine){
    final %>%
      filter(Lunch == 1)
  }})
  
  medi <- reactive({if('Mediterranean' %in% input$cuisine){
    final %>%
      filter(Mediterranean == 1)
  }})
  
  
  pac <- reactive({if('Pacific' %in% input$cuisine){
    final %>%
      filter(Pacific == 1)
  }})
  
  russian <- reactive({if('Russian' %in% input$cuisine){
    final %>%
      filter(Russian == 1)
  }})
  
  
  pub <- reactive({if('Pub' %in% input$cuisine){
    final %>%
      filter(Pub == 1)
  }})
  
  south <- reactive({if('South American' %in% input$cuisine){
    final %>%
      filter(South_American == 1)
  }})
  
  
  cuisine <- reactive({rbind( salad(),indian(), noodles(), seafood(),
                              african(), american(), arabian(),
                              breakfast(), bar(), asian(), british(),
                              central(), coffee(), dessert(), diet(),
                              europe(), medi(), lunch(), pac(), pub(),
                              russian(), south())
  })
  
  
  points <- reactive({cuisine()  %>%
      filter(postal_code %in% input$zip,
             stars >= input$rating[1],
             stars <= input$rating[2],
             attributes.RestaurantsPriceRange2 >= input$price[1],
             attributes.RestaurantsPriceRange2 <= input$price[2]
      )  %>%
      select(longitude,latitude, name, address) %>%
      print()
  })
  
  
  
  output$myMap = renderLeaflet(
    leaflet()  %>%
      addTiles() %>%
      addMarkers(data = points(), popup = ~paste(name, address, sep=": "), label = ~paste(name, address, sep=": "))
  )
  
  ##Manager
  output$percentagePos <- renderPrint(subset(reviews_sentiment_positive, business_id %in% input$b_id) %>%
                                        summarise(percentagePositivity = sentiment/5 * 100) %>% head(1))
  output$percentageNeg <- renderPrint(subset(reviews_sentiment_negative, business_id %in% input$b_id) %>%
                                        summarise(percentageNegativity = sentiment/5 * 100) %>% head(1))

  output$plot1 <- renderPlot({
    ## Most common cloud
    subset(review_words, business_id == input$b_id) %>%
      anti_join(stop_words)%>%
      count(word) %>%
      with(wordcloud(word,n,max.words = 100))
  })

  output$plot2 <- renderPlot({
    ## Positive negative cloud
    subset(review_words, business_id == input$b_id) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(max.words = 150)
  })


  mostCommon_by_Business <- reactiveValues(m=data.frame(subset(review_words, business_id == "0") %>%
                                                          semi_join(nrc_lexicon) %>%
                                                          group_by(business_id) %>%
                                                          count(word, sort = T)
  ))

  observeEvent(input$b_id,{
    mostCommon_by_Business$m <- data.frame(subset(review_words, business_id == input$b_id) %>%
                                             semi_join(nrc_lexicon) %>%
                                             group_by(business_id) %>%
                                             count(word, sort = T))
  })

  output$mostCommon = DT::renderDataTable({
    mostCommon_by_Business$m[1:1010,2:3]
  })


  mostPosNeg_by_Business <- reactiveValues(m=data.frame(subset(review_words, business_id == "0") %>%
                                                          inner_join(get_sentiments("bing")) %>%
                                                          group_by(business_id)%>%
                                                          count(word, sentiment, sort = T) %>%
                                                          ungroup()
  ))

  observeEvent(input$b_id,{
    mostPosNeg_by_Business$m <- data.frame(data.frame(subset(review_words, business_id == input$b_id) %>%
                                                        inner_join(get_sentiments("bing")) %>%
                                                        group_by(business_id)%>%
                                                        count(word, sentiment, sort = T) %>%
                                                        ungroup()))
  })

  output$mostCommonPosNeg = DT::renderDataTable({
    mostPosNeg_by_Business$m[1:10,2:4]
  })
  
  #Enduser Recommendation
  
  # p_topN <- reactive({predict(r_ubcf, mx_r[input$u_id],type="topNList",n=10)})
  # print(p_topN)
  # 
  # 
  # p <- reactive({idf_city %>%
  #     filter(restaurant_id %in% p_topN())  %>%
  #     select(longitude,latitude, name, address) %>%
  #     print()
  # })
  # 
  # 
  # output$reMap = renderLeaflet(
  #   leaflet()  %>%
  #     addTiles() %>%
  #     addMarkers(data = p(), popup = ~paste(name, address, sep=": "), label = ~paste(name, address, sep=": "))
  # )

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

