library(shiny)
library(dplyr)
library(rworldmap)
library(RColorBrewer)
library(Rstem)
library(plyr)
library(wordcloud)
library(shinydashboard)
## Using SentimentPolarityResult for using the data
SPR <- SentimentPolarityResult

# Server Logic
server <- function(input, output, session) {
    
    output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$datafile
        
        if (is.null(inFile))
            return(NULL)
        
       read.csv(inFile$datapath)
    })
    
    #---------------------------------------------------------------------------------------------------------------------------------
    
    
    country_list <- reactive({
        as.character(unlist(unique(subset(SPR, SPR$AccountArea == input$Area, select = c("AccountCountry")))))
    })
    
    #-----------------------------------------------------------------------------------------------------------------------------------
    
    ## FOR POPULATING ACCOUNT COUNTRY BASED ON ACCOUNT REGION
    observe({
        updateSelectInput(session, "Country",
                          choices = c("All",unique(subset(SPR, SPR$AccountArea == input$Area, select = c("AccountCountry")))))
    })
    
    #-----------------------------------------------------------------------------------------------------------------------------------
    
    # FOR POPULATING INDUSTRY SECTOR BASED ON SELECTED REGION AND COUNTRY SELECTED 
    observe({
        if(input$Area == "All")
            updateSelectInput(session, "Industry", 
                              choices = unique(subset(SPR, SPR$AccountArea == input$Area , select = c("IndustrySector"))))
        else
            updateSelectInput(session, "Industry", 
                              choices = unique(subset(SPR, SPR$AccountArea == input$Area & SPR$AccountCountry == input$Country, select = c("IndustrySector"))))
    })
    
    #-----------------------------------------------------------------------------------------------------------------------------------
   
    
    # FOR PRINTING AVERGAGE SENTIMENT SCORE BASED ON ACCOUNT AREA
    output$SentimentScoreResults <- renderTable({
        Avg_Score <- SPR %>% filter(AccountArea == input$Area) %>% summarize(avg_score = mean(SentimentScore))
        Avg_Score
    })
    
    #------------------------------------------------------------------------------------------------------------------------------------------
    
    # FOR PRINTING AVERGAGE SENTIMENT SCORE BASED ON ACCOUNT COUNTRY
    output$SentimentScoreResults1 <- renderTable({
        Avg_Score1 <- SPR %>% filter(AccountArea == input$Area & AccountCountry == input$Country) %>% summarize(avg_score1 = mean(SentimentScore))
        Avg_Score1
    })
    
    #------------------------------------------------------------------------------------------------------------------------------------------
    
    # For Highlighting Sentiment Map based on Selected Region
    output$SentimentMapArea <- renderPlot(
        {
            ocean.color <- brewer.pal(2, "Blues")
            
            #Avg_Score <- SPR %>% filter(AccountCountry %in% c("Canada", "Australia")) %>% summarise(group_by(AccountCountry), mean(SentimentScore))
            
            malDF <- data.frame(country = country_list())
            #Sentiment_Map = Avg_Score)
            
            malMap <- joinCountryData2Map(malDF, joinCode = "NAME",
                                          nameJoinColumn = "country")
            
            mapCountryData(malMap, nameColumnToPlot="country",oceanCol=ocean.color,
                           missingCountryCol = gray(.8),colourPalette='rainbow')
        }
    )
    
    output$SentimentMapCountry <- renderPlot(
        {
            if (is.null(input$data))
                return(NULL)
            
            malDF <- data.frame(country = country_list())
            #Sentiment_Map = Avg_Score)
            
            malMap <- joinCountryData2Map(malDF, joinCode = "NAME",
                                          nameJoinColumn = "country")
            
            mapCountryData(malMap, nameColumnToPlot="country", oceanCol=ocean.color,
                           missingCountryCol = gray(.8), colourPalette='rainbow')
            
            
        })
    #--------------------------------------------------------------------------------------------------------------------------------------------   
    ## For ploting on bar graph
    output$SentimentPlot <- renderPlot(
        {
            
            
        }
    )
       
    #--------------------------------------------------------------------------------------------------------------------------------------------
    ## For preparing Word Cloud for the specific Region
    output$wordcloud <- renderPlot(
        {
            subset.data <- subset(SPR, SPR$AccountArea == input$Area, select = c("PositiveWords"))
            pos.freq.df = count(as.matrix(wordStem(unlist(subset.data$PositiveWords))))
            pal.positive <- brewer.pal(8, "Greens")
            pal.positive=pal.positive[-(1:3)]
            wordcloud(pos.freq.df$x, pos.freq.df$freq, 
                      min.freq = 1,
                      colors = pal.positive, 
                      random.order = F)
        }
    )
    
    output$wordcloud1 <- renderPlot(
        {
            subset.data <- subset(SPR, SPR$AccountArea == input$Area, select = c("NegativeWords"))
            pos.freq.df = count(as.matrix(wordStem(unlist(subset.data$NegativeWords))))
            pal.negative <- brewer.pal(8, "Reds")
            pal.negative=pal.negative[-(1:3)]
            wordcloud(pos.freq.df$x, pos.freq.df$freq, 
                      min.freq = 1,
                      colors = pal.negative, 
                      random.order = F)
        }
    )
}
