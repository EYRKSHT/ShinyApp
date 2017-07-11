library(shinydashboard)

dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th")),
            fileInput("datafile", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            )
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        column(width = 4,selectInput("Area", "Account AREA", unique(SPR$AccountArea), selected = NULL, width='100%')),
                        column(width = 4,selectInput("Country", "Account Country", c("All", unique(SPR$AccountCountry)), selected = NULL, multiple = FALSE,width='100%')),
                        column(width = 4,selectInput("Industry", "Industry Sector", unique(SPR$IndustrySector), selected = NULL, multiple = FALSE,width='100%'))
                    ),
                   
                    HTML('<br><br><br>'),
                    fluidRow(
                       
                    column(width=4,box(tableOutput("SentimentScoreResults"),width = '100%',style="text-align:center")),
                    column(width=4,box(tableOutput("SentimentScoreResults1"),width = '100%',style="text-align:center"))
                    ),
                   HTML('<br><br><br>'),
                   fluidRow(
                   box(tittle= "Map" , solidHeader= TRUE, plotOutput("SentimentMapArea")),
                   box(tittle= "Map", plotOutput("SentimentPlot"))
                   ),
                   fluidRow(
                   box(tittle= "WordCloud" , solidHeader= TRUE,plotOutput("wordcloud")),
                   box(tittle= "WordCloud1" , solidHeader= TRUE,plotOutput("wordcloud1"))
                   )
                  
                    #plotOutput("SentimentMapCountry")
                    
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    h2("Widgets tab content"),
                    tableOutput("contents")
                
                    
                    
            )
        )
    )
)
