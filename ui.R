library(shiny)
library(RMySQL)

# initialize database connection
cons <- dbListConnections(MySQL())
for (con in cons){
  dbDisconnect(con)
}


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Podcast Producers Report"),
  helpText("May take 1-2 minutes to update."),
  helpText("Work in progress."),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel('Episode',
                 tags$hr(),
                 textInput("episode_id", label="Enter the episode Seamus ID", value="398701555")
        ),
        tabPanel('Date Range',
                 tags$hr(), 
                 dateRangeInput("start.end.dates",
                                label="Choose a date range:",
                                start=Sys.Date()-60,
                                end=Sys.Date()-1)
        ) 
        
        
      ),
      tags$hr(),
      
      submitButton("Send Query"),
      
      tags$hr()
      
    ),
      
      
    
    
    
    
    # Show the table
    mainPanel(
      htmlOutput("all_text"),
      plotOutput("minutes_elapsed",height="650px"),
      tags$hr()
      
    )
  
  )
  
))