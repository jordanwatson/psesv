

ui_vis4 <- fluidPage(
    title="Sea surface temperatures by state statistical area",
    p("Map may take a few moments to load. Click on a state statistical area polygon to view the sea surface temperature time series for that area."),
    leafletOutput("map"),
    fluidRow(column(3,
                    selectInput("graph", "Display data as daily, weekly, or monthly:", 
                                           choices = c("Daily", "Weekly average","Monthly average"))),
             column(4,offset=1,
                    actionButton("reset","Click to reset\nplot below")),
             column(4,sliderInput("yearrange", 
                                           "Year:",
                                           min = 2003, 
                                           max = max(mydata$year),
                                           value = c(2003,max(mydata$year)),
                                           sep=""))),
             plotOutput('mainplot')
    )
  
    


