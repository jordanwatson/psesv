

ui_vis4 <- fluidPage(
  tabsetPanel(
    tabPanel("Daily temperatures by statistical area",
             p("Map may take a few moments to load. Click on a state statistical area polygon to view the daily temperature time series for that area. There is a reset button at the bottom of the page and a slider to zoom the time series by year."),
             leafletOutput("map_day"),
             plotOutput('mainplot_day'),
             fluidRow(
               column(4,actionButton("reset_day","Reset Plot")),
               column(6,sliderInput("yearrange_day", 
                                    "Year:",
                                    min = 2003, 
                                    max = max(mydata$year),
                                    value = c(2003,max(mydata$year)),
                                    sep=""))
             )),
    tabPanel("Weekly temperatures by statistical area",
             p("Map may take a few moments to load. Click on a state statistical area polygon to view the weekly averaged temperature time series for that area. There is a reset button at the bottom of the page and a slider to zoom the time series by year."),
             leafletOutput("map_week"),
             plotOutput('mainplot_week'),
             fluidRow(
               column(4,actionButton("reset_week","Reset Plot")),
               column(6,sliderInput("yearrange_week", 
                                    "Year:",
                                    min = 2003, 
                                    max = max(mydata$year),
                                    value = c(2003,max(mydata$year)),
                                    sep=""))
             )),
    tabPanel("Monthly temperatures by statistical area",
             p("Map may take a few moments to load. Click on a state statistical area polygon to view the monthly averaged temperature time series for that area. There is a reset button at the bottom of the page and a slider to zoom the time series by year."),
             leafletOutput("map_month"),
             plotOutput('mainplot_month'),
             fluidRow(
               column(4,actionButton("reset_month","Reset Plot")),
               column(6,sliderInput("yearrange_month", 
                                    "Year:",
                                    min = 2003, 
                                    max = max(mydata$year),
                                    value = c(2003,max(mydata$year)),
                                    sep=""))
             )
    )
  )
)

