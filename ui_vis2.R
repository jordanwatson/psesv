
ui_vis2 <- fluidPage(
  tabsetPanel(
    tabPanel("Daily temperatures",
             titlePanel("Daily temperatures by ADFG statistical area"),
             p("Select ADFG statistical area to plot a daily temperature time series. Select multiple areas to compare time series."),
             fluidRow(
               column(4,
                      selectInput("dstat",label = "Choose statistical areas to compare",choices = data2$STAT_AREA %>% unique %>% sort,multiple = TRUE)
               ),
               plotOutput('daily_plot')
             )
    ),
    tabPanel("Weekly temperatures",
             titlePanel("Weekly temperatures by ADFG statistical area"),
             p("Select ADFG statistical area to plot a weekly temperature time series (daily temperatures averaged by week). Select multiple areas to compare time series."),
             fluidRow(
               column(4,
                      selectInput("wstat",label = "Choose statistical areas to compare",choices = data2$STAT_AREA %>% unique %>% sort,multiple = TRUE)
               ),
               plotOutput('weekly_plot')
             )
    ),
    tabPanel("Monthly temperatures",
             titlePanel("Monthly temperatures by ADFG statistical area"),
             p("Select ADFG statistical area to plot a monthly temperature time series (daily temperatures averaged by month). Select multiple areas to compare time series."),
             fluidRow(
               column(4,
                      selectInput("mstat",label = "Choose statistical areas to compare",choices = data2$STAT_AREA %>% unique %>% sort,multiple = TRUE)
               ),
               plotOutput('monthly_plot')
             )
    )))

