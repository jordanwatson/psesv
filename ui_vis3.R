
ui_vis3 <- fluidPage(
  tabsetPanel(
    tabPanel("NMFS Seasonal Anomaly",
             titlePanel("Seasonal sea surface temperature anomaly"),
             p("Select NMFS reporting area(s) to view winter and summer temperature anomalies. October - December of each winter corresponds to the previous year (e.g., winter 2010 includes Oct - Dec 2017 and Jan - Mar 2018). 2018 is omitted from summer anomalies because the data are incomplete. Winter 2003 includes only Jan - Mar 2003 and not Oct - Dec 2002. Depth filters remove statistical areas whose average depth is outside of the selected depth range. The default depth is set at 0 - 200m, a general approximation for the continental shelf. If the selected depth leads to a plotting error try a different depth range."),
             fluidRow(
               column(6,
                      selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE)),
               column(6,
                      sliderInput("range", "Depth (m):",min = -5000, max = 0,value = c(-200,0))
               ),
               plotOutput('seasonal_nmfs_plot')
             )),
    tabPanel("NMFS Monthly Anomaly",
             titlePanel("Monthly sea surface temperature anomaly"),
             p("Select NMFS reporting area(s) and month to view temperature anomalies (standard deviations) for those areas and that month. Depth filters remove statistical areas whose average depth is outside of the selected depth range. The default depth is set at 0 - 200m, a general approximation for the continental shelf. If the selected depth leads to a plotting error try a different depth range."),
             fluidRow(
               column(4,
                      selectInput('dnmfs_month', 'NMFS area', mynmfs, multiple=TRUE)),
               column(4,
                      selectInput('dmonth', 'Month', month.name)),
               column(4,
                      sliderInput("range_month", "Depth (m):",min = -5000, max = 0,value = c(-200,0))
               )),
             plotOutput('monthly_nmfs_plot')
    )
    )
  )

