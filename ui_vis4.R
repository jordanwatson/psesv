

ui_vis4 <- fluidPage(
  titlePanel("Daily sea surface temperature data"),
  p("Map may take a few moments to load. Click on a state statistical area polygon to view the daily temperature time series for that area."),
  leafletOutput("map"),
  #p(),
  plotOutput('mainplot')
)

