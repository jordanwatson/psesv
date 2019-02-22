vis <- "vis4"
source("Global.R")

#-------------------------------------------------------------------------------
#  The original shapefile for visualization 4 was downloaded from:
#  soa-adfg.opendata.arcgis.com/datasets/groundfish-statistical-areas-2001
#  The file is named "Groundfish_Statistical_Areas_2001.shp" and can be found in the data folder.
#stat6 <- readOGR(dsn="Data",layer="Groundfish_Statistical_Areas_2001")

#  The original file included both positive and negative longitudes across the international date line. 
#  Convert positive longitudes to negative in order to make a single contiguous map of Alaska waters.
#mypoly <- length(stat6@polygons)
#for(i in 1:mypoly){
#  mypoly2 <- length(stat6@polygons[[i]]@Polygons)
#  for(j in 1:mypoly2){
#    stat6@polygons[[i]]@Polygons[[j]]@coords[,1][stat6@polygons[[i]]@Polygons[[j]]@coords[,1]>0] <- stat6@polygons[[i]]@Polygons[[j]]@coords[,1]-360
#  }
#}

#  The original shapefile is more detailed than is necessary for the visualization. Simplify polygons using rmapshaper::ms_simplify
#library(rmapshaper)
#simple <- rmapshaper::ms_simplify(stat6)

#  This simplified file is used for the visualization and to make the visualization load faster is saved as "adf_stat_areas_simple.shp" and can be 
#  found in the data folder
#writeOGR(simple,dsn="Data","adfg_stat_areas_simple",driver="ESRI Shapefile")

#  For the purposes of the visualization, use a simplified version of the shapefile where some of the coastline detail is smoothed. This speeds up
#  display of the data. 

simple <- readOGR(dsn="Data",layer="adfg_stat_areas_simple")

# Find a center point for each region
#centers <- data.frame(gCentroid(simple, byid = TRUE))
#centers$region <- row.names(simple)


server_vis4 <- shinyServer(function(input, output) {
  #data <- reactiveValues(clickedShape=vector())
  click_list <- reactiveValues(ids=vector())
  # produce the basic leaflet map
  output$map <- renderLeaflet(
    
  #labels <- sprintf("<strong>%s</strong>",simple$STAT_AREA) %>% lapply(htmltools::HTML)
    
    
    leaflet(simple) %>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 1,
                  fillColor = "blue",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  layerId = ~STAT_AREA,
                  label=~STAT_AREA,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"
                  ))
  )
  
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    #data$clickedShape <- input$map_shape_click
    click <- input$map_shape_click
    click_list$ids <- c(click_list$ids, click$id)
    print(click_list$ids)
  })
  
  #output$myTable <- renderTable({
  #  return(data$clickedMarker$id)
  # observe the marker click info and print to console when it is changed.
  #})
  #output$mainplot <- renderPlot({
  #  req(data$clickedShape$id)
  #  dataset <- mydata %>% 
  #    filter(STAT_AREA%in%data$clickedShape$id)

  observeEvent(input$reset,{
    click_list$ids <- NULL
  })
  
    output$mainplot <- renderPlot({
      req(click_list$ids)
      
      if(is.null(click_list$ids)) return()
      
      dataset <- mydata %>% 
        filter(STAT_AREA%in%click_list$ids)
      
    p <- ggplot(dataset, aes(x = date, y = sst.mean,color=factor(STAT_AREA))) +
      geom_line() +
      theme_bw() + 
      #ggtitle(paste("Daily sea surface temperatures, stat area",click_list$ids)) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      scale_color_viridis(discrete=TRUE,
                          guide=guide_legend("Statistical Area",ncol=4)) + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=10),
            axis.title = element_text(size=14)) + 
      xlab("Date") +
      ylab(expression("Temperature " ( degree*C)))
    print(p)
  })
})