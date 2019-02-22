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

#------------------------------------------------------------------------------------
#  Define global parameters for each of the three tabs
#------------------------------------------------------------------------------------

#  Define the map parameters
mymap <- leaflet(simple) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = "blue",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              layerId = ~STAT_AREA,
              label=~STAT_AREA,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto"))

#  Define temperature plot parameters
myplotpars <- list(xlab("Date"),
                   ylab(expression("Temperature " ( degree*C))),
                   scale_x_datetime(date_breaks="1 year",date_labels="%Y"), 
                   scale_color_viridis(discrete=TRUE,
                      guide=guide_legend("Statistical Area",ncol=4)))

#  Define temperature plot theme
mytheme <- theme_bw() + 
    theme(legend.position="top",
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        axis.text = element_text(size=10),
        axis.title = element_text(size=14))
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#  Define server side functions
#------------------------------------------------------------------------------------

server_vis4 <- shinyServer(function(input, output) {

  #-------------------------------------------
  #  Create the daily tab
  #-------------------------------------------
  
    #  Populate click list as vector to allow for multiple selections
  click_list_day <- reactiveValues(ids=vector())
  # produce the basic leaflet map
  output$map_day <- renderLeaflet(
    mymap
  )
  
  observeEvent(input$map_day_shape_click, { # update the location selectInput on map clicks
    click_day <- input$map_day_shape_click
    #  Concatenate successive clicks to allow for plotting of multiple stat areas.
    click_list_day$ids <- c(click_list_day$ids, click_day$id)
    #print(click_list_day$ids)
  })
  
  # Populate the reset button.
  observeEvent(input$reset_day,{
    click_list_day$ids <- NULL
  })
  
  output$mainplot_day <- renderPlot({
    req(click_list_day$ids)
    
    #  Determine if the reset button was clicked, in which case abort plotting.
    if(is.null(click_list_day$ids)) return()
    
    #  Subset and plot data for selected stat areas.
    ggplot(
      mydata %>% 
        filter(STAT_AREA%in%click_list_day$ids & 
                 year>=input$yearrange_day[1] & 
                 year<=input$yearrange_day[2]), 
      aes(x = date, y = sst.mean,color=factor(STAT_AREA))) +
      geom_line() + 
      myplotpars + 
      mytheme
  })
  
  
  #-------------------------------------------
  #  Create the weekly tab
  #-------------------------------------------
  click_list_week <- reactiveValues(ids=vector())
  # produce the basic leaflet map
  output$map_week <- renderLeaflet(
    mymap
  )
  
  observeEvent(input$map_week_shape_click, { # update the location selectInput on map clicks
    click_week <- input$map_week_shape_click
    #  Concatenate successive clicks to allow for plotting of multiple stat areas.
    click_list_week$ids <- c(click_list_week$ids, click_week$id)
    #print(click_list_week$ids)
  })
  
  # Populate the reset button.
  observeEvent(input$reset_week,{
    click_list_week$ids <- NULL
  })
  
  output$mainplot_week <- renderPlot({
    req(click_list_week$ids)
    
    #  Determine if the reset button was clicked, in which case abort plotting.
    if(is.null(click_list_week$ids)) return()
    
    #  Subset and plot data for selected stat areas.
    ggplot(
      mydata %>% 
        filter(STAT_AREA%in%click_list_week$ids & 
                 year>=input$yearrange_week[1] & 
                 year<=input$yearrange_week[2]) %>% 
        group_by(cumwk,STAT_AREA) %>% 
        summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                  date=min(date),
                  year=year[1]), 
      aes(x = date, y = sst.mean,color=factor(STAT_AREA))) +
      geom_line() + 
      myplotpars + 
      mytheme
  })
  
  
  
  #-------------------------------------------
  #  Create the monthly tab
  #-------------------------------------------
  click_list_month <- reactiveValues(ids=vector())
  # produce the basic leaflet map
  output$map_month <- renderLeaflet(
    mymap
  )
  
  observeEvent(input$map_month_shape_click, { # update the location selectInput on map clicks
    click_month <- input$map_month_shape_click
    #  Concatenate successive clicks to allow for plotting of multiple stat areas.
    click_list_month$ids <- c(click_list_month$ids, click_month$id)
    #print(click_list_month$ids)
  })
  
  # Populate the reset button.
  observeEvent(input$reset_month,{
    click_list_month$ids <- NULL
  })
  
  output$mainplot_month <- renderPlot({
    req(click_list_month$ids)
    
    #  Determine if the reset button was clicked, in which case abort plotting.
    if(is.null(click_list_month$ids)) return()
    
    #  Subset and plot data for selected stat areas.
    ggplot(
      mydata %>% 
        filter(STAT_AREA%in%click_list_month$ids & 
                 year>=input$yearrange_month[1] & 
                 year<=input$yearrange_month[2]) %>% 
        group_by(cummo,STAT_AREA) %>% 
        summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                  date=min(date),
                  year=year[1]), 
      aes(x = date, y = sst.mean,color=factor(STAT_AREA))) +
      geom_line() + 
      myplotpars + 
      mytheme
  })
})


