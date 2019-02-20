library(shiny)
library(leaflet)
library(geojsonio)

#https://stackoverflow.com/questions/45953741/select-and-deselect-polylines-in-shiny-leaflet

url <- "pathTogeojson"

geojson <- geojsonio::geojson_read(url, what = "sp") 

shinyApp(
  ui <- fluidRow(
    leafletOutput("map")),
  
  server <- function(input, output, session) {
    
    click_list <- reactiveValues(ids = vector())  
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        setView(lng=16.357795000076294, lat=48.194883921677935, zoom = 15) %>%
        addPolylines(data=geojson, layerId = geojson@data$name_1, group = "selected", color="red", weight=3,opacity=1)
    })
    
    observeEvent(input$map_shape_click, {
      
      click <- input$map_shape_click
      proxy <- leafletProxy("map")
      click_list$ids <- c(click_list$ids, click$id)
      
      sel_lines <- geojson[geojson@data$name_1 %in% click_list$ids, ]
      
      if(click$id %in% sel_lines@data$id)
      {
        nameMatch <- sel_lines@data$name_1[sel_lines@data$id == click$id]
        click_list$ids <- click_list$ids[!click_list$ids %in% click$id] 
        click_list$ids <- click_list$ids[!click_list$ids %in% nameMatch]
        
        proxy %>% removeShape(layerId = click$id)
      }
      else
      {
        proxy %>% addPolylines(data = sel_lines, layerId = sel_lines@data$id, color="#6cb5bc", weight=5,opacity=1)
      }
    })
  })