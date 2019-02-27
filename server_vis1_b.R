vis="vis1"
source("Global.R")

#mystats <- c("All stat areas (may take a moment)",sort(unique(mydata$STAT_AREA)))
#mynmfsall <- c("All (may take a moment)",sort(unique(mydata$NMFSAREA)))
mystats <- sort(unique(mydata$STAT_AREA))
mynmfsall <- sort(unique(mydata$NMFSAREA))


server_vis1 <- function(input, output) {
  
  #----------------
  #  Data by statistical area
  #----------------
  
  #datastat <- reactive({
  #  req(input$statarealist)
    
    #if (input$statarealist == "All stat areas (may take a moment)" | is.null(input$statarealist)) {
    #  weeklyfilt2 <- quote(STAT_AREA != "@?><")
    #} else if (is.null(input$statarealist)) {
    #  weeklyfilt2 <- quote(STAT_AREA != "@?><")
    #} else {
    #  weeklyfilt2 <- quote(STAT_AREA == input$statarealist)
    #}
    
  #})
  #renderPrint({input$statarealist})
    
  daily_stat <- reactive({ # Make an if then statement that just reads mydata if select_all happens

    mydata %>%
      filter(STAT_AREA%in%input$statarealist)
    
  })
  
  weekly_stat <- reactive({
    mydata %>%
      filter(STAT_AREA%in%input$statarealist) %>% 
    group_by(week,year,STAT_AREA) %>% 
      summarise(sst.sd=round(sd(sst.mean,na.rm=TRUE),2),
                sst.mean=round(mean(sst.mean,na.rm=TRUE),2),
                `date week start`=min(date)) %>% 
      inner_join(mydata.extra)
  })
  
  monthly_stat <- reactive({
    mydata %>%
      filter(STAT_AREA%in%input$statarealist) %>% 
      group_by(month,year,STAT_AREA) %>% 
      summarise(sst.sd=round(sd(sst.mean,na.rm=TRUE),2),
                sst.mean=round(mean(sst.mean,na.rm=TRUE),2),
                `date month start`=min(date)) %>% 
      inner_join(mydata.extra)
  })
  
  #  Use the drop down to determine whether to view the daily, weekly, or monthly plot
  stattableInput <- reactive({
    switch(input$mystattable,
           "Daily"=daily_stat(),
           "Weekly average"=weekly_stat(),
           "Monthly average"=monthly_stat())
  })
  
  output$table1 <- renderDataTable({
    stattableInput()},options=list(pageLength=10)
  )
  
  # Downloadable csv of selected dataset ----
  output$download_stat_area_csv <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(stattableInput(), file, row.names = FALSE)
    }
  )
  
  # Downloadable RDS of selected dataset ----
  output$download_stat_area_RDS <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(stattableInput(), file=file)
    }
  )
  
  #---------------------------------------------------------------------
  #  Data by NMFS Area
  #---------------------------------------------------------------------
  
  #datanmfs <- reactive({
  #  req(input$nmfsarealist)
    #req(input$stat6list)
    
  #  if(input$nmfsarealist == "All (may take a moment)" | is.null(input$nmfsarealist)) {
  #    filt1 <- quote(NMFSAREA != "@?><")
  #  } else if (is.null(input$nmfsarealist)) {
  #    filt1 <- quote(NMFSAREA != "@?><")
  #  } else {
  #    filt1 <- quote(NMFSAREA == input$nmfsarealist) 
  #  }
    
  #})
  
  daily_data <- reactive({
    mydata %>%
      filter(NMFSAREA%in%input$nmfsarealist) %>% 
      group_by(date,NMFSAREA) %>% 
      summarise(sst.sd=round(sd(sst.mean,na.rm=TRUE),2),
                sst.mean=round(mean(sst.mean,na.rm=TRUE),2),
                year=year[1]) %>% 
      inner_join(mydata.extra %>% 
                   dplyr::select(-c(STATEFED,STAT_AREA,minlon,minlat,maxlon,maxlat,m.depth,sd.depth,year)) %>% 
                   distinct()) %>% 
      inner_join(nmfsdat) 
  })
  
  weekly_data <- reactive({
    mydata %>%
      filter(NMFSAREA%in%input$nmfsarealist) %>% 
      group_by(year,NMFSAREA,week) %>% 
      summarise(sst.sd=round(sd(sst.mean,na.rm=TRUE),2),
                sst.mean=round(mean(sst.mean,na.rm=TRUE),2),
                `date week start`=min(date)) %>% 
      inner_join(mydata.extra %>% 
                   dplyr::select(-c(STATEFED,STAT_AREA,minlon,minlat,maxlon,maxlat,m.depth,sd.depth,year))) %>% 
      inner_join(nmfsdat)
    })
  
  monthly_data <- reactive({
    mydata %>%
      filter(NMFSAREA%in%input$nmfsarealist) %>% 
      group_by(month,year,NMFSAREA) %>% 
      summarise(sst.sd=round(sd(sst.mean,na.rm=TRUE),2),
                sst.mean=round(mean(sst.mean,na.rm=TRUE),2),
                `date month start`=min(date)) %>% 
      inner_join(mydata.extra %>% 
                   dplyr::select(-c(STATEFED,STAT_AREA,minlon,minlat,maxlon,maxlat,m.depth,sd.depth,year))) %>% 
      inner_join(nmfsdat)
  })
  
  #  Use the drop down to determine whether to view the daily, weekly, or monthly plot
  tableInput <- reactive({
    switch(input$mynmfstable,
           "Daily"=daily_data(),
           "Weekly average"=weekly_data(),
           "Monthly average"=monthly_data())
  })
  
  output$table2 <- renderDataTable({
    tableInput()},options=list(pageLength=10)
  )
  
  # Downloadable csv of selected dataset ----
  output$download_nmfs_area_csv <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tableInput(), file, row.names = FALSE)
    }
  )
  
  # Downloadable RDS of selected dataset ----
  output$download_nmfs_area_RDS <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(tableInput(), file=file)
    }
  )
  
  
}