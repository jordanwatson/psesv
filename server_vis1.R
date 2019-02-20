
source("Global.R")

mystats <- c("All",sort(unique(mydata$STAT_AREA)))
mynmfsall <- c("All",sort(unique(mydata$NMFSAREA)))

mydata.extra <- mydata %>% 
  dplyr::select(-c(sst.mean,sst.sd,date,month,week,julian,date)) %>% 
  distinct()

weekly <- mydata %>% 
  group_by(week,year,STAT_AREA) %>% 
  summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
            sst.sd=sd(sst.mean,na.rm=TRUE)) %>% 
  inner_join(mydata.extra)

monthly <- mydata %>% 
  group_by(month,year,STAT_AREA) %>% 
  summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
            sst.sd=sd(sst.sd,na.rm=TRUE)) %>% 
  inner_join(mydata.extra)

server_vis1 <- function(input, output) {
  
  #----------------
  #  Daily data
  #----------------
  data <- reactive({
    req(input$nmfsarealist)
    req(input$stat6list)
    
    if(input$nmfsarealist == "All" | is.null(input$nmfsarealist)) {
      filt1 <- quote(NMFSAREA != "@?><")
    } else if (is.null(input$nmfsarealist)) {
      filt1 <- quote(NMFSAREA != "@?><")
    } else {
      filt1 <- quote(NMFSAREA == input$nmfsarealist) 
    }
    
    if (input$stat6list == "All" | is.null(input$stat6list)) {
      filt2 <- quote(STAT_AREA != "@?><")
    } else if (input$stat6list == "All") {
      filt2 <- quote(STAT_AREA != "@?><")
    } else {
      filt2 <- quote(STAT_AREA == input$stat6list)
    }
    
    mydata %>%
        filter_(filt1) %>%
        filter_(filt2)
  })
  
  output$table1 <- renderDataTable({
    data()},options=list(pageLength=10)
  )
  
  # Downloadable csv of selected dataset ----
  output$downloadData_csv <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Downloadable RDS of selected dataset ----
  output$downloadData_RDS <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(data(), file=file)
    }
  )
  
  
  #----------------
  #  Weekly data
  #----------------
  
  data.weekly <- reactive({
    req(input$nmfsarealist2)
    req(input$stat6list2)
    
    if(input$nmfsarealist2 == "All" | is.null(input$nmfsarealist2)) {
      weeklyfilt1 <- quote(NMFSAREA != "@?><")
    } else if (is.null(input$nmfsarealist2)) {
      weeklyfilt1 <- quote(NMFSAREA != "@?><")
    } else {
      weeklyfilt1 <- quote(NMFSAREA == input$nmfsarealist2) 
    }
    
    if (input$stat6list2 == "All" | is.null(input$stat6list2)) {
      weeklyfilt2 <- quote(STAT_AREA != "@?><")
    } else if (input$stat6list2 == "All") {
      weeklyfilt2 <- quote(STAT_AREA != "@?><")
    } else {
      weeklyfilt2 <- quote(STAT_AREA == input$stat6list2)
    }
    
    weekly %>%
        filter_(weeklyfilt1) %>%
        filter_(weeklyfilt2)
  })
  
  
  output$table2 <- shiny::renderDataTable({
    data.weekly()},options=list(pageLength=10)
  )
  
  # Downloadable csv of selected dataset ----
  output$downloadWeekly_csv <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.weekly(), file, row.names = FALSE)
    }
  )
  
  # Downloadable RDS of selected dataset ----
  output$downloadWeekly_RDS <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(data.weekly(), file=file)
    }
  )
  
  #----------------
  #  Monthly data
  #----------------
  
  data.monthly <- reactive({
    req(input$nmfsarealist3)
    req(input$stat6list3)
    
    if(input$nmfsarealist3 == "All" | is.null(input$nmfsarealist3)) {
      monthfilt1 <- quote(NMFSAREA != "@?><")
    } else if (is.null(input$nmfsarealist3)) {
      monthfilt1 <- quote(NMFSAREA != "@?><")
    } else {
      monthfilt1 <- quote(NMFSAREA == input$nmfsarealist3) 
    }
    
    if (input$stat6list3 == "All" | is.null(input$stat6list3)) {
      monthfilt2 <- quote(STAT_AREA != "@?><")
    } else if (input$stat6list3 == "All") {
      monthfilt2 <- quote(STAT_AREA != "@?><")
    } else {
      monthfilt2 <- quote(STAT_AREA == input$stat6list3)
    }
    
    (monthly %>%
        filter_(monthfilt1) %>%
        filter_(monthfilt2))
  })
  
  
  output$table3 <- shiny::renderDataTable({
    data.monthly()},options=list(pageLength=10)
  )
  
  # Downloadable csv of selected dataset ----
  output$downloadMonthly_csv <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.monthly(), file, row.names = FALSE)
    }
  )
  
  # Downloadable RDS of selected dataset ----
  output$downloadMonthly_RDS <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(data.monthly(), file=file)
    }
  )
}

