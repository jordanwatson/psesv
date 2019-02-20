
source("Global.R")

data2 <- mydata %>% mutate(newyr=ifelse(month<4,year-1,year),
                        month2=ifelse(month<4,month+12,month),
                        season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                        cumwk=(year-2003)*52+week,
                        cummo=(year-2003)*12+month) 

#---------------------------------------------
#  Multi panel
#---------------------------------------------

server_vis2 <- shinyServer(function(input, output) {
  
  rxdata2_daily <- reactive({
    req(input$dstat)
    dat <- data2 %>% 
      filter(STAT_AREA%in%input$dstat)
    dat
  })
  
  output$daily_plot <- renderPlot({
    p <- ggplot(rxdata2_daily() %>% mutate(date=as.POSIXct(date)), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab("Temperature (°C)") + 
      scale_color_viridis(discrete=TRUE,guide=guide_legend("Statistical Area")) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=10),
            axis.title = element_text(size=14))
    print(p)
  })
  
  rxdata2_weekly <- reactive({
    req(input$wstat)
    dat <- data2 %>% 
      filter(STAT_AREA%in%input$wstat) %>% 
      group_by(cumwk,STAT_AREA) %>% 
      summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                date=min(date),
                year=year[1])
    dat
  })
  
  output$weekly_plot <- renderPlot({
    p <- ggplot(rxdata2_weekly() %>% mutate(date=as.POSIXct(date)), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab("Temperature (°C)") + 
      scale_color_viridis(discrete=TRUE,guide=guide_legend("Statistical Area")) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=10),
            axis.title = element_text(size=14))
    print(p)
  })
  
  rxdata2_monthly<- reactive({
    req(input$mstat)
    dat <- data2 %>% 
      filter(STAT_AREA%in%input$mstat) %>% 
      group_by(cummo,STAT_AREA) %>% 
      summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                date=min(date),
                year=year[1])
    dat
  })
  
  output$monthly_plot <- renderPlot({
    p <- ggplot(rxdata2_monthly() %>% mutate(date=as.POSIXct(date)), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab("Temperature (°C)") + 
      scale_color_viridis(discrete=TRUE,guide=guide_legend("Statistical Area")) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=10),
            axis.title = element_text(size=14))
    print(p)
  })
})

shinyApp(ui_vis2, server_vis2)
