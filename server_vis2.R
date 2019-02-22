vis="vis2"

source("Global.R")

#data2 <- mydata %>% 
#  mutate(newyr=ifelse(month<4,year-1,year),
#         month2=ifelse(month<4,month+12,month),
#         season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
#         cumwk=(year-2003)*52+week,
#         cummo=(year-2003)*12+month) 

#---------------------------------------------
#  Multi panel
#---------------------------------------------

server_vis2 <- shinyServer(function(input, output) {
  
  rxdata2_daily <- reactive({
    req(input$dstat)
    data2 %>% 
      dplyr::select(date,statlab,STAT_AREA,sst.mean) %>% 
      filter(statlab%in%input$dstat)
    #dat
  })
  
  output$daily_plot <- renderPlot({
    p <- ggplot(rxdata2_daily(), 
                aes(x = date, y = sst.mean,color=factor(statlab)))

    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab(expression("Temperature " ( degree*C))) + 
      scale_color_viridis(discrete=TRUE,
                          guide=guide_legend("Statistical Area",ncol=2)) + 
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
    data2 %>% 
      filter(statlab%in%input$wstat) %>% 
      group_by(cumwk,statlab) %>% 
      summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                date=min(date),
                year=year[1])
  })
  
  output$weekly_plot <- renderPlot({
    p <- ggplot(rxdata2_weekly(), 
                aes(x = date, y = sst.mean,color=factor(statlab)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab(expression("Temperature " ( degree*C))) +
      scale_color_viridis(discrete=TRUE,
                          guide=guide_legend("Statistical Area",ncol=2)) + 
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
    data2 %>% 
      filter(statlab%in%input$mstat) %>% 
      group_by(cummo,statlab) %>% 
      summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                date=min(date),
                year=year[1])
  })
  
  output$monthly_plot <- renderPlot({
    p <- ggplot(rxdata2_monthly(), 
                aes(x = date, y = sst.mean,color=factor(statlab)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab(expression("Temperature " ( degree*C))) +  
      scale_color_viridis(discrete=TRUE,
                          guide=guide_legend("Statistical Area",ncol=2)) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=10),
            axis.title = element_text(size=14))
    print(p)
  })
})

