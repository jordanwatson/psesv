
source("Global.R")

data2 <- mydata %>% 
  mutate(newyr=ifelse(month<4,year-1,year),
                        month2=ifelse(month<4,month+12,month),
                        season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                        monthname=month.name[month]) 

mynmfs <- sort(unique(data2$NMFSAREA))


#----------------------------------------------------
#  Multi-panel layout
#----------------------------------------------------

server_vis3 <- shinyServer(function(input, output){
  ## values <- reactiveValues()  # unused
  ## Your data should be reactive - and reference `input` 
  ## to get user-entered values
  rxDataMonthly <- reactive({
    dat <- data2 %>% 
      filter(m.depth>=input$range_month[1] & m.depth<=input$range_month[2] & NMFSAREA%in%c(input$dnmfs_month) & monthname==input$dmonth) %>% 
      group_by(NMFSAREA,month,year) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE))
    dat
  })
  
  output$monthly_nmfs_plot <- renderPlot({
    req(input$dnmfs_month)
    req(input$dmonth)
    dataset <- rxDataMonthly()  # this is the subsetted data
    p <- ggplot(dataset, aes(x = year, y = tempanom, fill=factor(NMFSAREA))) +
      geom_bar(stat="identity",position="dodge") + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      geom_hline(yintercept=c(-0.5,0.5),linetype=2) + 
      xlab("Year") + 
      ylab("Temperature Anomaly (SD)") + 
      ggtitle(paste(input$dmonth)) + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14),
            plot.title = element_text(size=16),
            strip.text = element_text(size=14)) + 
      scale_x_continuous(breaks = 2003:2018, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017",""))
    print(p)
  })
  
  rxDataSeasonal <- reactive({
    #req(input$dnmfs)
    dat <- data2 %>% 
      filter(m.depth>=input$range[1] & m.depth<=input$range[2] & newyr>2002 & newyr<2018 & NMFSAREA%in%c(input$dnmfs)) %>% 
      group_by(NMFSAREA,season,newyr) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA,season) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE))
    dat
  })
  
  output$seasonal_nmfs_plot <- renderPlot({
    req(input$dnmfs)
    datasetSeasonal <- rxDataSeasonal()  # this is the subsetted data
    p <- ggplot(datasetSeasonal, aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
      geom_bar(stat="identity",position="dodge") + 
      facet_wrap(~season) + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      geom_hline(yintercept=c(-0.5,0.5),linetype=2) + 
      xlab("Year") + 
      ylab("Temperature Anomaly (SD)") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14),
            strip.text = element_text(size=14)) + 
      scale_x_continuous(breaks = 2003:2018, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017",""))
    print(p)
  })
})

