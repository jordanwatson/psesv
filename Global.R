library(dplyr)
library(ggplot2)
library(shiny)
library(viridis)
library(rgdal)
library(leaflet)
library(htmltools)
library(shinycssloaders)
library(shinyWidgets)


#mydata <- readRDS("Data/mur_SST_stat6_all_columns.RDS") %>% 
#dplyr::select(sst.mean,date,everything())

if(vis=="vis_table"){
  
  mydata <- readRDS("Data/temperature_data.RDS") %>% 
    left_join(readRDS("Data/date_data.RDS")) %>% 
    left_join(readRDS("Data/spatial_data.RDS") %>% 
                dplyr::select(-statlab)) %>% 
    data.frame %>% 
    mutate(date=as.Date(date))
  
  mydata.extra <- mydata %>% 
    dplyr::select(-c(sst.mean,sst.sd,date,month,week,julian,date)) %>% 
    distinct()
  
  nmfsdat <- readRDS("Data/nmfsarea_coords.RDS")
  
} else if (vis=="vis2"){
  
  spatial <- readRDS("Data/spatial_data.RDS") %>% 
    dplyr::select(STAT_AREA,statlab)
  
  data2 <- readRDS("Data/temperature_data.RDS") %>% 
    dplyr::select(-sst.sd) %>% 
    left_join(readRDS("Data/date_data.RDS") %>% 
                mutate(cumwk=(year-2003)*52+week,
                       cummo=(year-2003)*12+month) %>% 
                dplyr::select(-julian)) %>% 
    left_join(spatial)
  
  #statlabels <- sort(unique(spatial$statlab))
  
} else if (vis=="vis_nmfs"){
  
  data2 <- readRDS("Data/temperature_data.RDS") %>%
    left_join(readRDS("Data/date_data.RDS") %>% 
                dplyr::select(-c(julian,week)) %>% 
                mutate(newyr=ifelse(month<4,year-1,year),
                       month2=ifelse(month<4,month+12,month),
                       season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                       monthname=month.name[month])) %>% 
    left_join(readRDS("Data/spatial_data.RDS") %>% 
                dplyr::select(c(STAT_AREA,NMFSAREA,m.depth))) %>% 
    dplyr::select(-STAT_AREA)

  } else {

    mydata <- readRDS("Data/temperature_data.RDS") %>% 
    dplyr::select(-sst.sd) %>% 
    left_join(readRDS("Data/date_data.RDS") %>% 
                transmute(cumwk=(year-2003)*52+week,
                          cummo=(year-2003)*12+month,
                          date,
                          year))
}

