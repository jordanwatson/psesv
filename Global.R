library(dplyr)
library(ggplot2)
library(shiny)
library(viridis)
library(rgdal)
library(leaflet)


mydata <- readRDS("Data/mur_SST_stat6_all_columns.RDS") %>% 
  dplyr::select(sst.mean,date,everything())

