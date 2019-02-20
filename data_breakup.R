

temperature.dat <- mydata %>% 
  dplyr::select(sst.mean,date,STAT_AREA)

date.dat <- mydata %>% 
  dplyr::select(date,year,month,julian,week)

spatial.dat <- mydata %>% 
  dplyr::select(STAT_AREA,FMP_AREA_C,NMFSAREA,STATEFED,minlon,maxlon,minlat,maxlat,m.depth,sd.depth)

object.size(mydata)
object.size(temperature.dat)+object.size(date.dat)+object.size(temperature.dat)



saveRDS(mydata %>% 
  dplyr::select(sst.mean,date,STAT_AREA),file="Data/temperature_data.RDS")

saveRDS(mydata %>% 
  dplyr::select(date,year,month,julian,week) %>% 
    distinct(),file="Data/date_data.RDS") 

saveRDS(mydata %>% 
  dplyr::select(STAT_AREA,FMP_AREA_C,NMFSAREA,STATEFED,minlon,maxlon,minlat,maxlat,m.depth,sd.depth) %>% 
    distinct(),file="Data/spatial_data.RDS")




system.time({mydata <- readRDS("Data/mur_SST_stat6_all_columns.RDS") %>% 
  dplyr::select(sst.mean,date,everything())})
dim(mydata)
rm(list=ls());gc()

system.time({
  newdata <- readRDS("Data/temperature_data.RDS") %>% 
  left_join(readRDS("Data/date_data.RDS")) %>% 
    left_join(readRDS("Data/spatial_data.RDS"))
})


