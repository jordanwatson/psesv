

source("server_vis1.R")
source("ui_vis1.R")
shinyApp(ui_vis1, server_vis1)
rm(list=ls())


source("server_vis2.R")
source("ui_vis2.R")
shinyApp(ui_vis2, server_vis2)
rm(list=ls())


source("server_vis3.R")
source("ui_vis3.R")
shinyApp(ui_vis3, server_vis3)
rm(list=ls())


source("server_vis4.R")
source("ui_vis4.R")
shinyApp(ui_vis4, server_vis4)
rm(list=ls())