#mb.Rdata has 
#mb_stations - with Station, Lat, Lon, ID, Tsm, Ssm, Tbm, Sbm
# Where sm is mean of surface T or S
#mb_model is list [1:ID] of dataframes 
# each has model data Date,Tsurf,Tbot,Ssurf,Sbot
#setwd("~/R_apps/shiny-mb")
load("mb_obs.Rdata")
load("mb_model.Rdata")
load("mb_stations.Rdata")