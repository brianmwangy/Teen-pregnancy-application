library(dplyr)
library(readr)
library(zoo)
library(tidyr)
library(rgdal)
library(raster)


#loading national teen pregnancy stats
national<-read_csv("./www/nation.csv")
national$period<-as.yearqtr(national$period,"%Y Q%q")

#loading ward teen pregnancy stats
wardf<-read_csv("./www/wardf2.csv")
wardf$period<-as.yearqtr(wardf$period,"%Y Q%q")
yr<-unique(wardf$period)
cnty<-unique(wardf$county)
preg<-unique(wardf$Type)

#loading county stats
county<-read_csv("./www/county.csv")
county$period<-as.yearqtr(county$period,"%Y Q%q")
cnty2<-unique(county$county)


#loading subcounty stats
subcounty<-read_csv("./www/subcounty.csv")
subcounty$period<-as.yearqtr(subcounty$period,"%Y Q%q")
cnty3<-unique(subcounty$county)
preg2<-unique(subcounty$type)

#loading the kenya basemap
bsmap<-readOGR("./www/year2.shp")



