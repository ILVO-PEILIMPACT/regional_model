# SWAP-WOFOST model for Flanders in the context of the project PEILIMPACT
# Author: Diana Estrella <diana.estrella@ilvo.vlaanderen.be>

# This script creates yield and p-et maps in Flanders

setwd("C:/SWAP/Regional/model")

#Loading libraries
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(ggspatial)
library(RColorBrewer)
library(plyr)
source("./libraries/libraries.R")

#Data
dir_run <- "./output"
file_name<-str_c(dir_run,"/data_all_maps.csv")
crop="all"
years<-c(2015,2018,2021)

#Reading shapefiles
# river_shp <-"C:/SWAP/Meteorologicaldata/rivers/rivers_flanders.shp"
# shp <- readOGR(dsn = river_shp, stringsAsFactors = F)
# shp_db<-fortify(shp)


#yield maps all

get_map_all <- function(file, crop, years) {
  
  #Plot the map
  size=15
  values<-c(0, 25,50,75,100) #to show in the legend
  
  map<-ggplot(data = df)%>%
    + geom_point (aes(x=x_crd, y=y_crd, color=dmgtot), size=1)%>%
    # + geom_path(data=shp_db, aes(x=long, y=lat, group=group), color="grey")%>%
    + coord_sf(datum = st_crs(31370))%>%
    + scale_color_gradient2(name="Yield reduction (%)", low = "#2b83ba", mid="#ddf1b4", high="#d7191c", midpoint = 50)%>%
    + theme_light()%>%
    + theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'white', colour = 'white'),
            strip.background = element_blank(),
            strip.text = element_text(size = 14, color = "black"),
            panel.border = element_rect(color="grey"),
            # panel.spacing.y =unit(1,"lines"),
            text=element_text(size=size),
            legend.direction="horizontal",
            legend.position="top",
            legend.key.width = unit(1.2, "cm")
    )%>%
    + facet_grid(rows= vars(factor(crop, levels=c("grass", "silage maize", "potato", "winter wheat", "sugar beet"))), cols= vars(year), switch  = "y") 
  map
  ggsave(filename = str_c("../plots/yield_maps/yieldred_all.png"), plot =map, width = 10, height = 14) 
      
}

get_map_all(file = df, crop=crop, years=years)

#Precipitation deficit

get_prec_all <- function(file, years) {
  
  #Plot the map
  size=15
  
  map_prec<-ggplot(data = df)%>%
    + geom_point (aes(x=x_crd, y=y_crd, color=P_ET_yearly), size=0.5)%>%
    + coord_sf(datum = st_crs(31370))%>%
    + scale_color_gradient2(name="P-ETref (mm)", low = "#a6611a", mid="#f5f5f5", high="#018571", midpoint = 0)%>%
    + theme_light()%>%
    + theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'white', colour = 'white'),
            strip.background = element_blank(),
            strip.text = element_text(size = 14, color = "black"),
            panel.border = element_rect(color="grey"),
            text=element_text(size=size),
            legend.direction="horizontal",
            legend.position="top",
            legend.key.width  = unit(1.2, "cm")
    )%>%
    + facet_grid(cols=vars(year)) 
 
  ggsave(filename = str_c("../plots/p-et/precdef_all.png"), plot =map_prec, width = 10, height = 6) 
  
}

get_prec_all(file = df, years=years)

