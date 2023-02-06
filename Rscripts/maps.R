# SWAP-WOFOST model for Flanders in the context of the project PEILIMPACT
# Author: Diana Estrella <diana.estrella@ilvo.vlaanderen.be>

# This script creates yield maps in Flanders

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
file_name<-str_c(dir_run,"/data_maps_agric.csv")
crop="all"
years<-c(2015,2018,2021)


#Read yearly yields of all crops and filter critical years
df<-read_csv(file=file_name, show_col_types = FALSE, progress=FALSE)%>%
  filter(year%in%years)

df$crop<-factor(df$crop, labels=c( "potato","grass","silage maize", "sugar beet", "winter wheat")) #pay attention to the rename of categorie, the rename is done in alphabetical order

#yield maps all

get_map_all <- function(df) {
  
 
  size=15
  values<-c(0, 25,50,75,100) #to show in the legend
  
  year.labs<-c("2015 (normal)", "2018 (dry)", "2021 (wet)")
  names(year.labs)<-unique(df$year)
  
  #Plot the map
  map<-ggplot(data = df)%>%
    + geom_point (aes(x=x_crd, y=y_crd, color=dmgtot), size=1)%>%
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
    + facet_grid(rows= vars(factor(crop, levels=c("grass", "silage maize", "potato", "winter wheat", "sugar beet"))), labeller = labeller (year=year.labs),cols= vars(year), switch  = "y") 

  ggsave(filename = str_c("../plots/yield_maps/yieldred_all.png"), plot =map, width = 10, height = 14) 
      
}

get_map_all(df = df)

#Precipitation deficit

get_prec_all <- function(df) {
  
  size=15
  
  year.labs<-c("2015 (normal)", "2018 (dry)", "2021 (wet)")
  names(year.labs)<-unique(df$year)
  
  #Plot the map
  map_prec<-ggplot(data = df)%>%
    + geom_point (aes(x=x_crd, y=y_crd, color=P_ET_yearly), size=0.5)%>%
    + coord_sf(datum = st_crs(31370))%>%
    + scale_color_gradient2(name="P-ET0 (mm)", low = "#a6611a", mid="#f5f5f5", high="#018571", midpoint = 0)%>%
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

get_prec_all(df = df)

