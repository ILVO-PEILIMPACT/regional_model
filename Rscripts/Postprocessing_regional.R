# SWAP-WOFOST model for Flanders in the context of the project PEILIMPACT
# Flanders Research Institute for Agriculture, Fisheries and Food (ILVO) & Wageningen Environmental Research
# Authors: Diana Estrella <diana.estrella@ilvo.vlaanderen.be>, Martin Mulder <Martin2.Mulder@wur.nl>

#This code creates plots of the model results, regional analysis

setwd("C:/SWAP/Regional/model")

# ---- load libraries ----

source("./libraries/libraries.R")
source("./Rscripts/functions.R")

message(str_c("\nAnalyse results...\n"))

crop = "sugar_beet" # specify one of the 5 crops
dir_run <- "./output/sugar_beet"
file_sql<-"./database/regional_crop11_ind.sqlite"

dir_met <- "./meteo"
START <- as_date("1990-01-01")
END <-  as_date("2021-12-31")
file_soil_model<-"./soil_classification.csv"# soil texture based on the textural percentages and the Belgian soil classification system
file_statbel <- "./yields_statbel20122021.csv" #Avg yield for Flanders from STATBEL
years<- c(2018, 2015, 2021) # Critical years

#Read the processed model results
data_procc <- read_csv (file=str_c(dir_run,"/data_processing_all.csv"), show_col_types = FALSE, progress = FALSE )

#Precipitation deficit
def_prec<-prec_deficit_avg(file_sql=file_sql, dir_met=dir_met, start=START, end=END, discr="yearly")

def_prec_map<-prec_deficit(file_sql=file_sql, dir_met=dir_met, start=START, end=END, discr="yearly")%>%
  mutate(meteo_id=as.numeric(meteo_id))

#Yield data
yield_data<-data_procc%>%
  mutate(
    Y_pot = hrvpotbio/1000,
    Y_act = (Y_pot*(100-dmgtot)/100), #Actual yield considering direct and indirect effects
    year=as.integer(year)
  )

#Average yield
yield_data_avg <- get_yield_avg(yield_data=yield_data)

#Include avg observed yield based on STATBEL 2012-2021, and for grass based on ilvo varieties (see report chapter 1)
obs_stats<-read_csv(file = file_statbel, show_col_types = FALSE, progress=FALSE)%>%
  filter(Crop==crop)

yield_data_avg<-left_join(yield_data_avg, obs_stats, by="year")

#Soil texture
db_info_model<-get_soil_yield_model(file_sql=file_sql, file_soil=file_soil_model, yield_data=yield_data)


#Database for plotting maps
conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
name<-dbListTables(conn)
db_tmp <- as_tibble(dbReadTable(conn = conn, name = "Runs"))%>%
  dplyr::select(run_id, soil_id, meteo_id, x_crd, y_crd)

dbDisconnect(conn = conn)

yield_data_map<-left_join(yield_data, db_tmp, by="run_id")
yield_data_map <-left_join(yield_data_map, def_prec_map, by=c("meteo_id", "year"))
yield_data_map <-left_join(yield_data_map, soil_texture, by=c("soil_id"))
write_csv(file=str_c(dir_run,"/data_maps.csv"), x=yield_data_map, progress=FALSE)

yield_data_map<-read_csv(file=str_c(dir_run,"/data_maps.csv"), show_col_types = FALSE, progress=FALSE)

#Yield according to GWL

yield_gwl <- yield_data_map%>%
  group_by(run_id)%>%
  mutate(
    gwl_avg = mean(ghg+glg)/2,
    Y_act_avg = mean(Y_act),
    dmgtot_avg=mean(dmgtot),
    dmgdir_avg=mean(dmgdir),
    dmgwet_avg=mean(dmgwet),
    dmgdry_avg=mean(dmgdry)
  )%>%
  select(run_id, gwl_avg, Y_act_avg, dmgtot_avg, dmgdir_avg, dmgwet_avg, dmgdry_avg, soil_texture)%>%
  unique()

#Yield according to GWL in a dry, normal and wet years:2018, 2015, 2021
yield_gwl_crit <- yield_data_map%>%
  filter(year %in% years)%>%
  group_by(run_id, year)%>%
  mutate(
    gwl_avg = mean(ghg+glg)/2,
  )%>%
  select(run_id, year, gwl_avg, Y_act, dmgtot, dmgdir, dmgwet, dmgdry, soil_texture)%>%
  unique()

#PLOTS

if (!file.exists("../plots")){
  dir.create("../plots")
}

size=20

#Yearly average yield

yield<-c("Potential", "Actual", "STATBEL")
plot_yield_avg <-ggplot(data=yield_data_avg)%>%
  + geom_col(aes(x = as.character(year), y = Y_pot_avg, fill = yield[1]), width = 0.5) %>% 
  + geom_col(aes(x = as.character(year), y = Y_act_avg, fill = yield[2]), width = 0.5) %>% 
  + geom_line(aes(x = as.character(year), y = DMY, group=1, color=yield[3]), linetype="dashed")%>%
  + scale_fill_manual(values=c("Potential"="azure3","Actual"= "lightseagreen"))%>%
  + scale_color_manual(values=c("STATBEL"="red"))%>%
  + coord_cartesian(xlim =c(NA, NA), ylim = c(4, NA))%>%
  + labs(x="", y = "Dry matter yield (ton/ha)", fill="", color="") %>%
  + theme_light()%>%
  + theme(text=element_text(size=size), legend.position="top")%>%
  + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

ggsave(filename = str_c("../plots/average_yield_", crop, ".png"), plot =plot_yield_avg, width = 10, height = 6)


#Transpiration reduction

yield_stack<-yield_data_avg%>%
  select(year, dmgdry_avg, dmgwet_avg,dmgind_avg)%>%
  rename(
    indirect=dmgind_avg,
    wet = dmgwet_avg,
    dry = dmgdry_avg
  )

yield_reduction<-melt(yield_stack, id.var = c('year'), variable.name = 'yield_red')

Transp <- ggplot(data = yield_reduction) %>%
  + geom_col(aes(x = as.character(year), y = value, fill = yield_red), alpha=0.8 , width = 0.5) %>%
  + scale_fill_manual(values=c("dry"="red3", "wet"="dodgerblue3","indirect"="navajowhite3"))%>%
  + labs(x="", y = "Average yield reduction(%)", fill = "Stress type") %>%
  + theme_light()%>%
  + theme(text=element_text(size=size),legend.position="top")%>%
  + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

ggsave(filename = str_c("../plots/avg_yield_reduction_", crop, ".png"), plot =Transp, width = 10, height = 6)

#Comparison with soil texture
# Average yield

plot_soil <-ggplot(data=db_info_model)%>%
  + geom_col(aes(x = as.character(soil_texture), y = Y_act_avg, fill = yield[2]), width =0.6) %>% 
  + scale_fill_manual(values=c("Actual"= "lightseagreen"))%>%
  + labs(x="", y = "Average dry matter yield (ton/ha)", fill="") %>%
  + theme_light()%>%
  + theme(text=element_text(size=size), legend.position = "none")%>%
  + scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 8))

ggsave(filename = str_c("../plots/yield_soil_",crop, ".png"), plot =plot_soil, width = 10, height = 6)

 #Average yield reduction
soil_stack<-db_info_model%>%
  select(soil_texture, dmgdry_avg, dmgwet_avg,dmgind_avg)%>%
  rename(
    indirect=dmgind_avg,
    wet = dmgwet_avg,
    dry = dmgdry_avg
  )

yield_reduction_soil<-melt(soil_stack, id.var = c('soil_texture'), variable.name = 'yield_red')

plot_soil_red <-ggplot(data=yield_reduction_soil)%>%
  + geom_col(aes(x = as.character(soil_texture), y = value, fill = yield_red), width=0.6) %>% 
  + scale_fill_manual(values=c("dry"="red3", "wet"="dodgerblue3","indirect"="navajowhite3"))%>%
  + labs(x="", y = "Average yield reduction (%)", fill = "") %>%
  + theme_light()%>%
  + theme(text=element_text(size=size), legend.position="top")%>%
  + scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 8))


ggsave(filename = str_c("../plots/yield_soil_red",crop, ".png"), plot =plot_soil_red, width = 10, height = 6)


#Yearly average precipitation deficit

plot0<-ggplot(def_prec)%>%
  + geom_col( aes(x = as.factor(YYYY), y = P_ET_avg), fill="dodgerblue3")%>% 
  + labs(x="", y = "Average P-ETref (mm)") %>%
  + theme_light()%>%
  + theme(text=element_text(size=size), 
          panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(), 
          axis.ticks=element_line(color ="black") 
          )%>%
  + scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

ggsave(filename = str_c("../plots/prec_deficitv2.png"), plot =plot0, width = 10, height = 6)


#Yield according to gwl

plot_gwl <-ggplot(data=yield_gwl, aes(x = -1*(gwl_avg), y = Y_act_avg))%>%
  + geom_point(aes(color= soil_texture), alpha=0.5, size=0.8) %>%
  + geom_smooth(aes(color= soil_texture), se=FALSE) %>%
  + labs(x="Average groundwater level(cm)", y = "Average dry matter yield (ton/ha)", color="Soil texture") %>%
  + scale_x_continuous(limits = c(0,500), expand = c(0, 0))%>%
  + theme_light()%>%
  + theme(text=element_text(size=size))%>%
  + guides(color = guide_legend(override.aes = list(size=2)))

ggsave(filename = str_c("../plots/gwl_yield_",crop, ".png"), plot =plot_gwl, width = 10, height = 6)

#Critical years
plot_gwl_crit <-ggplot(data=yield_gwl_crit, aes(x = -1*(gwl_avg), y = Y_act))%>%
  + geom_point(aes(color= as.factor(year)), alpha=0.1, size=0.8) %>%
  + geom_smooth(aes(color=as.factor(year)), se=FALSE)%>%
  + labs(x="Average groundwater level(cm)", y = "Dry matter yield (ton/ha)", color="Year") %>%
  + scale_x_continuous(limits = c(0,500), expand = c(0, 0))%>%
  + scale_y_continuous(limits = c(0,10), expand = c(0, 0))%>%
  + theme_light()%>%
  + theme(text=element_text(size=size))%>%
  + guides(color = guide_legend(override.aes = list(size=2)))

ggsave(filename = str_c("../plots/gwl_yield_crit",crop, ".png"), plot =plot_gwl_crit, width = 10, height = 6)


#Analysis of all crops together-------------

myData<-read_csv(file="./output/data_all_maps.csv", show_col_types = FALSE, progress=FALSE)
db_all<-read_csv(file="./output/spatial_variation.csv", show_col_types = FALSE, progress=FALSE)
yield_avg_all<-read_csv(file="./output/temporal_variation.csv", show_col_types = FALSE, progress=FALSE)

#Rename crop names
db_all_tmp<-db_all
db_all_tmp$crop<-factor(db_all_tmp$crop, labels=c( "potato","grass","silage maize", "sugar beet", "winter wheat")) #pay attention to the rename of crops

yield_avg_all_tmp<- yield_avg_all
yield_avg_all_tmp$crop<-factor(yield_avg_all_tmp$crop, labels=c( "potato","grass","silage maize", "sugar beet", "winter wheat")) #pay attention to the rename of categories

#Include avg observed yield based on STATBEL 2012-2021, and for grass based on ilvo varieties (see report chapter 1)
obs_stats<-read_csv(file = file_statbel, show_col_types = FALSE, progress=FALSE)

yield_avg_obs<-left_join(yield_avg_all_tmp, obs_stats, by=c("year", "crop"))

yield_avg_obs<-yield_avg_obs%>%
  mutate(crit=if_else(year==2015|year==2018|year==2021, 1,0))

#Average yield
yield<-c("Potential", "Actual", "STATBEL")
plot_avg_yield_all<-ggplot(data = yield_avg_obs)%>%
  + geom_col(aes(x = as.character(year), y = Y_pot_avg, fill = yield[1], alpha=crit==1), width = 0.5) %>% 
  + geom_col(aes(x = as.character(year), y = Y_act_avg, fill = yield[2], alpha=crit==1), width = 0.5) %>% 
  + geom_line(aes(x = as.character(year), y = DMY, group=1, color=yield[3]), linetype="dashed")%>%
  + scale_fill_manual(values=c("Potential"="azure3","Actual"= "lightseagreen"))%>%
  + scale_color_manual(values=c("STATBEL"="red"))%>%
  + scale_alpha_manual(values=c(0.4,1), guide = "none")%>%
  + labs(x="", y = "Average dry matter yield (ton ha ^{-1})", fill="", color="", alpha="") %>%
  + theme_light()%>%
  + theme(
    text=element_text(size=size), 
    legend.position="top",
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14, color = "black"),
    strip.text.y = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color="grey"),
    strip.placement="outside",
  )%>%
  + scale_x_discrete(guide = guide_axis(check.overlap = TRUE), expand = c(0, 1))%>%
  + facet_grid(rows= vars(factor(crop, levels=c("grass", "silage maize", "potato", "winter wheat", "sugar beet"))), scales = "free") 


#Transpiration reduction
yield_stack_all<-yield_avg_obs%>%
  mutate(dmgind_avg=dmgtot_avg-(dmgdry_avg+dmgwet_avg))%>%
  select(year, crit, crop, dmgdry_avg, dmgwet_avg,dmgind_avg)%>%
  rename(
    indirect=dmgind_avg,
    wet = dmgwet_avg,
    dry = dmgdry_avg
  )

yield_red_all<-tibble(melt(yield_stack_all, id.var = c('year', 'crop', 'crit'), variable.name = 'yield_red'))

transp_all <- ggplot(data = yield_red_all) %>%
  + geom_col(aes(x = as.character(year), y = value, fill = yield_red, alpha=crit==1) , width = 0.5) %>%
  + scale_fill_manual(values=c("dry"="red3", "wet"="dodgerblue3","indirect"="navajowhite3"))%>%
  + scale_alpha_manual(values=c(0.4,0.8), guide = "none")%>%
  + labs(x="", y = "Average yield reduction (%)", fill = "Stress type") %>%
  + theme_light()%>%
  + theme(
    text=element_text(size=size),
    legend.position="top",
    strip.background = element_blank(), 
    strip.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )%>%
  + scale_x_discrete(guide = guide_axis(check.overlap = TRUE), expand = c(0, 1))%>%
  + facet_grid(rows= vars(factor(crop, levels=c("grass", "silage maize", "potato", "winter wheat", "sugar beet"))), switch  = "y")

#Figure 3 chapter "Regional analysis & plausibility check"
yield_red_all<-ggarrange( plot_avg_yield_all, transp_all,
                          nrow = 1,
                          ncol = 2)

ggsave(filename = str_c("../plots/yield_red_all.png"), plot =yield_red_all, width = 10, height = 10)

#Correlation between groundwater levels, yield and soil type 
db_all_tmp<-myData%>%
  filter(year%in% years)%>%
  mutate(gwl_avg=(glg+ghg)/2)

db_all_tmp$crop<-factor(db_all_tmp$crop, labels=c( "potato","grass","silage maize", "sugar beet", "winter wheat")) #pay attention to the rename of categories

#Figure 6 chapter "Regional analysis & plausibility check"
plot_gwl_all <-ggplot(data=db_all_tmp, aes(x = -1*(gwl_avg/100), y = Y_act))%>% 
  + geom_point(aes(color= soil_texture),  size=0.3, alpha=0.03)%>%
  + geom_smooth(aes(color= soil_texture), se=FALSE, linewidth=0.6) %>%
  + scale_color_manual(values=c("clay (E)"="orange3",
                                "heavy clay (U)"="orange4", 
                                "light sandy loam (P)"="seagreen2", 
                                "sandy loam (L)"="seagreen3", 
                                "sand (Z)"="seagreen4", 
                                "loam (A)"="maroon3", 
                                "loamy sand (S)"="maroon4")
  )%>%
  + labs(x="Average groundwater depth (m)", y = "Dry matter yield (ton ha ^{-1})", color="Soil texture") %>%
  + scale_x_continuous(limits = c(0,5), expand = c(0, 0))%>%
  + scale_y_continuous(limits = c(0,NA))%>%
  + theme_light()%>%
  + theme(
    text=element_text(size=size),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14, color = "black"),
    strip.text.y = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color="grey"),
    strip.placement="outside",
    panel.spacing.x =unit(1,"lines"),
  )%>%
  + facet_grid(rows= vars(factor(crop, levels=c("grass", "silage maize", "potato", "winter wheat", "sugar beet"))), cols=vars(year), scales = "free")

plot_gwl_all 

ggsave(filename = str_c("../plots/yield_gwl_all.png"), plot =plot_gwl_all, width = 10, height = 8)

message(str_c("\nDone! \n"))





