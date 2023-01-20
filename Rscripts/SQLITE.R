# SWAP-WOFOST model for Flanders in the context of the project PEILIMPACT
# Flanders Research Institute for Agriculture, Fisheries and Food (ILVO) & Wageningen Environmental Research
# Authors: Diana Estrella <diana.estrella@ilvo.vlaanderen.be>, Martin Mulder <Martin2.Mulder@wur.nl> 

#This code creates the sqlite database for regional analysis, assuming a homogeneous crop
#------------------------------------------------

# ---- arguments ----

# set modus
test <- TRUE

# load arguments
if (!test) {
  command_args <- commandArgs(trailingOnly = TRUE)
  if (length(command_args) != 1) {
    stop("wrong usage of script, arguments should be:\n- controlfile")
  }
} else {
  command_args <- NULL
  command_args[1] <- "control_regional_crop.inp"
  setwd("C:/SWAP/Regional/model")
}

# set arguments
ctrl <- command_args[1]

# ---- load libraries ----

source("./libraries/libraries.R")
source("./Rscripts/functions.R")


indirect<-TRUE # Indirect effects

message("\nProgram R-PROG started...\n")


# ---- initial part of procedure ----
#zonal layer

file_asc <- "./maps/JRC_meteo.asc"  # Reference map of Flanders, 500 m resolution

#Get dataframe of runs
db_tmp <- get_runs_regional(file_asc=file_asc, ctrl=ctrl)

#Simulation period
START <- as_date(get_record(file = ctrl, item = "START"))
END <- as_date(get_record(file = ctrl, item = "END")) 


#Dynamic information
# Generate groundwater levels time series based on maps (Sumaqua)

GWL <-get_groundwater (database=db_tmp, START=START, END=END)
write_csv (x=GWL, file= "./input_data/grondwater.csv")

#Or read groundwater levels previously generated
GWL<-read_csv(file="./input_data/grondwater.csv", show_col_types = FALSE, progress=FALSE)


# Initial soil moisture is assumed equal to the GWL at the start of simulation 
GWLI <-GWL%>%
  group_by(gw_id)%>%
  filter (DATE5 == START)%>% 
  mutate(scenario_id="direct")%>%
  mutate(GWLI = HBOT5-600)%>% #To transform again to GWL
  select(scenario_id, gw_id, GWLI)

write_csv (x=GWLI, file= "./input_data/initial_con.csv", progress=FALSE)

if (indirect){
  GWL_ind <- tibble(scenario_id = "indirect",DATE5 = c(START, END), HBOT5=100.0)
  GWLI_ind <- tibble(scenario_id = "indirect", GWLI = -500.0)
  
  write_csv(x=GWL_ind, file="./input_data/grondwater_ind.csv", progress=FALSE)
  write_csv(x=GWLI_ind, file="./input_data/initial_con_ind.csv", progress=FALSE)
}


# Main dataframe "Runs"

db <- get_main_database (database=db_tmp)


#To change the crop
# 
# db<-db%>%
#   mutate(crop_id=6)

#Sqlite database

# Reading input data 
input_data <- c(
  "discretisatie", 
  "eigenschappen", 
  "gewasontwikkeling",
  "gewaskalender",
  "gewasweerstand",
  "grasverlies",
  "grondwater",
  "grondwater_ind",
  "initial_con",
  "initial_con_ind",
  "irrigatie",
  "management",
  "output",
  "scenario",
  "meteo",
  "wortelzone"
)

#Create sqlite database and save it in the folder "database"

create_sqlite (list_data= input_data, database=db, START=START, END=END)

