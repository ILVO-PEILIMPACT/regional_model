# SWAP-WOFOST model for Flanders in the context of the project PEILIMPACT
# Flanders Research Institute for Agriculture, Fisheries and Food (ILVO) & Wageningen Environmental Research
# Original code received from Martin Mulder <Martin2.Mulder@wur.nl> on March 2022, License: GPL (>= 3)
# Changes for project PEILIMPACT made by Diana Estrella <diana.estrella@ilvo.vlaanderen.be>


#This code runs the model SWAP for an array of simulations, given the Sqlite database 
#------------------------------------------------

# set start of program
TIMPRGSTART <- Sys.time()

# set modus
test <- TRUE

# read arguments
#------------------------------------------------

if (!test) {
  command_args <- commandArgs(trailingOnly = TRUE)
  if (length(command_args) != 1) {
    stop("wrong usage of script, arguments should be:\n- ctrl")
  }
} else {
  command_args <- NULL
  command_args[1] <- "control_regional_crop.inp"
  setwd("C:/SWAP/Regional/model")
}
ctrl <- command_args[1]

# load libraries
#------------------------------------------------

source("./libraries/libraries.R")

# run R-script
#------------------------------------------------
if(!interactive()) pdf(NULL)

message(str_c("\nProgram R-PROG started...\n"))

# ---- pre-process ----

# set directories
dir_run <- get_dir(file = ctrl, item = "DIRRUN")

# set SWAP program and template
command <- get_record(file = ctrl, item = "PRGSWP")
tmplt_swp <- get_record(file = ctrl, item = "FILSWP")
tmplt_dra <- get_record(file = ctrl, item = "FILDRA", item_exists = FALSE)
dir_met <- get_record(file = ctrl, item = "DIRMET")
dir_ini <- get_record(file = ctrl, item = "DIRINI", item_exists = FALSE)
dir_crp <- get_record(file = ctrl, item = "DIRCRP")

# set database
file_sql <- get_record(file = ctrl, item = "FILSQL")

# set run_id
run_id <- get_record(file = ctrl, item = "RUNID")
if (run_id == "ALL") {
  run_id <- get_run_id_SQL(file_sql = file_sql)
} else {
  run_id <- string_sequence(string = run_id)
}


message(str_c("\nSimulate SWAP runs...\n"))

# run SWAP
pb <- progress_bar$new(total = length(run_id), format = " SWAP run: :what (:percent)", clear = TRUE)
for (s_run_id in run_id) {
  
  # prepare swap run
  file_swp <- str_c(dir_run, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0"), "/swap.swp")
  create_SWAP(file_swp = file_swp, file_sql = file_sql, run_id = s_run_id, tmplt_swp = tmplt_swp, dir_met = dir_met, dir_ini = dir_ini, dir_crp = dir_crp, tmplt_dra = tmplt_dra, quiet = TRUE)
  
  # execute swap
  pb$tick(tokens = list(what = s_run_id))
  run_SWAP(command = command, file_swp = file_swp, quiet = TRUE)
  
  # zip required results
  
  zip_SWAP(file_swp = file_swp, glob = "*.swp|*.crp|*.csv")
  
  # rename files
  
  path <- str_c(dir_run, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0"), "/swap.zip")
  new_path <- str_c(dir_run, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0"), ".zip")
  if (controlR::file_exists(file = path, quiet = TRUE)) {
    file_move(path = path, new_path = new_path)
    dir_delete(path = str_c(dir_run, "/run_", formatC(x = s_run_id, format = "d", width = 9, flag = "0")))
  }
}


# ---- return part of procedure ----

TIMPRGEND <- Sys.time()
TIMPRGCALC <- as.numeric(difftime(time1 = TIMPRGEND, time2 = TIMPRGSTART, units = "secs"))
message(paste0("\nProgram R-PROG successfully ended in ", floor(TIMPRGCALC / 60), " minutes and ", ceiling(TIMPRGCALC %% 60), " seconds"))
q(save = "no")