# SWAP-WOFOST model for Flanders in the context of the project PEILIMPACT
# Author: Diana Estrella <diana.estrella@ilvo.vlaanderen.be>

#Functions for generation of Sqlite database and postprocessing of the model results

get_runs <- function (file_coord, ctrl) {
  
  location <- read_csv(file = file_coord, show_col_types = FALSE, progress = FALSE )
  x_crd <- location$x
  y_crd <- location$y
  
  #Input layers from control file
  soil_layer <- get_record(file = ctrl, item = "SOIL")
  gwl_layer <- get_record(file = ctrl, item = "AVGGWL")
  ghg_layer <- get_record(file = ctrl, item = "GHG")
  glg_layer <- get_record(file = ctrl, item = "GLG")
  meteo_layer <- get_record(file = ctrl, item = "METEO")
  
  db <- data.frame(x_crd, y_crd)%>%
    mutate(
      soil_id = get_data_asc(file = soil_layer, x_crd = x_crd, y_crd = y_crd),
      crop_id = get_record(file = ctrl, item = "CROP"),    
      irrigation_id = 0,
      meteo_id = get_data_asc(file = meteo_layer, x_crd = x_crd, y_crd = y_crd),
      GWLEVEL = get_data_asc(file = gwl_layer, x_crd = x_crd, y_crd = y_crd),
      GHG = get_data_asc(file = ghg_layer, x_crd = x_crd, y_crd = y_crd),
      GLG = get_data_asc(file = glg_layer, x_crd = x_crd, y_crd = y_crd),
      scenario_id = "direct"
    )%>%
    filter(soil_id != -9999, meteo_id != -9999, GWLEVEL != -9999)%>%
    mutate(
      run_id = 1:nrow(.),
      gw_id = run_id
    )
  return (db)
}


get_runs_regional <- function (file_asc, ctrl) {
  
  asc <- read_asc(file = file_asc)
  
  db <- asc$data %>% filter(value != asc$nodata) %>%
    mutate(
      x_crd = asc$xmin + (0.5 * asc$dx) + (col - 1) * asc$dx,
      y_crd = asc$ymax - (0.5 * asc$dy) - (row - 1) * asc$dy,
      run_id = 1:nrow(.)
    )%>%
    select(run_id, x_crd, y_crd)
  
  #Input layers from control file
  soil_layer <- get_record(file = ctrl, item = "SOIL")
  gwl_layer <- get_record(file = ctrl, item = "AVGGWL")
  ghg_layer <- get_record(file = ctrl, item = "GHG")
  glg_layer <- get_record(file = ctrl, item = "GLG")
  meteo_layer <- get_record(file = ctrl, item = "METEO")
 
  db <- db %>%
    mutate(
      soil_id = get_data_asc(file = soil_layer, x_crd = x_crd, y_crd = y_crd),
      crop_id = get_record(file = ctrl, item = "CROP"),
      irrigation_id = 0,
      meteo_id = get_data_asc(file = meteo_layer, x_crd = x_crd, y_crd = y_crd),
      GWLEVEL = get_data_asc(file = gwl_layer, x_crd = x_crd, y_crd = y_crd),
      GHG = get_data_asc(file = ghg_layer, x_crd = x_crd, y_crd = y_crd),
      GLG = get_data_asc(file = glg_layer, x_crd = x_crd, y_crd = y_crd),
      scenario_id = "direct"
    )
  db<-db%>%
    filter(soil_id!=-9999, GWLEVEL != -9999)%>%
    mutate(
      run_id = 1:nrow(.),
      gw_id = run_id
    )
 
  return (db)
}

get_groundwater <- function (database, START, END) {
  
  gwl_criteria <- database%>%
    mutate(GWL = if_else(GWLEVEL>3 | GHG < (-500) | GLG < (-500)| GHG < GLG, GWLEVEL, -1))
  # Average groundwater levels are assumed when:
  # GWLEVEL>3 : groundwater levels deeper than 3 m  
  # GHG < (-500) | GLG < (-500): in locations where there is no data (-99999) or when GHG or GLG are deeper 
  # than 500 cm (soil profile is 600 cm)
  # GHG < GLG: locations where there are errors in the maps and GHG is deeper than GLG
  
  #First criteria: average groundwater levels transformed to pressure head (SWBOTB = 5)
  gwl_avg <- gwl_criteria%>%
    filter(GWL != -1)%>%
    mutate(HBOT5 = if_else(GWL<=5,((6-GWL)*100),100.0)) # soil profile has 6 m, max gwl allowed = 5m
  
  start <- gwl_avg %>%
    mutate(
      DATE5 = START,
      scenario_id = "direct"
    )%>%
    select(scenario_id, gw_id, DATE5, HBOT5)
  
  end <- gwl_avg %>%
    mutate(
      DATE5 = END,
      scenario_id = "direct"
    )%>%
    select(scenario_id, gw_id, DATE5, HBOT5)
  
  GWL_avg <- rbind(start, end) 
  
  #Second criteria: Groundwater level fluctuations approximated with a sinus function and GHG and GLG (SWBOTB = 5)
  
  gwl_sinus<-gwl_criteria %>%
    filter(GWL == -1)%>%
    mutate (Amp = abs(GLG-GHG)/2)%>%
    select (run_id, gw_id, GHG, GLG, Amp)
  
  run_id <- sort (gwl_sinus$run_id)
  days<-seq(0,365,30)
  years <- seq (year(START), year(END),1)
  GWL_sinus<-NULL
  
  for (s_run_id in run_id){
    message("Run", s_run_id,"...")
    GHG <-gwl_sinus%>%
      filter (run_id == s_run_id)%>%
      select (GHG)%>%
      deframe()
    
    Amp <-gwl_sinus%>%
      filter (run_id == s_run_id)%>%
      select (Amp)%>%
      deframe()
    
    GWLEVEL <-GHG-Amp+sin((days+80)*pi/180)*Amp
    HBOT5 <-600+GWLEVEL # GWLEVEL is a negative value
    GWL_fluc<-NULL
    
    for(year in years){
      GWL_fluc_tmp <- data.frame(s_run_id, days,HBOT5)%>%
        mutate(DATE5 = days+as.Date(str_c(year,"-01-01")))%>%
        rename(run_id = s_run_id)%>%
        select(run_id, days,DATE5, HBOT5)
      
      GWL_fluc <- rbind(GWL_fluc, GWL_fluc_tmp )
      
      GWL_tmp <- left_join( x = GWL_fluc, y = gwl_sinus, by = c("run_id"))%>%
        mutate(scenario_id = "direct")%>%
        select(scenario_id, gw_id, DATE5, HBOT5)
      
    }
    
    GWL_sinus <- rbind(GWL_sinus, GWL_tmp)
    
  }
  
  GWL <- rbind(GWL_avg,GWL_sinus)%>%
    arrange(gw_id)
  
  write_csv (x=GWL, file= "./input_data/grondwater.csv")
  
  return (GWL)
}


get_main_database <- function(database) {
  db <- database %>% 
    select(run_id, x_crd, y_crd, soil_id, crop_id, irrigation_id, meteo_id, gw_id, scenario_id)
  
  # Create unique combinations (indirect)
  
  if (indirect) {
    
    max_id <- nrow(db)
    db_ind <- db %>%
      select(crop_id, soil_id, meteo_id) %>%
      unique()
    db_ind <- db_ind %>%
      mutate(
        run_id = max_id + 1:nrow(db_ind),
        x_crd = NA_real_,
        y_crd = NA_real_,
        irrigation_id = 0,
        scenario_id = "indirect",
        gw_id = NA_real_
      )
    
    # combine and update run_id 
    db <- rbind(db, db_ind)%>%   
      mutate(
        run_id = 1:nrow(.)
      )
    
  }
  
  return (db)
}

create_sqlite <- function(list_data, database, START, END) {
  
  if (!file_exists("./database")) {
    dir.create("./database")
  }
  
  file_sql <-"./database/input_data.sqlite"
  if (file_exists(file_sql)) {
    file_delete(file_sql)
  }
  
  #open and writing the sqlite file
  
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql) 
  dbWriteTable(conn = conn, name = "Runs", value = database)
  
  for (name in list_data){
    file_name <- str_c("./input_data/", name, ".csv")
    data <- read_csv(file = file_name, show_col_types = FALSE, progress = FALSE )
    
    if (name == "meteo") {
      data<-data%>%
        mutate(
          TSTART = START,
          TEND = END
        )
      
    }else if (name == "gewaskalender") {
      data<-data%>%
        filter(CROPSTART>=START & CROPEND <= END)
    }
    
    # upload static data in the sqlite file
    
    dbWriteTable(conn = conn, name = name, value = data) 
  }
  
  # close database
  dbDisconnect(conn = conn)
  
}


#extract information from the database
extract_info_database <- function(file_sql){
  
  if (!file_exists("./data_sheets")) {
    dir.create("./data_sheets")
  }
  
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  name<-dbListTables(conn)
  for (x in name){
    db_tmp <- as_tibble(dbReadTable(conn = conn, name = x))
    write_csv(x=db_tmp, file = str_c("./data_sheets/",x,".csv") )
  }
  
  dbDisconnect(conn = conn)
}


get_runs_direct<-function(file_sql){
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  name<-dbListTables(conn)
  db<- as_tibble(dbReadTable(conn = conn, name = "Runs"))%>%
    filter(scenario_id=="direct")
  
  ids<-db$run_id
  
  dbDisconnect(conn = conn)
  
  return(ids)
}


#Postprocessing of the model results
get_data_procc<- function(run_id, dir_run, file_sql) {
  # Reads the data in result_output.csv and calculates the yearly yield and 
  # transpiration reduction due to too wet and too dry conditions, and indirect effects
  
  dir_tmp<-dir.create("./Temp")
  
  data_procc<-NULL
  
  for (s_run_id in run_id){
    run_info <- get_run_info_SQL(run_id = s_run_id, file_sql = file_sql)
    run_id_pot <- filter_run_id_SQL(file_sql = file_sql, meteo_id = run_info$meteo_id, soil_id = run_info$soil_id, crop_id = run_info$crop_id, scenario_id = "indirect")
    
    #Unzip output in a temporary folder
    extract_run_id(run_id = c(s_run_id, run_id_pot), dir_run = dir_run, dir_out = dir_tmp)
    
    #Calculates yearly yields and stress reduction
    
    file_dir<-str_c(dir_tmp, "/run_",formatC(x = s_run_id, format = "d", width = 9, flag = "0"),"/swap.swp")
    file_ind <-str_c(dir_tmp, "/run_",formatC(x = run_id_pot, format = "d", width = 9, flag = "0"),"/swap.swp")
    
    create_harvestinfo(file_swp=file_dir, run_info=run_info)
    create_harvestinfo(file_swp=file_ind, run_info=run_info)
    
    data_tmp<-collect_data_plot(file_ind=file_ind,file_dir=file_dir)%>%
      mutate(run_id=s_run_id)
    
    data_procc<-rbind(data_procc, data_tmp)
  }
  
  unlink(c(dir_tmp,"Temp"), recursive = TRUE)
  

  return(data_procc)
  
}


get_yield_avg <- function(yield_data) {
  
  #Average whole area
  yield_data_avg <-yield_data%>%
    group_by(year)%>%
    mutate(
      Y_pot_avg = mean(Y_pot),#Potential yield 
      Y_act_avg=mean(Y_act), #Actual yield considering direct and indirect effects
      dmgtot_avg= mean(dmgtot),
      dmgind_avg= mean(dmgind),
      dmgwet_avg= mean(dmgwet),
      dmgdry_avg= mean(dmgdry),
      ghg_avg= mean(ghg),
      glg_avg= mean(glg)
    )%>%
    select(year, Y_pot_avg, Y_act_avg, dmgtot_avg, dmgind_avg, dmgwet_avg, dmgdry_avg, ghg_avg, glg_avg)%>%
    unique()
  
  
  return(yield_data_avg)
}


get_yield_median <- function(yield_data) {
  
  #Average whole area
  yield_data_median <-yield_data%>%
    group_by(year)%>%
    mutate(
      Y_pot_me = median(Y_pot),#Potential yield 
      Y_act_me=median(Y_act), #Actual yield considering direct and indirect effects
      dmgtot_me= median(dmgtot),
      dmgind_me= median(dmgind),
      dmgwet_me= median(dmgwet),
      dmgdry_me= median(dmgdry),
      ghg_me= median(ghg),
      glg_me= median(glg)
    )%>%
    select(year, Y_pot_me, Y_act_me, dmgtot_me, dmgind_me, dmgwet_me, dmgdry_me, ghg_me, glg_me)%>%
    unique()
  
  
  return(yield_data_median)
}


get_soil_yield_model<-function(file_sql, file_soil, yield_data){
  soil_map <- read_csv(file = file_soil, show_col_types = FALSE, progress = FALSE )
  
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  name<-dbListTables(conn)
  db_tmp <- as_tibble(dbReadTable(conn = conn, name = "Runs"))
  
  dbDisconnect(conn = conn)
  
  db_soil<-left_join(db_tmp, soil_map, by = "soil_id")%>%
    filter(!is.na(x_crd))
  
  
  db_info_tmp <-left_join (db_soil, yield_data, by = "run_id")
  
  db_info_mean<-db_info_tmp%>%
    group_by(soil_texture)%>%
    mutate(
      Y_pot_avg = mean(Y_pot),#Potential yield 
      Y_act_avg=mean(Y_act), #Actual yield considering direct and indirect effects
      dmgind_avg= mean(dmgind),
      dmgwet_avg= mean(dmgwet),
      dmgdry_avg= mean(dmgdry),
    )%>%
    select(soil_texture, Y_pot_avg, Y_act_avg, dmgdry_avg, dmgwet_avg, dmgind_avg)%>%
    unique()
  
  db_info_mean<-tibble(db_info_mean)%>%
    mutate(id=1:nrow(.))
  
  return(db_info_mean)
}


#Average precipitation deficit based on meteo file
prec_deficit_avg<- function(file_sql, dir_met, start, end, discr) {
  # get grid no
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  name<-dbListTables(conn)
  db1<- as_tibble(dbReadTable(conn = conn, name = "Runs"))%>%
    filter(scenario_id=="direct")
  
  dbDisconnect(conn = conn)
  
  grid_no <-unique(db1$meteo_id)
  
  pattern <-"*.met"
  folders<-list.files(dir_met, recursive = TRUE, pattern = pattern)
  paths<-file.path(dir_met, folders)
  
  #Read meteo data 

  db <- read_csv(file = paths,  show_col_types = FALSE, progress = FALSE)%>%
    filter(YYYY>=year(start), YYYY<=year(end))%>%
    mutate(Station=substr(Station, 2,7))%>%
    filter(Station %in% grid_no)
           
  
  #Calculate precipitation deficit
  
  deficit <- db%>%
    mutate(P_ET = RAIN - ETref) %>%
    select(Station, DD, MM, YYYY, P_ET)
  
  if (discr=="monthly"){
    monthly_def <-deficit%>%
      group_by(Station, MM, YYYY)%>%
      mutate( P_ET_monthly = sum(P_ET)) 
    
    def_avg <-monthly_def%>%
      group_by (MM)%>%
      mutate(P_ET_avg = mean (P_ET_monthly))
    
    #Select only unique/distinct rows from the data frame
    def <- distinct(def_avg, P_ET_avg, .keep_all = TRUE)%>%
      select(MM, P_ET_avg)
    
  } else if (discr=="yearly"){
    yearly_def <-deficit%>%
      group_by(Station, YYYY)%>%
      mutate( P_ET_yearly = sum(P_ET)) 
    
    def_avg <-yearly_def%>%
      group_by (YYYY)%>%
      mutate(P_ET_avg = mean (P_ET_yearly))
    
    #Select only unique/distinct rows from the data frame
    def <- distinct(def_avg, P_ET_avg, .keep_all = TRUE)%>%
      select(YYYY, P_ET_avg)
    
  }else if (discr=="grid"){
    grid_def <-deficit%>%
      group_by(Station, YYYY)%>%
      mutate( P_ET_grid = sum(P_ET)) 
    
    def_avg <-grid_def%>%
      group_by (Station)%>%
      mutate(P_ET_avg = mean (P_ET_grid))
    
    #Select only unique/distinct rows from the data frame
    def <- distinct(def_avg, P_ET_avg, .keep_all = TRUE)%>%
      select(Station, P_ET_avg)
  }
  return (def)
}

#precipitation deficit
prec_deficit<- function(file_sql, dir_met, start, end, discr) {
  # get grid no
  conn <- dbConnect(RSQLite::SQLite(), dbname = file_sql)
  name<-dbListTables(conn)
  db1<- as_tibble(dbReadTable(conn = conn, name = "Runs"))%>%
    filter(scenario_id=="direct")
  
  dbDisconnect(conn = conn)
  
  grid_no <-unique(db1$meteo_id)
  
  pattern <-"*.met"
  folders<-list.files(dir_met, recursive = TRUE, pattern = pattern)
  paths<-file.path(dir_met, folders)
  
  #Read meteo data 
  
  db <- read_csv(file = paths,  show_col_types = FALSE, progress = FALSE)%>%
    filter(YYYY>=year(start), YYYY<=year(end))%>%
    mutate(Station=substr(Station, 2,7))%>%
    filter(Station %in% grid_no)
  
  
  #Calculate precipitation deficit
  
  deficit <- db%>%
    mutate(P_ET = RAIN - ETref) %>%
    select(Station, DD, MM, YYYY, P_ET)
  
  if (discr=="monthly"){
    monthly_def <-deficit%>%
      group_by(Station, MM, YYYY)%>%
      mutate( P_ET_monthly= sum(P_ET)) 
   
    #Select only unique/distinct rows from the data frame
    def <- distinct(monthly_def, P_ET_monthly, .keep_all = TRUE)%>%
      rename(
        meteo_id=Station,
        year=YYYY
        )%>%
      select(meteo_id, MM, year, P_ET_monthly)
    
  } else if (discr=="yearly"){
    yearly_def <-deficit%>%
      group_by(Station, YYYY)%>%
      mutate( P_ET_yearly = sum(P_ET)) 
    
    #Select only unique/distinct rows from the data frame
    def <- distinct(yearly_def, P_ET_yearly, .keep_all = TRUE)%>%
      rename(
        meteo_id=Station,
        year=YYYY
      )%>%
      select(meteo_id, year, P_ET_yearly)
    
  }
  return (def)
} 


get_dry_wet_years <-function(meteo, p1,p2,p3) {
  #Dry year=20%
  #Normal year=50%
  #Wet year=80%
  #meteo: average precipitation deficit obtained with function prec_deficit_avg
  #p1: percentile dry year
  #p2: median
  #p3: percentile wet year
  
  db_tmp<-meteo%>%
    arrange(P_ET_avg)
  
  db_tmp$n<-1:nrow(db_tmp)
  
  db<-db_tmp%>%
    mutate(prob=(n*100)/(nrow(.)))%>%
    rename(year=YYYY)
  
  q1<-p1/100
  q2<-p2/100
  q3<-p3/100
  
  perc1 <- quantile(db$P_ET_avg, q1)
  perc2 <- quantile(db$P_ET_avg, q2)
  perc3 <- quantile(db$P_ET_avg, q3)
  
  #prob vs precipitation
  size=20
  plot_prob<-ggplot(data = db, aes(x = prob, y = P_ET_avg))%>%
    + geom_line(color="blue")%>%
    + geom_segment(aes(x = p1, y = -300, xend = p1, yend = perc1), linetype="dashed")%>%
    + geom_segment(aes(x = 0, y = perc1, xend = p1, yend = perc1), linetype="dashed")%>%
    + geom_segment(aes(x = p2, y = -300, xend = p2, yend = perc2), linetype="dashed")%>%
    + geom_segment(aes(x = 0, y = perc2, xend = p2, yend = perc2), linetype="dashed")%>%
    + geom_segment(aes(x = p3, y = -300, xend = p3, yend = perc3), linetype="dashed")%>%
    + geom_segment(aes(x = 0, y = perc3, xend = p3, yend = perc3), linetype="dashed")%>%
    + labs(x="Probability of exceedance (%)", y = "Average P-ET (mm)")%>%
    + scale_x_continuous(limits = c(0,100), expand = c(0, 0))%>%
    + scale_y_continuous(limits = c(-300,300), expand = c(0, 0))%>%
    + theme_light()%>%
    + theme(text=element_text(size=size))
  
  
  plot_prob
  return (plot_prob)
  
}
