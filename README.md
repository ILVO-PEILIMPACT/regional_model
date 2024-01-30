# regional_model
SWAP-WOFOST at regional level in the context of the project PEILIMPACT, to describe the crop yield variability due to wet or dry conditions across Flanders, for grass, silage maize, potato, winter wheat and sugar beet. The model framework is explained in the project report https://ilvo_plant-peilimpact_nl.curve.space/Projectinformatie.
More information about the functioning of SWAP can be found in the manual https://www.swap.alterra.nl/. 

In synthesis, the model needs three main input files: meteo files (.met), crop files (.crp) and the main swap file (.swp). These files are supplied with information from the SQLITE database. The model runs using the model executable (swap_4.2.0.exe) and these input files. The output file “result_output.csv” contains output variables like daily crop transpiration and biomass, which is previously defined by the user. During postprocessing, the potential and actual dry matter yield for each year, and yield reduction due to water stress and/or indirect effects are calculated.

The first step to run the model is the generation of input data. The \textbf{crop folder} contains the crop files (.crp), where detailed crop parameters for simulating crop growth and biomass assimilation are specified. The file location.csv has the coordinates (in Belgian Lambert 72) of every location where the model will be run. The input_data folder comprises most of the input data for each run, such us soil parameters, crop management parameters, and other input variables, which are stored in the Sqlite database. The maps folder contains the ASCII maps for meteo and soil IDs, and average GWL, GHG and GLG values. This information is also included in the database. Sqlite databases are then saved in the folder database. Five main Sqlites databases were created for each crop type, assuming that each crop covers the whole area.

The meteo folder holds the weather time series for each 25 x 25 m grid, in the correct format (.met), and CO2 emissions (.co2) until 2021. The source folder contains the model executable, version 4.2.0. The folders libraries and R scripts have the R libraries and R scripts for generating the databases and running the model.

The swap.swp file is the main swap file, containing general information regarding simulation, meteorology, crop rotation, irrigation, soil water flow, heat flow and solute transport. The main swap file draws the required information from the sqlite database. The control file control.inp contains directories and paths of the input data files.
# Files

## soiltextureclass.csv  
USDA and Belgian soil texture classification for each soil layer and profile.

## soil_classification.csv  
Belgian soil texture classification for the top layer of each profile.

## yields_statbel20122021.csv  
Average yearly yields from 2012 to 2021, for silage maize, sugar beet, winter wheat,potato and grass.  
These values are obtained from [STATBEL](https://statbel.fgov.be/en/themes/agriculture-fishery/farm-and-horticultural-holdings), except for grass that is based on field experiments at [ILVO](https://rassenlijst.ilvo.vlaanderen.be/en/comparison-of-grass-variety-characteristics)


