## Project Overview

| **Project name**  | [PEILIMPACT](https://www.peilimpact.be/) |
|--------------------|------------------------------|
| **Description**  | Impact of groundwater levels on agricultural land in Flanders |
| **Author**  | Research Institute for Agriculture, Fisheries and Food (ILVO) [Diana Estrella, Sarah Garré, Tom De Swaef], KWR Water Research Institute [Ruud Bartholomeus], and Wageningen University & Research (WUR) [Martin Mulder, Mirjam Hack-ten Broeke] |
|  |  |

Last updated: 2025-09-15

## Context 

This repository contains the R code and input data files for running the model SWAP-WOFOST at regional level in the context of the project PEILIMPACT. The aim is to describe the crop yield variability due to wet or dry conditions across Flanders, for grass, silage maize, potato, winter wheat and sugar beet. The model framework is explained in the [PEILIMPACT report](https://ilvo_plant-peilimpact_nl.curve.space/).
More information about the functioning of SWAP model can be found in the [SWAP documentation](https://www.swap.alterra.nl/). 

In synthesis, the model needs three main input files: meteo files (.met), crop files (.crp) and the main swap file (.swp). The meteo file contains daily meteorological data of solar radiation, minimum and maximum temperature, vapour pressure, wind speed, rainfall, and optionally reference evapotranspiration. The crop file contains detailed crop parameters for simulating crop growth and CO2 assimilation. The swap.swp file is the main swap file containing general information regarding simulation, meteorology, crop rotation, irrigation, soil water flow, heat flow and solute transport. 

## Structure and configuration

The regional model was constructed by defining a 500 m resolution grid covering the extent of Flanders, each pixel corresponding to a run ID. The necessary input data for each run ID and crop type was stored in Sqlite databases. The Sqlite database uses the tabular information stored in csv files such as soil parameters and crop management parameters (**input_data** folder); and spatial information stored in ASCII maps such as average groundwater level at each pixel (**maps** folder). These ASCII maps and csv files are linked by means of IDs (e.g. meteo_id, soil_id, ..). For each run ID, input files (.crp and .swp) required by SWAP are created using information from the sqlite database, and the model is run using the model executable (**source** folder). The output of the model is the **result_output.csv** file, which contains several output variables like daily crop transpiration and biomass, which is previously specified in the Sqlite database. During postprocessing, the potential and actual dry matter yield for each year, and yield reduction due to water stress and/or indirect effects are calculated. 
The Sqlite databases and resulting and processed files are not provided in this github repository due to size limitations.

### Rscripts  
1. **SQLITE.R**: Creates the Sqlite database  
2. **Run_regional.R**: Runs the model SWAP for an array of simulations  
3. **Postprocessing_regional.R**:  Processes model results and creates plots  
4. **functions.R** : Contains R functions for generation of Sqlite database and postprocessing of the model results
5. **maps.R** : Crop yield maps

## Additional Files

### soiltextureclass.csv  
USDA and Belgian soil texture classification for each soil layer and profile.

### soil_classification.csv  
Belgian soil texture classification for the top layer of each profile.

### yields_statbel20122021.csv  
Average yearly yields from 2012 to 2021, for silage maize, sugar beet, winter wheat,potato and grass.  
These values are obtained from [STATBEL](https://statbel.fgov.be/en/themes/agriculture-fishery/farm-and-horticultural-holdings), except for grass that is based on field experiments at ILVO

## Requirements

R version 4.2.1  
SWAP version 4.2.0  

## License  

Attribution-NonCommercial-ShareAlike 4.0 International


## Disclaimer

This model version includes a disclaimer. See **DISCLAIMER.md** for details.

## Contact

sarah.garre@ilvo.vlaanderen.be


## Status

Finished
