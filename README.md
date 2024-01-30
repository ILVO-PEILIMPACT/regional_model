# regional_model
SWAP-WOFOST at regional level in the context of the project PEILIMPACT, to describe the crop yield variability due to wet or dry conditions across Flanders, for grass, silage maize, potato, winter wheat and sugar beet. The model framework is explained in the project report https://ilvo_plant-peilimpact_nl.curve.space/Projectinformatie.
More information about the functioning of SWAP can be found in the manual https://www.swap.alterra.nl/. 

In synthesis, the model needs three main input files: meteo files (.met), crop files (.crp) and the main swap file (.swp). These files are supplied with information from the Sqlite database. The model runs using the model executable and these input files. The file “result_output.csv” is the main output of the model, that contains output variables like daily crop transpiration and biomass, which is previously defined in the sqlite database. During postprocessing, the potential and actual dry matter yield for each year, and yield reduction due to water stress and/or indirect effects are calculated.
# Files

## soiltextureclass.csv  
USDA and Belgian soil texture classification for each soil layer and profile.

## soil_classification.csv  
Belgian soil texture classification for the top layer of each profile.

## yields_statbel20122021.csv  
Average yearly yields from 2012 to 2021, for silage maize, sugar beet, winter wheat,potato and grass.  
These values are obtained from [STATBEL](https://statbel.fgov.be/en/themes/agriculture-fishery/farm-and-horticultural-holdings), except for grass that is based on field experiments at ILVO
