# ForestPaths WP5, LPJ-GUESS output post-processing script, into ForestPaths exploratory cases format.
# author: Annemarie Eckes-Shephard (and chatGPT)
# The script does the following:
# for each case and scenario, retrieve, post-process 
# and unit-convert all variables possible and write 
# them out as a .csv file in the required format.
# add a readme to map the Exploratory Cases back to the protocol and original output files.


################################################################################################
################################################################################################
################################################################################################
plotall=FALSE

#setup:
#devtools::install_github("MagicForrest/DGVMTools")
library(data.table)
library(DGVMTools)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reldist)

source("..helper_functions.R")

# we use management names, and ForestPaths uses cases ( numbers). here I can quickly map one to the other:
# from Table 2b in Exploratory Cases M7.
# 999 means these are not actually part of ForestPaths
cases=data.frame( man=c("base","lightthin", "intensthin","longrot", "shortrot", "rettree", "fertfor","ceaseman", "sfire"), #,"ccf"
                  FPcase = c(0  ,       6,          7   , 8      ,      9    ,    10    ,     12   ,    14     , 11) )# , 999


#also create the correct expression for Scenario:
scenarios = data.frame(sc   = c("hist","ssp126","ssp370"),
                       FPsc = c("hist","SSP1-RCP2.6","SSP3-RCP7.0"))

#files needed for area-reporting and later conversion of gridcell output into NUTS regions:
landcover_fractions_from_2025 <- read.table("../landuse_change_forest_age/lu_inputfiles_noNF3/net_lu_HildaPucherMircaEurope.txt",head=TRUE)
landcover_fractions_from_2025 <- setDT(landcover_fractions_from_2025[which(landcover_fractions_from_2025$Year==2025),])
write.table(landcover_fractions_from_2025,"/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_ForestPaths_Exploratory_runs/notes/landcover_fractions_from_2025.txt")
forestcover_fractions_from_2025 <- read.table("../notes/forest_cover_fractions_from_2025.txt",head=TRUE)
forestcover_fractions_from_2025 <- read.table("../landuse_change_forest_age/lu_inputfiles_noNF3/luforest_HildaPucherMircaEurope.txt",head=TRUE)
forestcover_fractions_from_2025 <- setDT(forestcover_fractions_from_2025[which(forestcover_fractions_from_2025$Year==2025),])
write.table(forestcover_fractions_from_2025,"/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_ForestPaths_Exploratory_runs/notes/forest_cover_fractions_from_2025.txt")



#helper function:
create_Forest_Paths_table <- function(myData_in, var, unitss,NATURAL = NULL){
  
  
  #FOREST: 
  if(is.null(NATURAL)){
    #two ways of dealing with the data some are Stand-type level, others are Forest-level
    #Forest-level reformatting: # filter condition: if forest-type names don't exist, it is Forest-level
    if(!("Larix" %in% names(myData_in@data))){
      tmp <- myData_in@data[, .SD, .SDcols = c("Lon", "Lat", "Year", var)]
      
      names(tmp)[4] <- "value"
      tmp$variable <- var
      tmp$unit <- unitss
      
      
      #prepare the output file format:
      tmp$scenario            <- FPsc
      tmp$climate_model       <- "MPI-ESM1-2-HR"
      tmp$case                <-  FPcase
      #using this to distinguish between all forest mean and stand-type ("all_forest", "Larix" ,"Quercus",...) mean:
      tmp$land_use_category   <- "all_forest" #   OR: Larix        Picea      Pinus        Fagus    Quercus      otherNL   otherBL 
      tmp$management_type     <- "probabilistic_harvest" # cannot determine the exact management type
      
      # depending on the variable, this is handled differently:
      # area fraction
      # surface_area
      # in my understanding this becomes only necessary if the land_use_category is anything other than "all_forest".
      # then, we need to extract the respective area fraction of the forest_type from forestcover_fractions_from_2025
      if(unique(tmp$land_use_category) !="all_forest"){
        library(data.table)
        # Ensure both are data.tables
        setDT(tmp)
        setDT(forestcover_fractions_from_2025)
        nm <- unique(tmp$land_use_category)
        # merge on the right coordinates:
        tmp[, gridcell_fraction := forestcover_fractions_from_2025[tmp, on = .(Lat, Lon), get(nm)]]
        setDT(landcover_fractions_from_2025)
        tmp[, forest_fraction := landcover_fractions_from_2025[tmp, on = .(Lat, Lon), FOREST]]
        
      }else{ #all forest combined, so no forestcover specific info given (NA):
        tmp$gridcell_fraction = as.numeric(NA)
        setDT(landcover_fractions_from_2025)
        names(landcover_fractions_from_2025)[1:2] <- c("Lon","Lat")
        tmp[, forest_fraction := landcover_fractions_from_2025[tmp, on = .(Lat, Lon), FOREST]]
      }
      
      tmp$forest_model = "LPJ-GUESS"
      names(tmp)[1:3] <- c("lon","lat","year")
      
      
      tmp <- tmp[,c("scenario", "climate_model", "forest_model", "case",   "lat",   "lon", "year", "land_use_category", "management_type", "variable",    "unit", "value", "gridcell_fraction", "forest_fraction")]
      
      
      return(tmp)
      
      #else  forest-type names exist, there are lots more forest types and surface_areas to sort out:
    }else{
      
      #special condition for gini-coefficient input files:
      if(!("Forest_sum" %in% names(myData_in@data))){
        colnames <-  c("Larix", "Picea", "Pinus", "Fagus", "Quercus", "otherBL", "otherNL", "NatForest")
      }else{
        colnames <-  c("Larix", "Picea", "Pinus", "Fagus", "Quercus", "otherBL", "otherNL", "NatForest", "Forest_sum")
      }
      
      # Reshape the dataframe so that PFTs (Larix, Picea, etc.) become the 'land_use_category'
      tmp <- myData_in@data %>%
        pivot_longer(cols = all_of(colnames), 
                     names_to = "land_use_category", 
                     values_to = "value") %>%
        # Add additional columns as needed
        mutate(
          scenario = FPsc,  # Add scenario (adjust as necessary)
          climate_model = "MPI-ESM1-2-HR",  # Add climate model
          forest_model = "LPJ-GUESS",  # Add forest model
          case =  FPcase,  # Assign case number (adjust as necessary)
          management_type = "probabilistic_harvest",  # Add management type (adjust as necessary)
          variable = var,  # Define the variable
          unit = unitss,  # Define the unit
          gridcell_fraction = NA,  # Set the gridcell_fraction (if available)
          forest_fraction = NA  # Set the forest_fraction (if available)
        ) %>%
        select(scenario, climate_model, forest_model, case, Lat,Lon,
               year = Year, land_use_category, management_type, variable, unit, value, 
               gridcell_fraction, forest_fraction)
      
      #some necessary renaming to be quick:
      names(tmp)[5:6] <- c("Lat","Lon")
      tmp[which(tmp$land_use_category=="Forest_sum"),]$land_use_category <- "all_forest"
      
      # First, we reshape the forest cover fractions data
      forestcover_long <- forestcover_fractions_from_2025 %>%
        pivot_longer(cols = c(Larix, Picea, Pinus, Fagus, Quercus, otherNL, otherBL, NatForest), 
                     names_to = "land_use_category", 
                     values_to = "fraction") %>%
        select(Lat, Lon, Year, land_use_category, fraction)
      
      # Merge reshaped data with the forest cover fractions data
      merged_data <- tmp %>%
        left_join(forestcover_long, by = c("Lat" = "Lat", "Lon" = "Lon", "land_use_category" = "land_use_category")) %>%
        mutate(
          gridcell_fraction = fraction,  # Assign the fraction to surface_area
          forest_fraction = NA  # You can adjust this logic if needed
        ) %>%
        select(scenario, climate_model, forest_model, case, lat =Lat, lon =Lon, year, land_use_category, management_type, 
               variable, unit, value, gridcell_fraction, forest_fraction)
      
      setDT(merged_data)
      names(landcover_fractions_from_2025)[1:2] <- c("lon","lat")
      merged_data[, forest_fraction := landcover_fractions_from_2025[merged_data, on = .(lat, lon), FOREST]]
      
      merged_data <- merged_data[,c("scenario", "climate_model", "forest_model", "case",   "lat",   "lon", "year", "land_use_category", "management_type", "variable",    "unit", "value", "gridcell_fraction", "forest_fraction")]
      
      return(merged_data)
    }
  }
  
  
  #NATURAL
  if(!is.null(NATURAL)){ # exception to add natural landcover fraction information
    
    tmp <- myData_in@data[, .SD, .SDcols = c("Lon", "Lat", "Year", var)]
    
    names(tmp)[4] <- "value"
    tmp$variable <- var
    tmp$unit <- unitss
    
    
    #prepare the output file format:
    tmp$scenario            <- FPsc
    tmp$climate_model       <- "MPI-ESM1-2-HR"
    tmp$case                <-  FPcase
    #using this to distinguish between all forest mean and stand-type ("all_forest", "Larix" ,"Quercus",...) mean:
    tmp$land_use_category   <- "NATURAL" #   OR: Larix        Picea      Pinus        Fagus    Quercus      otherNL   otherBL 
    tmp$management_type     <- "probabilistic_harvest" # cannot determine the exact management type
    
    tmp$gridcell_fraction = as.numeric(NA)
    setDT(landcover_fractions_from_2025)
    names(landcover_fractions_from_2025)[1:2] <- c("Lon","Lat")
    tmp[, forest_fraction := landcover_fractions_from_2025[tmp, on = .(Lat, Lon), NATURAL]]
    
    
    tmp$forest_model = "LPJ-GUESS"
    names(tmp)[1:3] <- c("lon","lat","year")
    tmp <- tmp[,c("scenario", "climate_model", "forest_model", "case",   "lat",   "lon", "year", "land_use_category", "management_type", "variable",    "unit", "value", "gridcell_fraction", "forest_fraction")]
    
    
    return(tmp)
    
  }
  
}


################################################################################################
################################################################################################
################################################################################################



#Main post-processing loop:

###################################################
# file and case preparations: 

base_dir <- "/Volumes/Anne's Backup/v13/"  #"/Volumes/Anne's Passport/ForestPaths/sims/sims_backup/simsv8/" 
processed_folder <- "/Volumes/Anne's Backup/processed/v13" # "/Volumes/Anne's Passport/ForestPaths/processed/v9/"
#"base","shortrot",
managementslist <- c("shortrot")#fertfor","rettree","fertfor","rettree","base")# "ceaseman""sfire","","ccf")
#Volumes/My\ Passport/ForestPaths/sims #"lightthin", "intensthin","longrot", "shortrot",
# don't forget: shortrot ssp370
#management = "ceaseman"
#scenario ="ssp126"
scenariolist <- c("ssp370")#"ssp126"
for(management in managementslist){
  for(scenario in scenariolist){
    
    if(scenario == "hist"){
      folder_name <- paste0(base_dir,management, "_hist_MPI-ESM1-2-HR_",
                            scenario, "_diston")
    }else{
      folder_name <- paste0(base_dir,management, "_fut_MPI-ESM1-2-HR_",
                            scenario, "_diston")
    }
    
    #to avoid duplication when re-creating files
    if (!dir.exists(folder_name)) {
      cat(paste(folder_name,"does not exist,skipping"))
      next  # Skip to the next iteration if folder doesn't exist
    
    }
    
    lss <- dir(folder_name)
    if(!("cmass_active.out.gz" %in% lss)) {
      cat(paste(folder_name,"has no content,skipping"))
      next  # Skip to the next iteration if folder does not contain files
     
    }
    
    
    # Your actual code for valid combinations goes here
    cat("Processing:", management, scenario, "\n")  
    
    #management  = "longrot"
    #scenario    = "ssp370" #
    disturbance = "diston" # diston # an option not necessary for ForestPaths simulations, but necessary to handle here anyways.
    
    #eg lightthin_fut_MPI-ESM1-2-HR_ssp126_distoff_2025-04-25
    #retrieve ForestPaths wording:
    FPcase <- cases[which(cases$man == management ),]$FPcase
    FPsc <- scenarios[which(scenarios$sc==scenario),]$FPsc
    filename_processed = paste0(processed_folder,FPsc,"_",FPcase,"_LPJ-GUESS.csv")
    
    #if the processed_file already exists, then skip this case:
    lss <- dir(processed_folder)
    if(filename_processed %in% lss){
      cat(paste(filename_processed,"exists",skipping))
        next
    }
    
    #SSP3-RCP7.0_10_LPJ-GUESS.csv
    #retrieve the source file location + add some metadata:
    source.in = defineSource(id= paste0(management,"_",scenario),
                             dir= paste0(folder_name),
                             format = GUESS,
                             name = paste0(management,"_",scenario))
    
    #scenario="hist"
    ###################################################
    # variable processing
    
    
    # Species_richness "forest_all"
    ############################################################
    # LPJG-file:  cmass_forest, Woody species+ C3G
    # Species richness (number of species)
    var = "Species_richness"
    unitss = "-"
    cat(var)
    
    #read in output:
    myData <- getField(source = source.in, quant = "cmass_forest")
    # count all species that have Cmass KgC/m2 >2kg
    cols <- colnames(myData@data)[4:24]
    #subset for forest PFTs:
    myData@data <- myData@data[,1:24]
    #apply TRUE FALSE , if biomass >2 kg:
    myData@data[which(myData@data[,4:24]>2),] #<-  if(myData@data[,4:24]>2)
    nn <- data.table(myData@data[,4:24]> 2)
    df_binary <- nn * 1
    myData@data[,4:24] <- df_binary
    
    #count occurences:
    myData@data$Species_richness <- rowSums(myData@data[,4:24])
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData, unitss=unitss, var=var)#, NATURAL= "NATURAL")
    
    if(plotall){
      plotSpatial(myData,years = 2099,layers= "Species_richness")
    }
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    
    ############################################################
    
    # Species_richness "NATURAL" - NB this is now shrubland
    ############################################################
    # LPJG-file:  cmass_natural, Woody shrubs + C3G
    # Species richness (number of species)
    var = "Species_richness"
    unitss = "-"
    cat(var)
    
    #read in output:
    myData <- getField(source = source.in, quant = "cmass_natural")
    # count all species that have Cmass KgC/m2 >2kg
    cols <- colnames(myData@data)[4:24]
    #subset for forest PFTs:
    myData@data <- myData@data[,1:24]
    #apply TRUE FALSE , if biomass >2 kg:
    myData@data[which(myData@data[,4:24]>2),] #<-  if(myData@data[,4:24]>2)
    nn <- data.table(myData@data[,4:24]> 2)
    df_binary <- nn * 1
    myData@data[,4:24] <- df_binary
    
    #count occurences:
    myData@data$Species_richness <- rowSums(myData@data[,4:24])
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData, unitss=unitss, var=var, NATURAL= "NATURAL")
    
    if(plotall){
      plotSpatial(myData,years = 2099,layers= "Species_richness")
    }
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    
    ############################################################
    
    
    # Gini_coeff by forest_stand "forest_all"
    #[TODO] discuss with someone more knowledgeable what to do with the >150 category. (in theory could contain more sizeclasses than the rest, therefore pot. imbalanced; i dont imagine there being a lot of trees of that size in Europe though, so not an immediate issue)
    ############################################################
    # LPJG-file:  diamstruct_cmass_wood_*, individual Stands
    # Stand - level only.
    var = "Gini"
    unitss = "-"
    cat(var)
    
    
    standtypes <- c("Larix",        "Picea" ,     "Pinus",        "Fagus",    "Quercus" ,     "otherNL"  , "otherBL" ,"NatForest","forest")
    standtypefiles <- c("diamstruct_cmass_wood_Larix_st","diamstruct_cmass_wood_Picea_st","diamstruct_cmass_wood_Pinus_st","diamstruct_cmass_wood_Fagus_st","diamstruct_cmass_wood_Quercus_st","diamstruct_cmass_wood_otherNL_st","diamstruct_cmass_wood_otherBL_st")
    
    # prepare dataframe to collect all stand types:
    myData_placeholder <- getField(source = source.in, quant = "diamstruct_cmass_wood_otherBL_st")
    myData_placeholder@data <- myData_placeholder@data[,c("Lon","Lat","Year")]
    myData_placeholder@data <- cbind(myData_placeholder@data,as.data.frame(matrix(data=as.numeric(NA),nrow= dim(myData_placeholder@data)[1],ncol=length(standtypes))))
    #myData_placeholder@data$forest <- NA
    names(myData_placeholder@data)[4:12] <- standtypes
    
    # Vector of columns to pivot (all DBH classes)
    dbh_cols <- c("0_5", "5_10", "10_15", "15_20", "20_25", "25_30", "30_35", 
                  "35_40", "40_45", "45_50", "50_55", "55_60", "60_65", "65_70", "70_75",  "75_80" ,  "80_85" ,  "85_90" ,  "90_95"  , "95_100",  "100_105", "105_110" ,"110_115", "115_120" ,"120_125", "125_130", "130_135", "135_140", "140_145", "145_150" ,"gt150" )
    
    dbh_mids <- c(
      "0_5" = 2.5, "5_10" = 7.5, "10_15" = 12.5, "15_20" = 17.5,
      "20_25" = 22.5, "25_30" = 27.5, "30_35" = 32.5, "35_40" = 37.5,
      "40_45" = 42.5, "45_50" = 47.5, "50_55" = 52.5, "55_60" = 57.5,
      "60_65" = 62.5, "65_70" = 67.5, "70_75" = 72.5, "75_80" = 77.5,
      "80_85" = 82.5, "85_90" = 87.5, "90_95" = 92.5, "95_100" = 97.5,
      "100_105" = 102.5, "105_110" = 107.5, "110_115" = 112.5, "115_120" = 117.5,
      "120_125" = 122.5, "125_130" = 127.5, "130_135" = 132.5, "135_140" = 137.5,
      "140_145" = 142.5, "145_150" = 147.5, "gt150" = 152.5
    )
    for(stand_type in standtypes){
      #address input file differences for total forest and stand types
      if(stand_type=="forest"){
        file = paste0("diamstruct_cmass_wood_",stand_type)
      }else{
        file = paste0("diamstruct_cmass_wood_",stand_type,"_st")
      }
      
      #read in output:
      myData <- getField(source = source.in, quant = file)
      
      # Pivot from wide to long format
      dt_long <- myData@data %>%
        pivot_longer(cols = `0_5`:`gt150`,
                     names_to = "dbh_class",
                     values_to = "cmass_dbh_weight")
      
      # Add midpoint column
      dt_long <- dt_long %>%
        mutate(dbh_mid = dbh_mids[dbh_class])
      
      # why is a conversion to ba necessary? If I am not applying this transformation, LPJ-GUESS will, in comparison to EFISCEN, 
      # always  underestimate the gini, even if the forest structure is relatively equal.
      # test:
      # dt_long %>% 
      #    group_by(Year,Lat,Lon) %>% 
      #    mutate(gini_stand = gini(dbh)) %>% 
      #    ungroup() -> dt_long
      
      dt_long %>% 
        mutate(ba = pi * (dbh_mid^2)/4000)->dt_long
      
      #we use the cmass within each dbh category as weight for the gini
      
      dt_long %>% 
        group_by(Year,Lat,Lon) %>% 
        mutate(gini_stand_ba = gini(ba,weights=cmass_dbh_weight)) %>% 
        ungroup() -> dt_long
      
      # Extract gini_stand per (Lon, Lat, Year)
      setDT(dt_long)
      gini_dt <- unique(dt_long[, .(Lon, Lat, Year, gini_stand_ba)])
      #plot(gini_dt$gini_stand,gini_dt$gini_stand_ba) # test
      
      myData@data <- merge(myData@data,gini_dt,by = c("Lon","Lat","Year"))
      
      #put into the correct stand type; " sequential filling"
      # Extract relevant gini values
      gini_extract <- myData@data[, .(Lon, Lat, Year, gini_stand_ba)]
      
      #Adress file vs variable naming for aggregated "Forest" output
      if(stand_type=="forest"){
        stand_type ="Forest_sum"
      }
      # Perform an update join by reference — match and assign directly
      myData_placeholder@data[gini_extract, on = .(Lon, Lat, Year), (stand_type) := i.gini_stand_ba]
    }
    
    #NaN occurs in cases then there species is not present, so replace with NA:
    myData_placeholder@data[] <- lapply(myData_placeholder@data, function(col) {
      if (is.numeric(col)) {
        col[is.nan(col)] <- NA
      }
      col
    })
    
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData_placeholder, unitss=unitss, var=var)
    
    if(plotall){
      myData_placeholder@source@name <- "for stands only"
      myData_placeholder@quant@name  <- "Gini coefficient"
      
      plotSpatial(myData_placeholder,years = c(2025,2049,2050,2051,2100),layers= "otherBL",limit=c(0,1))
      swe <- extract_country(myData_placeholder,input_name = "Sweden")
      pol <- extract_country(myData_placeholder,input_name = "Poland")
      swe2 <- aggregateSpatial(swe,method="median")
      pol2 <- aggregateSpatial(pol,method="median")
      plotTemporal(swe2, y.lim = c(0,1))
      plotTemporal(pol2, y.lim = c(0,1))
    }
    
    #Perfect Equality (Gini = 0)
    #Perfect Inequality (Gini=1)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    
    ############################################################
    
    
    # Veteran trees_40
    # to adjust to EFISCEN-output:
    ############################################################
    # LPJG-file: diamstruct_forest.out
    # Forest-level
    # Veteran trees, (average number of veteran trees per hectare)
    # already at forest-level average
    
    var = "Veteran_trees_40"
    unitss = "1/m2"
    cat(var)
    
    #read in output:
    myData <- getField(source = source.in,quant="diamstruct_forest")
    
    # Define the column names that are involved in counting as veteran trees: 
    vet_trees_40    <- c(   "40_45"  , "45_50" ,  "50_55",   "55_60"  , "60_65" ,  "65_70" ,  "70_75" ,  "75_80"  ,
                            "80_85" ,  "85_90" ,  "90_95" ,  "95_100" , "100_105", "105_110", "110_115" ,"115_120", "120_125", "125_130" ,"130_135" ,"135_140", "140_145", "145_150", "gt150" )
    
    
    # Veteran_trees_40:
    myData@data$Veteran_trees_40   <- rowSums(myData@data[,..vet_trees_40]) /10000
    
    # unitconversion
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2050,layers= "Veteran_trees")
    }
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ############################################################
    
    
    # Veteran trees_50
    # to adjust to EFISCEN-output:
    ############################################################
    # LPJG-file: diamstruct_forest.out
    # Forest-level
    # Veteran trees, (average number of veteran trees per hectare)
    # already at forest-level average
    
    var = "Veteran_trees_50"
    unitss = "1/m2"
    cat(var)
    
    #read in output:
    myData <- getField(source = source.in,quant="diamstruct_forest")
    
    # Define the column names that are involved in counting as veteran trees: 
    vet_trees_50<- c( "50_55",   "55_60"  , "60_65" ,  "65_70" ,  "70_75" ,  "75_80"  ,
                      "80_85" ,  "85_90" ,  "90_95" ,  "95_100" , "100_105", "105_110", "110_115" ,"115_120", "120_125", "125_130" ,"130_135" ,"135_140", "140_145", "145_150", "gt150" )
    
    # Veteran_trees_40:
    myData@data$Veteran_trees_50   <- rowSums(myData@data[,..vet_trees_50]) /10000
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2050,layers= "Veteran_trees")
    }
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ############################################################
    
    
    # Stand biomass:
    ###########################################################
    #LPJG-files: cmass_forest
    # Forest-level
    # all biomass, including leaves, roots.
    # it is not actually what is called "Stand_C", because it does not include ground veg, but none of the models do. 
    # Stand_C
    # no post processing required.
    
    var ="Stand_C"
    unitss <- "kg C / m2"
    cat(var)
    
    #read in output:
    myData <- getField(source = source.in,quant="cmass_forest") #mburned_area.out
    
    # "rename"
    myData@data$Stand_C  <- myData@data$Total
    # unitconversion
    # cmass is already in kgC/m2
    
    #optional visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2010,layers= "Total")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    
    rm(tmp,myData)
    #########################################################
    
    
    # Stand biomass no ground vegetation:
    ###########################################################
    #LPJG-files: cmass_forest
    # all biomass, including leaves, roots.
    # it is not actually what is called "Stand_C", because it does not include ground veg, but none of the models do. 
    # Stand_C
    # LPJ-GUESS post-processing: 
    # Total - C3gr
    # [TODO] update in table
    var ="Stand_C_nogroundveg"
    unitss <- "kg C / m2"
    cat(var)
    
    #read in output:
    myData <- getField(source = source.in,quant="cmass_forest")#mburned_area.out
    
    # update total:
    myData@data$Stand_C_nogroundveg  <- myData@data$Total - myData@data$C3_gr
    # unitconversion
    # cmass is already in kgC/m2
    
    #optional visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2025,layers= "Total")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    
    rm(tmp,myData)
    ############################################################
    
    # Soil carbon stock
    # DO NOT SEND - set to NA for writing out
    # could I do a csoil_sts_natural ? 
    #############################################################
    # LPJG-File: csoil_sts
    # Soil carbon stock
    # same unit
    var = "Soil_C"
    unitss <- "kg C / m2" 
    cat(var)
    
    # read in output:
    #myData <- getField(source = source.in,quant="csoil_sts") 
    
    # simply "rename" column:
    #myData@data$Soil_C  <- myData@data$Forest_sum
    # unitconversion
    # csoil is already in kgC/m2
    
    #TOCHANGE: currently placeholder variable for constructing the final output file
    myData  <- getField(source = source.in,quant = "cmass_active") 
    myData@data$Soil_C <- NA
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2010,layers= "Soil_C")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ###############################################################
    
    
    # Growing stock:
    ###############################################################
    # Now I am reading in number of stems, calclate height from the diameter midpoint, calculate the volume from the new diameter height combination, and then multiply by number of trees
    # nstems already provided per ha
    #The 'growing stock' is the volume of timber—merchantable and non-merchantable—in the forest at a particular time.#
    #pers comm. Stefan Olin:
    #/2 takes into account the tapering, when calculating the volume of tree trunk:
    # volume_wood = PI / 2 * (stem_diameter(m)/2) ** 2 * height(m)
    # m^3 = (m^2) * m"
    var = "GS"
    unitss <- "m3/ha"
    cat(var)
    # mean in europe: #170 m³/ha
    
    #read in output:
    myData <- getField(source = source.in,quant = "diamstruct_forest") 
    
    # adhere to FAO definition: omit 0_5  5_10 columns:
    myData@data[["0_5"]]<- NULL
    myData@data[["5_10"]]<- NULL
    
    
    dbh_cols <- 4:ncol(myData@data)
    
    # diameter mid-points
    diam <- c(12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 82.5, 87.5, 92.5, 97.5,
              102.5, 107.5, 112.5, 117.5, 122.5, 127.5, 132.5, 137.5, 142.5, 147.5, 155.0)
    # [TODO] -- Stefan suggests to use diam_struct_BA and replace this with midpoint.
    
    # create rough height estimate:
    height <- 60 * (diam /100)^(2/3)
    # tunk volume estimate (something between a cone and a cylinder):
    volume_wood = pi / 2 * (diam/100/2) ** 2 * height
    
    #now, multiply this by the number density (indiv/ha), and we have the growing stock (m3/ha):
    myData@data[, (dbh_cols) := Map(`*`, .SD, volume_wood), .SDcols = dbh_cols]
    
    #create the total growing stock:
    myData@data$GS <- rowSums(myData@data[,..dbh_cols])
    
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2014,layers= "GS")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ############################################################################
    
    # Stand basal area Forest
    # [TODO] the total diameter BA is currently derived in post-processing, but will be 
    # updated with the next runs.
    ############################################################################
    # LPJG output: diamstruct_ba_forest
    # provided in ha.  diam_ba_lc * M2_PER_HA
    # [TODO] update in table
    var = "BA"
    unitss="m2/ha"
    
    cat(var)
    
    # read in output:
    # mean individual basal area per sizeclass
    myData_BA <- getField(source = source.in,quant="diamstruct_ba_forest")
    dbh_cols <-  4:ncol(myData_BA@data)
    
    # read in output:
    # number of individuals
    myData_N <- getField(source = source.in,quant="diamstruct_forest")
    
    myData <- myData_BA
    # Perform element-wise division for each specified column
    tmp <- myData_BA@data[, ..dbh_cols] * myData_N@data[, ..dbh_cols]
    
    # Replace Inf, -Inf, and NaN with 0 (no entries, no trees)
    tmp <- tmp[, lapply(.SD, function(x) {
      x[is.infinite(x) | is.nan(x)] <- 0.0
      return(x)
    })]
    
    #omit small dbh-classes:
    #tmp[["0_5"]]<- NULL
    #tmp[["5_10"]]<- NULL
    
    # unitconversion and "export" into the BA column
    # m2 to ha
    myData@data$BA<- rowSums(tmp)/10000
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2014,layers= "BA")
    }
    
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    rm(myData_BA, myData_N, tmp)
    ###################################################################
    
    
    # Stand basal area Forest
    # [TODO] the total diameter BA is currently derived in post-processing
    ############################################################################
    # LPJG output: diamstruct_ba_forest
    # provided in ha.  diam_ba_lc * M2_PER_HA
    # [TODO] update in table
    var = "BA"
    unitss="m2/ha"

    cat(var)
    
    # read in output:
    # mean individual basal area per sizeclass
    myData_BA <- getField(source = source.in,quant="diamstruct_ba_forest")
    dbh_cols <-  4:ncol(myData_BA@data)
    
    # read in output:
    # number of individuals
    myData_N <- getField(source = source.in,quant="diamstruct_forest")
    
    myData <- myData_BA
    # Perform element-wise division for each specified column
    tmp <- myData_BA@data[, ..dbh_cols] * myData_N@data[, ..dbh_cols]
    
    # Replace Inf, -Inf, and NaN with 0 (no entries, no trees)
    tmp <- tmp[, lapply(.SD, function(x) {
      x[is.infinite(x) | is.nan(x)] <- 0.0
      return(x)
    })]
    
    #omit small dbh-classes:
    #tmp[["0_5"]]<- NULL
    #tmp[["5_10"]]<- NULL
    
    # unitconversion and "export" into the BA column
    # m2 to ha
    myData@data$BA<- rowSums(tmp)/10000
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2014,layers= "BA")
    }
    
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var,NATURAL = "NATURAL")
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    rm(myData_BA, myData_N, tmp)
    ###################################################################
    
    
    
    # Stand density
    ###################################################################
    #LPJG file:  diamstruct_forest
    # already in per ha as LPJG output
    #google: generally varies between 1000 - 2500 trees - OK
    var = "N"
    unitss <- "1/ha"
    cat(var)
    
    #read in output:
    myData <- getField(source = source.in,quant = "diamstruct_forest")  #diam_dens_lc * M2_PER_HA means it should be in m2 at this point. but the numbers look better as ha...
    
    # omit 0_5  5_10 columns:
    myData@data[["0_5"]]<- NULL
    myData@data[["5_10"]]<- NULL
    
    dbh_cols <-  4:ncol(myData@data)
    
    #unitconversion
    #indiv/m2 to indiv/ha:
    # crazy values. no conversion for now#myData@data[, (dbh_cols) := Map(`/`, .SD, 10000), .SDcols = dbh_cols]
    
    #calculate total:
    myData@data$N <- rowSums(myData@data[,..dbh_cols])
    
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2015,layers= "N")
    }
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    #############################################################################
    
    
    # Deadwood
    #####################################################################
    # clitter_course_sts, clitter_fine_sts
    # in LPJ-GUESS provides trunk deadwood KgC/m2
    # ForestPaths wants: m3 (volume..)
    # I use wood density to convert this into volume. This means that I am assuming that the deadwood is equally dense to alive trees at any point, but old deadwood ist obvioulsy less dense. We do not have deadwood decay and therefore deadwood volume in our model. We have a deadwood pool of Carbon, to which I can assign alive PFTs' wood densities.
    var = "Deadwood_tot"
    unitss <- "m3"
    cat(var)
    
    #read in output:
    #clitter_course_sts.out.gz
    clitter_coarse <- getField(source = source.in,quant = "clitter_course_sts")  #heartwood
    clitter_fine <- getField(source = source.in,quant = "clitter_fine_sts")    #sapwood  # per unit stand area
    #myData <- getField(source = source.in,quant = "clitter_sts") 
    
    myData <- clitter_fine
    cols <- names(myData@data)[4:ncol(myData@data)]
    myData@data[, (cols) := clitter_coarse@data[, ..cols] + clitter_fine@data[, ..cols]]
    
    #isolate relevant columns:
    myData@data <- myData@data[,c("Lon" ,  "Lat" , "Year","Larix", "Picea", "Pinus", "Fagus", "Quercus", "otherBL", "otherNL","NatForest","Forest_sum")]
    
    
    # Unit conversion: from C to volume using mean wood density
    species <- c("Larix", "Picea", "Pinus", "Fagus", "Quercus", "otherBL", "otherNL", "NatForest","Forest_sum")
    for (sp in species) {
      wd <- mean_wooddensity_df[mean_wooddensity_df$Luforest == sp, "MeanWoodDensity"]
      myData@data[[sp]] <- myData@data[[sp]] * wd
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    rm(clitter_fine,clitter_coarse,tmp,myData)
    #######################################################################
    
    #stand dbh distribution number of stems
    #this will cause a big file
    ###################################################################
    #LPJG file:  diamstruct_forest
    # already in per ha as LPJG output
    #google: generally varies between 1000 - 2500 trees - OK
    
    unitss <- "1/ha"
    cat(var)
    
    vars    <- c("dbh_0_5"  , "dbh_5_10" , "dbh_10_15" , "dbh_15_20",  "dbh_20_25",  "dbh_25_30", "dbh_30_35", "dbh_35_40", "dbh_40_45" ,"dbh_45_50", "dbh_50_55" ,"dbh_55_60", "dbh_60_65", "dbh_65_70", "dbh_70_75" ,"dbh_75_80", "dbh_80_85", "dbh_85_90", "dbh_90_95", "dbh_95_100", "dbh_100_105")
    
    #read in output:
    myData <- getField(source = source.in,quant = "diamstruct_forest") 
    
    
    for (i in 1:length(vars)){
      names(myData@data)[3+i] <- vars[i]
      # "rename" each relevant variable in turn:
      #(provide the correct name for the reogranisation function:)
      var = vars[i]
      #reorganise into ForestPaths format:
      tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
      
      # Write the first chunk to the file, or append if file exists
      if (!file.exists(filename_processed)) {
        write.table(tmp, file = filename_processed, sep = ",",
                    row.names = FALSE, col.names = TRUE)
      } else {
        write.table(tmp, file = filename_processed, sep = ",",
                    row.names = FALSE, col.names = FALSE, append = TRUE)
      }
    }
    
    #############################################################################
    
    
    #stand dbh distribution carbon content
    #this will cause a big file
    ###################################################################
    #LPJG file:  diamstruct_forest
    # already in per ha as LPJG output
    #google: generally varies between 1000 - 2500 trees - OK
    
    unitss <- "kg C / m2"
    cat(var)
    
    vars    <- c("dbh_0_5"  , "dbh_5_10" , "dbh_10_15" , "dbh_15_20",  "dbh_20_25",  "dbh_25_30", "dbh_30_35", "dbh_35_40", "dbh_40_45" ,"dbh_45_50", "dbh_50_55" ,"dbh_55_60", "dbh_60_65", "dbh_65_70", "dbh_70_75" ,"dbh_75_80", "dbh_80_85", "dbh_85_90", "dbh_90_95", "dbh_95_100", "dbh_100_105")
    
    #read in output:
    myData <- getField(source = source.in,quant = "diamstruct_cmass_forest") 
    
    
    for (i in 1:length(vars)){
      names(myData@data)[3+i] <- vars[i]
      # "rename" each relevant variable in turn:
      #(provide the correct name for the reogranisation function:)
      var = vars[i]
      #reorganise into ForestPaths format:
      tmp <- create_Forest_Paths_table(myData,unitss=unitss,var=var)
      
      # Write the first chunk to the file, or append if file exists
      if (!file.exists(filename_processed)) {
        write.table(tmp, file = filename_processed, sep = ",",
                    row.names = FALSE, col.names = TRUE)
      } else {
        write.table(tmp, file = filename_processed, sep = ",",
                    row.names = FALSE, col.names = FALSE, append = TRUE)
      }
    }
    
    #####################################################################
    
    # Deadwood and litter C 
    #######################################################################
    # not clear on what the definition is. If you want something else, we might be able to provide it.
    # we currently provide leaf, twigs and wood together.
    var = "Detritus_C"
    unitss <- "kg C / m2"
    cat(var)
    
    
    #read in output:
    #clitter_course_sts.out.gz
    myData <- getField(source = source.in,quant = "clitter_sts") #cpool_forest.ou clitter_sts
    
    #isolate relevant columns:
    myData@data <- myData@data[,c("Lon" ,  "Lat" , "Year","Larix", "Picea", "Pinus", "Fagus", "Quercus", "otherBL", "otherNL","NatForest","Forest_sum")]
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ########################################################################
    
    
    # [park this] Albedo 
    #######################################################################
    # [park this] Albedo  [TODO] # not post-processed. need to find Tom's table again or a different way of getting to it. Maybe merge the albedo output in.
    # lpjg: /// NPP (kgC/m2), equivalent to ForestPaths
    #var = "Albedo"
    #unitss <- "-"
    #cat(var)
    #table on landcover type and LAI?
    # [TODO] later: merge in lai output
    # [TODO] don't provide it for now
    
    #read in output:
    #myData   <- getField(source = source.in,quant = "cflux_natural") # anpp_forest ?
    
    
    #"rename" to correct FP variable name and combine soil evaporation and transpiration
    #myData@data$Albedo <- myData@data$Fire
    
    #first visualisations for output:
    #if(plotall){
    #  plotSpatial(myData,years = 2099,layers= "Fire")
    #}
    #reorganise into ForestPaths format:
    #tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    
    # Write the first chunk to the file, or append if file exists
    #if (!file.exists(filename_processed)) {
    #  write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    #} else {
    #  write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    #}
    ##########################################################################
    
    # Evapotranspiration
    ##########################################################################
    #Water budget check done below. Works fine. 
    # Evapotranspiration
    # aaevap:
    #	/// annual sum of soil evaporation (mm/year)"
    #transpiration:
    # / annual sum of AET (mm/year)
    var = "ET"
    unitss <- "mm/m2/a"
    cat(var)
    
    #read in output:
    myData_evap   <- getField(source = source.in,quant = "evap_sts") #cpool_forest.ou clitter_sts evap_sts (soil evaporation)
    myData_transp <- getField(source = source.in,quant = "transpiration_sts") 
    myData_int <- getField(source = source.in,quant = "intercep_sts") 
    
    #"rename"  to correct FP variable name and combine soil evaporation and transpiration
    myData <- myData_transp
    myData@data$ET <- myData_transp@data$Forest_sum + myData_evap@data$Forest_sum  + myData_int@data$Forest_sum
    
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2014,layers= "ET")
    }
    
    
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    rm(myData_transp, myData_evap,myData_int)
    #############################################################################
    
    # NPP
    #############################################################################
    
    # lpjg: /// NPP (kgC/m2), equivalent to ForestPaths
    var = "NPP"
    unitss <- "kgC m-2 a-1"
    
    #read in output:
    myData   <- getField(source = source.in,quant = "anpp_sts") # anpp_forest ?
    
    #"rename" to correct FP variable name and combine soil evaporation and transpiration
    myData@data$NPP <- myData@data$Forest_sum
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2099,layers= "NPP")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ################################################################################
    
    
    
    # NBP - forest
    ########################################################################
    # lpjg file: cflux_forest
    # lpjg: /// NBP (kgC/m2), equivalent to ForestPaths
    var = "NBP"
    unitss <- "kgC m-2 a-1"
    
    #C mass in pool kgC/m2
    #read in output:
    myData   <- getField(source = source.in,quant = "cflux_forest") #anpp_forest ?
    
    
    #"rename" to correct FP variable name and remove landuse change flux
    myData@data$NBP <- myData@data$NEE - myData@data$LU_ch
    
    #myData  <- getField(source = source.in,quant = "cmass_active") 
    #TOCHANGE: currently placeholder variable for constructing the final output file
    #myData@data$NBP <- NA
    
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 1901,layers= "Total",limits = c(0,50))
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    #########################################################################
    
    
    #NEE - forest
    #[TODO] [rerun] rerun with updated cflux table [TODO] 
    ################################################################################
    #file: cflux
    # currently omitting Fire and Harvest to look like what 
    #Tom calls Fluxtower NEE
    var = "NEE"
    unitss <- "kgC m-2 a-1"
    cat(var)
    
  
    #read in output:
    myData   <- getField(source = source.in,quant = "cflux_forest") # anpp_forest ?
    
    #"rename" to correct FP variable name and combine soil evaporation and transpiration
    myData@data$NEE <- myData@data$NEE-myData@data$Fire-myData@data$Harvest - myData@data$LU_ch
    
    
    #TOCHANGE: currently placeholder variable for constructing the final output file
    #myData  <- getField(source = source.in,quant = "cmass_active") 
    #myData@data$NEE <- NA
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2024,layers= "NEE")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ################################################################################
    
    
    # Run-off 
    #############################################################################
    # lpjg: //arunoff
    #/// annual sum of runoff (mm/year)
    # 1 mm = 1 L/m2
    #and:
    # 1 mm = 1 liter = 0.001 m3
    #, equivalent to ForestPaths
    var = "Run_off"
    unitss <- "m3/m2/a"
    cat(var)
    
    #read in output:
    myData   <- getField(source = source.in,quant = "runoff_sts") # anpp_forest ?
    
    #"rename" to correct FP variable name and combine soil evaporation and transpiration
    #unitconversion: mm/year
    # m3/m2/year
    myData@data$Run_off <- myData@data$Forest_sum * 0.001
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2024,layers= "Run_off")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ############################################################################
    
    #soil moisture
    # to be provided in the next iteration.
    ############################################################################
    # LPJG files: mwcont_lower.out, mwcont_upper.out
    # computing the weighted average of upper(50cm) soil layer and lower (100cm) soil layers,
    # averaged across all 12 months for an annual average.
    var = "SM"
    unitss <- "%" # % of soil? or % of available pore space in soil ? 
    cat(var)
    
    #read in output:
    myData_upper   <- getField(source = source.in,quant = "mwcont_upper")
    myData_lower   <- getField(source = source.in,quant = "mwcont_lower")
    
    #depth-weighted mean:
    d1 <- 50
    d2 <- 100
    myData_upper@data[, SM_month := (d1 * myData_upper@data$mwcont_upper + d2 * myData_lower@data$mwcont_lower) / (d1 + d2)]
    
    # Compute annual mean per gridcell
    myData_lower@data <- myData_upper@data[, .(SM = mean(SM_month, na.rm = TRUE)), by = .(Lon, Lat, Year)]
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData_lower,years = c(2050,2070,2100),layers= "SM")
    }
    
    #isolate necessary columns:
    myData_lower@data <- myData_lower@data[, .SD, .SDcols = c("Lon", "Lat", "Year", var)]
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    #############################################################################
    
    
   
    # Wind-damage losses 
    #########################################################################
    var = "Wind_losses"
    unitss <- "kgC /a /m2"
    cat(var)
    #read in output:
    myData   <- getField(source = source.in,quant = "storm")
    
    #"rename" to correct FP variable name and combine soil evaporation and transpiration
    #unitconversion: 
    #unclear whether needed. Check once variable is the correct one.
    myData@data$Wind_losses <- myData@data$DamWoodC #double dam_wood; // Damaged wood c mass 
    
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2050,layers= "Wind_losses")
      dd_total <-  aggregateSpatial(myData,method = "sum",layers= "Wind_losses") #too low, scaling problem?
      
    } 
    
    #isolate necessary columns:
    myData@data <- myData@data[, .SD, .SDcols = c("Lon", "Lat", "Year", var)]
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ##########################################################################
    
    # fire losses.
    #[TODO] Discuss with Martin/Lars. very low. Check warmer Scenarios with more fuel.
    ##########################################################################
    # fire losses. Fire emissions are divided up by forest and natural cover:
    var = "Fire_losses"
    unitss <- "kgC /a /m2"
    cat(var)
    #read in output:
    myData_forest   <- getField(source = source.in,quant = "cflux_forest") 
    myData_natural   <- getField(source = source.in,quant = "cflux_natural")
    
    #"rename" 
    myData_forest@data$Fire_losses <- myData_forest@data$Fire
    myData_natural@data$Fire_losses <- myData_natural@data$Fire
    

    if(plotall){
      myData_forest@data$GridcellArea <-GridcellArea_m2(myData_forest@data$Lat)
      myData_forest@data$Fire_area <- myData_forest@data$GridcellArea* myData_forest@data$Fire
      plotSpatial(myData_forest,years = 2099,layers= "Fire_losses",limit=c(0,0.001))
    }
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData_forest,var=var,unitss=unitss)
    tmp2 <- create_Forest_Paths_table(myData_natural,var=var,unitss=unitss,NATURAL=1)
    
    tmp <- rbind(tmp,tmp2)
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    rm(tmp2,tmp,myData_natural,myData_forest)
    ##########################################################################
    
    # Insect losses
    #########################################################################
    # Insect losses 
    var = "Insect_losses"
    unitss <- "kgC /a /m2"
    cat(var)
    #read in output:
    myData   <- getField(source = source.in,quant = "storm") 
    
    #"rename" to correct FP variable name
    #unitconversion: 
    #unclear whether needed. Check once variable is the correct one.
    myData@data$Insect_losses <- myData@data$InsDWC
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData,years = 2050,layers= "Insect_losses")
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    #########################################################################
    
    
    # Natural mortality
    #[TODO] output currently scaled to a gridcell level, I don't think that is correct, because we want Forest mortality
    #[TODO] hack this output in commonoutput.cpp upon rerun.
    #########################################################################
    # allowing for all sizeclasses (0-10, 10-20, etc..)
    var = "Natural_mortality"
    unitss <- "kgC /a /m2"
    cat(var)
    #read in output:
    myData_greff   <- getField(source = source.in,quant = "closs_greff")  # closs_greff.out.gz  closs_age.out.gz   closs_other.out.gz
    myData_age   <- getField(source = source.in,quant = "closs_age")      # closs_greff.out.gz  closs_age.out.gz   closs_other.out.gz
    myData_other   <- getField(source = source.in,quant = "closs_other")  # closs_greff.out.gz  closs_age.out.gz   closs_other.out.gz
    
    #sum up all mortality mechanisms:
    myData_other@data[,c(4:19)] <- myData_other@data[,c(4:19)] + myData_age@data[,c(4:19)] + myData_greff@data[,c(4:19)]
    
    #"rename" to correct FP variable name and combine soil evaporation and transpiration
    #unitconversion: 
    #unclear whether needed. Check once variable is the correct one.
    myData_other@data$Natural_mortality <- rowSums(myData_other@data[,c(4:19)])
    #first visualisations for output:
    if(plotall){
      plotSpatial(myData_other,year=2099,layer="Natural_mortality")
    }
    myData <- myData_other
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    rm(myData_other,myData_age,myData_greff)
    #######################################################################
    
    # forest productivity (timber  -  woody increment by stand type)
    #######################################################################
    #not a standard variable, but useful.
    # forest productivity 
    var = "Forest_productivity" 
    # we said we can only provide C, but maybe I can convert, using the above tree shape approach I used for Growing stock
    unitss <- "Kg C /m2 /a"
    cat(var)
    #So convert from C to volume:
    #is basically net woody growth increment.
    #we can provide this by stand type, e.g.:
    standtypes <- c("Larix",        "Picea" ,     "Pinus",        "Fagus",    "Quercus" ,     "otherNL"  , "otherBL" ,"NatForest","forest")
   
    # prepare dataframe to collect all stand types:
    myData_placeholder <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_otherBL_st")
    myData_placeholder@data <- myData_placeholder@data[,c("Lon","Lat","Year")]
    myData_placeholder@data <- cbind(myData_placeholder@data,as.data.frame(matrix(data=as.numeric(NA),nrow=dim(myData_placeholder@data)[1],ncol=length(standtypes))))
    names(myData_placeholder@data)[4:12] <- standtypes
    

    for(stand_type in standtypes){
      
      if(stand_type=="forest"){
        file = paste0("diamstruct_cmass_wood_inc_",stand_type)
      }else{
        file = paste0("diamstruct_cmass_wood_inc_",stand_type,"_st")
      }
      
      #read in output:
      myData <- getField(source = source.in, quant = file)
      myData@data$Total <- rowSums(myData@data[,4:34])
      
    
      #put into the correct stand type; " sequential filling"
      # Extract relevant values/columns:
      extract <- myData@data[, .(Lon, Lat, Year, Total)]
      
      #Adress file vs variable naming for aggregated "Forest" output
      if(stand_type=="forest"){
        stand_type ="Forest_sum"
      }
      
      # Perform an update join by reference — match and assign directly
      myData_placeholder@data[extract, on = .(Lon, Lat, Year), (stand_type) := i.Total]
    }
    
    
    #reorganise into ForestPaths format; woody increment by stand type:
    tmp <- create_Forest_Paths_table(myData_placeholder, unitss=unitss, var=var)
    
    if(plotall){
      
      plotSpatial(myData_placeholder,years = c(2025,2049,2050,2051,2100),layers= "otherBL",limit=c(0,1))
      swe <- extract_country(myData_placeholder,input_name = "Sweden")
      pol <- extract_country(myData_placeholder,input_name = "Poland")
      sp <- extract_country(myData_placeholder,input_name = "Spain")
      swe2 <- aggregateSpatial(swe,method="mean")
      pol2 <- aggregateSpatial(pol,method="mean")
      sp2 <- aggregateSpatial(sp,method="mean")
      plotTemporal(swe2, y.lim = c(0,1))
      plotTemporal(pol2, y.lim = c(0,1))
    }
    
    if(plotall){
      
      plotSpatial(myData,year=2050,layer="Forest_productivity")
      
    }
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    #######################################################################
    
    
    # Timber_production
    # stand-level
    #######################################################################
    # "Timber production"
    var = "Timber_production" 
    #by stand type
    # we said we can only provide C, but maybe One can convert, using the above tree shape approach I used for Growing stock
    cat(var)
    unitss <- "Kg C /m2 /a"
    
    
    ##cmass_wood_harv_sts.out -> harvested wood C before removing part to the product pool (kgC/m2)  # stand-level
    #cmass_wood_harv_killed   -> total C amount killed.
    #closs_harv_ccut 
    #[!!!] calculate and provide harv eff.fraction: cmass_wood_harv_stand_st / cmass_wood_harv_killed_st
    
    #read in output:
    #cmass_wood_harv_killed
    myData_harv_ccut     <- getField(source = source.in,quant = "cmass_wood_harv_sts") #gridcell-level # closs_harv_ccut.out  closs_harv_luc.out   closs_harv_thin.out
    #myData_harv_st_cc    <- getField(source = source.in,quant = "stemloss_harv_ccut")   # closs_harv_ccut.out  closs_harv_luc.out   closs_harv_thin.out
    #myData_harv_thin     <- getField(source = source.in,quant = "closs_harv_thin")  # closs_harv_ccut.out 
    #myData_harv_st_thin  <- getField(source = source.in,quant = "stemloss_harv_thin") #closs_harv_luc.out   closs_harv_thin.out
    
    #combine: 
    #myData <- myData_harv_thin
    #myData@data[,c(4:19)] <- myData_harv_thin@data[,c(4:19)] + myData_harv_ccut@data[,c(4:19)]
    #myData_st <- myData_harv_thin
    #myData_st@data[,c(4:19)] <- myData_harv_st_thin@data[,c(4:19)] + myData_harv_st_cc@data[,c(4:19)]
    
    
    #dbh_cols <- 4:ncol(myData@data)
    
    # no distinction between PFTs harvested. Therefore, I assume a mean wood density to arrive at volume: 
    
    #diam <- c(5, 15,25,35,45,55,65,75,85,95,105,115,125,135,145,155)
    
    #height <- 60 * (diam /100)^(2/3)
    # use lpjg eqn for wood volume estimate:
    #volume_wood = pi / 2 * (diam/100/2) ** 2 * height
    
    #now, multiply volume_wood by the number density (indiv/ha)
    #myData@data[, (dbh_cols) := Map(`*`, .SD, volume_wood), .SDcols = dbh_cols]
    
    #create the total growing stock:
    #final <- myData
    #final@data[,c(4:19)] <- myData@data[,c(4:19)]*myData_st@data[,c(4:19)]
    
    #final@data$Timber_production <- rowSums(final@data[,..dbh_cols])
    
    #plotSpatial(final,year=1980,layer="Timber_production")
    
    
    #"rename" to correct FP variable name and combine soil evaporation and transpiration
    #unitconversion: 
    #unclear whether needed. Check once variable is the correct one.
    #myData_harv_thin@data$Timber_production <- rowSums(myData_harv_thin@data[,c(4:19)]) 
    
    myData <- myData_harv_ccut
    
    if(plotall){
      
      plotSpatial(myData,year=2080,layer="Forest_sum")
      
    }
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    #######################################################################
    
    
    
    #nitrogen losses.
    #[TODO] some output columns are fused
    #[TODO] check unit in LPJG:
    #######################################################################
    # not the same as Nlosses to freshwaters
    # nloss are assumed to have gone into a water body or the sea.
    # nloss in LPJG no exchange with other gridcells 
    # sanity-checked; leaching values overall a bit too high, but their distribution is plausible
    #https://www.sciencedirect.com/science/article/pii/S0048969721023548 Fig4
    var = "N_losses"
    unitss <- "kgN m-2 a-1"
    cat(var)
    
    #read in output:
    #TOCHANGE: currently placeholder variable for constructing the final output file
    myData  <- getField(source = source.in,quant = "nflux") 
    myData@data$Total <- NA
    
    #unitconversion: 
    #unclear whether needed. Check once variable is the correct one.
    #since the unit wanted is Kg/m2, I keep it as it is.
    myData@data$N_losses <- myData@data$leach#/10000 #myData@data$leach 
    
    
    #reorganise into ForestPaths format:
    tmp <- create_Forest_Paths_table(myData,var=var,unitss=unitss)
    
    if(plotall){
      
      plotSpatial(myData,years=c(2025,2050,2070,2099),layer="N_losses",limit=c(0,150))
      
    }
    # Write the first chunk to the file, or append if file exists
    if (!file.exists(filename_processed)) {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(tmp, file = filename_processed, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    ##################################################################
    
    
    ###################################################
    # Create the Readme to make easily backwards compatible with protocol without reading this script
    readme_text <- paste0(
      "ForestPaths filename" , filename_processed, "corresponds to  \n",
      "LPJ-GUESS run setup: ", management, "_hist_MPI-ESM1-2-HR_", scenario, "_", disturbance, "\n",
      "see protocol https://docs.google.com/document/d/1L0u5M5tGZ5JigZbHcEwjmav4roWxC4-BOq3jkHbeS2I/edit?usp=sharing"
    )
    
    # Write to file:
    writeLines(readme_text, paste0(filename_processed,"_readme"))
    
  }
}
###################################################


