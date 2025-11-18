

# necessary data to calculate volume -relvated outputs
# from a grep on pft and woddens on LPJ-GUESS instruction file:
raw_data <- c(
  'pft "Abi_alb" (', 'wooddens 252.5',
  'pft "BES" (', 
  'pft "Bet_pen" (', 'wooddens 285.0',
  'pft "Bet_pub" (', 'wooddens 285.0',
  'pft "Car_bet" (', 'wooddens 340.0',
  'pft "Cor_ave" (', 'wooddens 292.5',
  'pft "Fag_syl" (', 'wooddens 337.5',
  'pft "Fra_exc" (', 'wooddens 332.5',
  'pft "Jun_oxy" (', 'wooddens 287.5',
  'pft "Lar_dec" (', 'wooddens 262.5',
  'pft "MRS" (',
  'pft "Pic_abi" (', 'wooddens 225.0',
  'pft "Pic_sit" (', 'wooddens 182.5',
  'pft "Pin_syl" (', 'wooddens 282.5',
  'pft "Pin_hal" (', 'wooddens 307.5',
  'pft "Pop_tre" (', 'wooddens 238.8',
  'pft "Que_coc" (', 'wooddens 257.5',
  'pft "Que_ile" (', 'wooddens 420.0',
  'pft "Que_pub" (', 'wooddens 377.5',
  'pft "Que_rob" (', 'wooddens 307.5',
  'pft "Til_cor" (', 'wooddens 227.5',
  'pft "Ulm_gla" (', 'wooddens 312.5'
)

# Initialize vectors to store PFT names and wood density values
pfts <- c()
wooddens_vals <- c()

# Loop through the data to extract PFT names and wood density values
for (i in 1:(length(raw_data) - 1)) {
  if (grepl('^pft', raw_data[i])) {
    # Extract the PFT name from the current line
    pft_name <- sub('^pft "', '', raw_data[i])
    pft_name <- sub('" \\(', '', pft_name)
    
    # The next line should contain the wooddens value
    if (grepl('wooddens', raw_data[i + 1])) {
      wooddens_value <- sub('^wooddens ', '', raw_data[i + 1])
      pfts <- c(pfts, pft_name)
      wooddens_vals <- c(wooddens_vals, wooddens_value)
    }
  }
}

# Create a dataframe
df_wooddens <- data.frame(PFT = pfts, WoodDensity = as.numeric(wooddens_vals))


# Define Luforest categories and their corresponding PFTs
luforest_categories <- list(
  Quercus = c("Que_ile", "Que_rob", "Que_pub"),
  Fagus = c("Fag_syl"),
  Pinus = c("Pin_syl", "Pin_hal"),
  Picea = c("Pic_abi"),
  Larix = c("Lar_dec"),
  otherBL = c("Car_bet", "Fra_exc", "Til_cor", "Bet_pen", "Bet_pub", "Cor_ave"),
  otherNL = c("Abi_alb", "Jun_oxy"),
  NatForest = c("Pin_syl", "Pin_hal","Abi_alb", "Jun_oxy","Car_bet", "Fra_exc", "Til_cor", "Bet_pen", "Bet_pub", "Cor_ave","Lar_dec","Pic_abi","Que_ile", "Que_rob", "Que_pub","Fag_syl"),
  Forest_sum = c("Pin_syl", "Pin_hal","Abi_alb", "Jun_oxy","Car_bet", "Fra_exc", "Til_cor", "Bet_pen", "Bet_pub", "Cor_ave","Lar_dec","Pic_abi","Que_ile", "Que_rob", "Que_pub","Fag_syl")
)

# Create an empty dataframe to store the mean wood density for each Luforest category
mean_wooddensity_df <- data.frame(Luforest = character(0), MeanWoodDensity = numeric(0))


# Loop through each Luforest category to calculate the mean wood density
for (category in names(luforest_categories)) {
  # Get the PFTs for this category
  pfts_in_category <- luforest_categories[[category]]
  
  # Filter the df to get the wood density for these PFTs
  wooddens_in_category <- df_wooddens[df_wooddens$PFT %in% pfts_in_category, "WoodDensity"]
  
  # Calculate the mean wood density for this category
  mean_wooddensity <- mean(wooddens_in_category, na.rm = TRUE)
  
  # Add the result to the new dataframe
  mean_wooddensity_df <- rbind(mean_wooddensity_df, data.frame(Luforest = category, MeanWoodDensity = mean_wooddensity))
}



#for standardised plotting:

managementcol   <- c("red","orange","lightgreen","lightblue","blue","purple","pink","brown","grey","black","magenta")

get_management_col <- function(management){
  
  idx <- which(management == managementslist)
  col = managementcol[idx]
  return(col)
}




get_management_col <- function(management) {
  idx <- match(management, managementslist)
  if (any(is.na(idx))) {
    stop("One or more management names not found")
  }
  return(managementcol[idx])
}

#
print_mean_variable <- function(dt) {
  mean_val <- mean(dt[["value"]], na.rm = TRUE)
  cat("Mean of ", var, ": ", mean_val, "\n")
}

#to obtain total wind damage per area, must get the forested area fraction:

GridcellArea_m2 <- function(lats,res=NULL){
  if(is.null(res)){
    res=0.5
  }
  R =6371221.3
  dlat = res*pi/180
  rads = lats *pi/180
  area = R^2 *cos(rads)*dlat*sin(dlat/2)*2
  return(area)
}

#calculate the "effect size" (difference) of a given layer between two years, for entire Europe:
effect_size <- function(x, layer,year_low,year_high){
  
  if(var =="cmass_sts" | var =="clitter_sts"){
    #make extra sure only forested areas are included in the mean: [TODO] makes sense?
    if (any(x@data$Forest_sum == 0)) {
      x@data <- x@data[x@data$Forest_sum != 0, ]
    }
  }
  
  #initialise:  
  data_inside <- x
  data_inside <- aggregateSpatial(x,method="mean")
  
  #plotTemporal(data_inside,"Forest_sum")
  low.result <- data_inside@data[which(data_inside@data$Year==year_low),get(layer)]
  high.result <- data_inside@data[which(data_inside@data$Year==year_high),get(layer)]
  final = low.result - high.result
  
  return(final)
}

#helper functions:
extract_country <- function(myData,input_name,path_to_gridlist_with_country_names ="../european_applications/testrun/gridlist_final.txt_country_names_added") {
  
  europe_Cnames <- read.table(head=TRUE ,path_to_gridlist_with_country_names)
  # Merge using dplyr (assumes myData is an sf object with lat/lon columns)
  myData_new <- myData@data %>%
    left_join(europe_Cnames, by = c("Lat" = "lat", "Lon" = "lon"))
  
  # Filter by country name
  myData_Country <- myData_new %>%
    filter(country_name == input_name)
  
  myData_out <- myData
  myData_out@data <- myData_Country
  #add country to id:
  if(class(myData)[1] =="Comparison"){
    myData_out@name <- paste0(myData@name,"_",input_name)
  }
  
  if(class(myData)[1] =="Field"){
    myData_out@source@id <- paste0(myData@source@id,"_",input_name)
    myData_out@source@name <- paste0(myData@source@name,"_",input_name)
  }
  else{
    cat("error -  must add class-specific handling of metadata here. only implrmented for field and comparison")
  }
  
  myData_out@data$country_code <-  NULL
  myData_out@data$continent    <- NULL
  myData_out@data$country_name <- NULL
  return(myData_out)
}



#helper functions:
extract_countries <- function(myData,country_name_list,path_to_gridlist_with_country_names ="../european_applications/testrun/gridlist_final.txt_country_names_added") {
  
  europe_Cnames <- read.table(head=TRUE ,path_to_gridlist_with_country_names)
  # Merge using dplyr (assumes myData is an sf object with lat/lon columns)
  myData_new <- myData@data %>%
    left_join(europe_Cnames, by = c("Lat" = "lat", "Lon" = "lon"))
  
  # Filter by country name
  myData_Country <- myData_new %>%
    filter(country_name %in% country_name_list) #country_name_list
  
  myData_out <- myData
  myData_out@data <- myData_Country
  #add country to id:
  if(class(myData)[1] =="Comparison"){
    myData_out@name <- paste0(myData@name,"_",country_name_list) 
  }
  
  if(class(myData)[1] =="Field"){
    myData_out@source@id <- paste0(myData@source@id,"_",country_name_list)
    myData_out@source@name <- paste0(myData@source@name,"_",country_name_list)
  }
  else{
    cat("error -  must add class-specific handling of metadata here. only implrmented for field and comparison")
  }
  
  myData_out@data$country_code <-  NULL
  myData_out@data$continent    <- NULL
  myData_out@data$country_name <- NULL
  return(myData_out)
}

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
        tmp[, surface_area := forestcover_fractions_from_2025[tmp, on = .(Lat, Lon), get(nm)]]
        setDT(landcover_fractions_from_2025)
        tmp[, forest_surface_area := landcover_fractions_from_2025[tmp, on = .(Lat, Lon), FOREST]]
        
      }else{ #all forest combined, so no forestcover specific info given (NA):
        tmp$surface_area = as.numeric(NA)
        setDT(landcover_fractions_from_2025)
        names(landcover_fractions_from_2025)[1:2] <- c("Lon","Lat")
        tmp[, forest_surface_area := landcover_fractions_from_2025[tmp, on = .(Lat, Lon), FOREST]]
      }
      
      tmp$forest_model = "LPJ-GUESS"
      names(tmp)[1:3] <- c("lon","lat","year")
      
      
      tmp <- tmp[,c("scenario", "climate_model", "forest_model", "case",   "lat",   "lon", "year", "land_use_category", "management_type", "variable",    "unit", "value", "surface_area", "forest_surface_area")]
      
      #test for mean value
      print_mean_variable(tmp)
      
      return(tmp)
      
      #else  forest-type names exist, there are lots more forest types and surface_areas to sort out:
      }else{
        
        # Reshape the dataframe so that PFTs (Larix, Picea, etc.) become the 'land_use_category'
        tmp <- myData_in@data %>%
          pivot_longer(cols = c(Larix, Picea, Pinus, Fagus, Quercus, otherBL, otherNL),#, NatForest), 
          #pivot_longer(cols = c(Larix, Picea, Pinus, Fagus, Quercus, otherBL, otherNL,Forest_sum), 
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
            surface_area = NA,  # Set the surface_area (if available)
            forest_surface_area = NA  # Set the forest_surface_area (if available)
          ) %>%
          select(scenario, climate_model, forest_model, case, Lat,Lon,
                 year = Year, land_use_category, management_type, variable, unit, value, 
                 surface_area, forest_surface_area)
        
        #some necessary renaming to be quick:
        names(tmp)[5:6] <- c("Lat","Lon")
        tmp[which(tmp$land_use_category=="Forest_sum"),]$land_use_category <- "all_forest"
        
        # First, we reshape the forest cover fractions data
        forestcover_long <- forestcover_fractions_from_2025 %>%
          pivot_longer(cols = c(Larix, Picea, Pinus, Fagus, Quercus, otherNL, otherBL),#, NatForest), 
                       names_to = "land_use_category", 
                       values_to = "fraction") %>%
          select(Lat, Lon, Year, land_use_category, fraction)
        
        # Merge reshaped data with the forest cover fractions data
        merged_data <- tmp %>%
          left_join(forestcover_long, by = c("Lat" = "Lat", "Lon" = "Lon", "land_use_category" = "land_use_category")) %>%
          mutate(
            surface_area = fraction,  # Assign the fraction to surface_area
            forest_surface_area = NA  # You can adjust this logic if needed
          ) %>%
          select(scenario, climate_model, forest_model, case, lat =Lat, lon =Lon, year, land_use_category, management_type, 
                 variable, unit, value, surface_area, forest_surface_area)
        
        setDT(merged_data)
        names(landcover_fractions_from_2025)[1:2] <- c("lon","lat")
        merged_data[, forest_surface_area := landcover_fractions_from_2025[merged_data, on = .(lat, lon), FOREST]]
       #x
        merged_data <- merged_data[,c("scenario", "climate_model", "forest_model", "case",   "lat",   "lon", "year", "land_use_category", "management_type", "variable",    "unit", "value", "surface_area", "forest_surface_area")]
        
        #test for mean value
        print_mean_variable(merged_data)
        
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
    
    tmp$surface_area = as.numeric(NA)
    setDT(landcover_fractions_from_2025)
    names(landcover_fractions_from_2025)[1:2] <- c("Lon","Lat")
    tmp[, forest_surface_area := landcover_fractions_from_2025[tmp, on = .(Lat, Lon), NATURAL]]


    tmp$forest_model = "LPJ-GUESS"
    names(tmp)[1:3] <- c("lon","lat","year")
    tmp <- tmp[,c("scenario", "climate_model", "forest_model", "case",   "lat",   "lon", "year", "land_use_category", "management_type", "variable",    "unit", "value", "surface_area", "forest_surface_area")]
    
    #test for mean value
    print_mean_variable(tmp)
  return(tmp)
  
  }
  
}


create_gobj <-function(model_out, y.lim = NULL,layers=NULL){
  if(!is.null(layers)){ # enhance title, so that the reader understands what layers are currently plotted
    p_out <- plotTemporal(model_out, layers=layers,title = paste(model_out@source@name," \n", layers), y.lim )
  }else{# if everything is plotted:
    p_out <- plotTemporal(model_out, layers=layers,title = model_out@source@name, y.lim )
    
  }
  
  return(p_out)
}

#test gini calculations on dummy dataset by Ajdin
#libraries
library(tidyverse)
library(here)
library(reldist)


# from MartJan 
# load files --------------------------------------------------------------
#read.csv(here("Data", "testGini.csv"))->df

# tailor function for calculating the gini   ------------------------------
#The function calculates Gini index for the full stand and for the dominant species only
#in case the stand has multiple species. 
#the functions need a dataset with 3 basic info:
#      - the stand id with column called "plot"
#      - the species column called "species"
#      - the dbh column called "dbh"

gini_calculations<-function(df){
  #calculate basal area per tree
  df %>% 
    mutate(ba = pi * (dbh^2)/4000)->df
  
  #sum up per species and per plot
  df %>% 
    group_by(plot) %>% mutate(plot_ba = sum(ba)) %>% ungroup() %>% 
    group_by(plot, species) %>% mutate(spp_ba_plot = sum(ba)) %>% ungroup() -> df
  
  #gini stand
  df %>% 
    group_by(plot) %>% 
    mutate(gini_stand = gini(ba)) %>% 
    ungroup()->df
  
  #calc main species and save in a different df
  df %>% 
    group_by(plot, species) %>% 
    mutate(rel_ba = round((spp_ba_plot*100/plot_ba),5)) %>% ungroup() %>% 
    group_by(plot) %>% 
    mutate(dominant_species = case_when(rel_ba == max(rel_ba)  ~ species,
                                        rel_ba != max(rel_ba) ~ NA)) %>% ungroup() %>% 
    na.omit()->df_dominant
  
  #gini dominant species
  df_dominant %>% 
    group_by(plot) %>% 
    mutate(gini_dominant = gini(ba)) %>% 
    ungroup()->df_dominant
  
  #merge back the two df
  df %>% 
    left_join(df_dominant)->df
  
  #return final df
  return(df)
}

# example -----------------------------------------------------------------
#gini_calculations(df)->gini.df


calculate_gini <- function(source_name,management_name){
  standtypes <- c("Larix",        "Picea" ,     "Pinus",        "Fagus",    "Quercus" ,     "otherNL"  , "otherBL" ,"NatForest","forest")
  standtypefiles <- c("diamstruct_cmass_wood_Larix_st","diamstruct_cmass_wood_Picea_st","diamstruct_cmass_wood_Pinus_st","diamstruct_cmass_wood_Fagus_st","diamstruct_cmass_wood_Quercus_st","diamstruct_cmass_wood_otherNL_st","diamstruct_cmass_wood_otherBL_st")
  
  # prepare dataframe to collect all stand types:
  myData_placeholder <- getField(source = source_name, quant = "diamstruct_cmass_wood_otherBL_st")
  myData_placeholder@data <- myData_placeholder@data[,c("Lon","Lat","Year")]
  myData_placeholder@data <- cbind(myData_placeholder@data,as.data.frame(matrix(data=as.numeric(NA),nrow= dim(myData_placeholder@data)[1],ncol=length(standtypes))))
  #myData_placeholder@data$forest <- NA
  names(myData_placeholder@data)[4:12] <- standtypes
  
  if(management_name =="rettree"){
    #run with an older version, where the retention tree code worked. but this means that some of the output columns have different names. Once I have checked what the matter is with why rettree does not work anymore after the EUroapp merge, I will remove this
    # Vector of columns to pivot (all DBH classes)
    dbh_cols <- c("0_5", "5_10", "10_15", "15_20", "20_25", "25_30", "30_35", 
                  "35_40", "40_45", "45_50", "50_55", "55_60", "60_65", "65_70", "70_75",  "75_80" ,  "80_85" ,  "85_90" ,  "90_95"  , "95_100",  "100_105", "105_110" ,"110_115", "115_120" ,"120_125", "125_130", "130_135", "135_140", "140_145", "145_150" ,">150" )
    
    dbh_mids <- c(
      "0_5" = 2.5, "5_10" = 7.5, "10_15" = 12.5, "15_20" = 17.5,
      "20_25" = 22.5, "25_30" = 27.5, "30_35" = 32.5, "35_40" = 37.5,
      "40_45" = 42.5, "45_50" = 47.5, "50_55" = 52.5, "55_60" = 57.5,
      "60_65" = 62.5, "65_70" = 67.5, "70_75" = 72.5, "75_80" = 77.5,
      "80_85" = 82.5, "85_90" = 87.5, "90_95" = 92.5, "95_100" = 97.5,
      "100_105" = 102.5, "105_110" = 107.5, "110_115" = 112.5, "115_120" = 117.5,
      "120_125" = 122.5, "125_130" = 127.5, "130_135" = 132.5, "135_140" = 137.5,
      "140_145" = 142.5, "145_150" = 147.5, ">150" = 152.5
    )
  }else{
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
  }
  for(stand_type in standtypes){
    #address input file differences for total forest and stand types
    if(stand_type=="forest"){
      file = paste0("diamstruct_cmass_wood_",stand_type)
    }else{
      file = paste0("diamstruct_cmass_wood_",stand_type,"_st") #diamstruct_cmass_wood_otherBL_st
    }
    
    #read in output:
    myData <- getField(source = source_name, quant = file)
    colnamess <- colnames(myData@data)
    if(">150" %in% colnamess){
      # Pivot from wide to long format
      dt_long <- myData@data %>%
        pivot_longer(cols = `0_5`:`>150`,
                     names_to = "dbh_class",
                     values_to = "cmass_dbh_weight")
      
    }else{
      # Pivot from wide to long format
      dt_long <- myData@data %>%
        pivot_longer(cols = `0_5`:`gt150`,
                     names_to = "dbh_class",
                     values_to = "cmass_dbh_weight")
    }
    
    
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
  return(myData_placeholder)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Initialize empty data collection structure
#' @param n_managements Number of management scenarios
#' @param n_countries Number of countries
#' @param n_rows Number of time steps
initialize_collection <- function(n_managements =length(managements_list), n_countries =length(archetype_countries), n_rows = 77) {
  df_template <- as.data.frame(
    matrix(NA, ncol = n_managements + 1, nrow = n_rows)
  )
  names(df_template) <- c(managements_list, "Year")
  
  collection <- vector("list", length = n_countries)
  names(collection) <- country_list
  
  for (i in seq_along(collection)) {
    collection[[i]] <- df_template
  }
  
  return(collection)
}

#' Get folder name for simulation
#' @param management Management scenario name
#' @param scenario Climate scenario
#' @param base_dir Base directory path
get_folder_name <- function(management, scenario, base_dir = BASE_DIR) {
  time_period <- if (scenario == "hist") "hist" else "fut"
  paste0(base_dir, management, "_", time_period, "_MPI-ESM1-2-HR_", 
         scenario, "_diston")
}

#' Process variable for all countries and managements
#' @param var_config List containing variable-specific configuration
#' @return List of data frames (one per country)
process_variable <- function(var_config) {
  
  cat("\n=== Processing:", var_config$name, "===\n")
  
  # Initialize collection
  results <- initialize_collection(
    length(managements_list), 
    length(country_list),
    var_config$n_years
  )
  
  # Loop through countries, managements, scenarios
  for (country in country_list) {
    for (management in managements_list) {
      for (scenario in scenario_list) {
        
        cat("Processing:", country, "-", management, "-", scenario, "\n")
        
        # Define source
        folder_name <- get_folder_name(management, scenario)
        source.in <- defineSource(
          id = paste0(management, "_", scenario),
          dir = folder_name,
          format = GUESS,
          name = paste0(management, "_", scenario)
        )
        
        # Extract and process data using variable-specific function
        processed_data <- var_config$extract_fn(source.in, country)
        
        # Store results
        results[[country]][[management]] <- processed_data$values
        results[[country]]$Year <- processed_data$years
      }
    }
  }
  
  return(results)
}

#' Create multi-panel plot for a variable
#' @param data_list List of data frames (one per country)
#' @param plot_config List containing plot configuration
create_country_plots <- function(data_list, plot_config) {
 
   #make plotting config backwards compatible. Need a cex setting to adjust 
   # for long variables/Titles.
   if(is.null(plot_config$cex)){
    plot_config$cex=3
  }
  
  pdf(
    file = paste0(OUTPUT_DIR, plot_config$filename),
    width = 8.27,
    height = 11.69
  )
  
  par(
    mfrow = c(2, 2),
    mar = c(0, 0, 0, 0),
    oma = c(8, 4, 10, 1)
  )
  
  for (country in country_list) {
    plot_single_country(
      data = data_list[[country]],
      country = country,
      config = plot_config
    )
  }
  
  # Add overall labels
  mtext(plot_config$ylab, side = 2, outer = TRUE, cex = 1, line = 2.2)
  mtext(plot_config$title, side = 3, outer = TRUE, adj = 0.5, cex =  plot_config$cex , line = 1.2)
  mtext("Year", side = 1, line = 2, outer = TRUE)
  
  dev.off()
  
  cat("Plot saved:", plot_config$filename, "\n")
}

#' Plot data for a single country
#' @param data Data frame for one country
#' @param country Country name
#' @param config Plot configuration
plot_single_country <- function(data, country, config) {
  
  if (is.null(data) || !"Year" %in% names(data)) return()
  
  mgmts <- setdiff(names(data), "Year")
  
  # Get styling for first management
  first_mgmt <- mgmts[1]
  mgmt_style <- management_df[management_df$management == first_mgmt, ]
  
  # Create base plot
  plot(
    data$Year, data[[first_mgmt]],
    col = mgmt_style$col,
    type = "l",
    lty = mgmt_style$lty,
    ylim = config$ylim,
    xlab = "",
    xaxt = "n",
    yaxt = "n",
    lwd = 2.5
  )
  
  # Add country label and border
  mtext(country, cex = 2, side = 3, adj = 0.5, line = -2)
  country_col <- archetypes_df$colour[archetypes_df$country == country]
  box(col = country_col, lwd = 3)
  
  # Add axes conditionally
  if (country == "Poland") {
    axis(side = 2)
  }
  if (country %in% c("Germany")) {
    axis(side = 1)
    axis(side = 2)
  }
  if (country == "Spain") {
    axis(side = 1)
  }
  
  # Add legend conditionally
  if (config$legend_country == country) {
    legend(
      config$legend_position,
      legend = management_df$management,
      col = management_df$col,
      lty = management_df$lty,
      bty = "n",
      ncol = 2,
      cex = 1,
      lwd = 2
    )
  }
  
  # Add remaining management lines
  for (mgmt in mgmts[-1]) {
    mgmt_style <- management_df[management_df$management == mgmt, ]
    lines(
      data$Year, data[[mgmt]],
      col = mgmt_style$col,
      lty = mgmt_style$lty,
      lwd = 2.5
    )
  }
}

#' Calculate difference from baseline
#' @param data_list List of data frames
#' @return List with differences from base scenario
calculate_baseline_diff <- function(data_list) {
  lapply(data_list, function(df) {
    cols_to_adjust <- setdiff(names(df), "Year")
    df[, cols_to_adjust] <- df[, cols_to_adjust] - df$base
    df
  })
}

#' Convert units for a data list
#' @param data_list List of data frames
#' @param conversion_factor Factor to multiply by
#' @return List with converted values
convert_units <- function(data_list, conversion_factor) {
  lapply(data_list, function(df) {
    cols_to_adjust <- setdiff(names(df), "Year")
    df[, cols_to_adjust] <- df[, cols_to_adjust] * conversion_factor
    df
  })
}

# ============================================================================
# VARIABLE-SPECIFIC EXTRACTION FUNCTIONS
# ============================================================================

#' Extract timber production data
extract_timber_production <- function(source.in, country) {
  
  # Read data
  myData <- getField(source = source.in, quant = "cmass_wood_harv_sts")
  
  # Apply rolling mean
  myData@data[, Forest_roll := frollmean(
    Forest_sum, n = 5, align = "left"
  ), by = .(Lat, Lon)]
  
  # Merge with land cover
  myData@data <- myData@data[
    landcover_fractions_from_2025, 
    on = .(Lon, Lat)
  ]
  
  # Calculate areas
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat, res = 0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  # Extract country
  myData_country <- extract_country(myData, input_name = country)
  
  # Calculate weighted mean
  means_country <- myData_country@data[
    !is.na(Forest_roll),
    .(Total = sum(Forest_roll * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  # Convert units: kgC/m2 to m3/ha
  values <- means_country$Total / mean_wood_density * 10000
  
  list(values = values, years = means_country$Year)
}

#' Extract stand biomass data
extract_stand_biomass <- function(source.in, country) {
  
  # Read data
  myData <- getField(source = source.in, quant = "cmass_forest")
  
  # Merge with land cover
  myData@data <- myData@data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  # Calculate areas
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat, res = 0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  # Extract country
  myData_country <- extract_country(myData, input_name = country)
  
  # Calculate weighted mean
  weighted_means <- myData_country@data[
    !is.na(Total) & !is.na(Forest_area),
    .(weighted_Total = sum(Total * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  list(values = weighted_means$weighted_Total, years = weighted_means$Year)
}

#' Extract deadwood data
extract_deadwood <- function(source.in, country) {
  
  # Read coarse and fine litter
  clitter_coarse <- getField(source = source.in, quant = "clitter_course_sts")
  clitter_fine <- getField(source = source.in, quant = "clitter_fine_sts")
  
  # Combine
  myData <- clitter_fine
  cols <- names(myData@data)[4:ncol(myData@data)]
  myData@data[, (cols) := clitter_coarse@data[, cols, with = FALSE] + 
                clitter_fine@data[, cols, with = FALSE]]
  
  # Keep only Forest_sum
  myData@data <- myData@data[, .(Lon, Lat, Year, Forest_sum)]
  
  # Convert units: kgC/m2 to m3/ha
  myData@data$Forest_sum <- myData@data$Forest_sum / mean_wood_density * 10000
  
  # Extract country
  myData_country <- extract_country(myData, input_name = country)
  
  # Calculate mean
  means_country <- myData_country@data[
    !is.na(Forest_sum),
    .(Total = mean(Forest_sum, na.rm = TRUE)),
    by = .(Year)
  ]
  
  list(values = means_country$Total, years = means_country$Year)
}

#' Extract NBP data
extract_nbp <- function(source.in, country) {
  
  # Read data
  myData <- getField(source = source.in, quant = "cflux_forest")
  
  # Calculate NBP (sign convention: positive = uptake)
  myData@data$NBP <- (myData@data$NEE - myData@data$LU_ch) * -1.0
  
  # Merge with land cover
  myData@data <- myData@data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  # Calculate areas
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat, res = 0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  # Calculate cumulative NBP
  myData_NBP <- myData@data[
    !is.na(NBP) & !is.na(Forest_area),
    .(NBP_cumul = cumsum(NBP)),
    by = .(Lat, Lon)
  ]
  myData@data$NBP_cumul <- myData_NBP$NBP_cumul
  
  # Extract country
  myData_country <- extract_country(myData, input_name = country)
  
  # Calculate weighted mean
  weighted_means <- myData_country@data[
    !is.na(NBP_cumul) & !is.na(Forest_area),
    .(weighted_Total = sum(NBP_cumul * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  list(values = weighted_means$weighted_Total, years = weighted_means$Year)
}

#' Extract natural mortality data
extract_natural_mortality <- function(source.in, country) {
  
  # Read all mortality components
  myData_greff <- getField(source = source.in, quant = "closs_greff")
  myData_age <- getField(source = source.in, quant = "closs_age")
  myData_other <- getField(source = source.in, quant = "closs_other")
  myData_thin <- getField(source = source.in, quant = "closs_thin")
  
  # Sum all mechanisms
  myData_other@data[, 4:19] <- myData_other@data[, 4:19] + 
    myData_age@data[, 4:19] + 
    myData_greff@data[, 4:19] + 
    myData_thin@data[, 4:19]
  
  myData_other@data$Total <- rowSums(myData_other@data[, 4:19])
  
  # Merge with land cover
  myData_other@data <- myData_other@data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  # Calculate areas
  myData_other@data$GridcellArea_m2 <- GridcellArea_m2(
    myData_other@data$Lat, res = 0.5
  )
  myData_other@data$Forest_area <- myData_other@data$GridcellArea_m2 * 
    myData_other@data$FOREST
  
  # Extract country
  myData_country <- extract_country(myData_other, input_name = country)
  
  # Calculate weighted mean
  means <- myData_country@data[
    ,
    .(weighted_Total = sum(Total * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  list(values = means$weighted_Total, years = means$Year)
}

#' Extract NAI data (Net Annual Increment)
extract_nai <- function(source.in, country) {
  
  # Read data
  myData <- getField(source = source.in, quant = "forest_cflux_veg")
  
  # Merge with land cover
  myData@data <- myData@data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  # Calculate areas
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat, res = 0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  # Extract country
  myData_country <- extract_country(myData, input_name = country)
  
  # Calculate weighted mean
  weighted_means <- myData_country@data[
    !is.na(for_NAI) & !is.na(Forest_area),
    .(weighted_NAI = sum(for_NAI * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  list(values = weighted_means$weighted_NAI, years = weighted_means$Year)
}


#' Extract NAI_tot data (Net Annual Increment)
extract_nai_tot <- function(source.in, country) {
  
  # Read data
  myData <- getField(source = source.in, quant = "forest_cflux_veg")
  
  # Merge with land cover
  myData@data <- myData@data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  # Calculate areas
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat, res = 0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  # Extract country
  myData_country <- extract_country(myData, input_name = country)
  
  # Calculate weighted mean
  weighted_means <- myData_country@data[
    !is.na(tot_NAI) & !is.na(Forest_area),
    .(weighted_NAI = sum(tot_NAI * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  list(values = weighted_means$weighted_NAI, years = weighted_means$Year)
}

#' Extract GAI data (Gross Annual Increment)
extract_gai <- function(source.in, country) {
  
  # Read data
  myData <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_forest")
  myData@data$Total <- rowSums(myData@data[, 4:34])
  
  # Keep relevant columns
  myData@data <- myData@data[, .(Lon, Lat, Year, Total)]
  
  # Merge with land cover
  myData@data <- myData@data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  # Calculate areas
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat, res = 0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  # Extract country
  myData_country <- extract_country(myData, input_name = country)
  
  # Calculate weighted mean
  weighted_means <- myData_country@data[
    !is.na(Total) & !is.na(Forest_area),
    .(weighted_GAI = sum(Total * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  list(values = weighted_means$weighted_GAI, years = weighted_means$Year)
}

# ============================================================================
# DISTURBANCE MORTALITY EXTRACTION FUNCTION
# ============================================================================
# This extracts mortality from disturbances: wind, bark beetles, and fire

#' Extract disturbance mortality data
#' 
#' This combines three sources of disturbance mortality:
#' 1. Fire and other disturbances (closs_dist)
#' 2. Bark beetle damage (from storm output, InsDWC)
#' 3. Wind damage (from storm output, DamWoodC)
#' 
#' @param source.in DGVMTools source object
#' @param country Country name for extraction
#' @return List with values (kgC/m2/year) and years
extract_disturbance_mortality <- function(source.in, country) {
  
  cat("  Extracting disturbance mortality components...\n")
  
  # -------------------------------------------------------------------------
  # 1. FIRE AND OTHER DISTURBANCES
  # -------------------------------------------------------------------------
  # This gives total carbon lost to disturbances at gridcell level
  myData_dist <- getField(source = source.in, quant = "closs_dist")
  
  # Sum across diameter classes to get total
  myData_dist@data$Total <- rowSums(myData_dist@data[, 4:19], na.rm = TRUE)
  
  # Keep only essential columns
  myData_dist@data <- myData_dist@data[, .(Lon, Lat, Year, Total)]
  setnames(myData_dist@data, "Total", "Fire_Other")
  
  cat("    Fire/other disturbances: ", nrow(myData_dist@data), "rows\n")
  
  # -------------------------------------------------------------------------
  # 2. BARK BEETLE DAMAGE
  # -------------------------------------------------------------------------
  # InsDWC is total insect damage per gridcell (sum over forest area)
  # We need to convert this to per m2 basis
  myData_insects <- getField(source = source.in, quant = "storm")
  
  # Merge with land cover to get forest area
  myData_insects@data <- myData_insects@data[
    landcover_fractions_from_2025, 
    on = .(Lon, Lat)
  ]
  
  # Calculate gridcell and forest areas
  myData_insects@data$GridcellArea_m2 <- GridcellArea_m2(
    myData_insects@data$Lat, 
    res = 0.5
  )
  myData_insects@data$Forest_area <- myData_insects@data$GridcellArea_m2 * 
    myData_insects@data$FOREST
  
  # Convert from total C per gridcell to C per m2 of forest
  # InsDWC is already scaled by forest area, but we need per m2
  myData_insects@data$Insects <- myData_insects@data$InsDWC * 
    myData_insects@data$Forest_area / myData_insects@data$GridcellArea_m2
  
  # Keep only essential columns
  myData_insects@data <- myData_insects@data[, .(Lon, Lat, Year, Insects)]
  
  cat("    Insect damage: ", nrow(myData_insects@data), "rows\n")
  
  # -------------------------------------------------------------------------
  # 3. WIND DAMAGE
  # -------------------------------------------------------------------------
  # DamWoodC is damaged wood at gridcell level
  # It's already divided by forest fraction in the model output
  myData_wind <- getField(source = source.in, quant = "storm")
  
  # Merge with land cover
  myData_wind@data <- myData_wind@data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  # Wind losses need to be scaled back to gridcell area
  # (they were divided by FOREST + NATURAL in the model)
  myData_wind@data$Wind <- myData_wind@data$DamWoodC * 
    (myData_wind@data$FOREST + myData_wind@data$NATURAL)
  
  # Keep only essential columns
  myData_wind@data <- myData_wind@data[, .(Lon, Lat, Year, Wind)]
  
  cat("    Wind damage: ", nrow(myData_wind@data), "rows\n")
  
  # -------------------------------------------------------------------------
  # 4. MERGE ALL SOURCES
  # -------------------------------------------------------------------------
  # Use full joins to preserve all data and identify mismatches
  
  join_keys <- c("Year", "Lat", "Lon")
  
  # Start with wind (usually has most complete coverage)
  merged_data <- myData_wind@data
  
  # Join insects
  merged_data <- merge(
    merged_data,
    myData_insects@data,
    by = join_keys,
    all = TRUE  # Full join
  )
  cat("    After insects merge: ", nrow(merged_data), "rows\n")
  
  # Join fire/other
  merged_data <- merge(
    merged_data,
    myData_dist@data,
    by = join_keys,
    all = TRUE  # Full join
  )
  cat("    After fire/other merge: ", nrow(merged_data), "rows\n")
  
  # Check for missing data
  n_missing_wind <- sum(is.na(merged_data$Wind))
  n_missing_insects <- sum(is.na(merged_data$Insects))
  n_missing_fire <- sum(is.na(merged_data$Fire_Other))
  
  if (n_missing_wind > 0) {
    warning("  ", n_missing_wind, " rows missing wind data")
  }
  if (n_missing_insects > 0) {
    warning("  ", n_missing_insects, " rows missing insect data")
  }
  if (n_missing_fire > 0) {
    warning("  ", n_missing_fire, " rows missing fire/other data")
  }
  
  # Replace NAs with 0 (assume no disturbance if missing)
  merged_data[is.na(Wind), Wind := 0]
  merged_data[is.na(Insects), Insects := 0]
  merged_data[is.na(Fire_Other), Fire_Other := 0]
  
  # Calculate total disturbance mortality
  merged_data$mort_dist <- merged_data$Wind + 
    merged_data$Insects + 
    merged_data$Fire_Other
  
  # -------------------------------------------------------------------------
  # 5. AGGREGATE TO COUNTRY LEVEL
  # -------------------------------------------------------------------------
  # Add forest area for weighting
  merged_data <- merged_data[
    landcover_fractions_from_2025,
    on = .(Lon, Lat)
  ]
  
  merged_data$GridcellArea_m2 <- GridcellArea_m2(merged_data$Lat, res = 0.5)
  merged_data$Forest_area <- merged_data$GridcellArea_m2 * merged_data$FOREST
  
  # Convert to DGVMTools format for country extraction
  myData_dist@data <- merged_data
  
  # Extract country
  myData_country <- extract_country(myData_dist, input_name = country)
  
  # Calculate area-weighted mean
  means <- myData_country@data[
    !is.na(mort_dist) & !is.na(Forest_area),
    .(weighted_Total = sum(mort_dist * Forest_area, na.rm = TRUE) / 
        sum(Forest_area, na.rm = TRUE)),
    by = .(Year)
  ]
  
  cat("    Final data: ", nrow(means), " years\n")
  
  # Diagnostic: show breakdown for first year
  if (nrow(means) > 0) {
    first_year_data <- myData_country@data[Year == means$Year[1]]
    wind_contrib <- weighted.mean(
      first_year_data$Wind, 
      first_year_data$Forest_area, 
      na.rm = TRUE
    )
    insect_contrib <- weighted.mean(
      first_year_data$Insects,
      first_year_data$Forest_area,
      na.rm = TRUE
    )
    fire_contrib <- weighted.mean(
      first_year_data$Fire_Other,
      first_year_data$Forest_area,
      na.rm = TRUE
    )
    
    cat("    Year", means$Year[1], "breakdown (kgC/m2/year):\n")
    cat("      Wind:        ", sprintf("%.4f", wind_contrib), "\n")
    cat("      Insects:     ", sprintf("%.4f", insect_contrib), "\n")
    cat("      Fire/Other:  ", sprintf("%.4f", fire_contrib), "\n")
    cat("      Total:       ", sprintf("%.4f", means$weighted_Total[1]), "\n")
  }
  
  list(values = means$weighted_Total, years = means$Year)
}
