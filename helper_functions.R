

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
extract_country <- function(myData,input_name) {
  
  europe_Cnames <- read.table("../european_applications/testrun/gridlist_final.txt_country_names_added",head=TRUE)
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