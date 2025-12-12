# ForestPaths LPJ-GUESS output post-processing and plotting script
# Author: Annemarie Eckes-Shephard
# Refactored for readability and maintainability

# ============================================================================
# SETUP
# ============================================================================

library(data.table)
library(DGVMTools)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reldist)

source("helper_functions.R")

# Configuration
PLOT_ALL <- FALSE
BASE_DIR <- "/Volumes/Anne's Backup/v13/"


# ============================================================================
# DEFINE MANAGEMENT SCENARIOS AND COUNTRIES
# ============================================================================

# Management scenarios (excluding fertfor, ceaseman, ceaseman_cct)
managements_list <- c("base", "lightthin", "intensthin", "longrot",
                      "shortrot", "rettree", "sfire", "ccf")

# Visual styling for managements
management_df <- data.frame(
  management = managements_list,
  col = c(1, "#0A9B49", "#A18F2D", "#01B0F0", "#D430F5", 
          "#7030A0", "#973735", "#FBA918"),
  pch = c(1, 3, 3, 4, 4, 5, 7, 8),
  lty = c(1, 3, 3, 4, 4, 5, 7, 8),
  stringsAsFactors = FALSE
)

# Archetype countries
archetype_countries <- c("Poland", "Sweden", "Germany", "Spain")
country_colours <- c("#54AC0C", "#AD8B1A", "#E960CC", "#DF726A")
archetypes_df <- data.frame(
  country = archetype_countries, 
  colour = country_colours
)

# All countries (should be defined elsewhere - placeholder)
country_list <- archetype_countries  # Replace with full list

# Scenarios
scenario_list <- c("ssp370")#,"ssp370")

# ============================================================================
# LOAD REFERENCE DATA
# ============================================================================

# Land cover fractions
landcover_fractions_from_2025 <- fread(
  "../landuse_change_forest_age/lu_inputfiles_noNF3/net_lu_HildaPucherMircaEurope.txt"
)[Year == 2025]

# Forest cover fractions
forestcover_fractions_from_2025 <- fread(
  "../landuse_change_forest_age/lu_inputfiles_noNF3/luforest_HildaPucherMircaEurope.txt"
)[Year == 2025]

# Mean wood density 
mean_wood_density <- mean_wooddensity_df[mean_wooddensity_df$Luforest == "Forest_sum", "MeanWoodDensity"]


# ============================================================================
# MAIN PROCESSING WORKFLOW
# ============================================================================

#' Main function to process all variables
main <- function() {
  
  cat("\n====================================\n")
  cat("ForestPaths Post-Processing Started\n")
  cat("====================================\n\n")
  
  # 1.a TIMBER PRODUCTION wood C
  timber_config <- list(
    name = "Timber Production",
    extract_fn = extract_timber_production_wood_C,
    n_years = 73
  )
  timber_data_wood <- process_variable(timber_config)
  
  # Plot in m3/ha/year
  create_country_plots(
    timber_data_wood,
    list(
      filename = "Timber_production_m3ha_cmass_wood_harv_sts.pdf",
      title = "timber production, woodC, m3/ha/year",
      ylab = "timber production, m3/ha/year",
      ylim = c(0, 6),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  if(scenario == "ssp370"){
    ylim =  c(-1, 1)
  }else{
    ylim =  c(-1.5, 1.5)
  }
  # Plot differences from baseline (m3/ha)
  timber_wood_diff_m3 <- calculate_baseline_diff(timber_data_wood)
  create_country_plots(
    timber_wood_diff_m3,
    list(
      filename = "Timber_production_m3ha_diff_cmass_wood_harv_sts.pdf",
      title = expression(Delta ~ "timber production, woodC, m3/ha/year"),
      ylab = expression(Delta ~ "timber production, m3/ha/year"),
      ylim = ylim,
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  # Convert to kgC/m2/year and plot
  timber_wood_kgC <- convert_units(timber_data_wood, mean_wood_density / 10000)
  create_country_plots(
    timber_wood_kgC,
    list(
      filename = "Timber_production_kgCm2year_cmass_wood_harv_sts.pdf",
      title = "timber production, woodC, kgC/m2/year",
      ylab = "timber production, kgC/m2/year",
      ylim = c(0, 0.25),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  if(scenario == "ssp370"){
    ylim = c(-0.03, 0.03)
  }else{
    ylim = c(-0.05, 0.05)
  }
  # Plot differences from baseline (kgC/m2/year)
  timber_wood_diff_kgC <- calculate_baseline_diff(timber_wood_kgC)
  create_country_plots(
    timber_wood_diff_kgC,
    list(
      filename = "Timber_production_kgCm2year_diff_cmass_wood_harv_sts.pdf",
      title = expression(Delta ~ "timber production, woodC, kgC/m2/year"),
      ylab = expression(Delta ~ "timber production, kgC/m2/year"),
      ylim = ylim,
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  # 1.b TIMBER PRODUCTION all C
  timber_config_allC <- list(
    name = "Timber Production",
    extract_fn = extract_timber_production_allC,
    n_years = 73
  )
  timber_data_allC <- process_variable(timber_config_allC)
  
  
  if(scenario == "ssp370"){
    ylim = c(0, 10)
  }else{
    ylim = c(0, 20)
  }
  
  # Plot in m3/ha/year
  create_country_plots(
    timber_data_allC,
    list(
      filename = "Timber_production_m3ha_cmass_harv_killed_sts.pdf",
      title = "timber production, totC, m3/ha/year",
      ylab = "timber production, m3/ha/year",
      ylim = ylim,
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  if(scenario == "ssp370"){
    ylim =   c(-2, 2)
  }else{
    ylim =   c(-4.5, 4.5)
  }
  # Plot differences from baseline (m3/ha)
  timber_allC_diff_m3 <- calculate_baseline_diff(timber_data_allC)
  create_country_plots(
    timber_allC_diff_m3,
    list(
      filename = "Timber_production_m3ha_diff_cmass_harv_killed_sts.pdf",
      title = expression(Delta ~ "timber production, totC, m3/ha/year"),
      ylab = expression(Delta ~ "timber production, m3/ha/year"),
      ylim = ylim,
      legend_position = "bottomleft",
      legend_country = "Spain"
    )
  )
  
  
  if(scenario == "ssp370"){
    ylim =  c(0, 0.3)
  }else{
    ylim =  c(0, 0.55)
  }
  
  # Convert to kgC/m2/year and plot
  timber_allC_kgC <- convert_units(timber_data_allC, mean_wood_density / 10000)
  create_country_plots(
    timber_allC_kgC,
    list(
      filename = "Timber_production_kgCm2year_cmass_harv_killed_sts.pdf",
      title = "timber production, totC, kgC/m2/year",
      ylab = "timber production, kgC/m2/year",
      ylim = ylim,
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  
  if(scenario == "ssp370"){
    ylim = c(-0.1, 0.1)
  }else{
    ylim =  c(-0.4, 0.4)
  }
  
  # Plot differences from baseline (kgC/m2/year)
  timber_allC_diff_kgC <- calculate_baseline_diff(timber_allC_kgC)
  create_country_plots(
    timber_allC_diff_kgC,
    list(
      filename = "Timber_production_kgCm2year_diff_cmass_harv_killed_sts.pdf",
      title = expression(Delta ~ "timber production, totC, kgC/m2/year"),
      ylab = expression(Delta ~ "timber production, kgC/m2/year"),
      ylim = ylim,
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  
  # 2. STAND BIOMASS
  stand_config <- list(
    name = "Stand Biomass",
    extract_fn = extract_stand_biomass,
    n_years = 77
  )
  stand_data <- process_variable(stand_config)
  
  create_country_plots(
    stand_data,
    list(
      filename = "StandC_kgCm2.pdf",
      title = "stand biomass, kgC/m2",
      ylab = "stand biomass, kgC/m2",
      ylim = c(5, 12),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  stand_data_diff <- calculate_baseline_diff(stand_data)
  create_country_plots(
    stand_data_diff,
    list(
      filename = "StandC_kgCm2_diff.pdf",
      title =  expression(Delta ~"stand biomass, kgC/m2"),
      ylab =  expression(Delta ~"stand biomass, kgC/m2"),
      ylim = c(-2, 2),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  # 3. DEADWOOD
  deadwood_config <- list(
    name = "Deadwood",
    extract_fn = extract_deadwood,
    n_years = 77
  )
  deadwood_data <- process_variable(deadwood_config)
  
 
  #conversion to kgC/m2 
  #[TODO] check conversion was correct:
  create_country_plots(
    deadwood_data,
    list(
      filename = "Deadwood_kgCm2.pdf",
      title = "Deadwood pool, kgC/m2",
      ylab = "Deadwood pool,kgC/m2",
      ylim = c(0, 6.2),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  
  #difference plot for deadwood in kgC
  deadwood_kg_diff <- calculate_baseline_diff(deadwood_data)
  create_country_plots(
    deadwood_kg_diff,
    list(
      filename = "Deadwood_kgCm2_diff.pdf",
      title = expression(Delta ~"Deadwood pool, kgC/m2"),
      ylab = expression(Delta ~"Deadwood pool,kgC/m2"),
      ylim = c(-0.5, 0.5),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  deadwood_data_m3ha <- convert_units(deadwood_data, 1/(0.5 * mean_wood_density) * 10000 ) # note: 0.5 assumption is wrong here!!!
  ### conversion to m3/ha
  create_country_plots(
    deadwood_data_m3ha,
    list(
      filename = "Deadwood_m3ha.pdf",
      title = "Deadwood pool, m3/ha",
      ylab = "Deadwood pool, m3/ha",
      ylim = c(40, 420),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  deadwood_diff <- calculate_baseline_diff(deadwood_data_m3ha)
  create_country_plots(
    deadwood_diff,
    list(
      filename = "Deadwood_m3ha_diff.pdf",
      title = expression(Delta ~ "Deadwood pool, m3/ha"),
      ylab = expression(Delta ~ "Deadwood pool, m3/ha"),
      ylim = c(-20, 20),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  # 4. NBP
  nbp_config <- list(
    name = "NBP",
    extract_fn = extract_nbp,
    n_years = 77
  )
  nbp_data <- process_variable(nbp_config)
  
  create_country_plots(
    nbp_data,
    list(
      filename = "NBP_cumul_kgCm2.pdf",
      title = "cumulative NBP, kgC/m2",
      ylab = "cumulative NBP, kgC/m2",
      ylim = c(0, 4),
      legend_position = "center",
      legend_country = "Spain"
    )
  )
  
  # 5. NATURAL MORTALITY
  mortality_config <- list(
    name = "Natural Mortality",
    extract_fn = extract_natural_mortality,
    n_years = 77
  )
  mortality_data <- process_variable(mortality_config)
  
  
  #running mean:
  mortality_data_smoothed <- apply_rolling_mean(mortality_data,n = 5)
  
  
  if(scenario == "ssp370"){
    ylim = c(0, 0.2)
  }else{
    ylim = c(0, 0.15)
  }
  
  create_country_plots(
    mortality_data_smoothed,
    list(
      filename = "mortality_natural_kgCm2year.pdf",
      title = "Carbon lost through natural mortality, kgC/m2/year",
      ylab = "Carbon lost through natural mortality, kgC/m2/year",
      ylim = c(0, 0.15),
      legend_position = "bottomright",
      legend_country = "Poland",
      cex=1.7
    )
  )
  
  #running mean:
  
  # Calculate mortality as percentage of stand biomass
  mortality_pct <- lapply(names(stand_data), function(country) {
    dfB <- stand_data[[country]]
    dfM <- mortality_data[[country]]
    mgmt_cols <- setdiff(colnames(dfB), "Year")
    rate_df <- dfM
    rate_df[, mgmt_cols] <- (dfM[, mgmt_cols] / dfB[, mgmt_cols]) * 100
    rate_df
  })
  names(mortality_pct) <- names(stand_data)
  
  mortality_pct_smoothed <- apply_rolling_mean(mortality_pct,n = 5)
  create_country_plots(
    mortality_pct_smoothed,
    list(
      filename = "mortality_natural_percyear.pdf",
      title = "Natural mortality rate %/year",
      ylab = "Natural mortality rate, %/year",
      ylim = c(0, 4.5),
      legend_position = "center",
      legend_country = "Poland"
    )
  )
  
  
  # calculate difference( in natural mortality rate (%)
  #[TOASK] [?] create smoothing for difference plots?
  mortality_pct_diff <- calculate_baseline_diff(mortality_pct)
  mortality_pct_diff_smoothed <- apply_rolling_mean(mortality_pct_diff,n = 5)
  create_country_plots(
    mortality_pct_diff_smoothed,
    list(
      filename = "mortality_natural_percyear_diff.pdf",
      title = expression(Delta ~"Natural mortality rate %/year"),
      ylab = expression(Delta ~"Natural mortality rate, %/year"),
      ylim = c(-0.2, 0.2),
      legend_position = "bottomright",
      legend_country = "Poland"
    )
  )
  
  
  # 6a. NAI
  nai_config <- list(
    name = "Net Annual Increment",
    extract_fn = extract_nai,
    n_years = 77
  )
  nai_data <- process_variable(nai_config)
  
  
  if(scenario == "ssp370"){
    ylim = c(-0.2, 0.4)
  }else{
    ylim = c(-0.1, 0.4)
  }
  
  create_country_plots(
    nai_data,
    list(
      filename = "NAI_kgCyear.pdf",
      title = "NAI, kgC/m2/year",
      ylab = "NAI, kgC/m2/year",
      ylim = ylim,
      legend_position = "bottomright",
      legend_country = "Spain"
    )
  )
  # NAI  difference in kgC/m2/year
  nai_kgC_diff <- calculate_baseline_diff(nai_data)
  create_country_plots(
    nai_kgC_diff,
    list(
      filename = "NAI_kgCyear_diff.pdf",
      title = expression(Delta~"NAI, kgC/m2/year"),
      ylab = expression(Delta~"NAI, kgC/m2/year"),
      ylim = c(-0.05, 0.05),
      legend_position = "bottomright",
      legend_country = "Poland"
    )
  )
  
  if(scenario == "ssp370"){
    ylim = c(-6, 16)
  }else{
    ylim = c(-5, 15)
  }
  
  # Convert to m3/ha/year
  nai_m3 <- convert_units(nai_data, 10000 / (0.5 * mean_wood_density))
  create_country_plots(
    nai_m3,
    list(
      filename = "NAI_m3hayr.pdf",
      title = "NAI, m3/ha/year",
      ylab = "NAI, m3/ha/year",
      ylim = ylim,
      legend_position = "bottomright",
      legend_country = "Poland"
    )
  )
  
  
  if(scenario == "ssp370"){
    legend_position = "bottomleft"
    ylim = c(-2.3, 2.3)
  }else{
    legend_position = "bottomright"
    ylim = c(-2, 2)
  }
  
  #calculate NAI difference
  nai_m3_diff <- calculate_baseline_diff(nai_m3)
  create_country_plots(
    nai_m3_diff,
    list(
      filename = "NAI_m3hayr_diff.pdf",
      title = expression(Delta~"NAI, m3/ha/year"),
      ylab = expression(Delta~"NAI, m3/ha/year"),
      ylim = ylim,
      legend_position = legend_position,
      legend_country = "Poland"
    )
  )
  
  # 6b. NAI_tot
  nai_config <- list(
    name = "Net Annual Increment",
    extract_fn = extract_nai_tot,
    n_years = 77
  )
  nai_tot_data <- process_variable(nai_config)
  
  
  if(scenario == "ssp370"){
    ylim = c(-0.2, 0.3)
  }else{
    ylim = c(-0.1, 0.2)
  }
  
  create_country_plots(
    nai_tot_data,
    list(
      filename = "NAI_tot_kgCyear.pdf",
      title = "NAI, kgC/m2/year",
      ylab  = "NAI, kgC/m2/year",
      ylim  = ylim,
      legend_position = "bottomright",
      legend_country  = "Spain"
    )
  )
  
  # NAI_tot difference pot in kgC/m2/year
  nai_kgC_diff <- calculate_baseline_diff(nai_tot_data)
  create_country_plots(
    nai_kgC_diff,
    list(
      filename = "NAI_tot_kgCyear_diff.pdf",
      title = expression(Delta~"NAI, kgC/m2/year"),
      ylab = expression(Delta~"NAI, kgC/m2/year"),
      ylim = c(-0.04, 0.04),
      legend_position = "bottomleft",
      legend_country = "Germany"
    )
  )
  
  
  if(scenario == "ssp370"){
    ylim = c(-10, 20)
  }else{
    ylim =  c(-5, 15)
  }
  
  # Convert to m3/ha/year
  nai_m3 <- convert_units(nai_tot_data, 10000 / (0.5 * mean_wood_density))
  create_country_plots(
    nai_m3,
    list(
      filename = "NAI_tot_m3hayr.pdf",
      title = "NAI, m3/ha/year",
      ylab = "NAI, m3/ha/year",
      ylim = ylim,
      legend_position = "bottomright",
      legend_country = "Poland"
    )
  )
  
  
  if(scenario == "ssp370"){
    ylim =  c(-2.7, 2.7) 
  }else{
    ylim =  c(-2.5, 2.5)
  }
  # NAI_tot difference pot in m3/ha/year
  nai_m3_diff <- calculate_baseline_diff(nai_m3)
  create_country_plots(
    nai_m3_diff,
    list(
      filename = "NAI_tot_m3hayr_diff.pdf",
      title = expression(Delta~"NAI, m3/ha/year"),
      ylab = expression(Delta~"NAI, m3/ha/year"),
      ylim = ylim,
      legend_position = "bottomleft",
      legend_country = "Germany"
    )
  )
  
  
  
  # 7. GAI
  gai_config <- list(
    name = "Gross Annual Increment",
    extract_fn = extract_gai,
    n_years = 77
  )
  gai_data <- process_variable(gai_config)
  
  create_country_plots(
    gai_data,
    list(
      filename = "GAI_kgCyear.pdf",
      title = "GAI, kgC/m2/year",
      ylab = "GAI, kgC/m2/year",
      ylim = c(0, 0.5),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  # GAI_tot difference pot in m3/ha/year
  gai_data_diff <- calculate_baseline_diff(gai_data)
  create_country_plots(
    gai_data_diff,
    list(
      filename = "GAI_kgCyear_diff.pdf",
      title = expression(Delta~"GAI, kgC/m2/year"),
      ylab = expression(Delta~"GAI, kgC/m2/year"),
      ylim = c(-0.025, 0.025),
      legend_position = "bottomleft",
      legend_country = "Germany"
    )
  )
  
  # Convert to m3/ha/year
  gai_m3 <- convert_units(gai_data, 10000 / (0.5 * mean_wood_density))
  create_country_plots(
    gai_m3,
    list(
      filename = "GAI_m3hayr.pdf",
      title = "GAI, m3/ha/year",
      ylab = "GAI, m3/ha/year",
      ylim = c(0, 30),
      legend_position = "bottomright",
      legend_country = "Poland"
    )
  )
  # NAI_tot difference pot in m3/ha/year
  gai_m3_diff <- calculate_baseline_diff(gai_m3)
  create_country_plots(
    gai_m3_diff,
    list(
      filename = "GAI_m3hayr_diff.pdf",
      title = expression(Delta~"GAI, m3/ha/year"),
      ylab = expression(Delta~"GAI, m3/ha/year"),
      ylim = c(-2, 2),
      legend_position = "bottomleft",
      legend_country = "Germany"
    )
  )
  
  # 8. DISTURBANCE MORTALITY
  disturbance_mortality_config <- list(
    name = "Disturbance Mortality",
    extract_fn = extract_disturbance_mortality,
    n_years = 77
  )
  disturbance_mortality_data <- process_variable(disturbance_mortality_config)
  
  
  # apply running mean before plotting:
  disturbance_mortality_data_smoothed <- apply_rolling_mean(disturbance_mortality_data,n = 5)
  # Plot absolute values
  create_country_plots(
    disturbance_mortality_data_smoothed,
    list(
      filename = "mortality_disturbance_kgCm2year.pdf",
      title = "Carbon lost through disturbance mortality, kgC/m2/year",
      ylab = "Carbon lost through disturbance mortality, kgC/m2/year",
      ylim = c(0, 0.05),
      legend_position = "center",
      legend_country = "Spain",
      cex = 1.7
    )
  )
  
  # apply running mean before plotting:
  # Calculate as percentage of stand biomass
  disturbance_mortality_pct <- lapply(names(stand_data), function(country) {
    dfB <- stand_data[[country]]
    dfM <- disturbance_mortality_data[[country]]
    mgmt_cols <- setdiff(colnames(dfB), "Year")
    rate_df <- dfM
    rate_df[, mgmt_cols] <- (dfM[, mgmt_cols] / dfB[, mgmt_cols]) * 100
    rate_df
  })
  names(disturbance_mortality_pct) <- names(stand_data)
  
  
  # apply running mean before plotting:
  disturbance_mortality_pct_smoothed <- apply_rolling_mean(disturbance_mortality_pct,n = 5)
  create_country_plots(
    disturbance_mortality_pct_smoothed,
    list(
      filename = "mortality_disturbance_percyear.pdf",
      title = "Disturbance mortality rate, %/year",
      ylab = "Disturbance mortality rate, %/year",
      ylim = c(0, 0.5),
      legend_position = "bottomright",
      legend_country = "Sweden",
      cex = 1.7
    )
  )
  

  # Plot differences from baseline (percentage)
  disturbance_mortality_pct_diff <- calculate_baseline_diff(disturbance_mortality_pct)
  # apply running mean before plotting:
  disturbance_mortality_pct_diff_smoothed <- apply_rolling_mean(disturbance_mortality_pct_diff,n = 5)
  create_country_plots(
    disturbance_mortality_pct_diff_smoothed,
    list(
      filename = "mortality_disturbance_percyear_diff.pdf",
      title = expression(Delta ~ "Disturbance mortality rate, %/year"),
      ylab = expression(Delta ~ "Disturbance mortality rate, %/year"),
      ylim = c(-0.1, 0.1),
      legend_position = "bottom",
      legend_country = "Spain",
      cex = 1.7
    )
  )
  
  
  # Plot differences from baseline (absolute)
  disturbance_mortality_diff <- calculate_baseline_diff(disturbance_mortality_data)
  # apply running mean before plotting:
  disturbance_mortality_pct_diff_smoothed <- apply_rolling_mean(disturbance_mortality_diff,n = 5)
  create_country_plots(
    disturbance_mortality_pct_diff_smoothed,
    list(
      filename = "mortality_disturbance_kgCm2year_diff.pdf",
      title = expression(Delta ~ "Carbon lost through disturbance mortality, kgC/m2/year"),
      ylab = expression(Delta ~ "Carbon lost through disturbance mortality, kgC/m2/year"),
      ylim = c(-0.01, 0.01),
      legend_position = "bottomleft",
      legend_country = "Poland",
      cex = 1.7
    )
  )
  
  
  
  cat("\n====================================\n")
  cat("Post-Processing Complete!\n")
  cat("====================================\n\n")
}

# Run the main workflow
if (interactive() || !exists("testing_mode")) {
  main()
}


#============================================================================================================
#difference maps:
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map (simplified polygons)
world <- ne_countries(scale = "medium", returnclass = "sf")




#############SSP370
#Baseline scenario for 370
scenario ="ssp370"
#retrieve the source file location + add some metadata:
source.in = defineSource(id= paste0("base_",scenario),
                         paste0(base_dir,"base_fut_MPI-ESM1-2-HR_",scenario,"_diston"),
                         format = GUESS,
                         name = paste0("base_",scenario))

myData <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_forest")
myData@data$Total <- rowSums(myData@data[, 4:34])

# Keep relevant columns
myData@data <- myData@data[, .(Lon, Lat, Year, Total)]

# Merge with land cover
myData@data <- myData@data[
  landcover_fractions_from_2025,
  on = .(Lon, Lat)
]
myData@data <- myData@data[landcover_fractions_from_2025, on = .(Lon, Lat)]  
myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat,res=0.5)
myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST


myData_base <- myData



for(management in managementslist[-1]){
  
    source.in = defineSource(id= paste0(management,"_",scenario),
                             dir= paste0(base_dir,management,"_fut_MPI-ESM1-2-HR_ssp370_diston"),
                             format = GUESS,
                             name = paste0(management,"_",scenario))
    
    myData <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_forest")
    myData@data$Total  <-  rowSums(myData@data[,4:dim(myData@data)[2]])#/10000 #unitconversion m2 to ha
    myData@data <- myData@data[landcover_fractions_from_2025, on = .(Lon, Lat)]  
    myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat,res=0.5)
    myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
    
    
    #extract years first:
    myData2050 <- myData
    myData2050@data <- myData@data[which(myData@data$Year > 2040 & myData@data$Year <= 2050),]
    
    myData_base_2050 <- myData_base
    myData_base_2050@data <- myData_base_2050@data[which(myData_base_2050@data$Year > 2040 & myData_base_2050@data$Year  <= 2050),]
    
    
    # identify the columns with size classes (all except Lon, Lat, Year)
    size_cols <- setdiff(names(myData_base_2050@data), c("Lon", "Lat", "Year"))
    size_cols <- c("Year","Total","Forest_area")
   
    # compute mean across years for each Lon, Lat
    myData_base_2050@data <- myData_base_2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                                   by = .(Lon, Lat), 
                                                   .SDcols = size_cols]
    # compute mean across years for each Lon, Lat
    myData2050@data <- myData2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                       by = .(Lon, Lat), 
                                       .SDcols = size_cols]
    
    myData_base_2050@data$Total_diff <-   myData2050@data$Total -  myData_base_2050@data$Total 
    
    #unitconversion:
    #myData_base_2050@data$Total_diff <- myData_base_2050@data$Total_diff*0.001*10000 # ton/ha
    
    # Define Europe extent (rough bounding box)
    xlim <- c(-11, 35)   # longitude range
    ylim <- c(34, 72)    # latitude range
    
    if(management=="lightthin"){
      limits = c(-0.025,0.03)
    }else 
    if(management=="intensthin"){
      limits = c(-0.05,0.04)
    }else 
    if(management=="rettree"){
      limits = c(-0.04,0.04)
    }else 
    if(management=="sfire"){
      limits = c(-0.03,0.03)
    }else if(management=="ccf"){
      limits = c(-0.04,0.05)
    }else(
      limits = range(myData_base_2050@data$Total_diff)
    )
    
    # Get world map (simplified polygons)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    png(filename=paste0("../Figures_and_reports/Policy_Lab_maps/",scenario,"_2050_GAI_map_",management,"-baseline.png"),
        width = 120, height = 90, units = "mm", res = 630)  # bigger canvas
    
    # Plot using ggplot2
    print(
      ggplot() + 
      
      # Background: world polygons (grey land, lightblue sea background)
      geom_sf(data = world, fill = "grey90", color = "grey40") +
      
      geom_tile(data= myData_base_2050@data, aes(x = Lon, y = Lat, fill = Total_diff)) +
      
      scale_fill_gradient2(
        low = "blue",    # negative values
        mid = "white",   # zero
        high = "red",    # positive values
        midpoint = 0,    # center of scale
        limits = limits,
        name = expression(Delta ~ "kgC/m2")
      ) +
      coord_sf(xlim=xlim,ylim=ylim,expand=FALSE) +
      labs(
        x = "Longitude",
        y = "Latitude",
        # title = paste("Difference in cumulative carbon uptake between\n", management , " and base over 25 years")
        title = paste( management ,"- base in 2050")#"- base"
      ) +
      theme_minimal()
    )
    
    dev.off()
}


for(management in managementslist[-1]){
  
  ###spatial analysis 1:
  source.in = defineSource(id= paste0(management,"_",scenario),
                           dir= paste0(base_dir,management,"_fut_MPI-ESM1-2-HR_ssp370_diston"),
                           format = GUESS,
                           name = paste0(management,"_",scenario))
  
  myData <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_forest")
  myData@data$Total  <-  rowSums(myData@data[,4:dim(myData@data)[2]])#/10000 #unitconversion m2 to ha
  myData@data <- myData@data[landcover_fractions_from_2025, on = .(Lon, Lat)]  
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat,res=0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  
  #extract years first:
  myData2050 <- myData
  myData2050@data <- myData@data[which(myData@data$Year > 2090 & myData@data$Year <= 2100),]
  
  myData_base_2050 <- myData_base
  myData_base_2050@data <- myData_base_2050@data[which(myData_base_2050@data$Year > 2090 & myData_base_2050@data$Year  <= 2100),]
  
  
  # identify the columns with size classes (all except Lon, Lat, Year)
  size_cols <- setdiff(names(myData_base_2050@data), c("Lon", "Lat", "Year"))
  size_cols <- c("Year","Total","Forest_area")
  
  # compute mean across years for each Lon, Lat
  myData_base_2050@data <- myData_base_2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                                 by = .(Lon, Lat), 
                                                 .SDcols = size_cols]
  # compute mean across years for each Lon, Lat
  myData2050@data <- myData2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                     by = .(Lon, Lat), 
                                     .SDcols = size_cols]
  
  myData_base_2050@data$Total_diff <-   myData2050@data$Total -  myData_base_2050@data$Total 
  
  #unitconversion:
  #myData_base_2050@data$Total_diff <- myData_base_2050@data$Total_diff*0.001*10000 # ton/ha
  
  # Define Europe extent (rough bounding box)
  xlim <- c(-11, 35)   # longitude range
  ylim <- c(34, 72)    # latitude range
  if(management=="lightthin"){
    limits = c(-0.08,0.1)
  }else
  if(management=="intensthin"){
    limits = c(-0.07,0.1)
  }else
  if(management=="rettree"){
    limits = c(-0.12,0.06)
  }else
  if(management=="sfire"){
    limits = c(-0.05,0.06)
  }else(
    limits = range(myData_base_2050@data$Total_diff)
  )
  
  # Get world map (simplified polygons)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  
  png(filename=paste0("../Figures_and_reports/Policy_Lab_maps/",scenario,"_2100_GAI_map_",management,"-baseline.png"),
      width = 120, height = 90, units = "mm", res = 630)  # bigger canvas
  
  # Plot using ggplot2
  print(
    ggplot() + 
    
    # Background: world polygons (grey land, lightblue sea background)
    geom_sf(data = world, fill = "grey90", color = "grey40") +
    
    geom_tile(data= myData_base_2050@data, aes(x = Lon, y = Lat, fill = Total_diff)) +
    
    scale_fill_gradient2(
      low = "blue",    # negative values
      mid = "white",   # zero
      high = "red",    # positive values
      midpoint = 0,    # center of scale
      limits=limits,
      name = expression(Delta ~ "kgC/m2")
    ) +
    coord_sf(xlim=xlim,ylim=ylim,expand=FALSE) +
    labs(
      x = "Longitude",
      y = "Latitude",
      # title = paste("Difference in cumulative carbon uptake between\n", management , " and base over 25 years")
      title = paste( management ,"- base in 2100")#"- base"
    ) +
    theme_minimal()
  )
  
  dev.off()
}







#############SSP126
#Baseline scenario for 370
scenario ="ssp126"
#retrieve the source file location + add some metadata:
source.in = defineSource(id= paste0("base_",scenario),
                         paste0(base_dir,"base_fut_MPI-ESM1-2-HR_",scenario,"_diston"),
                         format = GUESS,
                         name = paste0("base_",scenario))

myData <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_forest")
myData@data$Total <- rowSums(myData@data[, 4:34])

# Keep relevant columns
myData@data <- myData@data[, .(Lon, Lat, Year, Total)]

# Merge with land cover
myData@data <- myData@data[
  landcover_fractions_from_2025,
  on = .(Lon, Lat)
]
myData@data <- myData@data[landcover_fractions_from_2025, on = .(Lon, Lat)]  
myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat,res=0.5)
myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST


myData_base <- myData



for(management in managementslist[-1]){
  
  source.in = defineSource(id= paste0(management,"_",scenario),
                           dir= paste0(base_dir,management,"_fut_MPI-ESM1-2-HR_ssp126_diston"),
                           format = GUESS,
                           name = paste0(management,"_",scenario))
  
  myData <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_forest")
  myData@data$Total  <-  rowSums(myData@data[,4:dim(myData@data)[2]])#/10000 #unitconversion m2 to ha
  myData@data <- myData@data[landcover_fractions_from_2025, on = .(Lon, Lat)]  
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat,res=0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  
  #extract years first:
  myData2050 <- myData
  myData2050@data <- myData@data[which(myData@data$Year > 2040 & myData@data$Year <= 2050),]
  
  myData_base_2050 <- myData_base
  myData_base_2050@data <- myData_base_2050@data[which(myData_base_2050@data$Year > 2040 & myData_base_2050@data$Year  <= 2050),]
  
  
  # identify the columns with size classes (all except Lon, Lat, Year)
  size_cols <- setdiff(names(myData_base_2050@data), c("Lon", "Lat", "Year"))
  size_cols <- c("Year","Total","Forest_area")
  
  # compute mean across years for each Lon, Lat
  myData_base_2050@data <- myData_base_2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                                 by = .(Lon, Lat), 
                                                 .SDcols = size_cols]
  # compute mean across years for each Lon, Lat
  myData2050@data <- myData2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                     by = .(Lon, Lat), 
                                     .SDcols = size_cols]
  
  myData_base_2050@data$Total_diff <-   myData2050@data$Total -  myData_base_2050@data$Total 
  
  #unitconversion:
  #myData_base_2050@data$Total_diff <- myData_base_2050@data$Total_diff*0.001*10000 # ton/ha
  
  # Define Europe extent (rough bounding box)
  xlim <- c(-11, 35)   # longitude range
  ylim <- c(34, 72)    # latitude range
  if(management=="longrot"){
    limits = c(-0.025,0.04)
  }else 
    if(management=="intensthin"){
      limits = c(-0.05,0.04)
    }else 
      if(management=="rettree"){
        limits = c(-0.04,0.08)
      }else 
        if(management=="sfire"){
          limits = c(-0.03,0.04)
        }else if(management=="ccf"){
          limits = c(-0.04,0.05)
        }else(
          limits = range(myData_base_2050@data$Total_diff)
        )
 
  
  
  # Get world map (simplified polygons)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  png(filename=paste0("../Figures_and_reports/Policy_Lab_maps/",scenario,"_2050_GAI_map_",management,"-baseline.png"),
      width = 120, height = 90, units = "mm", res = 630)  # bigger canvas
  
  # Plot using ggplot2
  print(
    ggplot() + 
      
      # Background: world polygons (grey land, lightblue sea background)
      geom_sf(data = world, fill = "grey90", color = "grey40") +
      
      geom_tile(data= myData_base_2050@data, aes(x = Lon, y = Lat, fill = Total_diff)) +
      
      scale_fill_gradient2(
        low = "blue",    # negative values
        mid = "white",   # zero
        high = "red",    # positive values
        midpoint = 0,    # center of scale
        limits = limits,
        name = expression(Delta ~ "kgC/m2")
      ) +
      coord_sf(xlim=xlim,ylim=ylim,expand=FALSE) +
      labs(
        x = "Longitude",
        y = "Latitude",
        # title = paste("Difference in cumulative carbon uptake between\n", management , " and base over 25 years")
        title = paste( management ,"- base in 2050")#"- base"
      ) +
      theme_minimal()
  )
  
  dev.off()
}




for(management in managementslist[-1]){
  
  ###spatial analysis 1:
  source.in = defineSource(id= paste0(management,"_",scenario),
                           dir= paste0(base_dir,management,"_fut_MPI-ESM1-2-HR_ssp126_diston"),
                           format = GUESS,
                           name = paste0(management,"_",scenario))
  
  myData <- getField(source = source.in, quant = "diamstruct_cmass_wood_inc_forest")
  myData@data$Total  <-  rowSums(myData@data[,4:dim(myData@data)[2]])#/10000 #unitconversion m2 to ha
  myData@data <- myData@data[landcover_fractions_from_2025, on = .(Lon, Lat)]  
  myData@data$GridcellArea_m2 <- GridcellArea_m2(myData@data$Lat,res=0.5)
  myData@data$Forest_area <- myData@data$GridcellArea_m2 * myData@data$FOREST
  
  
  #extract years first:
  myData2050 <- myData
  myData2050@data <- myData@data[which(myData@data$Year > 2090 & myData@data$Year <= 2100),]
  
  myData_base_2050 <- myData_base
  myData_base_2050@data <- myData_base_2050@data[which(myData_base_2050@data$Year > 2090 & myData_base_2050@data$Year  <= 2100),]
  
  
  # identify the columns with size classes (all except Lon, Lat, Year)
  size_cols <- setdiff(names(myData_base_2050@data), c("Lon", "Lat", "Year"))
  size_cols <- c("Year","Total","Forest_area")
  
  # compute mean across years for each Lon, Lat
  myData_base_2050@data <- myData_base_2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                                 by = .(Lon, Lat), 
                                                 .SDcols = size_cols]
  # compute mean across years for each Lon, Lat
  myData2050@data <- myData2050@data[, lapply(.SD, function(x) weighted.mean(x, w = Forest_area, na.rm = TRUE)), 
                                     by = .(Lon, Lat), 
                                     .SDcols = size_cols]
  
  myData_base_2050@data$Total_diff <-   myData2050@data$Total -  myData_base_2050@data$Total 
  
  #unitconversion:
  #myData_base_2050@data$Total_diff <- myData_base_2050@data$Total_diff*0.001*10000 # ton/ha
  
  # Define Europe extent (rough bounding box)
  xlim <- c(-11, 35)   # longitude range
  ylim <- c(34, 72)    # latitude range
  
  if(management=="ccf"){
    limits = c(-0.18,0.1)
  }else
  if(management=="intensthin"){
    limits = c(-0.07,0.05)
  }else
    if(management=="lightthin"){
      limits = c(-0.09,0.05)
    }else
      if(management=="longrot"){
        limits = c(-0.055,0.09)
      }else
  if(management=="rettree"){
    limits = c(-0.10,0.05)
  }else
  if(management=="sfire"){
    limits = c(-0.05,0.055)
  }else(
    limits = range(myData_base_2050@data$Total_diff)
  )
  # Get world map (simplified polygons)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  
  png(filename=paste0("../Figures_and_reports/Policy_Lab_maps/",scenario,"_2100_GAI_map_",management,"-baseline.png"),
      width = 120, height = 90, units = "mm", res = 630)  # bigger canvas
  
  # Plot using ggplot2
  print(
    ggplot() + 
      
      # Background: world polygons (grey land, lightblue sea background)
      geom_sf(data = world, fill = "grey90", color = "grey40") +
      
      geom_tile(data= myData_base_2050@data, aes(x = Lon, y = Lat, fill = Total_diff)) +
      
      scale_fill_gradient2(
        low = "blue",    # negative values
        mid = "white",   # zero
        high = "red",    # positive values
        midpoint = 0,    # center of scale
        limits=limits,
        name = expression(Delta ~ "kgC/m2")
      ) +
      coord_sf(xlim=xlim,ylim=ylim,expand=FALSE) +
      labs(
        x = "Longitude",
        y = "Latitude",
        # title = paste("Difference in cumulative carbon uptake between\n", management , " and base over 25 years")
        title = paste( management ,"- base in 2100")#"- base"
      ) +
      theme_minimal()
  )
  
  dev.off()
}



#============================================================================================================





