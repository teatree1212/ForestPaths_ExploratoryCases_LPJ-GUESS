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
OUTPUT_DIR <- "../Figures_and_reports/Figures_Policylab_ssp126/"

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
scenario_list <- c("ssp126")

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
  
  # 1. TIMBER PRODUCTION
  timber_config <- list(
    name = "Timber Production",
    extract_fn = extract_timber_production,
    n_years = 73 # [TODO] crate on-the-fly n_years depedning on what extract_fn will extract..hack for now
  )
  timber_data <- process_variable(timber_config)
  
  # Plot in m3/ha/year
  create_country_plots(
    timber_data,
    list(
      filename = "Timber_production_m3ha.pdf",
      title = "timber production, m3/ha/year",
      ylab = "timber production, m3/ha/year",
      ylim = c(0, 6),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  # Plot differences from baseline
  timber_diff <- calculate_baseline_diff(timber_data)
  create_country_plots(
    timber_diff,
    list(
      filename = "Timber_production_m3ha_diff.pdf",
      title = expression(Delta ~ "timber production, m3/ha/year"),
      ylab = expression(Delta ~ "timber production, m3/ha/year"),
      ylim = c(-1.5, 1.5),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  # Convert to kgC/m2/year and plot
  timber_kgC <- convert_units(timber_data, mean_wood_density / 10000)
  create_country_plots(
    timber_kgC,
    list(
      filename = "Timber_production_kgCm2year.pdf",
      title = "timber production, kgC/m2/year",
      ylab = "timber production, kgC/m2/year",
      ylim = c(0, 0.25),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  # Plot differences from baseline, in kgCm2year
  timber_diff <- calculate_baseline_diff(timber_kgC)
  create_country_plots(
    timber_diff,
    list(
      filename = "Timber_production_kgCm2year_diff.pdf",
      title = expression(Delta ~ "timber production, kgC/m2/year"),
      ylab = expression(Delta ~ "timber production, kgC/m2/year"),
      ylim = c(-0.05, 0.05),
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
  
  create_country_plots(
    deadwood_data,
    list(
      filename = "Deadwood_m3ha.pdf",
      title = "Deadwood pool, m3/ha",
      ylab = "Deadwood pool, m3/ha",
      ylim = c(40, 210),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  deadwood_diff <- calculate_baseline_diff(deadwood_data)
  create_country_plots(
    deadwood_diff,
    list(
      filename = "Deadwood_m3ha_diff.pdf",
      title = expression(Delta ~ "Deadwood pool, m3/ha"),
      ylab = expression(Delta ~ "Deadwood pool, m3/ha"),
      ylim = c(-10, 10),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  #conversion to kgC/m2 
  #[TODO] check conversion was correct:
  deadwood_data_kgC <- convert_units(deadwood_data, mean_wood_density / 10000)
  create_country_plots(
    deadwood_data_kgC,
    list(
      filename = "Deadwood_kgCm2.pdf",
      title = "Deadwood pool, kgC/m2",
      ylab = "Deadwood pool,kgC/m2",
      ylim = c(0, 7),
      legend_position = "bottomleft",
      legend_country = "Poland"
    )
  )
  
  
  #difference plot for deadwood in kgC
  deadwood_kg_diff <- calculate_baseline_diff(deadwood_data_kgC)
  create_country_plots(
    deadwood_kg_diff,
    list(
      filename = "Deadwood_kgCm2_diff.pdf",
      title = expression(Delta ~"Deadwood pool, kgC/m2"),
      ylab = expression(Delta ~"Deadwood pool,kgC/m2"),
      ylim = c(-0.4, 0.4),
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
  
  create_country_plots(
    mortality_data,
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
  
  create_country_plots(
    mortality_pct,
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
  create_country_plots(
    mortality_pct_diff,
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
  
  create_country_plots(
    nai_data,
    list(
      filename = "NAI_kgCyear.pdf",
      title = "NAI, kgC/m2/year",
      ylab = "NAI, kgC/m2/year",
      ylim = c(-0.1, 0.2),
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
  # Convert to m3/ha/year
  nai_m3 <- convert_units(nai_data, 10000 / (0.5 * mean_wood_density))
  create_country_plots(
    nai_m3,
    list(
      filename = "NAI_m3hayr.pdf",
      title = "NAI, m3/ha/year",
      ylab = "NAI, m3/ha/year",
      ylim = c(-5, 15),
      legend_position = "bottomright",
      legend_country = "Poland"
    )
  )
  
  #calculate NAI perc difference
  nai_m3_diff <- calculate_baseline_diff(nai_m3)
  create_country_plots(
    nai_m3_diff,
    list(
      filename = "NAI_m3hayr_diff.pdf",
      title = expression(Delta~"NAI, m3/ha/year"),
      ylab = expression(Delta~"NAI, m3/ha/year"),
      ylim = c(-2, 2),
      legend_position = "bottomright",
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
  
  create_country_plots(
    nai_tot_data,
    list(
      filename = "NAI_tot_kgCyear.pdf",
      title = "NAI, kgC/m2/year",
      ylab = "NAI, kgC/m2/year",
      ylim = c(-0.1, 0.2),
      legend_position = "bottomright",
      legend_country = "Spain"
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
  
  # Convert to m3/ha/year
  nai_m3 <- convert_units(nai_tot_data, 10000 / (0.5 * mean_wood_density))
  create_country_plots(
    nai_m3,
    list(
      filename = "NAI_tot_m3hayr.pdf",
      title = "NAI, m3/ha/year",
      ylab = "NAI, m3/ha/year",
      ylim = c(-5, 15),
      legend_position = "bottomright",
      legend_country = "Poland"
    )
  )
  
  # NAI_tot difference pot in m3/ha/year
  nai_m3_diff <- calculate_baseline_diff(nai_m3)
  create_country_plots(
    nai_m3_diff,
    list(
      filename = "NAI_tot_m3hayr_diff.pdf",
      title = expression(Delta~"NAI, m3/ha/year"),
      ylab = expression(Delta~"NAI, m3/ha/year"),
      ylim = c(-2.5, 2.5),
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
  # NAI_tot difference pot in m3/ha/year
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
  
  # Plot absolute values
  create_country_plots(
    disturbance_mortality_data,
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
  
  create_country_plots(
    disturbance_mortality_pct,
    list(
      filename = "mortality_disturbance_percyear.pdf",
      title = "Disturbance mortality rate, %/year",
      ylab = "Disturbance mortality rate, %/year",
      ylim = c(0, 0.4),
      legend_position = "bottomright",
      legend_country = "Sweden",
      cex = 1.7
    )
  )
  
  # Plot differences from baseline (percentage)
  disturbance_mortality_pct_diff <- calculate_baseline_diff(disturbance_mortality_pct)
  create_country_plots(
    disturbance_mortality_pct_diff,
    list(
      filename = "mortality_disturbance_percyear_diff.pdf",
      title = expression(Delta ~ "Disturbance mortality rate, %/year"),
      ylab = expression(Delta ~ "Disturbance mortality rate, %/year"),
      ylim = c(-0.2, 0.2),
      legend_position = "bottom",
      legend_country = "Spain",
      cex = 1.7
    )
  )
  
  # Plot differences from baseline (absolute)
  disturbance_mortality_diff <- calculate_baseline_diff(disturbance_mortality_data)
  create_country_plots(
    disturbance_mortality_diff,
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
