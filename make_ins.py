


import sys

# Accept a folder name as an optional command-line argument,
# for the occasions when I only want one particular folder updated.
target_folder = sys.argv[1] if len(sys.argv) > 1 else None


import os
import platform
import socket
import shutil

# Get the hostname and system name
hostname = socket.gethostname()
system_name = platform.system()

# Assign paths based on the detected system
if "cosmos" in hostname:
    #current_path = "/lunarc/nobackup/projects/snic2020-6-23/ForestPaths/sims/"
    guess_link = "/lunarc/nobackup/projects/snic2020-6-23/ForestPaths/european_applications_ForestPaths/build/guess" # not needed, provided in submit.sh
    forcing_path = "/lunarc/nobackup/projects/snic2020-6-23/lpjguess/data/isimip/isimip3/"
    other_inputs_path = "/lunarc/nobackup/projects/snic2020-6-23/annemarie/ForestPaths/input/"
    statepath = "/lunarc/nobackup/projects/snic2020-6-23/annemarie/ForestPaths/sims/base_hist_MPI-ESM1-2-HR_hist_distoff/" # just quicly now..

elif "simba" in hostname:
    #current_path = "/scratch/annemarie/ForestPaths/sims/"
    guess_link = "/scratch/annemarie/ForestPaths/european_applications_ForestPaths/build/guess" # not needed, provided in submit.sh
    forcing_path = "/scratch/data/isimip3/"
    other_inputs_path = "/scratch/annemarie/ForestPaths/input/"
    statepath = "/scratch/annemarie/ForestPaths/sims/"

elif system_name == "Darwin":  # MacOS
    #current_path = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_ForestPaths_Exploratory_runs/sims/"
    guess_link = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_ForestPaths_Exploratory_runs/european_applications_ForestPaths/build/guess"
    forcing_path = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/1_Input/"# just tsting.
    other_inputs_path = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_ForestPaths_Exploratory_runs/input/"
    statepath = "/scratch/annemarie/ForestPaths/sims/"

else:
    print("Unknown system. Please set paths manually.")
    exit(1)


gridlist="gridlist.txt"
#lupath="/scratch/annemarie/ForestPaths/input/landuse_change_forest_age/"
#lufileext="hildaPucherMircaGenusmap"
#current_path="/scratch/annemarie/ForestPaths/european_applications/"
#guess_link="/scratch/annemarie/ForestPaths/european_applications/build/guess"


# Print selected paths for debugging
print(f"Running on: {hostname}")
print(f"Guess Link: {guess_link}")


###################################################################################################################################


modes1=["base","lightthin", "intensthin","longrot", "shortrot", "rettree", "sfire", "fertfor","ceaseman","ccf"]
modes2=[ "hist", "ssp126" , "ssp370"] # "hist" already set up and run
modes3=["diston","distoff"]
gcm = "MPI-ESM1-2-HR" # usful for non ForestPaths simulations.


def line(t):
    t.write("!//////////////////////////////////////////////////////////////////////////////\n")

def n(t):
    t.write("\n")

def restart_part(mode2,statepath,t):
    if mode2=="hist":
        restart=0
        firstyear="1850"
        lastyear="2014"
    else:
        restart=1
        firstyear="2015"
        lastyear="2100"
    svst=0
    if restart == 0:
        svst=1
    t.write("!//////////////////////////////////////////////////////////////////////////////\n")
    t.write("!///  SAVE STATE / RESTART PART ///////////////////////////////////////////////\n")
    line(t)
    t.write("restart "+str(restart)+"\n")
    t.write("save_state "+str(svst)+"\n")
    t.write('state_path "'+statepath+'"\n')
    t.write("state_year 1364\n") #AHES should not be 650 but 2014? 2014-1200 = 1364 ; length(seq(650,2014,1)) [1] 1365; "works" with 1200
    t.write("firsthistyear "+firstyear+"\n")
    t.write("lasthistyear "+lastyear+"\n")
    line(t)


def management_part(mode1,t):
    line(t)
    line(t)
    t.write("!//////////////////////////////////////////////////////////////////////////////\n")
    t.write("!///  FOREST MANAGEMENT PART //////////////////////////////////////////////////\n")
    t.write("!Forest management differs in the future scenarios and thus requires different settings or input files\n")
    line(t)
    t.write("!!! Stochastic wood harvest input:\n")
    t.write("!!! must setharvest_system 'probability_input' in st/mt \n")



    if mode1 == "rettree":
        t.write("!Alter harvesting rules of EAF so that 20% of the stand volume is left standing at harvest \n")
        t.write("!here assumed to be equivalent to carbon units, removing only 80% Cwood \n")
        t.write('param "retention_tree_fraction" (num 0.20)  \n')
    else:
        t.write("!Full clearcut \n")
        t.write('param "retention_tree_fraction" (num 0.00)  \n')

    if mode1 =="nfert";
        t.write("!nfert of stands >mean 20 cm dbh  \n")
        t.write('param "nfert_scenario" (num 1.0)  \n')
    else:
        t.write("!nfert off \n")
        t.write('param "nfert_scenario" (num 0.00)  \n')

    #default for fire in management is on, changed for sfire below
    if mode1 != "sfire":
        t.write( 'group "mt_fire" ( \n')
        t.write( 'suppress_fire 0 \n'  )
        t.write('    cutfirstyear 0 \n')
        t.write(  ')\n'                )

    #default for all, except cease_management, changed for cease_management below
    if mode1 != "ceaseman":
        t.write( '!only in the cease_management scenario, ceasman contains different specified options \n')
        t.write( '!my solution is to create individual management types to avoid the planting selection problem, and the linearity of the insfiles:\n')
        t.write( 'mt "mt_ceaseman_picea" (\n')
        t.write( '    gr_mt_man\n'         )
        t.write( '    selection "Pic_abi" !AHES no Pic_sit allowed, "removed from all stand types" - not sure why\n')
        t.write('    cutfirstyear 0 \n')
        t.write( ')\n'                    )

        t.write('mt "mt_ceaseman_Larix" (\n')
        t.write('    gr_mt_man\n')
        t.write('    selection "Lar_dec"\n')
        t.write('    cutfirstyear 0 \n')
        t.write(')\n')

        t.write('mt "mt_ceaseman_Pinus" (\n')
        t.write('    gr_mt_man\n')
        t.write('    selection "Pin_syl Pin_hal"\n')
        t.write('    cutfirstyear 0 \n')
        t.write(')\n')

        t.write('mt "mt_ceaseman_Fagus" (\n')
        t.write('    gr_mt_man\n')
        t.write('    selection "Fag_syl"\n')
        t.write('    cutfirstyear 0 \n')
        t.write(')\n')

        t.write('mt "mt_ceaseman_Quercus" (\n')
        t.write('    gr_mt_man\n')
        t.write('    selection "Que_ile Que_rob Que_coc Que_pub"\n')
        t.write('    cutfirstyear 0 \n')
        t.write(')\n')

        t.write('mt "mt_ceaseman_otherBL" (\n')
        t.write('    gr_mt_man\n')
        t.write('    selection "Car_bet Fra_exc Til_cor Bet_pen Bet_pub Cor_ave"\n')
        t.write('    cutfirstyear 0 \n')
        t.write(')\n')

        t.write('mt "mt_ceaseman_otherNL" (\n')
        t.write('    gr_mt_man\n')
        t.write('    selection "Abi_alb Jun_oxy"\n')
        t.write('    cutfirstyear 0 \n')
        t.write(')\n')



    if mode1=="lightthin":
        t.write('param "file_woodharv_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_mean_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Broadleaf.txt_luc_coords_added_red") \n')
        t.write('param "file_woodharv_intens_mean_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Conifer.txt_luc_coords_added_red") \n')
        t.write('param "file_woodharv_intens_sd_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Broadleaf.txt_luc_coords_added_red") \n')
        t.write('param "file_woodharv_intens_sd_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Conifer.txt_luc_coords_added_red") \n')
        t.write('param "file_CC_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_CC_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Conifer.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Conifer.txt_luc_coords_added")\n')
    elif mode1=="intensthin":
        t.write('param "file_woodharv_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_mean_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Broadleaf.txt_luc_coords_added_inc") \n')
        t.write('param "file_woodharv_intens_mean_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Conifer.txt_luc_coords_added_inc") \n')
        t.write('param "file_woodharv_intens_sd_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Broadleaf.txt_luc_coords_added_inc") \n')
        t.write('param "file_woodharv_intens_sd_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Conifer.txt_luc_coords_added_inc") \n')
        t.write('param "file_CC_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_CC_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Conifer.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Conifer.txt_luc_coords_added")\n')
    elif mode1=="shortrot":
        t.write('param "file_woodharv_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Broadleaf.txt_luc_coords_added_inc") \n')
        t.write('param "file_woodharv_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Conifer.txt_luc_coords_added_inc") \n')
        t.write('param "file_woodharv_intens_mean_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_mean_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_sd_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_sd_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_CC_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_CC_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Conifer.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Conifer.txt_luc_coords_added")\n')
    elif mode1=="longrot":
        t.write('param "file_woodharv_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Broadleaf.txt_luc_coords_added_red") \n')
        t.write('param "file_woodharv_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Conifer.txt_luc_coords_added_red") \n')
        t.write('param "file_woodharv_intens_mean_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_mean_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_sd_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_sd_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_CC_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_CC_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Conifer.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Conifer.txt_luc_coords_added")\n')
        #elif mode1=="ceaseman":
            #doing a ceaseman via landuse change map means that we cannot track the forested area's trajectory (should we want to)
            # [Q] is it possible to create a management type acting from 2025, that can mimic "no management?"
            # e.g. "continuous"- with all settings at 0 ?
    else:
        t.write('param "file_woodharv_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_harvest_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_mean_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_mean_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_mean_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_sd_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Broadleaf.txt_luc_coords_added") \n')
        t.write('param "file_woodharv_intens_sd_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/intensity_sd_Conifer.txt_luc_coords_added") \n')
        t.write('param "file_CC_prob_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_CC_prob_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/p_clearcut_Conifer.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_BL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Broadleaf.txt_luc_coords_added")\n')
        t.write('param "file_woodharv_size_preference_NL" (str "'+other_inputs_path+'Probabilistic_harvest/lpjg_inputfiles_extrapolate_fmapOnly6_041224/size_target_Conifer.txt_luc_coords_added")\n')

        if mode1 == "sfire":
             t.write( 'group "mt_fire" ( \n')
             t.write( 'suppress_fire 1 \n'  )
             t.write( 'cutfirstyear 0 \n')
             t.write(  ')\n'                )


             # here I am trying to immitate a management type that reflects no management, it is supposed to start in year 2025, and that is set in main.ins
        if mode1 == "ceaseman":
             #default for all, except cease_management, changed for cease_management below
             t.write( '!only in the cease_management scenario, ceasman contains different specified options \n')
             t.write( '!my solution is to create individual management types to avoid the planting selection problem, and the linearity of the insfiles:\n')
             t.write( 'mt "mt_ceaseman_picea" (\n')
             t.write( '   gr_mt_ceaseman\n'       )
             t.write( '   cutfirstyear 0 \n'      )
             t.write( ')\n'                       )

             t.write('mt "mt_ceaseman_Larix" (\n')
             t.write('    gr_mt_ceaseman\n'      )
             t.write('    cutfirstyear 0 \n'     )
             t.write(')\n')

             t.write('mt "mt_ceaseman_Pinus" (\n')
             t.write('    gr_mt_ceaseman\n')
             t.write('    cutfirstyear 0 \n')
             t.write(')\n')

             t.write('mt "mt_ceaseman_Fagus" (\n')
             t.write('    gr_mt_ceaseman\n')
             t.write('    cutfirstyear 0 \n')
             t.write(')\n')

             t.write('mt "mt_ceaseman_Quercus" (\n')
             t.write('    gr_mt_ceaseman\n')
             t.write('    cutfirstyear 0 \n')
             t.write(')\n')

             t.write('mt "mt_ceaseman_otherBL" (\n')
             t.write('    gr_mt_ceaseman\n')
             t.write('    cutfirstyear 0 \n')
             t.write(')\n')

             t.write('mt "mt_ceaseman_otherNL" (\n')
             t.write('    gr_mt_ceaseman\n')
             t.write('    cutfirstyear 0 \n')
             t.write(')\n')

        #PUT ALL RELEVANT MANAGEMENT STUFF HERE; POTENTIALLY HAS TO BE DISINGUISHED WITH IF_STATEMENTS FOR DIFFERENT MODES.

    line(t)

    line(t)


def disturbance_part(mode3,t):
    line(t)
    t.write("!//////////////////////////////////////////////////////////////////////////////\n")
    t.write("!/// PROGNOSTIC DISTURBANCES ///////////////////////////////////////////////\n")
    if mode3 =="diston":
        t.write('    storm_mortality 1\n')
        t.write('    !AHES Jan 2025: \n')
        t.write('    !AHES Jan 2025: disabled feature that insect outbreak and storms would occur in  managed stands only. Now these\n')
        t.write('    ! disturbances occur in all type of stands.\n')
        t.write('    storm_mortality_managed_from 2024   ! Year from which storm mortality is turned on in managed stands\n')
        t.write('    insect_mortality 1\n')
        t.write('    insect_mortality_managed_from 2024  ! Year from which insect mortality is turned on in managed stands\n')
        t.write('    param "file_windload" (str "/home/anne1212/snic2022-6-59/annemarie/ForestPaths/input/future_disturbances/windloadfromWDP/windload_random2014_2100.txt")\n')
        t.write('    param "file_dist_wind_prob" (str "")\n')

    else:
        t.write('    storm_mortality 0\n')
        t.write('    !AHES Jan 2025: \n')
        t.write('    !AHES Jan 2025: disabled feature that insect outbreak and storms would occur in  managed stands only. Now these\n')
        t.write('    ! disturbances occur in all type of stands.\n')
        t.write('    storm_mortality_managed_from 2100! Year from which storm mortality is turned on in managed stands\n')
        t.write('    insect_mortality 0\n')
        t.write('    insect_mortality_managed_from 2100! Year from which insect mortality is turned on in managed stands\n')
        t.write('    param "file_windload" (str "")\n')
        t.write('    param "file_dist_wind_prob" (str "")\n')

    #!AHES MUST THINK ABOUT FIRE;THAT CAN ALSO BE HANDLED IN MANAGEMENT
    #!AHES Fredrik's wind developments - update?

    line(t)

# AHES not relevant for these simulations, as landcover is constant:
def landcover_part(mode, lcfilespath,filenameext,t):
    line(t)
    t.write("!////    LANDCOVER INPUT FILES    ////////////////////////////////////////\n")
    line(t)
    if mode=="baseline_allnatural":
        t.write('param "file_lu"              (str "") \n')
        t.write('param "file_grossSTC"        (str "") \n')
        t.write('param "file_luforest"        (str "") \n')
        t.write('param "file_lucrop"          (str "") \n')
        t.write('param "file_cutinterval_st"  (str "") \n')
    else:
        t.write('import "crop_n.ins" \n')
        t.write('param "file_lu"              (str "'+lcfilespath+'net_lu_'+filenameext+'.txt") \n')
        t.write('param "file_grossSTC"        (str "'+lcfilespath+'gross_st_trans_'+filenameext+'.txt") \n')
        t.write('param "file_luforest"        (str "'+lcfilespath+'lu_forest_'+filenameext+'.txt") \n')
        #t.write('param "file_luforest"        (str "'+lcfilespath+'lu_forest_'+filenameext+"_"+mode+'.txt") \n')
        t.write('param "file_lucrop"          (str "'+lcfilespath+'lu_crop_'+filenameext+'.txt") \n')
        #!t.write('param "file_cutinterval_st"  (str "'+lcfilespath+'cutinterval.txt") \n') !AHES Feb 2025. also removed "mode" extensions in the above names.
    line(t)
    n(t)


def climate_part(mode2,t,forcing_path):
    if mode2 == "hist":
        years="1850_2100"
        mode1="ssp126" # hacking this - as the historical part is identical in all ssps, just select this one to get historic forcing for the spinup.
        tmp = "histssp126"
        #co2string=mode1
        n(t)
    else:
        years="1850_2100"
        mode1=mode2
        tmp =str("hist"+mode2) # seems I have to load in the files from 1850 onwards, otherwise the restart doesn't wnt to work for me.
        #co2string="hist" # to enable flexible reading in of co2 files
    line(t)

    n(t)
    t.write("!//////// CO2 INPUT   //////////////////////////\n")

    t.write('param "file_co2"        (str "'+forcing_path+'co2/co2_'+tmp+'_annual_'+years+'.txt")\n')
    line(t)
    t.write("!//////// CLIMATE INPUT   //////////////////////////\n")
    n(t)
    t.write('param "variable_temp" (str "tas") \nparam "variable_prec" (str "pr")\nparam "variable_insol"  (str "rsds")\n')
    t.write('param "variable_wind"  (str "sfcwind")\nparam "variable_relhum" (str "hurs")\nparam "variable_min_temp"  (str "tasmin")\nparam "variable_max_temp"  (str "tasmax")  \n')
    n(t)
    t.write('param "file_temp"      (str "'+forcing_path+'climate_land_only_v2/climate3b/' +mode1+'/MPI-ESM1-2-HR-lpjg/mpi-esm1-2-hr_r1i1p1f1_w5e5_hist_'+mode1+'_tas_global_daily_'+years+'.nc4")        \n')
    t.write('param "file_prec"      (str "'+forcing_path+'climate_land_only_v2/climate3b/' +mode1+'/MPI-ESM1-2-HR-lpjg/mpi-esm1-2-hr_r1i1p1f1_w5e5_hist_'+mode1+'_pr_global_daily_'+years+'.nc4")         \n')
    t.write('param "file_insol"     (str "'+forcing_path+'climate_land_only_v2/climate3b/' +mode1+'/MPI-ESM1-2-HR-lpjg/mpi-esm1-2-hr_r1i1p1f1_w5e5_hist_'+mode1+'_rsds_global_daily_'+years+'.nc4")       \n')
    t.write('param "file_wind"      (str "'+forcing_path+'climate_land_only_v2/climate3b/' +mode1+'/MPI-ESM1-2-HR-lpjg/mpi-esm1-2-hr_r1i1p1f1_w5e5_hist_'+mode1+'_sfcwind_global_daily_'+years+'.nc4")    \n')
    t.write('param "file_relhum"    (str "'+forcing_path+'climate_land_only_v2/climate3b/' +mode1+'/MPI-ESM1-2-HR-lpjg/mpi-esm1-2-hr_r1i1p1f1_w5e5_hist_'+mode1+'_hurs_global_daily_'+years+'.nc4")       \n')
    t.write('param "file_min_temp"  (str "'+forcing_path+'climate_land_only_v2/climate3b/' +mode1+'/MPI-ESM1-2-HR-lpjg/mpi-esm1-2-hr_r1i1p1f1_w5e5_hist_'+mode1+'_tasmin_global_daily_'+years+'.nc4")     \n')
    t.write('param "file_max_temp"  (str "'+forcing_path+'climate_land_only_v2/climate3b/' +mode1+'/MPI-ESM1-2-HR-lpjg/mpi-esm1-2-hr_r1i1p1f1_w5e5_hist_'+mode1+'_tasmax_global_daily_'+years+'.nc4")     \n')
    n(t)
    t.write('! Wet days can only be used with monthly precipitation   \n')
    t.write('param "file_wetdays"     (str "")  \n')
    t.write('param "variable_wetdays" (str "")  \n')
    n(t)
    t.write('param "file_vpd"      (str "")      \n')
    t.write('param "variable_vpd"  (str "")      \n')
    n(t)
    t.write('param "file_pres" (str "")          \n')
    t.write('param "variable_pres" (str "")      \n')
    n(t)
    t.write('param "file_specifichum" (str "")   \n')
    t.write('param "variable_specifichum" (str "")\n')
    n(t)
    t.write('! Soil map \n')
    t.write('param "file_soildata" (str "/lunarc/nobackup/projects/snic2020-6-23/lpjguess/data/soil/WISE/soilmap_center_interpolated.dat") \n') #only appropriate for Cosmos, but hacking this for now.
    n(t)

    t.write("!//////// NITROGEN INPUT   //////////////////////////\n")

    t.write("! AHES JAN 2025 added nitrogen inputs below, as per Peter Antoni's GCB instruction file, that uses the isimip  forcing data: \n")
    t.write("! N deposition (blank string to use constant pre-industrial level of 2 kgN/ha/year, unless netCDF file_mndrydep and file_mnwedep are specified) \n")
    t.write('param "file_ndep"     (str "") \n')
    t.write('ndep_timeseries  "" \n')
    n(t)
    t.write('param "file_mNHxdrydep" (str "/'+forcing_path+'n-deposition/histsoc-ssp126soc-wetdry-lpjguess/ndep_drynhx_histsoc_'+tmp+'_monthly_'+years+'_lpjg.nc4")   \n')
    t.write('param "variable_mNHxdrydep" (str "drynhx") \n')
    t.write('param "file_mNOydrydep" (str "'+forcing_path+'n-deposition/histsoc-ssp126soc-wetdry-lpjguess/ndep_drynoy_histsoc_'+tmp+'_monthly_'+years+'_lpjg.nc4") \n')
    t.write('param  "variable_mNOydrydep" (str "drynoy") \n')
    n(t)
    t.write('param "file_mNHxwetdep" (str "'+forcing_path+'n-deposition/histsoc-ssp126soc-wetdry-lpjguess/ndep_wetnhx_histsoc_'+tmp+'_monthly_'+years+'_lpjg.nc4")  \n')
    t.write('param "variable_mNHxwetdep" (str "wetnhx") \n')
    t.write('param "file_mNOywetdep" (str "'+forcing_path+'n-deposition/histsoc-ssp126soc-wetdry-lpjguess/ndep_wetnoy_histsoc_'+tmp+'_monthly_'+years+'_lpjg.nc4") \n')
    t.write('param "variable_mNOywetdep" (str "wetnoy") \n')




    n(t)

    line(t)


def ndep_part(mode1, mode2,t):
    if mode2=="hist":
        mode3="ssp370"
    else:
        mode3=mode2

    if mode1=="fertfor":
        t.write('param "file_mNHxdrydep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
                +'soc-wetdry-lpjguess/ndep_drynhx_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        t.write('param "file_mNOydrydep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
                +'soc-wetdry-lpjguess/ndep_drynoy_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        t.write('param "file_mNHxwetdep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
                +'soc-wetdry-lpjguess/ndep_wetnhx_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        t.write('param "file_mNOywetdep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
                +'soc-wetdry-lpjguess/ndep_wetnoy_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        n(t)
    else:
        t.write('param "file_mNHxdrydep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
            +'soc-wetdry-lpjguess/ndep_drynhx_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        t.write('param "file_mNOydrydep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
                    +'soc-wetdry-lpjguess/ndep_drynoy_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        t.write('param "file_mNHxwetdep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
                    +'soc-wetdry-lpjguess/ndep_wetnhx_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        t.write('param "file_mNOywetdep" (str "' +forcing_path+ 'n-deposition/histsoc-' + mode3
                    +'soc-wetdry-lpjguess/ndep_wetnoy_histsoc_'+mode3+'_monthly_1850_2100_lpjg.nc4") \n')
        n(t)

    t.write('param "variable_mNHxdrydep" (str "drynhx") \n')
    t.write('param "variable_mNOydrydep" (str "drynoy")     \n')
    t.write('param "variable_mNHxwetdep" (str "wetnhx") \n')
    t.write('param "variable_mNOywetdep" (str "wetnoy")    \n')
    n(t)
    t.write('!time varying ndep (normal run, default), >0 use ndep of fixed_ndep_year (1)=for the whole run, \n')
    t.write( '!(2)=from fixed_ndep_year onward  (3) // use fixed ndep of fixed_ndep_year for years before it  \n')
    t.write('fixed_ndep 2 !(0)  \n')
    n(t)
    t.write('fixed_ndep_year 2023 !ndep year to use for fixed_ndep>0 (def. 1850) [1850..2100]\n') # [TODO] why not 2025?
    n(t)
    #t.write('param "file_co2"        (str "/scratch/data/isimip3/co2/co2_hist'+mode3+'_annual_1850_2100.txt")\n')

def popdens(t):
    t.write('param "file_popdens"    (str "'+forcing_path+'pop/lpjg-popd/population-density_3b_2015soc_30arcmin_annual_1601_2100.lpjg.nc") \n')
    t.write('param "variable_popdens" (str "popd") \n')
    n(t)


#filenameext
def write_insfile(filename,mode1,mode2,mode3,forcing_path,statepath):
    t=open(filename,"w")
    t.write("!//////////////////////////////////////////////////////////////////////////////\n")
    t.write("!/////////////      VARIABLE PART OF LPJ-GUESS INS FILES     \n")
    t.write("!/////////////         "+mode1+"   "+mode2+"\n")
    line(t)
    n(t)
    n(t)
    #AHES constant : t.write('param "file_gridlist" (str "'+gridlist+'") \n')
    if mode2=="hist":
        restart_part(mode2,statepath,t)
        n(t)
        climate_part(mode2,t,forcing_path)
        ndep_part(mode1,mode2,t)
        management_part(mode1,t)
        popdens(t) #AHES not actually variable in scenarios, but path varies
    else:
        restart_part(mode2,statepath,t)
        n(t)
        #AHES constant : landcover_part(mode1,lupath,lufileext,t)
        climate_part(mode2,t,forcing_path)
        ndep_part(mode1,mode2,t)
        management_part(mode1,t)
        disturbance_part(mode3,t)
        popdens(t) #not actually variable, bu tpath varies
    t.close()



# Get absolute state path quickly now:
#don't remake state path. Historical run already done:
#state_folder = "state"
#statepath = os.path.abspath(state_folder)

# Ensure state folder exists
#os.makedirs(state_folder, exist_ok=True)
#shutil.copy("main.ins", state_folder)  # Copy main.ins into state folder
#write_insfile(f"{state_folder}/variable_part.ins",  "rettree","hist", "distoff", gridlist, lupath, lufileext, statepath)

# Loop through modes and create flat structure
for mode1 in modes1:
    for mode2 in modes2:

         # Only allow 'base_hist' combo when mode2 is 'hist'
        if mode2 == "hist" and mode1 != "base":
            continue

        # Set up folder prefix
        if mode2 == "hist":
            prefix = f"{mode1}_hist_{gcm}_{mode2}"
        else:
            prefix = f"{mode1}_fut_{gcm}_{mode2}"

        for mode3 in modes3:
            folder_name = f"{prefix}_{mode3}"

            # Check if we're filtering for a specific scenario to be updated for its run settings:
            if target_folder and folder_name != target_folder:
                continue

            # Check if the folder exists
            if os.path.exists(folder_name):
                contains_out_files = any((f.endswith(".out") or f.endswith(".out.gz")) for f in os.listdir(folder_name))
                contains_run_folder = any(os.path.isdir(os.path.join(folder_name, d)) and d.startswith("run") for d in os.listdir(folder_name))
                contains_state_file = any(f.endswith(".state") for f in os.listdir(folder_name))

                if contains_out_files or contains_run_folder or contains_state_file:
                    print("Skipping folder: %s (contains *out files or a 'run' folder)" % folder_name)
                    continue  # Skip recreating the folder

                # If no *out files or run folder, remove and recreate it
                shutil.rmtree(folder_name)

            if not os.path.exists(folder_name):
                os.makedirs(folder_name)  # Create scenario folder

            # Copy main.ins and others into each scenario folder
            shutil.copy("main.ins", folder_name)

            shutil.copy("submit.sh", folder_name)

            #shutil.copy("paths_MPI.ins", folder_name)

            shutil.copy("gridlist.txt", folder_name)

            #Copy the compression script into each scenario foldercd ..
            #shutil.copy("compress.sh", folder_name)

            # Create symbolic link inside the folder
            os.symlink(guess_link, os.path.join(folder_name, "guess_link"))

            #if mode2 == "hist":
            #    continue
            #else:
                # Generate variable_part.ins inside the folder
            write_insfile(os.path.join(folder_name, "variable_part.ins"), mode1, mode2, mode3, forcing_path, statepath)

            print("Created folder: %s" % folder_name)
