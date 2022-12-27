# main directory
mainwd = MAIN_PATH
setwd(mainwd)

# data for 2015-2019 (as in the main text)
years_select = 2015:2019
# data for 2015-2021
# years_select = 2015:2021

# using the green-area index (as in the main text)
decompose_green = 0
# decomposing the green-area index (crops vs green other areas)
# decompose_green = 1

# install.packages('renv')
library('renv')
# create the initial renv lockfile, write the renv auto-loaders to the project .Rprofile and renv/activate.R
# renv::init()
# reproduce our R environment
renv::restore()

# options(warn=-1)

# image path
imgPth = paste0(mainwd, "/img")
# raw data files
pthDat = paste0(mainwd, "data/")

# save datasets
savdat = 1
# save figures
savfig = 1

# step 1: data generation
##############################################################
source("1_generate_data.R")
##############################################################

# step 2: descriptive statistics
##############################################################
source("2_descriptive.R")
# This script reproduces:
# Figs. 2-5, S12-S14, S2-S6, S9-S11
##############################################################

# step 3: create regression models (linear and counts)
##############################################################
source("3_createmodels.R")
# This script reproduces:
# Figs. S7, S8, S15
##############################################################

# step 4: retrieve MIP results for the linear model
##############################################################
source("4_MIPanalysis.R")
# This script reproduces:
# Figs. S16, S17, S18, S19, S20
# Tables 1, S4, S6
##############################################################

if (length(years_select)==5 && decompose_green==0){
  # step 5: other details and comparisons for MIB-based solution
  ##############################################################
  source("5_MIPfurther.R")
  # This script reproduces:
  # Tables S5, S7, S10, S11, S12
  ##############################################################
  
  # step 6: compute other methods and models
  ##############################################################
  source("6_otherMethods.R")
  # This script reproduces:
  # Fig. S21
  # Tables: S8, S9
  ##############################################################
}
# options(warn=0)
