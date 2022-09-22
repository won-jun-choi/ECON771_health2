# ==============================================
# Meta
# Title: 
# Author: Wonjun Choi
# Last edit: 2022-09-22
# Description: This is a master file to run the project codes.
# ==============================================
rm(list=ls())
source('setup.R')
WAU(name="wonjun")

# Load and Merge Data
source('data.R')

# Tables
source('table_sumstat_rev.R')
source('table_sumstat_uncompcare.R')

# Figures
source('fig_uncompcare.R')
