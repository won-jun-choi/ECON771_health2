# ==============================================
# Meta
# Title: 
# Author: Wonjun Choi
# Last edit: 2022-09-23
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
source('table_twfe.R')
source('table_event_study.R')
source('table_sunab.R')

# Figures
source('fig_uncompcare.R')
source('fig_sunab.R')
source('fig_CS_event.R')
source('fig_honest.R')
