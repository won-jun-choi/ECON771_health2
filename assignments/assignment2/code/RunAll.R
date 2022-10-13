# Meta =======================
# Title: RunAll
# Author: Wonjun Choi
# Last Edit: Oct-13-2022
# Description: haha

# Setup =======================
source('setup.R')
WAU(name='wonjun')

# Data ==========================
# source('data.R')
df <- vroom(here(dir_root,'temp','superfatdata.csv'))

# Table =============================
source('table_summary.R')

# Figure ============================
