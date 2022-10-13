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
source('table_summary.R')  # 1
source('table_twfe_claim_int.R')  # 3
source('table_AET.R')  # 4
source('table_iv.R')  # 5
source('table_DWHtest.R')  # 6

# Figure ============================
source('figure_claim_by_integrated.R')  # 2
