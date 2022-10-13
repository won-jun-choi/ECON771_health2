if (sys.nframe()==0){ # if __name__==__main__
  rm(list=ls())
  gc()
  source('setup.R')
  WAU(name="wonjun")
}

# Import data
df <- data.frame()
for (y in 2012:2017) {
  print(y)
  df_MDPPAS <- vroom(paste0(dir_data,"/MDPPAS/PhysicianData_",y,".csv")) # csv
  df_puf <- vroom(paste0(dir_data,"/utilization-payment-puf/",y,  # tsv
                         '/Medicare_Provider_Util_Payment_PUF_CY',y,'.txt'))
  names(df_puf) <- tolower(names(df_puf))
  
  df_MDPPAS$npi <- as.character(df_MDPPAS$npi)  # num -> char
  df_MDPPAS <- df_MDPPAS %>% 
    select(npi, Year, pos_asc, pos_opd, pos_office, group1, group2) %>%
    group_by(Year, npi) %>%
    mutate(HOPD = pos_opd,
           OFFICE = pos_office,
           ASC = pos_asc,
           INT = if_else(HOPD/(HOPD+OFFICE+ASC)>=0.75,1,0)) %>%  # see question 2
    select(Year, npi, INT, group1)  # group1 is used as a key when merging IV
  
  df_puf <- df_puf %>%
    select(npi, nppes_credentials, starts_with('average_'), line_srvc_cnt,
           bene_unique_cnt) %>%
    filter(grepl('MD|M.D.', nppes_credentials, ignore.case = T))
  
  df_year <- inner_join(df_MDPPAS, df_puf, by='npi')
  df <- bind_rows(df, df_year)
}
rm(list=c('df_MDPPAS','df_puf','df_year'))
gc()
vroom_write(df, file = here(dir_root,'temp','superfatdata.csv'), delim = ",")
