if (sys.nframe()==0){ # if __name__==__main__
  source('setup.R')
  WAU(name="wonjun")
  df <- vroom(here(dir_root,'temp','superfatdata.csv'))
  exclude <- (df %>% filter(Year==2012 & INT==1))$npi
}
iv <- vroom(here(dir_root,'temp','instrument.csv'))
rm(list=c('df_temp'))
gc()
df_temp <- df %>% 
  filter(!is.na(INT)) %>%
  filter(!(npi %in% exclude)) %>%
  group_by(Year, npi) %>%
  mutate(log_claims = log(sum(line_srvc_cnt, na.rm=T))) %>%
  select(npi, Year, log_claims, INT, group1) %>%
  distinct(Year, npi, .keep_all = TRUE) %>%
  left_join(iv, by=c('Year','group1'='tax_id')) %>%
  filter(!is.na(practice_rev_change)) %>%
  rename(PriceChange = practice_rev_change)
gc()

reg_1S <- feols(INT ~ PriceChange | npi+Year, data=df_temp)
df_temp$INThat <- reg_1S$fitted.values
reg_2S <- feols(log_claims ~ INThat | npi+Year, data=df_temp)
reg_RF <- feols(log_claims ~ PriceChange | npi+Year, data=df_temp)

#etable(reg_1S,reg_RF,reg_2S)
etable(reg_1S, reg_RF, reg_2S,
       tex=T, style.tex=style.tex('aer'),
       file=here(dir_root,'tex','tab_iv.tex'),
       replace=T)

rm(list=c('reg_1S','reg_2S','reg_RF','iv'))
gc()
