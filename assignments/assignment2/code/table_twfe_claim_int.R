if (sys.nframe()==0){ # if __name__==__main__
  source('setup.R')
  WAU(name="wonjun")
  df <- vroom(here(dir_root,'temp','superfatdata.csv'))
}

exclude <- (df %>% filter(Year==2012 & INT==1))$npi
df_temp <- df %>% 
  filter(!is.na(INT)) %>%
  filter(!(npi %in% exclude)) %>%
  group_by(Year, npi) %>%
  mutate(claims = log(sum(line_srvc_cnt, na.rm=T))) %>%
  select(npi, Year, claims, INT) %>%
  distinct(Year, npi, .keep_all = TRUE)

reg <- feols(claims ~ INT | npi+Year, data=df_temp)
etable(reg,
       tex=T, style.tex=style.tex('aer'),
       file=here(dir_root,'tex','tab_twfe_claim_int.tex'),
       replace=T)
print('created tab_twfe_cliam_int.tex')
rm(list = c('reg'))
gc()
