if (sys.nframe()==0){ # if __name__==__main__
  source('setup.R')
  WAU(name="wonjun")
  df <- vroom(here(dir_root,'temp','superfatdata.csv'))
  iv <- vroom(here(dir_root,'temp','instrument.csv'))

  # from table_iv
  df_temp <- df %>% 
    filter(!is.na(INT)) %>%
    filter(!(Year==2012 & INT==1)) %>%
    group_by(Year, npi) %>%
    mutate(log_claims = log(sum(line_srvc_cnt, na.rm=T))) %>%
    select(npi, Year, log_claims, INT, group1) %>%
    distinct(Year, npi, .keep_all = TRUE) %>%
    left_join(iv, by=c('Year','group1'='tax_id')) %>%
    filter(!is.na(practice_rev_change)) %>%
    rename(PriceChange = practice_rev_change)
  gc()
}

reg1 <- feols(INT ~ PriceChange | npi+Year, data=df_temp)
df_temp$INTres <- reg1$residuals
reg2 <- feols(log_claims ~ INT + INTres | npi+Year, data=df_temp)

etable(reg2,
       tex=T, style.tex=style.tex('aer'),
       file=here(dir_root,'tex','tab_DWHtest.tex'),
       replace=T)
print('created DWHtest.tex')

rm(list=c('reg1','reg2'))
gc()
