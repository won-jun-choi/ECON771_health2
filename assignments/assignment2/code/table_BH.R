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

# rep=100
# for (i in 1:rep) {
#   source('generate_pseudo_iv.R')
#   if (i==1) {
#     mu <- piv %>% 
#       mutate(practice_rev_change = practice_rev_change/rep) %>%
#       rename(mu = practice_rev_change)
#   } else {
#     mu$mu <- mu$mu + piv$practice_rev_change/rep
#     gc()
#   }
#   print('generated mu')
# }
# vroom_write(mu, file=here(dir_root,'temp','pseudoIV.csv'))
mu <- vroom(here(dir_root,'temp','pseudoIV.csv'))

df_temp <- df_temp %>% 
  left_join(mu, by=c('Year','group1'='tax_id')) %>%
  mutate(PCcentered = PriceChange - mu)
gc()

reg <- feols(INT~PCcentered|npi+Year, data=df_temp)
df_temp$INThat <- reg$fitted.values
reg <- feols(log_claims~INThat|npi+Year, data=df_temp)
etable(reg,
       tex=T, style.tex=style.tex('aer'),
       file=here(dir_root,'tex','tab_BH.tex'),
       replace=T)
print('created tab_BH.tex')
rm(list=c('mu','piv','reg'))
gc()
