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

# residualize
reg <- feols(log_claims ~ 1 | npi+Year, data=df_temp)
df_temp$Yres <- reg$residuals
reg <- feols(INT ~ 1 | npi+Year, data=df_temp)
df_temp$Dres <- reg$residuals
reg <- feols(PriceChange ~ 1 | npi+Year, data=df_temp)
df_temp$Zres <- reg$residuals

# Anderson-Rubin CI
reg <- ivmodel(Y=df_temp$Yres, D=df_temp$Dres, Z=df_temp$Zres)
CI_AR <- AR.test(reg)

# Lee (2021) tF
reg <- feols(INT~PriceChange | npi+Year, data=df_temp)
t_1S <- reg$coefficients[['PriceChange']]/reg$se[['PriceChange']]
F_1S <- t_1S^2
print(paste0('First stage F is ',round(F_1S)))

lower <- reg$coefficients[['PriceChange']] - 1.96*reg$se[['PriceChange']]*correction
# see 1S F statistics -> Find F statistics in Table3 -> Read the bottom line
