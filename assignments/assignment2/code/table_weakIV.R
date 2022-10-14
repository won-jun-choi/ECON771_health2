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
AR <- AR.test(reg)
CI_AR <- AR$ci

# Lee (2021) tF
reg <- feols(INT~PriceChange | npi+Year, data=df_temp)
F_1S <- (reg$coefficients[['PriceChange']]/reg$se[['PriceChange']])^2
print(paste0('First stage F is ',round(F_1S)))
cf <- 1  # correction factor: the third line of Table 3
df_temp$INThat <- reg$fitted.values
reg <- feols(log_claims~INThat | npi+Year, data=df_temp)
lower <- reg$coefficients[['INThat']]-1.96*reg$se[['INThat']]*cf
upper <- reg$coefficients[['INThat']]+1.96*reg$se[['INThat']]*cf
CI_Lee <- data.frame('lower'=lower,'upper'=upper)

tab <- rbind(CI_AR, CI_Lee)
row.names(tab) <- c('Anderson-Rubin','Lee')

xtab <- xtable(tab,
               align = c("c","c","c"),
               caption = "Confidence intervals",
               label = "tab:ci")
print(xtab, file=here(dir_root,'tex','tab_ci.tex'),include.rownames=T)
print("created tab_ci.tex")
rm(list=c('cf','reg','tab','xtab','F_1S','CI_Lee','CI_AR','AR'))
gc()
