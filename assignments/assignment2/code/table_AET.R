if (sys.nframe()==0){ # if __name__==__main__
  source('setup.R')
  WAU(name="wonjun")
  df <- vroom(here(dir_root,'temp','superfatdata.csv'))
  
  # from table_twfe_claim_int
  df_temp <- df %>% 
    filter(!is.na(INT)) %>%
    filter(!(Year==2012 & INT==1)) %>%
    group_by(Year, npi) %>%
    mutate(claims = log(sum(line_srvc_cnt, na.rm=T))) %>%
    select(npi, Year, claims, INT) %>%
    distinct(Year, npi, .keep_all = TRUE)
}

# delta_D
reg <- feols(claims ~ INT, data=df_temp) # se:iid but est is ok
delta_D <- reg$coefficients[['INT']]
R2_D <- 1 - (reg$ssr)/(var(df_temp$claims)*dim(df_temp)[1])

# delta_Dx
reg <- feols(claims ~ INT | npi+Year, data=df_temp)
delta_Dx <- reg$coefficients[['INT']]
R2_Dx <- 1 - (reg$ssr)/(var(df_temp$claims)*dim(df_temp)[1])

rm(list=c('reg'))
gc()

# table
rho_list <- seq(0, 2, 0.5)
R2max_list <- seq(0.5, 1, 0.1)
tab <- data.frame()
for (i in 1:length(rho_list)){
  row <- data.frame()
  for (j in 1:length(R2max_list)){
    rho <- rho_list[i]
    R2max <- R2max_list[j]
    if (R2max > R2_Dx) {
      delta_star <- delta_Dx - rho*(delta_D-delta_Dx)*(R2max-R2_Dx)/(R2_Dx-R2_D)
      interval <- paste0('[',round(delta_Dx,2),',',round(delta_star,2),']')
    } else {
      interval <- 'NA'
    }
    colname <- paste0('$R_{max}^2=',R2max,'$')
    row[1,colname] <- interval
  }
  tab <- bind_rows(tab, row)
}
rownames(tab) <- c('$\\rho=0$','$\\rho=0.5$','$\\rho=1$','$\\rho=1.5$',
                   '$\\rho=2$')
xtab <- xtable(tab,
               align=c('c','c','c','c','c','c','c'),
               caption = "Altonji, Elder, and Taber (2005)")
outfile <- here(dir_root,'tex','tab_AET.tex')
print(xtab,file=outfile,include.rownames=F)
print('created tab_AET.tex')

rm(list=c('row','tab','colname','delta_D','delta_Dx','delta_star','i','j',
          'R2_D','R2_Dx','R2max','R2max_list','rho','rho_list'))
gc()
