if (sys.nframe()==0){  # if __name__==__main__
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

df_temp <- df %>%
  filter(!is.na(uncomp_care)) %>%
  mutate(Y = uncomp_care/1000000,
         D = ifelse(year>= year_adopted,1,0),
         D = ifelse(!is.na(D),D,0))
reg_DD_twfe <- feols(Y~D| provider + year, data=df_temp)

reg_DD_subsample <- function(ssyr) {
  reg <- feols(Y~D | provider+year,
               data=df_temp %>% 
                 filter(expanded==FALSE |
                          year_adopted==ssyr))
  return(reg)
}
reg_DD_twfe_subsample = lapply(2014:2016, reg_DD_subsample)

twfe <- list(reg_DD_twfe)
twfe <- append(twfe, reg_DD_twfe_subsample)

tab <- msummary(twfe,stars=TRUE,
                coef_rename = c("D"="expand"),
                gof_map=c("nobs", "r.squared"),
                notes="Each column shows the estimation results of (1) Full sample, (2) Only 2014, (3) Only 2015, (4) Only 2016",
                output = "gt")
gt::gtsave(tab, filename=here('output','tab_twfe.tex'))
print("created tab_twfe.tex")