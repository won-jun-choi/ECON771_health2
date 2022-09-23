if (sys.nframe()==0){  # if __name__==__main__
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

df_temp <- df %>%
  filter(!is.na(uncomp_care)) %>%
  mutate(uncomp_care = uncomp_care/1000000,
         relative_time = year - as.integer(year_adopted),
         relative_time = ifelse(!is.na(relative_time),relative_time,0))

reg_event <- 
  feols(uncomp_care~i(relative_time, expanded, ref=-1) |provider+year,
        cluster = ~state, data = df_temp)

df_temp <- df_temp %>% filter((expanded==F)|year_adopted==2014)

reg_event_ss <- 
  feols(uncomp_care~i(relative_time,expanded,ref=-1)|provider+year,
        cluster = ~state, data = df_temp)

etable(reg_event, reg_event_ss, tex=T, style.tex=style.tex('aer'),
       file=here("output","tab_event.tex"), replace=T)