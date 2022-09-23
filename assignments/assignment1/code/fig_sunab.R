if (sys.nframe()==0){  # if __name__==__main__
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

df_temp <- df %>%
  filter(!is.na(uncomp_care)) %>%
  mutate(uncomp_care = uncomp_care/1000000,
         expand_year = ifelse(expanded==FALSE,999,as.integer(year_adopted)),
         time_to_treat = ifelse(expanded==FALSE,-1, year-expand_year))

reg_sunab <- feols(uncomp_care~sunab(expand_year, time_to_treat)|provider+ year,
                   cluster=~state,
                   data=df_temp)

jpeg(filename=here('output','fig_sunab.jpg'))
iplot(reg_sunab)
dev.off()