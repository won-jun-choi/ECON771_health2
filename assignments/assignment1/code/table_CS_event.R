if (sys.nframe()==0){  # if __name__==__main__
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

df_temp <- df %>%
  filter(!is.na(uncomp_care)) %>%
  mutate(uncomp_care = uncomp_care/1000000,
         expand_year = ifelse(expanded==FALSE,0,as.integer(year_adopted))) %>%
  group_by(provider) %>%
  mutate(providergroup=cur_group_id()) %>% ungroup()

reg_CS <- att_gt(yname="uncomp_care",
                 tname="year",
                 idname="providergroup",
                 gname="expand_year",
                 data=df_temp, panel=TRUE, est_method="dr",
                 allow_unbalanced_panel=TRUE)

reg_CS_event <- aggte(reg_CS, type="dynamic")

msummary(reg_CS_event, output=here('output','tab_cs_event.tex'))