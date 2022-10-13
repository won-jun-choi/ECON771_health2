if (sys.nframe()==0){ # if __name__==__main__
  source('setup.R')
  WAU(name="wonjun")
  df <- vroom(here(dir_root,'temp','superfatdata.csv'))
}

tab <- df %>% 
  group_by(npi) %>%
  summarise(
    tot_spending = sum(average_medicare_allowed_amt*line_srvc_cnt, na.rm=T),
    tot_claim = sum(line_srvc_cnt, na.rm=T),
    tot_patient = sum(bene_unique_cnt, na.rm=T)) %>%
  summarise_at(c('tot_spending','tot_claim','tot_patient'),
               list(Mean=mean, S.D.=sd, Min=min, Max=max),
               na.rm = T) %>%
  pivot_longer(cols = everything(),
               names_to = c(' ','.value'),
               names_sep = "_",
               names_prefix = "tot_")

xtab <- xtable(tab,
               align = c("c","c","c","c","c","c"),
               caption = "Summary statistics",
               label = "tab:sum_stat")
print(xtab, file=here(dir_root,'tex','tab_sumstat.tex'),include.rownames=F)
print("created tab_sumstat.tex")
rm(list=c("tab",'xtab'))
gc()
