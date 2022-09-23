if (sys.nframe()==0){  # if __name__==__main__
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

tab <- df %>% 
  group_by(year) %>%
  summarise(mean=mean(uncomp_care/1000000,na.rm=T),
            sd=sd(uncomp_care/1000000,na.rm = T),
            min=min(uncomp_care/1000000,na.rm=T),
            max=max(uncomp_care/1000000,na.rm=T)) %>%
  mutate(year = as.integer(year))
xtab <- xtable(tab,
               align = c("c","c","c","c","c","c"),
               caption = "Summary statistics of uncompensated care")
outfile = paste0(dir_root,'/output/tab_sumstat_uncompcare.tex')
print(xtab, file=outfile, include.rownames=F)
print(paste0("Created ",outfile))