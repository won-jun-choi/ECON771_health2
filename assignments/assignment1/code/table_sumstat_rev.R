if (sys.nframe()==0){  # if __name__==__main__
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

tab <- df %>%
  group_by(year) %>%
  summarise(mean=mean(tot_pat_rev,na.rm=T)/1000000,
            sd=sd(tot_pat_rev,na.rm = T)/1000000,
            min=min(tot_pat_rev,na.rm=T)/1000000,
            max=max(tot_pat_rev,na.rm=T)/1000000) %>%
  mutate(year = as.integer(year))
xtab <- xtable(tab,
              align = c("c","c","c","c","c","c"),
              caption = "Summary statistics of hospital total revenue")
outfile = paste0(dir_root,'/output/tab_sumstat_rev.tex')
print(xtab, file=outfile, include.rownames=F)
print(paste0("Created ",outfile))