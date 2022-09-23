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

reg_sunab <- 
  feols(uncomp_care~sunab(expand_year, time_to_treat, no_agg=T)|provider+ year,
        cluster=~state,
        data=df_temp)

tab14 <- esttable(reg_sunab,
                  keep = c("time_to_treat = -\\d+ x cohort = 2014",
                           "time_to_treat = \\d+ x cohort = 2014"))
tab15 <- esttable(reg_sunab,
                  keep = c("time_to_treat = -\\d+ x cohort = 2015",
                           "time_to_treat = \\d+ x cohort = 2015"))
tab16 <- esttable(reg_sunab,
                  keep = c("time_to_treat = -\\d+ x cohort = 2016",
                           "time_to_treat = \\d+ x cohort = 2016"))
tab_list <- list(tab14,tab15,tab16)
f <- function(tab) {
  newtab <- tab
  rnames <- rownames(tab)
  rownames(newtab) <- str_replace(rnames, " x cohort = \\d+","")
  newtab$key <- rownames(newtab)
  newtab <- as.tibble(newtab)
  return(newtab)
}
newtab_list <- lapply(tab_list, f)

tab14 <- f(tab14)
tab15 <- f(tab15)
tab16 <- f(tab16)

jaja <- tab14 %>% 
  left_join(tab15, by='key', suffix=c("14","15")) %>%
  left_join(tab16, by="key", suffix=c("","16"))

rnames <- jaja$key
jaja <- jaja %>% select(!key) %>%
  rename("E = 14" = reg_sunab14,
         "E = 15" = reg_sunab15,
         "E = 16" = reg_sunab)
jaja$" " <- rnames
jaja <- jaja %>% select(" ","E = 14","E = 15","E = 16")

print(xtable(jaja),
      file=here('output','tab_sunab.tex'),
      include.rownames=F)
