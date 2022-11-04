# Last Edit: 2022-11-03
# Replication code for Table 1 in Ericson (2014)

if (sys.nframe()==0){
  source('setup.R')
  WAU(name='wonjun')
  df <- read_dta(here(dir_data,'Ericson 2014','DataFiles','Data_main.dta'))
}

df <- df %>% 
  group_by(uniqueID) %>%
  mutate(cohort = min(year)) %>%
  ungroup() %>%
  mutate(enhanced = ifelse(benefit=='E',1,0)) %>%
  group_by(orgParentCode) %>%
  mutate(entry_year = min(year)) %>%
  mutate(already = ifelse(year==entry_year,0,1)) %>%
  ungroup() %>%
  group_by(orgParentCode, state) %>%
  mutate(entry_year_state = min(year)) %>%
  mutate(already_state = ifelse(year==entry_year,0,1)) %>%
  ungroup()

tab <- df %>%
  filter(year==cohort) %>% 
  group_by(year) %>%
  summarise('Mean monthly premium' = as.integer(mean(premium)),
            'sd_premium' = as.integer(sd(premium)),
            'Mean deductible' = as.integer(mean(deductible)),
            'sd_deductible' = as.integer(sd(deductible)),
            'Fraction enhanced benefit' = round(mean(enhanced),2),
            '...in the U.S.' = round(mean(already),2),
            '...in the same state' = round(mean(already_state),2),
            'N Unique Firms' = n_distinct(orgParentCode),
            'N Plans' = n()) %>%
  t() %>%
  row_to_names(row_number=1)
  
xtab <- xtable(tab, align = 'cccccc')
print(xtab, file = here(dir_root,'output','tab_descstat.tex'))
