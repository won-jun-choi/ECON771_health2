# Meta
# Title:
# Author: Wonjun Choi
# Last edit: 2022-09-22
# Description:
# ==============================================================
if (sys.nframe()==0){ # if __name__==__main__
  source('setup.R')
  WAU(name="wonjun")
}

# Import data
df_HCRIS <- read.csv(paste0(dir_data,"/HCRIS-Ian/data/output/HCRIS_Data.txt"),
                     sep = '\t', header=TRUE)
f <- paste0(dir_data,"/cms-pos-ian/data/output/pos-data-combined.txt")
df_pos <- read.csv(f, sep = '\t')
f <- paste0(dir_data,"/Insurance-Access-ian/Insurance-Access/data/output/medicaid_expansion.txt")
df_KFF <- read.csv(f, sep = '\t')

df_HCRIS <- df_HCRIS %>% 
  filter(year>=2003 & year<=2019) %>%
  mutate(provider = provider_number,
         year = as.integer(year))

df_pos <- df_pos %>% filter(year>=2003 & year<=2019)

df_KFF <- df_KFF %>%
  mutate(state = c(state.abb,'DC')[match(State, 
                                         c(state.name,'District of Columbia'))],
         year_adopted = format(as.Date(df_KFF$date_adopted), format="%Y")) %>%
  select(state, year_adopted, expanded)

# Merge
df <- df_HCRIS %>% 
  mutate(provider=as.character(provider),
         year=as.character(year)) %>% 
  inner_join(df_pos, by=c("provider"="provider","year"="year")) %>%
  mutate(year=as.numeric(year),
         state = state.y) %>%
  left_join(df_KFF, by='state')

# select variables
df <- df %>% 
  filter(!(provider==151327),
         !(state=='PR' | state=='VI')) %>%  # misreported
  select(provider, year, uncomp_care, tot_pat_rev, expanded, year_adopted,
         state, own_type)

rm(list=c("df_HCRIS","df_pos","df_KFF"))