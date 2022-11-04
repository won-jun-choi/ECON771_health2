
# Load data
df <- read_dta(here(dir_data,'Ericson 2014','DataFiles','Data_main.dta'))

filename <- here(dir_data,'Ericson 2014','DataFiles','Data_subsidyinfo.dta')
df_subsidy <- read_dta(filename)
df_subsidy <- df_subsidy %>%
  pivot_longer(cols = c('s2006','s2007','s2008','s2009','s2010'),
               names_to = 'year',
               values_to = 'subsidy') %>%
  mutate(year = parse_number(year))

df <- df %>% left_join(df_subsidy, by=c('PDPregion','year'))
rm(list = c('df_subsidy'))

# define variables that are repeatedly used in analysis
df <- df %>% 
  group_by(state, year) %>%
  mutate(stateYrEnroll = sum(enrollment, na.rm=T)) %>%
  ungroup() %>%
  mutate(LISsubsidy = premium - subsidy,
         share = enrollment/stateYrEnroll,
         log_share = log(share))
