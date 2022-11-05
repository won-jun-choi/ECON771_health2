if (sys.nframe()==0){
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

RDwindow2006 <- df %>%
  filter(year==2006,
         LISsubsidy <= 4,
         LISsubsidy >= -4,
         benefit=='B') %>%
  select(uniqueID) %>% unlist()

df_temp <- df %>%
  filter(uniqueID %in% RDwindow2006) %>%
  group_by(uniqueID) %>%
  mutate(L1.premium = lag(premium, n=1, default=NA, order_by=year),
         L2.premium = lag(premium, n=2, default=NA, order_by=year),
         L3.premium = lag(premium, n=3, default=NA, order_by=year),
         L4.premium = lag(premium, n=4, default=NA, order_by=year)) %>%
  ungroup() %>%
  mutate(Dpremium = premium - L1.premium,
         log_premium = log(premium))

share2006 <- df %>%
  filter(uniqueID %in% RDwindow2006,
         year==2006) %>%
  select(uniqueID, share) %>%
  rename(share2006 = share)

df_temp <- df_temp %>% left_join(share2006, by='uniqueID') %>% 
  mutate(log_share2006 = log(share2006))

lm <- feols(Dpremium ~ log_share2006 | state+year | c(log_share2006)~LIS,
            data=df_temp)
summary(lm)

lm <- feols(log_premium ~ log_share2006 | state+year | c(log_share2006)~LIS,
            data=df_temp)
summary(lm)
