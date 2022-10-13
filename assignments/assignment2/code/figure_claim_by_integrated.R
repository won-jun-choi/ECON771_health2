if (sys.nframe()==0){ # if __name__==__main__
  source('setup.R')
  WAU(name="wonjun")
  df <- vroom(here(dir_root,'temp','superfatdata.csv'))
}

fig <- df %>%
  filter(!is.na(INT)) %>%
  group_by(Year, npi, INT) %>%
  mutate(tot_claims = sum(line_srvc_cnt, na.rm=T)) %>%
  group_by(Year, INT) %>%
  summarise(claims = mean(tot_claims, na.rm=T)) %>%
  ggplot(aes(y=claims, x=Year, group=INT)) +
  geom_line() +
  labs(x = "Year", y="Average number of claims")
# show(fig)
ggsave(here(dir_root,'tex','fig_claim_by_int.jpg'),
       width = 10, height = 7, units='cm',
       dpi=200)
print('created fig_claim_by_int.jpg')
rm(list=c('fig'))
gc()
