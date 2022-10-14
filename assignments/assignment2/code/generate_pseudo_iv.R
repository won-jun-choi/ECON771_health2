gc()

piv <- data.frame()
for (y in 2012:2017) {
  print(y)
  price.shock <- vroom(here(dir_root,'temp',paste0('priceshock',y,'.csv')))
  # Random assign
  price.shock$price_shock<-price.shock$price_shock[sample(1:nrow(price.shock))]
  
  price.shock <- price.shock %>%
    mutate(denom = line_srvc_cnt*price_nonfac_orig_2010,
           numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) %>%
    group_by(npi) %>%
    summarize(phy_numer=sum(numer, na.rm=TRUE),
              phy_denom=sum(denom, na.rm=TRUE),
              tax_id=first(tax_id)) %>%
    ungroup() %>%
    mutate(phy_rev_change=phy_numer/phy_denom) %>%    
    group_by(tax_id) %>%
    summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
    ungroup()
  
  price.shock$Year <- y
  
  piv <- bind_rows(piv, price.shock)
}
print("created pseudo iv")
rm(list=c('y','price.shock'))
gc()
