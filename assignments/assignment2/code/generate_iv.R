rm(list=ls())
gc()
source('setup.R')
WAU('wonjun')

taxid.base <- vroom(here(dir_data,'MDPPAS','PhysicianData_2009.csv')) %>%
  select(npi, tax_id = group1) %>%
  mutate(npi = as.character(npi))
pfs <- vroom(here(dir_data,'Physician Fee Schedule 2010 Update','PFS_update_data.txt'))

iv <- data.frame()
for (y in 2012:2017) {
  print(y)
  medicare.puf <- vroom(
    here(dir_data,'utilization-payment-puf',y,
         paste0('Medicare_Provider_Util_Payment_PUF_CY',y,'.txt')))
  names(medicare.puf) <- tolower(names(medicare.puf))
  medicare.puf <- medicare.puf %>%
    filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE)) %>%
    select(npi, nppes_credentials, hcpcs_code, line_srvc_cnt)
  
  pfs.yearly <- pfs %>% filter(year==min(y,2013))
  
  price.shock <- medicare.puf %>% 
    inner_join(taxid.base, by="npi") %>%
    inner_join(pfs.yearly %>% 
                 select(hcpcs, dprice_rel_2010,
                        price_nonfac_orig_2010,
                        price_nonfac_orig_2007), 
               by=c("hcpcs_code"="hcpcs")) %>%
    mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, 
                   price_nonfac_orig_2007), replace_na, 0) %>%
    mutate(price_shock = case_when(
      y<=2013 ~ ((y-2009)/4)*dprice_rel_2010,
      y>2013  ~ dprice_rel_2010),
      denom = line_srvc_cnt*price_nonfac_orig_2010,
      numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010)
  
  # save for question 8 (pseudo iv)
  filename <- here(dir_root,'temp',paste0('priceshock',y,'.csv'))
  vroom_write(price.shock, file = filename, delim = ",")
  
  price.shock <- price.shock %>%
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
  
  iv <- bind_rows(iv, price.shock)
}
vroom_write(iv, file = here(dir_root,'temp','instrument.csv'), delim = ",")
print("created instrument.csv")
rm(list=c('y','iv','medicare.puf','pfs','pfs.yearly','price.shock','taxid.base'))
gc()
