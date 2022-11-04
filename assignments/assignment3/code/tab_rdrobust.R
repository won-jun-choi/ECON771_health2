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
  mutate(L1.LISsubsidy = lag(LISsubsidy, n=1, default=NA, order_by=year),
         L2.LISsubsidy = lag(LISsubsidy, n=2, default=NA, order_by=year),
         L3.LISsubsidy = lag(LISsubsidy, n=3, default=NA, order_by=year),
         L4.LISsubsidy = lag(LISsubsidy, n=4, default=NA, order_by=year)) %>%
  ungroup()

y <- df_temp%>%filter(year==2006)%>%pull(log_share)

cls_rdrobust <- function(year,score,p) {
  rdd <- rdrobust(y = df_temp%>%filter(year==year)%>%pull(log_share),
                  x = -1*df_temp%>%filter(year==year)%>%pull(score),
                  p=p, bwselect='cerrd')
  return(rdd)
}

panel1 <- data.frame()
for (y in 1:5) {
  if (y == 1) {score <- 'LISsubsidy'}
  else {score <- paste0('L',y-1,'.LISsubsidy')}
  rdd <- cls_rdrobust(year=2005+i, score, p=1)
  coef <- round(rdd$coef[1],3)
  se <- paste0("(",round(rdd$se[1],3),")")
  bw = round(rdd$bws[1,1],3)
  row <- data.frame(coef, se, bw)
  panel1 <- rbind(panel1,row)
}
rownames(panel1) <- c(2006:2010)
panel1 <- panel1 %>% t()

panel2 <- data.frame()
for (y in 1:5) {
  if (y == 1) {score <- 'LISsubsidy'}
  else {score <- paste0('L',y-1,'.LISsubsidy')}
  rdd <- cls_rdrobust(year=2005+i, score, p=2)
  coef <- round(rdd$coef[1],3)
  se <- paste0("(",round(rdd$se[1],3),")")
  bw = round(rdd$bws[1,1],3)
  row <- data.frame(coef, se, bw)
  panel2 <- rbind(panel2,row)
}
rownames(panel2) <- c(2006:2010)
panel2 <- panel2 %>% t()
panel2

xtab <- xtable(panel1)
print(xtab, file=here(dir_root,'output','tab_rdrobust_panel1.tex'))
xtab <- xtable(panel2)
print(xtab, file=here(dir_root,'output','tab_rdrobust_panel2.tex'))
