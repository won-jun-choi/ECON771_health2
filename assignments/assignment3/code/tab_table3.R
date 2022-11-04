if (sys.nframe()==0){
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

my_RD <- function(y_name,x_name,h,data,p=1,c=0) {
  data$y <- data %>% pull(y_name)
  data$x <- data %>% pull(x_name)
  data <- data %>% filter( (x < h) & (x > -h))
  data$left <- 1*(data$x <= 0)
  data$x_left <- data$left * data$x
  data$x_right <- (1-data$left) * data$x

  if (p==1) {
    ols <- lm(y~left + x_left + x_right,data=data)
  } else if (p==2) {
    data$x_left2 <- data$x_left^2
    data$x_right2 <- data$x_right^2
    ols <- lm(y~left + x_left + x_left2 + x_right + x_right2, data=data)
  }
  print("NEED TO CLUSTER THE ERROR!!")
  return(ols)
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

ols2006 <- my_RD('log_share','LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2006),p=1,c=0)
ols2007 <- my_RD('log_share','L1.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2007),p=1,c=0)
ols2008 <- my_RD('log_share','L2.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2008),p=1,c=0)
ols2009 <- my_RD('log_share','L3.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2009),p=1,c=0)
ols2010 <- my_RD('log_share','L4.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2010),p=1,c=0)

panel1 <- msummary(list('2006'=ols2006, '2007'=ols2007, '2008'=ols2008,
                        '2009'=ols2009, '2010'=ols2010),
                   output = here(dir_root,'output','tab_panel1.tex'),
                   vcov = ~orgParentCode,
                   coef_omit='(Intercept)',
                   coef_rename=c('left'='Below Benchmark, 2006',
                                 'x_left'='... Below Benchmark',
                                 'x_right'='... Above Benchmark'),
                   gof_map=c('nobs',
                             'r.squared'),
                   stars=c('*'=0.10,'**'=0.05,'***'=0.01))
# print(panel1)

ols2006 <- my_RD('log_share','LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2006),p=2,c=0)
ols2007 <- my_RD('log_share','L1.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2007),p=2,c=0)
ols2008 <- my_RD('log_share','L2.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2008),p=2,c=0)
ols2009 <- my_RD('log_share','L3.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2009),p=2,c=0)
ols2010 <- my_RD('log_share','L4.LISsubsidy',h=4,
                 data=df_temp%>%filter(year==2010),p=2,c=0)
panel2 <- msummary(list('2006'=ols2006, '2007'=ols2007, '2008'=ols2008,
                        '2009'=ols2009, '2010'=ols2010),
                   output = here(dir_root,'output','tab_panel2.tex'),
                   vcov = ~orgParentCode,
                   coef_map=c('left'='Below Benchmark, 2006'),
                   gof_map=c('nobs',
                             'r.squared'),
                   stars=c('*'=0.10,'**'=0.05,'***'=0.01))
