if (sys.nframe()==0){
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

my_RD <- function(y_name,x_name,h,data,p=1,c=0) {
  data$y <- data %>% pull(y_name)
  data$x <- data %>% pull(x_name)
  data <- data %>% filter( (x < h) & (x > -h))
  data$right <- 1*(data$x <= 0)
  data$x_left <- (1-data$right) * data$x
  data$x_right <- data$right * data$x

  if (p==1) {
    ols <- lm(y~right + x_left + x_right,data=data)
  } else if (p==2) {
    data$x_left2 <- data$x_left^2
    data$x_right2 <- data$x_right^2
    ols <- lm(y~right + x_left + x_left2 + x_right + x_right2, data=data)
  }
  print("NEED TO CLUSTER THE ERROR!!")
  return(ols)
}

LISsubsidy2006 <- df %>%
  filter(year==2006) %>%
  group_by(uniqueID) %>%
  summarise()
  
df_temp <- df %>%
  filter(benefit == 'B',
         LISsubsidy <= 10, LISsubsidy >=-10) %>%
  group_by(u)

ols2016 <- my_RD('log_share','LISsubsidy',h=4,data=df_temp,p=1,c=0)
summary(ols2016)

df_temp <- df %>%
  filter(benefit == 'B',
         year==2007,
         LISsubsidy <= 10, LISsubsidy >=-10)

ols2017 <- my_RD('log_share','LISsubsidy',h=4,data=df_temp,p=1,c=0)
summary(ols2017)
