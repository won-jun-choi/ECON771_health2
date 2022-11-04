if (sys.nframe()==0){
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

df_temp <- df %>%
  filter(benefit == 'B',
         year==2006,
         LISsubsidy <= 10, LISsubsidy >=-10)

res <- rddensity(X = df_temp$LISsubsidy)
fig <- rdplotdensity(rdd=res, X=df_temp$LISsubsidy,
                     plotRange = c(-10,10))

jpeg(filename = here(dir_root,'output','fig_density.jpg'),
     width = 500, height = 300)
fig
dev.off()
