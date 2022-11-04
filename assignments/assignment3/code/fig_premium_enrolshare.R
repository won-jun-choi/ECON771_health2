if (sys.nframe()==0){
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

df_temp <- df %>%
  filter(benefit == 'B',
         year==2006,
         LISsubsidy <= 10, LISsubsidy >=-10)

rd_linear <- rdplot(y = df_temp$log_share,
                    x = df_temp$LISsubsidy,
                    h = 4, p=1, hide=T)
rd_linear_params <- rd_linear$rdplot$plot_env$coef

rd_quartic <- rdplot(y = df_temp$log_share,
             x = df_temp$LISsubsidy,
             c = 0, p = 4, h=10, n=20)

bin.avg <- as_tibble(rd_quartic$vars_bins)

plot.bin <- bin.avg %>%
  filter(rdplot_mean_x <= 10 & rdplot_mean_x >= -10) %>%
  ggplot(aes(x=rdplot_mean_x,y=rdplot_mean_y)) +
  geom_point() + theme_bw() +
  geom_line(aes(x=))

show(plot.bin)
