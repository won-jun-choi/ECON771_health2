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
linear <- as_tibble(rd_linear$vars_poly)

rd_quartic <- rdplot(y = df_temp$log_share,
                     x = df_temp$LISsubsidy,
                     c = 0, p = 4, h=10, hide=T,
                     binselect = 'esmv')
bin.avg <- as_tibble(rd_quartic$vars_bins)
quartic <- as_tibble(rd_quartic$vars_poly)

J <- rd_quartic$J
print(J)

fig <- bin.avg %>%
  ggplot() +
  geom_point(aes(x=rdplot_mean_x,y=rdplot_mean_y)) + theme_bw() +
  geom_line(mapping=aes(x=rdplot_x,y=rdplot_y),data=linear,linetype='dashed') +
  geom_line(mapping=aes(x=rdplot_x,y=rdplot_y),data=quartic) +
  xlab("Monthly Premium - LIS Subsidy, 2006") +
  ylab("Log Enrollment Share, 2006")

ggsave(fig, filename=here(dir_root,'output','fig_binopt.jpg'),
       width = 7, height = 4)
rm(list=c('linear','quartic','rd_linear','rd_quartic','plot.bin','bin.avg'))
gc()