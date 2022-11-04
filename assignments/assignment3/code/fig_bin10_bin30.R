if (sys.nframe()==0){
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

# J = 10
J = 10
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
                     c = 0, p = 4, h=10, n=J, hide=T)
bin.avg <- as_tibble(rd_quartic$vars_bins)
quartic <- as_tibble(rd_quartic$vars_poly)

fig1 <- bin.avg %>%
  ggplot() +
  geom_point(aes(x=rdplot_mean_x,y=rdplot_mean_y)) + theme_bw() +
  geom_line(mapping=aes(x=rdplot_x,y=rdplot_y),data=linear,linetype='dashed') +
  geom_line(mapping=aes(x=rdplot_x,y=rdplot_y),data=quartic) +
  xlab("Monthly Premium - LIS Subsidy, 2006") +
  ylab("Log Enrollment Share, 2006")
ggsave(fig1, filename=here(dir_root,'output','fig_rdplot_j10.jpg'),
       width = 5, height = 3)

# J = 30
J = 30
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
                     c = 0, p = 4, h=10, n=J, hide=T)
bin.avg <- as_tibble(rd_quartic$vars_bins)
quartic <- as_tibble(rd_quartic$vars_poly)

fig2 <- bin.avg %>%
  ggplot() +
  geom_point(aes(x=rdplot_mean_x,y=rdplot_mean_y)) + theme_bw() +
  geom_line(mapping=aes(x=rdplot_x,y=rdplot_y),data=linear,linetype='dashed') +
  geom_line(mapping=aes(x=rdplot_x,y=rdplot_y),data=quartic) +
  xlab("Monthly Premium - LIS Subsidy, 2006") +
  ylab("Log Enrollment Share, 2006")

ggsave(fig2, filename=here(dir_root,'output','fig_rdplot_j30.jpg'),
       width = 5, height = 3)
rm(list=c('linear','quartic','rd_linear','rd_quartic','plot.bin','bin.avg',
          'fig1','fig2'))
