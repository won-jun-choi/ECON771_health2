if (sys.nframe()==0){  # if __name__==__main__
  source('setup.R')
  WAU(name='wonjun')
  source('data.R')
}

df %>% 
  filter(own_type=="Non-profit Private"|own_type=="Profit") %>%
  group_by(year, own_type) %>%
  summarise(mean = mean(uncomp_care, na.rm=T)/1000000) %>%
  ggplot(aes(x=year, y=mean, color=own_type)) +
  geom_line(size=0.8) + 
  guides(color=guide_legend(title="Ownership")) +
  theme_bw()+
  #  ggtitle("Average hospital uncompensated care") +
  labs(y = "Average uncompensated care (in $100000)", x = "Year")
outfile = paste0(dir_root,"/output/fig_uncompcare.jpg")
ggsave(filename=outfile)
print(paste0("Created ",outfile))