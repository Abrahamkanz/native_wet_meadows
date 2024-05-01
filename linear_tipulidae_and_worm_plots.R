
#Creating data frames for figures

worm_plot_dtw <- relict_data_doc %>% 
  select(loc,dtw_cm_mean,worm_gm3_diplo,worm_gm3_apor) %>% 
  gather(key="taxa",value="biomass_g_m3",3:4 ) %>% 
  mutate(taxa=ifelse(taxa=="worm_gm3_apor","Aporrectodea","Diplocardia")) %>% 
  merge(elevation_all)

worm_plot_moist <- relict_data_doc %>% 
  select(loc,soil_moist_mean,worm_gm3_diplo,worm_gm3_apor) %>% 
  gather(key="taxa",value="biomass_g_m3",3:4 ) %>% 
  mutate(taxa=ifelse(taxa=="worm_gm3_apor","Aporrectodea","Diplocardia")) %>% 
  merge(elevation_all)


worm_dtw_plot <- ggplot(worm_plot_dtw,aes(x=dtw_cm_mean,y=biomass_g_m3,color=taxa))+
  geom_smooth(method = 'glm')+
  theme_classic()+
  scale_color_manual(values=c("gray0","gray50"))+
  xlab("Water Table Depth (cm)")+
  ylab(expression(paste('Biomass (g/m'^3,')')))+
  labs(color="")+
  scale_y_continuous(limits = c(0,NA), oob=squish)+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50),
        legend.position = c(0.8,0.9),
        legend.key.width = unit(5,"cm"))

worm_dtw_plot

ggsave(worm_dtw_plot, file="worm_dtw_plot.jpg", dpi=600, width=20, height=15, units="in")




worm_moist_plot <- ggplot(worm_plot_moist,aes(x=soil_moist_mean,y=biomass_g_m3,color=taxa))+
  geom_smooth(method = 'lm')+
  theme_classic()+
  scale_color_manual(values=c("gray0","gray50"))+
  xlab("Soil Moisture (%)")+
  ylab(expression(paste('Biomass (g/m'^3,')')))+
  labs(color="")+
  scale_y_continuous(limits = c(0,NA), oob=squish)+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50),
        legend.position = c(0.8,0.9),
        legend.key.width = unit(5,"cm"))

worm_moist_plot

ggsave(worm_moist_plot, file="worm_moist_plot.jpg", dpi=600, width=20, height=15, units="in")



test <- lm(worm_gm3_apor~soil_moist_mean+day_of_year, data = relict_data_doc)
summary(test)
confint(test)






#Creation of Tipulidae figures


tipu_dtw <- ggplot(relict_data_doc,aes(x=dtw_cm_mean,y=arth_gm3_tipu))+
  geom_smooth(method = 'glm',color="gray50")+
  theme_classic()+
  xlab("Water Table Depth (cm)")+
  ylab(expression(paste('Tipulidae Biomass (g/m'^3,')')))+
  scale_y_continuous(limits = c(0,NA), oob=squish)+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))


tipu_dtw

ggsave(tipu_dtw, file="tipu_dtw.jpg", dpi=600, width=20, height=15, units="in")




tipu_moist <- ggplot(relict_data_doc,aes(x=soil_moist_mean,y=arth_gm3_tipu))+
  geom_smooth(method = 'glm',color="gray50")+
  theme_classic()+
  xlab("Soil Moisture (%)")+
  ylab(expression(paste('Tipulidae Biomass (g/m'^3,')')))+
  scale_y_continuous(limits = c(0,NA), oob=squish)+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50),
        plot.margin = margin(1,1,1.5,1.2, "cm"))


tipu_moist

ggsave(tipu_moist, file="tipu_moist.jpg", dpi=600, width=20, height=15, units="in")






