library(tidyverse)
library(lubridate)
library(BiodiversityR) # also loads vegan
library(ggplot2)
library(readxl)
library(ggsci)
library(ggrepel)
library(ggforce)
library(ggfortify)
library(tidyverse)
library(HDInterval)
library(ggridges)
library(ordr)
library(scales)




#Creation of depth to water comparison figure


depth_to_water_fig <- relict_data_doc %>% 
  mutate(month = format(as.Date(date), "%m"),
         year_dtw = format(as.Date(date), "%y")) %>% 
  mutate(month=ifelse(month=="05","06",month)) %>% 
  ggplot(aes(x=month,y=dtw_cm_mean,color=wetness))+
  geom_boxplot(lwd=3)+
  scale_x_discrete(labels=c("05"="May","06"="June","07"="July","08"="August"))+
  scale_color_manual(values=c("non"="grey70","wet"="grey0"),name="",labels=c("Dry","Wet"))+
  ylab("Depth to Water (cm)")+
  xlab("Month")+
  ylim(-125,0)+
  theme_classic()+
  theme(axis.text.x=element_text(size=40),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40),
        axis.title.y = element_text(size=40),
        text = element_text(size = 60),
        legend.position = c(0.8,0.8),
        legend.key.width = unit(3,"cm"))


depth_to_water_fig

ggsave(depth_to_water_fig, file="depth_to_water_fig.jpg", dpi=600, width=20, height=15, units="in")




