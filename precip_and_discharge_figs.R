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


#Creation of figures using precipitation and river discharge information

#Precipitation

ct_prcp_month2 <- ggplot(data=study_precip,aes(x=month,y=mean,group=Year))+
  geom_line(aes(linetype=Year,color=Year),size=2.5)+
  scale_color_grey(start=0.1, end=0.8)+
  scale_linetype_manual(values=c("dotted","longdash","dashed","solid"))+
  scale_x_discrete(limits = c("January","February","March","April","May","June",
                              "July","August","September","October","November",
                              "December"))+
  labs(y = "Monthly Precipitation (cm)",
       x = "Month",
       color="",linetype="")+
  theme_classic()+
  theme(axis.text.x=element_text(size=30,angle = 45,hjust = 1),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=30),
        axis.title.y = element_text(size=40),
        text = element_text(size = 50),
        legend.position = c(0.8,0.8),
        legend.key.width = unit(5,"cm"))


ct_prcp_month2

ggsave(ct_prcp_month2, file="ct_prcp_month2.jpg", dpi=600, width=20, height=15, units="in")



#River Discharge

before_prrip_discharge_monthly <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year<=2006) %>%  
  group_by(month) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE)) %>% 
  mutate(year="1930-2006")

prrip_pre19_discharge_monthly <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year>=2007) %>%
  filter(year<=2018) %>% 
  group_by(month) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE)) %>% 
  mutate(year="2007-2018")

prrip_discharge_monthly <- rawDailyData %>% 
  separate(Date,c("year","month","day")) %>% 
  filter(year>=2007) %>%  
  group_by(month) %>% 
  summarise(mean=mean(discharge_m3_sec,na.rm = TRUE)) %>% 
  mutate(year="2007-2023") %>% 
  rbind(before_prrip_discharge_monthly)

prrip_river_discharge <- ggplot(data = prrip_discharge_monthly,aes(month,mean,group=year,linetype=year,colour=year))+
  geom_line(aes(linetype=year,color=year),size=2.5)+
  scale_color_manual(values=c("1930-2006"="grey70","2007-2023"="grey0"),name="",labels=c("Pre-Wet Cycle","Post-Wet Cycle"))+
  scale_linetype_manual(values=c("1930-2006"="solid","2007-2023"="dashed"),name="",labels=c("Pre-Wet Cycle","Post-Wet Cycle"))+
  ylab(bquote('Discharge' ~ m^'3' ~ '/sec'))+
  xlab("Month")+
  ylim(0,100)+
  labs(color="",linetype="")+
  guides(colour = guide_legend(override.aes = list(size = 40)))+
  theme_classic()+
  theme(axis.text.x=element_text(size=40),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40),
        axis.title.y = element_text(size=40),
        text = element_text(size = 50),
        legend.position = c(0.8,0.8),
        legend.key.width = unit(5,"cm"))

prrip_river_discharge

ggsave(prrip_river_discharge, file="prrip_river_discharge.jpg", dpi=600, width=20, height=15, units="in")








