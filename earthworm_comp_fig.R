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



earthworm_comp_fig <- ggplot(invert_comparison)+
  geom_bar(aes(x=pub,y=mean),
           stat='identity')+
  geom_errorbar(aes(x=pub,ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9))+
  theme_classic()+
  xlab("")+
  ylab(expression(paste('Earthworm Biomass (g/m'^3,')')))+
  scale_y_continuous(limits = c(0,NA), oob=squish)+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15),
                   limits = c("Nagel and Harding 1987","Davis 1991",
                              "Davis et al 2006", "Current"))+
  theme(axis.text.x=element_text(size=50,angle = 45,hjust = 1),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))


earthworm_comp_fig


ggsave(earthworm_comp_fig, file="earthworm_comp_fig.jpg", dpi=600, width=20, height=15, units="in")


