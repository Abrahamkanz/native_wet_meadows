library(dunn.test)
library(FSA)
library(sf)
library(BAMMtools)


#########################################################################

kruskal.test(relict_data_doc$soil_moist_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$dtw_cm_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$spring_depth21_cm_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$spring_depth22_cm_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$cond_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$bulk_dens_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$top_nmean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$nh4_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$om_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$ld,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$root_g_mean,relict_data_doc$wetness,method = "bonferroni")


#########################################################################################

kruskal.test(relict_data_doc$spring_depth21_cm_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(spring_depth21_cm_mean),
            se=sd(spring_depth21_cm_mean)/sqrt(n_wet)) %>%
  ungroup()

########################################

kruskal.test(relict_data_doc$spring_depth22_cm_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(spring_depth22_cm_mean),
            se=sd(spring_depth22_cm_mean)/sqrt(n_wet)) %>%
  ungroup()

###########################################

kruskal.test(relict_data_doc$dtw_cm_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(dtw_cm_mean),
            se=sd(dtw_cm_mean)/sqrt(n_wet)) %>%
  ungroup()

###########################################

kruskal.test(relict_data_doc$cond_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(cond_mean),
            se=sd(cond_mean)/sqrt(n_wet)) %>%
  ungroup()

###########################################

kruskal.test(relict_data_doc$elevation_mean,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$elevation_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(elevation_mean),
            se=sd(elevation_mean)/sqrt(n_wet)) %>%
  ungroup()


###########################################

kruskal.test(relict_data_doc$soil_moist_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  reframe(mean=mean(soil_moist_mean),
          se=sd(soil_moist_mean)/sqrt(n_wet)) %>%
  ungroup() %>% 
  unique()

###########################################

kruskal.test(relict_data_doc$celcius_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(celcius_mean),
            se=sd(celcius_mean)/sqrt(n_wet)) %>%
  ungroup()

###########################################

kruskal.test(relict_data_doc$top_nmean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(top_nmean),
            se=sd(top_nmean)/sqrt(n_wet)) %>%
  ungroup()


###########################################

kruskal.test(relict_data_doc$nh4_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(nh4_mean),
            se=sd(nh4_mean)/sqrt(n_wet)) %>%
  ungroup()

###########################################

kruskal.test(relict_data_doc$som_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(som_mean),
            se=sd(som_mean)/sqrt(n_wet)) %>%
  ungroup()


###########################################

kruskal.test(relict_data_doc$bulk_dens_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(bulk_dens_mean),
            se=sd(bulk_dens_mean)/sqrt(n_wet)) %>%
  ungroup()


###########################################

kruskal.test(relict_data_doc$ld,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(ld),
            se=sd(ld)/sqrt(n_wet)) %>%
  ungroup()

###########################################

kruskal.test(relict_data_doc$root_g_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(root_g_mean),
            se=sd(root_g_mean)/sqrt(n_wet)) %>%
  ungroup()

###########################################

kruskal.test(relict_data_doc$sand_mean,relict_data_doc$wetness,method = "bonferroni")

relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(sand_mean),
            se=sd(sand_mean)/sqrt(n_wet)) %>%
  ungroup() %>% 
  unique()

###########################################

kruskal.test(relict_data_doc$clay_mean,relict_data_doc$wetness,method = "bonferroni")


test <- relict_data_doc %>%
  group_by(wetness) %>% 
  summarise(mean=mean(clay_mean),
            se=sd(clay_mean)/sqrt(n_wet)) %>%
  ungroup()

##########################################################################################


kruskal.test(relict_data_doc$wwis,relict_data_doc$wetness,method = "bonferroni")


relict_data_doc %>%
  group_by(wetness) %>% 
  reframe(mean=mean(wwis),
          se=sd(wwis)/sqrt(n_wet),
          min=min(wwis),
          max=max(wwis)) %>%
  ungroup() %>% 
  unique()


##########################################################################################

#Kruskal-Wallis testing of invertebrate biomass across wetness categories

kruskal.test(relict_data_doc$arth_gm3_tipu,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_taban,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$worm_gm3_diplo,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$worm_gm3_apor,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_strat,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_scara,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_carab,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_lepi,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_arma,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_elat,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_curc,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$arth_gm3_staph,relict_data_doc$wetness,method = "bonferroni")
kruskal.test(relict_data_doc$tot_bio,relict_data_doc$wetness,method = "bonferroni")
