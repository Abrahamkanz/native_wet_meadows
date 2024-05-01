library(tidyverse)

getmode <- function(x) {
  ux <- unique(na.omit(x))
  tx <- tabulate(match(x, ux))
  if(length(ux) != 1 & sum(max(tx) == tx) > 1) {
    if (is.character(ux)) return(NA_character_) else return(NA_real_)
  }
  max_tx <- tx == max(tx)
  return(ux[max_tx])
}

soil_temps_21_merge <- soil_temps_21 %>% 
  separate(sample,c("s","sample"),-1) %>% 
  select(-s)

soil_20_21 <- soil_20_21_full %>% 
  mutate(soil_moist = soil_moist_num,
         dtw_cm = dtw_cm_num) %>% 
  select(-...1,-dtw_cm_num,-soil_moist_num) %>% 
  mutate(dirt_mass = as.numeric(dirt_mass),
         elevation1 = as.numeric(elevation1),
         elevation2 = as.numeric(elevation2),
         cond = as.numeric(cond),
         ribbon_mm = as.numeric(ribbon_mm),
         root_g = as.numeric(root_g),
         dirt_mass = as.numeric(dirt_mass),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         soil_moist = as.numeric(soil_moist),
         dtw_cm = as.numeric(dtw_cm),
         redox = na_if(redox, "NA"),
         probe = na_if(probe, "NA"),
         gley = na_if(gley, "NA"),
         som = na_if(som, "NA"))


soil_20_21_mod <- soil_20_21 %>% 
  merge(soil_temps_21_merge, all = T)

test <- soil22 %>%
  rbind(soil_20_21_mod) 

soil_summaries_all <- soil22 %>%
  rbind(soil_20_21_mod) %>% 
  mutate(som = tolower(som),
         gley = tolower(gley),
         redox = tolower(redox),
         loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "si1","si",loc)) %>% 
  separate(som,c("h","som"),-1) %>% 
  mutate(som = as.numeric(som),
         dirt_mass = ifelse(dirt_mass == 0, NA, dirt_mass)) %>% 
  mutate(dirt_vol = 1647.4072755,
         bulk_dens = dirt_mass/dirt_vol) %>%
  group_by(loc) %>% 
  summarise(cond_mean = mean(cond,na.rm = TRUE),
            cond_min = min(cond,na.rm = TRUE),
            cond_max = max(cond,na.rm = TRUE),
            cond_sd = sd(cond,na.rm = TRUE),
            redox = getmode(redox),
            gley = getmode(gley),
            ribbon_mm_mean = mean(ribbon_mm,na.rm = TRUE),
            ribbon_mm_min = min(ribbon_mm,na.rm = TRUE),
            ribbon_mm_max = max(ribbon_mm,na.rm = TRUE),
            ribbon_mm_sd = sd(ribbon_mm,na.rm = TRUE),
            som_mode = getmode(som),
            som_mean = mean(som, na.rm = TRUE),
            root_g_mean = mean(root_g,na.rm = TRUE),
            root_g_min = min(root_g,na.rm = TRUE),
            root_g_max = max(root_g,na.rm = TRUE),
            root_g_sd = sd(root_g,na.rm = TRUE),
            soil_moist_mean = mean(soil_moist,na.rm = TRUE),
            soil_moist_min = min(soil_moist,na.rm = TRUE),
            soil_moist_max = max(soil_moist,na.rm = TRUE),
            soil_moist_sd = sd(soil_moist,na.rm = TRUE),
            dtw_cm_mean = mean(dtw_cm,na.rm = TRUE),
            dtw_cm_min = min(dtw_cm,na.rm = TRUE),
            dtw_cm_max = max(dtw_cm,na.rm = TRUE),
            dtw_cm_sd = sd(dtw_cm,na.rm = TRUE),
            bulk_dens_mean = mean(bulk_dens,na.rm = TRUE),
            bulk_dens_min = min(bulk_dens,na.rm = TRUE),
            bulk_dens_max = max(bulk_dens,na.rm = TRUE),
            bulk_dens_sd = sd(bulk_dens,na.rm = TRUE),
            celcius_mean = mean(celcius,na.rm = TRUE),
            celcius_min = min(celcius,na.rm = TRUE),
            celcius_max = max(celcius,na.rm = TRUE),
            celcius_sd = sd(celcius,na.rm = TRUE),
            elevation_mean = mean((elevation1+elevation2)/2,na.rm = TRUE),
            elevation_min = min((elevation1+elevation2)/2,na.rm = TRUE),
            elevation_max = max((elevation1+elevation2)/2,na.rm = TRUE),
            elevation_sd = sd((elevation1+elevation2)/2,na.rm = TRUE))

unique(spring_depths22$site)


write.csv(soil22,file = "soil22.csv",row.names = FALSE)
write.csv(soil_20_21_mod ,file = "soil_20_21_mod.csv",row.names = FALSE)



spring_depths21_summary <- spring_depths21 %>% 
  mutate(loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "prs1","prs",loc),
         loc = ifelse(loc == "si1","si",loc),
         loc = ifelse(loc == "urep1","urep",loc)) %>% 
  filter(loc != "mm2",
         loc != "dnr1") %>% 
  group_by(loc) %>% 
  select(loc, depth_cm) %>% 
  summarise(spring_depth21_cm_mean = mean(depth_cm,na.rm = TRUE),
            spring_depth21_cm_min = min(depth_cm,na.rm = TRUE),
            spring_depth21_cm_max = max(depth_cm,na.rm = TRUE),
            spring_depth21_cm_sd = sd(depth_cm,na.rm = TRUE))


spring_depths22_summary <- spring_depths22 %>% 
  mutate(loc = site) %>% 
  group_by(loc) %>% 
  filter(loc != "dnr") %>% 
  select(loc, depth_cm) %>% 
  summarise(spring_depth22_cm_mean = mean(depth_cm,na.rm = TRUE),
            spring_depth22_cm_min = min(depth_cm,na.rm = TRUE),
            spring_depth22_cm_max = max(depth_cm,na.rm = TRUE),
            spring_depth22_cm_sd = sd(depth_cm,na.rm = TRUE))

spring_depths_all_summary <- spring_depths21_summary %>% 
  full_join(spring_depths22_summary)

write.csv(spring_depths22,file = "spring_depths22.csv",row.names = FALSE)
write.csv(spring_depths21 ,file = "spring_depths21.csv",row.names = FALSE)


collected_22 <- inverts22 %>% 
  ungroup() %>% 
  group_by(id) %>% 
  summarise(abun=sum(quantity))

write.csv(collected_22,"collected_22.csv")

inverts_20_21 <- inverts_20_21 %>% 
  select(-...1)

inverts_20_21_mod <- inverts_20_21 %>% 
  mutate(loc = ifelse(loc == "pd2","pd1",loc),
         loc = ifelse(loc == "si1","si",loc),
         loc=tolower(loc)) %>% 
  mutate(id=ifelse(id=="isop","arma",ifelse(id=="isopod","arma",id)))

arth_summary_all <- inverts22 %>% 
  select(-wt_pan,-wt_pan_invert,-`worm/snail/slug_jar?`) %>% 
  mutate(date = yday(date)) %>%
  rbind(inverts_20_21_mod) %>% 
  mutate(id = ifelse(id=="*curc","curc",ifelse(id=="*elat","elat",ifelse(id=="*scara","scara",id))),
         quantity = as.numeric(quantity),
         weight = wt_mg,
         id=ifelse(id=="havest","harvest",id),
         id=ifelse(id=="harvest","phalangida",id),
         id=ifelse(id=="melo","meloi",id),
         id=ifelse(id=="melol","meloi",id),
         id=ifelse(id=="plant","planthopper",id),
         id=ifelse(id=="plant_hopper","planthopper",id),
         id=ifelse(id=="aran","arach",id)) %>%
  group_by(loc, id) %>% 
  summarise(arth_tot = sum(weight,na.rm=TRUE),
            arth_mean = mean(weight,na.rm=TRUE),
            arth_min = min(weight),
            arth_max = max(weight),
            arth_sd = sd(weight,na.rm=TRUE),
            arth_abun = sum(quantity,na.rm = TRUE),
            arth_abun_mean = mean(quantity,na.rm=TRUE),
            arth_abun_min = min(quantity),
            arth_abun_max = max(quantity),
            arth_abun_sd = sd(quantity,na.rm = TRUE),
            arth_mean_gm3 = mean(weight,na.rm=TRUE)/0.0125,
            arth_gm3 = sum(weight,na.rm=TRUE)/(0.0125*6),
            arth_mean_abun_m3 = mean(quantity,na.rm=TRUE)/0.0125,
            arth_abun_m3 = sum(quantity,na.rm=TRUE)/(0.0125*6))


arth_all <- inverts22 %>% 
  select(-wt_pan,-wt_pan_invert,-`worm/snail/slug_jar?`) %>% 
  mutate(date = yday(date)) %>%
  rbind(inverts_20_21_mod) %>% 
  mutate(id = ifelse(id=="*curc","curc",ifelse(id=="*elat","elat",ifelse(id=="*scara","scara",id))),
         quantity = as.numeric(quantity),
         weight = wt_mg,
         id=ifelse(id=="havest","harvest",id),
         id=ifelse(id=="harvest","phalangida",id),
         id=ifelse(id=="melo","meloi",id),
         id=ifelse(id=="melol","meloi",id),
         id=ifelse(id=="plant","planthopper",id),
         id=ifelse(id=="plant_hopper","planthopper",id),
         id=ifelse(id=="aran","arach",id))


write.csv(arth_all,"arth_all.csv", row.names = FALSE)



arth_summary_all_wide <- arth_summary_all %>%
  ungroup() %>% 
  group_by(loc) %>% 
  pivot_wider(names_from = id, values_from = c("arth_tot","arth_mean","arth_min","arth_max","arth_sd",
                                               "arth_abun","arth_abun_mean", "arth_abun_min","arth_abun_max",
                                               "arth_abun_sd","arth_gm3","arth_abun_m3",
                                               "arth_mean_gm3", "arth_mean_abun_m3"), names_sep="_") %>% 
  mutate(arth_abun_diptera=sum(c(arth_abun_strat,arth_abun_tipu,arth_abun_doli,arth_abun_taban)))



arth_summary_all_wide[is.na(arth_summary_all_wide)] <- 0

write.csv(arth_summary_all_wide,"arth_summary_all_wide.csv", row.names = FALSE)




worm_20_21 <- worm_mass_20_21 %>% 
  mutate(s = sample,
         weight = dry_mass,
         loc = ifelse(loc == "si1","si",loc)) %>% 
  select(-...1,-sample,-dry_mass)

worm_22 %>% 
  ungroup() %>% 
  group_by(id) %>% 
  summarise(abun=sum(quantity))

worm_summary_all <- worm_22 %>% 
  mutate(loc = site,
         loc = ifelse(loc == "si1","si",loc)) %>% 
  select(-date,-site,-notes) %>% 
  rbind(worm_20_21) %>%
  group_by(loc,id) %>% 
  select(-s) %>% 
  summarise(worm_abun = sum(quantity,na.rm=TRUE),
            worm_abun_mean = mean(quantity,na.rm=TRUE),
            worm_abun_min = min(quantity),
            worm_abun_max = max(quantity),
            worm_abun_sd = sd(quantity,na.rm=TRUE),
            worm_tot = sum(weight,na.rm=TRUE),
            worm_mean = mean(weight,na.rm=TRUE),
            worm_min = min(weight),
            worm_max = max(weight),
            worm_sd = sd(weight,na.rm=TRUE),
            worm_mean_gm3 = mean(weight/0.0125,na.rm=TRUE),
            worm_gm3 = sum(weight,na.rm=TRUE)/(0.0125*6),
            worm_mean_abun_m3 = mean(quantity,na.rm=TRUE)/0.0125,
            worm_abun_m3 = sum(quantity,na.rm=TRUE)/(0.0125*6))

worms_all <- worm_22 %>% 
  mutate(loc = site,
         loc = ifelse(loc == "si1","si",loc)) %>% 
  select(-date,-site,-notes) %>% 
  rbind(worm_20_21)

write.csv(worms_all,"worms_all.csv", row.names = FALSE)


worm_summary_all_wide <- worm_summary_all %>% 
  pivot_wider(names_from = id, values_from = c("worm_abun","worm_abun_mean", "worm_abun_min","worm_abun_max","worm_abun_sd",
                                               "worm_tot","worm_mean","worm_min","worm_max","worm_sd","worm_mean_gm3","worm_gm3",
                                               "worm_mean_abun_m3","worm_abun_m3"), names_sep="_") %>% 
  mutate(loc = ifelse(loc == "phm1","phm",loc))


worm_summary_all_wide[is.na(worm_summary_all_wide )] <- 0


write.csv(worm_summary_all_wide,"worm_summary_all_wide.csv", row.names = FALSE)


soil_summaries_22 %>% 
  rbind(soil_summaries_20_21)

data_summary_all <-  soil_summaries_all%>% 
  full_join(worm_summary_all_wide) %>% 
  full_join(arth_summary_all_wide) %>% 
  full_join(spring_depths_all_summary) %>% 
  full_join(grazing_scores)

data_summary_all[is.na(data_summary_all)] <- 0

write.csv(data_summary_all,"data_summary_all.csv", row.names = FALSE)


site_list <-as.data.frame(unique(data_summary_all$loc))
site_list <- site_list %>% 
  mutate(loc = `unique(data_summary_all$loc)`) %>% 
  select(loc)

write.csv(site_list,"site_list.csv", row.names = FALSE)


test <- veg_summaries_full %>% 
  mutate(loc = ifelse(loc == "prs1","prs",loc),
         loc = ifelse(loc == "si1","si",loc))


site_trt_flood_all <- site_type_all %>% 
  merge(flood_freq) %>% 
  mutate(loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "si1","si",loc))


write.csv(site_trt_flood_all,"site_trt_flood_all.csv", row.names = FALSE)



elevation_22 <- soil22 %>% 
  select(loc,elevation1,elevation2) %>% 
  mutate(elevation = (elevation1+elevation2)/2) %>% 
  group_by(loc) %>% 
  summarise(elevation_mean = mean(elevation,na.rm = TRUE),
            elevation_min = min(elevation,na.rm = TRUE),
            elevation_max = max(elevation,na.rm = TRUE),
            elevation_sd = sd(elevation,na.rm = TRUE))

elevation_all <- elevation_22 %>% 
  rbind(elevations_20_21) %>% 
  mutate(loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "si1","si",loc))


write.csv(elevation_all,"elevation_all.csv", row.names = FALSE)


veg_summaries_full <- veg_summaries_new %>% 
  mutate(loc = ifelse(loc == "prs1","prs",loc))

####################################New addition to data doc- soil testing lab

soil_key <- soil_key20 %>% 
  rbind(soil_key21) %>% 
  rbind(soil_key22) %>% 
  mutate(loc=site) %>% 
  select(-site) %>% 
  mutate(sample=code) %>% 
  select(-code)

fert <- fert20 %>% 
  rbind(fert21) %>% 
  rbind(fert22) %>% 
  mutate(sample=`Sample No.`) %>% 
  filter(TopN!="(lbs/A)") %>% 
  clean_names() %>% 
  select(top_n,om,tn,nh4,p,k,sample) %>% 
  rbind(fert_new)

fert_new <- soil_lab2 %>% 
  clean_names() %>% 
  filter(top_n!="(lbs/A)") %>% 
  mutate(sample=sample_no) %>% 
  select(top_n,om,tn,nh4,p,k,sample)

texture <- texture21 %>% 
  rbind(texture22) %>% 
  select(-`Lab ID`,-`Date Received`,-TestCode) %>% 
  clean_names() %>% 
  filter(sample!="NA")

soil_lab_all <- soil_key %>% 
  merge(fert) %>% 
  merge(texture) 

#organic matter and total nitrogen is not present for 2022 yet. 

soil_lab <- soil_key %>% 
  merge(fert) %>% 
  merge(texture) %>% 
  select(-sample,-s) %>% 
  mutate(top_n=as.numeric(top_n),
         om=as.numeric(om),
         tn=as.numeric(tn),
         sand=as.numeric(ifelse(sand=="NA","0",sand)),
         silt=as.numeric(silt),
         clay=as.numeric(clay)) %>% 
  group_by(loc) %>% 
  summarise(top_nmean = mean(top_n,na.rm = TRUE),
            top_nmin = min(top_n,na.rm = TRUE),
            top_nmax = max(top_n,na.rm = TRUE),
            top_nsd = sd(top_n,na.rm = TRUE),
            om_mean = mean(om,na.rm = TRUE),
            om_min = min(om,na.rm = TRUE),
            om_max = max(om,na.rm = TRUE),
            om_sd = sd(om,na.rm = TRUE),
            nh4_mean = mean(nh4,na.rm = TRUE),
            nh4_min = min(nh4,na.rm = TRUE),
            nh4_max = max(nh4,na.rm = TRUE),
            nh4_sd = sd(nh4,na.rm = TRUE),
            sand_mean = mean(sand,na.rm = TRUE),
            sand_min = min(sand,na.rm = TRUE),
            sand_max = max(sand,na.rm = TRUE),
            sand_sd = sd(sand,na.rm = TRUE),
            silt_mean = mean(silt,na.rm = TRUE),
            silt_min = min(silt,na.rm = TRUE),
            silt_max = max(silt,na.rm = TRUE),
            silt_sd = sd(silt,na.rm = TRUE),
            clay_mean = mean(clay,na.rm = TRUE),
            clay_min = min(clay,na.rm = TRUE),
            clay_max = max(clay,na.rm = TRUE),
            clay_sd = sd(clay,na.rm = TRUE))

write.csv(soil_key,"soil_key.csv", row.names = FALSE)


topn_mod <- soil_key %>% 
  merge(fert) %>% 
  merge(texture) %>% 
  select(-sample,-s) %>% 
  mutate(top_n=as.numeric(top_n),
         om=as.numeric(om),
         tn=as.numeric(tn),
         sand=as.numeric(ifelse(sand=="NA","0",sand)),
         silt=as.numeric(silt),
         clay=as.numeric(clay)) %>% 
  filter(top_n<=10.0) %>% 
  merge(relict_wetness) %>% 
  summarise(top_nmean_mod = mean(top_n,na.rm = TRUE),
            top_nmin_mod = min(top_n,na.rm = TRUE),
            top_nmax_mod = max(top_n,na.rm = TRUE),
            top_nsd_mod = sd(top_n,na.rm = TRUE),
            cv=sd(top_n)/mean(top_n))

#####################################Put it all together

data_doc <- data_summary_all %>% 
  merge(site_trt_flood_all) %>% 
  merge(veg_summaries_full) %>% 
  merge(soil_lab) %>% 
  mutate(wis_categories = ifelse(wwis < 3, "wet",ifelse(wwis >= 3 & wwis < 3.2, "tr","up")),
         flood_num = as.numeric(ifelse(flood=="Frequently","4",
                                       ifelse(flood=="Occasionally","3",
                                              ifelse(flood=="Rarely","2","1"))))) %>% 
  merge(site_date_all) %>% 
  merge(site_year) %>% 
  mutate(day_of_year=yday(date))


write.csv(data_doc,"data_doc.csv", row.names = FALSE)


#######################################Just relict(native) sites

relict_data_doc <- data_doc %>% 
  filter(trt == "relict") %>% 
  mutate(worm_pa_diplo = as.integer(ifelse(worm_tot_diplo!="0","1","0")),
         worm_pa_apor = as.integer(ifelse(worm_tot_apor!="0","1","0")),
         arth_pa_tipu = as.integer(ifelse(arth_tot_tipu!="0","1","0")),
         arth_pa_scara = as.integer(ifelse(arth_tot_scara!="0","1","0")),
         arth_pa_staph = as.integer(ifelse(arth_tot_staph!="0","1","0")),
         arth_pa_curc = as.integer(ifelse(arth_tot_curc!="0","1","0")),
         arth_pa_carab = as.integer(ifelse(arth_tot_carab!="0","1","0")),
         arth_pa_elat = as.integer(ifelse(arth_tot_elat!="0","1","0")),
         arth_pa_strat = as.integer(ifelse(arth_tot_strat!="0","1","0")),
         arth_pa_doli = as.integer(ifelse(arth_tot_doli!="0","1","0")),
         arth_pa_form = as.integer(ifelse(arth_tot_form!="0","1","0")),
         arth_pa_arma = as.integer(ifelse(arth_tot_arma!="0","1","0")),
         arth_pa_lepi = as.integer(ifelse(arth_tot_lepi!="0","1","0"))) %>% 
  merge(site_year) %>% 
  merge(newram_all_edit) %>% 
  mutate(ribbon_category = ifelse(ribbon_mm_mean <= 15.0,"sand",ifelse(ribbon_mm_mean >= 50.0,"clay","loam")),
         dtw_category=ifelse(dtw_cm_mean<= 30,"low",ifelse(dtw_cm_mean<60,"med-low",ifelse(dtw_cm_mean<90,"med-high","high"))),
         spring22_dtw_category=ifelse(spring_depth22_cm_mean<= 30,"low",ifelse(spring_depth22_cm_mean<60,"med-low",ifelse(spring_depth22_cm_mean<90,"med-high","high"))),
         spring21_dtw_category=ifelse(spring_depth21_cm_mean<= 30,"low",ifelse(spring_depth21_cm_mean<60,"med-low",ifelse(spring_depth21_cm_mean<90,"med-high","high"))),
         spring_closest=ifelse(year=="22",spring_depth22_cm_mean,spring_depth21_cm_mean),
         wis_categories_mod=ifelse(wis_categories=="wet","wet","non"),
         p_exotic=ifelse(p_exotic==0.0,0.000001,p_exotic),
         worm_pa_apor=as.factor(worm_pa_apor)) %>% 
  merge(veg_comm_mod2) %>% 
  merge(invert_bio_relict) %>% 
  merge(site_date) %>% 
  merge(n_add) %>% 
  merge(n_year) %>% 
  merge(period_n) %>% 
  merge(n_cat_per_add) %>% 
  merge(n_cat_year_add) %>% 
  merge(n_per_year_add) %>%
  merge(n_dtw_spring21_add) %>% 
  merge(n_dtw_spring22_add) %>% 
  merge(n_dtw_add) %>%
  left_join(n_dtw_year_add) %>% 
  left_join(n_dtw_per_add) %>% 
  left_join(n_dtw_spring21_year_add) %>% 
  left_join(n_dtw_spring21_per_add) %>% 
  left_join(n_dtw_spring22_year_add) %>% 
  left_join(n_dtw_spring22_per_add) %>% 
  mutate(day_of_year=yday(date)) %>% 
  left_join(spring_dtw_dates) %>% 
  mutate(spring_samp=-ifelse(year=="22",spring_depth22_cm_mean,
                             ifelse(year=="21",spring_depth21_cm_mean,NA))) %>% 
  mutate(elevation_km=elevation_mean/1000) %>% 
  left_join(spring_discharge_all) %>% 
  mutate(dtw_cm_mean=-dtw_cm_mean) %>% 
  mutate(wetness = ifelse(wwis<3.0,"wet","non")) %>% 
  mutate(n_wet=ifelse(wetness=="wet",24,30))


