#Checking for normality

#normal
shapiro.test(relict_data_doc$wis)
shapiro.test(relict_data_doc$soil_moist_mean)
shapiro.test(relict_data_doc$dtw_cm_mean)
shapiro.test(relict_data_doc$spring_depth21_cm_mean)
shapiro.test(relict_data_doc$c)
shapiro.test(relict_data_doc$sand_mean)
shapiro.test(relict_data_doc$clay_mean)
shapiro.test(relict_data_doc$e)
shapiro.test(relict_data_doc$fqi)


#Transformation makes normal

shapiro.test(relict_data_doc$root_g_mean)
shapiro.test(test$root_g_mean_tran)

shapiro.test(relict_data_doc$om_mean)
shapiro.test(test$om_mean_tran)

shapiro.test(relict_data_doc$nh4_mean)
shapiro.test(test$nh4_mean_tran)

#non

shapiro.test(relict_data_doc$wwis)

shapiro.test(relict_data_doc$spring_depth22_cm_mean)
shapiro.test(test$spring_depth22_cm_mean_tran)

shapiro.test(relict_data_doc$cond_mean)
shapiro.test(test$cond_mean_tran)


shapiro.test(relict_data_doc$bulk_dens_mean)
shapiro.test(test$bulk_dens_mean_tran)

shapiro.test(relict_data_doc$celcius_mean)
shapiro.test(test$celcius_mean_tran)


shapiro.test(relict_data_doc$elevation_mean)
shapiro.test(test$elevation_mean_tran)

shapiro.test(relict_data_doc$wc)
shapiro.test(test$wc_tran)

shapiro.test(relict_data_doc$ld)
shapiro.test(test$ld_tran)

shapiro.test(relict_data_doc$bg)
shapiro.test(test$bg_tran)

shapiro.test(relict_data_doc$top_nmean)
shapiro.test(test$top_nmean_tran)




shapiro.test(relict_data_doc$silt_mean)
