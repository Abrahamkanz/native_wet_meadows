#############################################Z-test

devtools::install_github('alanarnholt/BSDA')

library(BSDA)

########################Apor

z.test(relict_data_doc$worm_gm3_apor, mu=5.62, sigma.x=2.10)

z.test(relict_data_doc_wet$worm_gm3_apor, mu=5.62, sigma.x=2.10)

z.test(relict_data_doc_dry$worm_gm3_apor, mu=5.62, sigma.x=2.10)

########################Diplo

z.test(relict_data_doc$worm_gm3_diplo, mu=0.134, sigma.x=0.22)

z.test(relict_data_doc_wet$worm_gm3_diplo, mu=0.134, sigma.x=0.22)

z.test(relict_data_doc_dry$worm_gm3_diplo, mu=0.134, sigma.x=0.22)

########################Scara

z.test(relict_data_doc$arth_gm3_scara, mu=3.04, sigma.x=2.60)

z.test(relict_data_doc_wet$arth_gm3_scara, mu=3.04, sigma.x=2.60)

z.test(relict_data_doc_dry$arth_gm3_scara, mu=3.04, sigma.x=2.60)

########################Tipu

z.test(relict_data_doc$arth_gm3_tipu, mu=0.122, sigma.x=0.065)

z.test(relict_data_doc_wet$arth_gm3_tipu, mu=0.122, sigma.x=0.065)

z.test(relict_data_doc_dry$arth_gm3_tipu, mu=0.122, sigma.x=0.065)


##########################Arma

z.test(relict_data_doc$arth_gm3_arma, mu=1.32, sigma.x=0.28)

z.test(relict_data_doc_wet$arth_gm3_arma, mu=1.32, sigma.x=0.28)

z.test(relict_data_doc_dry$arth_gm3_arma, mu=1.32, sigma.x=0.28)