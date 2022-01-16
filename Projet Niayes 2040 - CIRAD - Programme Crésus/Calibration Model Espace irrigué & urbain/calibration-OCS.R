


# FONCTIONS PERSONNALISEES R-NIAYES 2040
source(file.path(getwd(),'functions_calibration-OCS.R'))

#### extraire les rasters simulés

get_My_raster()

#### importation des rasters et calcul des indicateurs
library(raster)

rmse_urbain<-indicateur(bande=1)
rmse_irrigation<-indicateur(bande=2)

write.csv2(rmse_irrigation,file.path("output","rmse_irrigation2018.csv"))
write.csv2(rmse_urbain,file.path("output","rmse_urbain2018.csv"))

#### meilleurs simulations model espace irrigué & urbain selon les indicateurs
rmse_ir<-rcsv_sep('output/rmse_irrigation2018.csv')[-1]
rmse_ur<-rcsv_sep('output/rmse_urbain2018.csv')[-1]

rmse_model<-rmse_ir[1:9]
rmse_model$rmsle<-rmse_ir$rmsle+rmse_ur$rmsle
rmse_model$rmse<-rmse_ir$rmse+rmse_ur$rmse
rmse_model$nash_sutcliffe<-rmse_ir$nash_sutcliffe+rmse_ur$nash_sutcliffe
write.csv2(rmse_model,file.path("output","rmse_model_urbain&irrigation2018.csv"))



##########***********************************************************###############