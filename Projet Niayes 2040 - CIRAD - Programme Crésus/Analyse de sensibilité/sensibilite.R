

# FONCTIONS 
source(file.path(getwd(),'functions_visualisation.R'))

#### importer les données
path_table = 'C:/Users/kouno/Desktop/Programme_Crésus/Calibration Model Hydro/output/tableau_rsme.csv'
rmse<-rcsv_sep(path_table)[-1]

# Mise en forme de la table
#selection des resolutions à considérer
rmse[3:7]<-data.frame(lapply(rmse[3:7],as.numeric))
set<-subset(rmse,resolution==300|resolution==500|
              resolution==700|resolution==900)
#selection des variables à considérer
table<-set[,c("consourb","moyirrigculture","varstock","resolution","rmse_agg_dgpz")]
morris(table)