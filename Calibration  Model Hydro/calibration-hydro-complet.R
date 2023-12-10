


# FONCTIONS PERSONNALISEES R-NIAYES 2040
source(file.path(getwd(),'functions_calibration-hydro-complet.R'))

#optimisation
list_parametre=c("varperm","varstock","popHaIrrig","popHaUrb")
opti_model(getwd(),list_parametre=list_parametre)


#calcule et representation des indices par station pour la meilleure simulation

simul="simulation_6.0_50.00.0_0.0bilanid_0.0__0.0_6.0_50.0.csv"
simul_par_id(simul)

###### Var perm Optimal
reso="300.0"
list_indice<-list.files("output/indice_par_station",pattern = ".csv$")
st<-rcsv_sep(file.path("output","indice_par_station",list_indice[1]))$zone
for (s in st){
  p<-c()
  for (idc in list_indice){
    d<-rcsv_sep(file.path("output","indice_par_station",idc))
    sr<-substr(tools::file_path_sans_ext(idc),14,nchar(tools::file_path_sans_ext(idc)))
    split_list<-(as.list(strsplit(sr, '_')[[1]]))
    sid <- data.frame(matrix(unlist(split_list),nrow=1,ncol=(length(split_list))))
    names(sid) <- list_parametre
    d1<-d[which(d$zone==s),c("rmsle","zone")]
    p<-rbind(p,cbind(sid,d1))}
  write.csv2(p,file.path("output","Varp",paste(s,".csv",sep="")))}

##### GRAPHIQUE PAR SIMULATION
df<-rcsv_sep('output/tableau_rsme.csv')
list_o<-c()
for (i in 1:dim(df)[1]){
  o<-paste(df[i,4:5],collapse="__")
  o1<-paste(df[i,6:7],collapse="_")
  o<-paste(c(o,o1),collapse="_")
  list_o<-c(list_o,o)}

obs<-na.omit(extract_obs())
for (o in unique(list_o)){
  #o<-gsub("[^_]*_(.*)", "\\1",o)
  #o<-gsub("[^_]*_(.*)", "\\1",o)
  s<-(Filter(function(x) grepl(".csv", x),list.files('data/simulations',pattern=o)))
  graph_station_simul(obs,s,o)}     
dir_raster="data/rasters"
ListIDcell <-Filter(function(x) grepl(".csv$", x),list.files('data/rasters',pattern = "idcell"))
Filter(function(x) grepl(".csv$", x),list.files('data/rasters',pattern = "idcell"))
read.csv(file.path(dir_raster, Filter(function(x) grepl(reso,x), ListIDcell)[1]),
         header = TRUE, sep = ";", dec = ",")
#graphique par station
df<-rcsv_sep('output/tableau_rsme.csv')
list_o<-c()
for (i in 1:dim(df)[1]){
  o<-paste(df[i,4:7],collapse="_")
  list_o<-c(list_o,o)}