

# SCRIPT CALIBRATION OCCUPATION DU SOL ET IRRIGATION R-NIAYES 2040

#function 1A : création de dossiers voulus lorsqu'ils n'existent pas
create_folder=function(x,path=getwd()){
  
  for (f in x){
    
    folder<-file.path(path, f)
    
    if (!dir.exists(folder)){
      dir.create(folder)
      print(paste(f,"has been created!"))
    } else {
      print(paste(folder,"already exists!"))}
  }
}

#function 1B : rechercher les fichiers voulu dans le répertoire initial
find_myfiles=function(files=NULL,ext=NULL,
                      patt=NULL,dir=NULL,
                      fold=NULL){
  initdir<-getwd()
  
  if (is.null(dir)){dir<-getwd()}
  if (!is.null(fold)){dir<-file.path(dir,fold)}
  
  setwd(dir)
  
  #tous les dossiers du repertoire de travail
  all_work_dir<-list.dirs(path = ".",
                          full.names = TRUE, recursive = TRUE)
  
  #list des chemins des fichiers recherchés
  list_path<-c()
  
  #si le nom du fichier est précisé
  if (!is.null(files)){
    for (file in files){
      
      ##si l'extension du fichier est donnée
      
      if(!is.null(ext)){
        file<-tools::file_path_sans_ext(file)
        
        files_path<-reader::find.file(paste(file,
                                            ext,sep=""),dir=dir,dirs=all_work_dir)
        
        #si un pattern est donné
        
        if (!is.null(patt)){
          
          files_path<-
            Filter(function(x) grepl(paste(ext,
                                           "$",sep=""), x),
                   list.files(files_path,
                              pattern = patt,
                              full.names=T))}
        
        list_path<-c(list_path,files_path)
      }
      
      ##si l'extension n'est pas donnée 
      #et que l'extension n'est pas dans le nom
      
      if(is.null(ext) & (tools::file_ext(file)=="")){
        
        print("Ajoutez l'extension au nom du fichier!!")
        print("Si le problème, 
                persiste alors le fichier n'existe probablement 
                pas dans ce répertoire!!")}
      
      ##si l'extension n'est pas donnée 
      #et que l'extension est dans le nom
      
      if(is.null(ext) & (tools::file_ext(file)!="")){
        
        ext<-tools::file_ext(file)
        files_path<-reader::find.file(file,
                                      dir=dir,dirs=all_work_dir)
        
        #si un pattern est donné
        
        if (!is.null(patt)){
          
          files_path<-list.files(files_path,
                                 pattern = patt)}
      }
    }
    list_path<-c(list_path,files_path)
  }
  
  #si le nom du fichier n'est pas précisé
  
  if(is.null(files)){
    print("pas de nom fichier")
    
    ##si l'extension du fichier n'est pas donnée
    
    if(is.null(ext) & (!is.null(patt))){
      
      for (di in all_work_dir){
        p<-list.files(di,pattern = patt,full.names=T)
        list_path<-c(list_path,p)}
    }
    
    if(is.null(ext) & (is.null(patt))){
      
      for (di in all_work_dir){
        p<-list.files(di)
        list_path<-c(list_path,p,full.names=T)}
    }
    
    ##si l'extension du fichier est donnée
    
    if(!is.null(ext) & (!is.null(patt))){
      
      for (di in all_work_dir){
        p<-list.files(di,pattern = patt,full.names=T)
        p<-Filter(function(x) grepl(paste(ext,
                                          "$",sep=""), x),p)
        list_path<-c(list_path,p)}
    }
    
    if(is.null(ext) & (is.null(patt))){
      
      for (di in all_work_dir){
        p<-list.files(di,full.names=T)
        p<-Filter(function(x) grepl(paste(ext,
                                          "$",sep=""),x),p)
        list_path<-c(list_path,p)}
    }
    list_path}
  setwd(initdir)
  return(list_path)}


#function 1C : renommer un fichier existant

rename_rfile <- function (file=file,updat=file,
                          cdir=NULL,dir=NULL){
  
  if(!is.null(dir)){file<-file.path(dir,file)
  updat<-file.path(dir,updat)}
  
  if (file.exists(file)) {
    file.rename(file,updat)
    
  } else {
    cat("The file does not exist")
  }
}


split_bar<-function(x){
  as.list(strsplit(tools::file_path_sans_ext(x),
                   '/')[[1]])[-1]
}

split_bar8<-function(x){
  as.list(strsplit(tools::file_path_sans_ext(x),
                   '_')[[1]])[-1]
}


#function 1E : copier ou déplacer des données dans les bons dossiers à partir des sorties brutes
tranf_raster=function(full.pth,new_f="data/rasters",
                      methode="copy"){
  
  d<-split_bar(full.pth)
  current_f<-file.path("data","simulations",
                       d[[1]],d[[2]])
  
  file<-paste(d[[3]],".tif",sep="")
  
  if (methode=="copy"){ 
    file.copy(file.path(current_f,file),new_f)
    
  }
  
  if (methode=="move"){
    filesstrings::file.move(file.path(current_f,file),new_f)
  }
  
  updat<-gsub("simulation",
              paste("simulation_",d[[3]],
                    "_",sep=""),d[[1]])
  updat<-paste(updat,".tif",sep="")
  
  
  rename_rfile(dir=new_f,file=file,updat=updat)
}

#function 1F : Récuperer les données Rasters Simulée pour les copier dans le dossier prévu pour

get_My_raster<-function(raster="2014",dir="data",fold="simulations"){
  
  raster_dir<-find_myfiles(patt=raster,ext=".tif",
                           fold=fold,dir=dir)
  
  
  for (ri in raster_dir){
    tranf_raster(ri)}
}

#function 1F: créer les graphique observations contre simulations

graph_station_simul=function(obs,s,o="irrigation"){
  
  obs<-obs
  simul<-s
  
  
  outpath<-
    
    
    i<-0
  plot_list<-c()
  
  library(ggplot2)
  
  plot <-ggplot(X, aes(x=date)) + 
    ylim(0, 7)+ geom_line(aes(y=piezo, color="green")) + 
    geom_line(aes(y=piezostation,color="red")) +
    scale_colour_discrete(name="Piezo (m)",
                          labels= c("ocelet" ,"station")) +
    ggtitle(label = paste("zone ",z),
            subtitle = "Piezo Calculee / Simulee")
  plot_list[[z]] = plot
  
  jpeg(file.path(outpath,paste("plot",z,".tiff", sep="")))
  print(plot)
  dev.off()
}

rcsv_sep=function(file_path){
  L <- readLines(file_path, n = 1)
  if (grepl(";", L)) read.csv2(file_path) else read.csv(file_path)
}


#function : calculer le Nash-Sutcliffe efficiency

rmse=function(x,y){
  sqrt( sum((x-y)^2,na.rm=TRUE )/length(y))
}

denom_nash=function(x){
  m<-mean(x,na.rm=TRUE )
  sqrt( sum((x-m)^2,na.rm=TRUE )/length(x))
}

nash_sutcliffe=function(x,y){   
  a<-rmse(x,y)
  dn<-denom_nash(x)
  res<-1-(a/dn)
  return(res)
}

#optimisation du model
indicateur=function(bande=1,dir=getwd(),
                    list_parametre=NULL,
                    opt=NULL){
  
  if (is.null(opt)){
    opt<-"0.0_6.0_3.0_0.4_0.0_0.0_true_12.0_70.0"
    
  }
  
  if (bande==1){patt="irrig"}
  if (bande==2){patt="urbain"}
  
  parametre<-c("expCoeff","penteInfCoeff","distrouteCoeff",
               "ratioliste","voisUrbCoef","routesCoef",
               "route_vois","popHaIrrig","popHaUrb")
  
  if (is.null(list_parametre)){
    list_parametre=parametre}
  
  print("préparation des données:
    cette opération peut prendre un peu de temps...")
  
  
  #extraction des observations
  initM<-list.files("data/observations",full.names=T)
  bande_observ<-raster(initM,band=bande)
  
  
  #extraction des bandes des rasters
  list.simul<-Filter(function(x) grepl(".tif$", x),
                     list.files("data/rasters",pattern = patt))
  
  indicateurs<-vector(mode="list",length=length(list.simul))
  a<-0
  
  for (simul in list.simul){
    print(simul)
    a<-a+1
    #extraction des bandes des rasters
    bande_simul<-raster(file.path("data/rasters",simul))
    observation<-round(data.frame(rasterToPoints(bande_observ)),1)
    
    
    simulation<-round(data.frame(rasterToPoints(bande_simul)),1)
    table<-na.omit(dplyr::left_join(observation,simulation,
                                    by=c("x"="x","y"="y")))
    
    names(table)<-c("coord1","coord2","observation","simulation")
    
    
    #Les indicateurs
    indicateur<-data.frame(matrix(0,nrow=1,
                                  ncol=length(parametre)))
    
    names(indicateur)<-parametre
    
    index1<-Metrics::rmse(table$observation,table$simulation)
    index2<-Metrics::rmsle(table$observation,table$simulation)
    index3<-nash_sutcliffe(table$observation,table$simulation)
    
    splits<-split_bar8(simul)
    
    if(length(splits)<=1){
      splits<-split_bar8(opt)}
    
    for (i in 2:length(splits)){
      indicateur[1,i-1]<-splits[[i]]
    }
    
    
    indicateur$rmse<-(index1)
    indicateur$rmsle<-(index2)
    indicateur$nash_sutcliffe<-(index3)
    
    indicateurs[[a]]<-indicateur
  }
  indicateurs<-dplyr::bind_rows(indicateurs)
  return(indicateurs)
  
}

normaliz_zero<-function(x){
  x<-na.omit(x)
  min<-min(x)
  max<-max(x)
  
  val<-as.numeric(lapply(x,function(y) (y-min)/(max-min)))
  
  return(val)
}

