

all_packages<-c("dplyr","ggplot2","raster","readxl","Hmisc","corrplot","ggridges",
  "plyr","haven","readxl","VIM","tidyverse","ggpubr","zoo","clusterSim",
  "psych","MASS","kmed","igraph","mclust","quantable","FactoMineR",
  "factoextra","randomForest","data.table","caret","glmnet","magrittr",
  "MASS","FactoMineR","clustertend","cluster","JLutils","Rtsne","ClustOfVa")

for (i in all_packages){
  if (i %in% rownames(installed.packages())){print(i)}
  install.packages(i)}
