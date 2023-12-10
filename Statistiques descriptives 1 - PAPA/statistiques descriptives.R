
#les bases de données
library(readxl)
esther <- read_excel("data/base_esther_arrondissements.xlsx")
head(esther)

papa <- read_excel("data/base_papa_arrondissements.xlsx")
head(papa)

library(Hmisc)

describe(data)
lubridate::parse_date_time(esther$date_enq,
          orders = c("ymd","dmy","mdy"),tz="GMT")

#DEMOGRAPHIE ET SURFACE

#Esther
surf<-subset(esther,select=c("taille_men","surface_ch","surface_ir","surface_to","surf_culti"))

surfp<-subset(papa,select=c("taille_men","suptotculm"))
surfp$inter<-(cut2(surfp$suptotculm, m=100.25))
surfp$inter<-as.character(surfp$inter)

summary(surf)

#papa
surfp<-subset(papa,select=c("taille_men","suptotculm","NOM"))
surfp$inter<-(cut2(surfp$suptotculm, m=100.25))
surfp$inter<-as.character(surfp$inter)

summary(surfp)
Hmisc::describe(surfp$inter)
Hmisc::describe(surfp$NOM)


#a-) Correlation entre taille de ménage et surface (totale, irriguée, cultivée...)
library(corrplot)
#mcor <- cor(surf)
mcor2 <- cor(surfp[,1:2])

#corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)[1]
corrplot(mcor2, type="upper", order="hclust", tl.col="black", tl.srt=45)[1]

#b-) Nuage de point
by(surfp[,1:2], surfp$NOM, FUN = function(X) pairs(X))
#pairs(surf)
pairs(surfp[,1:2])

summary(lm(surf$taille_men~surf$surface_to))

summary(lm(surfp$taille_men~surfp$suptotculm))

#c-) Lien par groupe
by(surfp, surfp$NOM, FUN = function(X) cor(X$taille_men,
                     X$suptotculm, method = "spearman"))

#d-) Lien entre surfac et capital

papa$exploit_cat<-""
for (i in 1:length(papa$id)){
  if ((papa$syst_exhau[i]=="Manuel") & (papa$suptotculm[i]<5)){papa$exploit_cat[i]<-"inf_5_ha & irrig_man"}
  if ((papa$suptotculm[i]>=2) & (papa$suptotculm[i]<=6) & (papa$syst_exhau[i]!="Manuel") & (papa$revenu_ag_[i]>0)){papa$exploit_cat[i]<-"2_to_6ha & irrig_non_man & marech"}
  if ((papa$suptotculm[i]>=3) & (papa$suptotculm[i]<=10) & (papa$syst_exhau[i]!="Manuel") & (papa$revenu_ele[i]>0)){papa$exploit_cat[i]<-"3_to_10ha & irrig_non_man & elev_verg"}
  if (papa$exploit_cat[i]==""){papa$exploit_cat[i]<-"autres"}
}

cap1<-subset(esther,select=c("taille_men","surface_ch","surface_ir","surface_to","surf_culti","capital_ha","NOM"))

cap2<-subset(papa,select=c("taille_men","suptotculm","longitude","membre_act","capital","cout_intra","NOM","exploit_cat"))
cap2$capital<-as.numeric(cap2$capital)
cap2$inter<-(cut2(cap2$suptotculm, m=100.25))
cap2$inter<-as.character(cap2$inter)
cap2<-na.omit(cap2)
summary(cap2)
Hmisc::describe(cap2["NOM"])
Hmisc::describe(cap2["exploit_cat"])

cap2["inter2"]<-""
for (i in 1:length(cap2$suptotculm)){
  if (cap2$suptotculm[i]<3.25){cap2$inter2[i]<-"moins de 3.25"}
  if ((cap2$suptotculm[i]>=3.25) & (cap2$suptotculm[i]<5)){cap2$inter2[i]<-"[3.25, 5["}
  if ((cap2$suptotculm[i]>=5) & (cap2$suptotculm[i]<10)){cap2$inter2[i]<-"[5, 10["}
  if ((cap2$suptotculm[i]>=10) & (cap2$suptotculm[i]<=15)){cap2$inter2[i]<-"[10, 15]"}
  if (cap2$suptotculm[i]>15){cap2$inter2[i]<-"plus de 15"}
}

library(corrplot)
mcor2 <- cor(cap2[,1:6])
corrplot(mcor2, type="upper", order="hclust", tl.col="black", tl.srt=45)[1]

pairs(cap2[,1:6])

plot(cap2$suptotculm,cap2$court_intra)

by(cap2, cap2$exploit_cat, FUN = function(X) cor(X$capital,
                     X$cout_intra, method = "spearman"))


data.frame(table(cap2$exploit_cat))

##AUTRES ANALYSES

library(ggplot2)
# Graphe basique
p <- ggplot(papa, aes(x=taille_men)) + 
  geom_density()
p
# Ajouter la ligne de la moyenne
p+ geom_vline(aes(xintercept=mean(taille_men)),
              color="blue", linetype="dashed", size=1)


a <- ggplot(papa, aes(x = taille_men))

a + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(taille_men)), 
             linetype = "dashed", size = 0.6)

unique(cut2(papa$taille_men, m=100.25))


surfp<-subset(papa,select=c("taille_men","suptotculm","NOM"))
surfp$inter<-(cut2(surfp$suptotculm, m=100.25))
surfp$inter<-as.character(surfp$inter)
head(surfp)

surfp["inter2"]<-""
for (i in 1:length(surfp$suptotculm)){
  if (surfp$suptotculm[i]<3.25){surfp$inter2[i]<-"moins de 3.25"}
  if ((surfp$suptotculm[i]>=3.25) & (surfp$suptotculm[i]<5)){surfp$inter2[i]<-"[3.25, 5["}
  if ((surfp$suptotculm[i]>=5) & (surfp$suptotculm[i]<10)){surfp$inter2[i]<-"[5, 10["}
  if ((surfp$suptotculm[i]>=10) & (surfp$suptotculm[i]<=15)){surfp$inter2[i]<-"[10, 15]"}
  if (surfp$suptotculm[i]>15){surfp$inter2[i]<-"plus de 15"}
}

Hmisc::describe(surfp$NOM)

table(surfp$NOM)

library(ggplot2)
library(ggridges)
theme_set(theme_ridges())

ggplot(surfp, aes(x = taille_men, y = NOM)) +
  geom_density_ridges(aes(fill = NOM)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800","#A0A0A0A0",
                               "#0000FFA0","#FC4E07","#0000FF"))

ggplot(surfp, aes(x = factor(1), y = taille_men)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = inter, shape = NOM), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#A0A0A0A0",
                                "#0000FFA0","#FC4E07","#0000FF")) + 
  labs(x = NULL)   # Remove x axis label

ggplot(surfp, aes(x = taille_men, y = inter2)) +
  geom_density_ridges(aes(fill = inter2)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800","#A0A0A0A0",
                               "#0000FFA0","#FC4E07","#0000FF"))+
  stat_summary(fun = "quantile", fun.args = list(probs = seq(.1, .9, by = .1)), 
               geom = "hline", aes(yintercept = ..y..))

# Change line color by commune
a <- ggplot(papa, aes(x =taille_men))

a+ geom_density(aes(color = NOM)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#A0A0A0A0", 
                                "#0000FFA0","#FC4E07","#0000FF"))



boxplot(surfp$suptotculm)
abline(h=quantile(surfp$suptotculm,0.1),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.2),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.3),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.4),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.5),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.6),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.7),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.8),col="red",lty=2)
abline(h=quantile(surfp$suptotculm,0.90),col="red",lty=2)


library(plyr)
mu <- ddply(surfp, "NOM", summarise, 
            q1=round(quantile(taille_men,0.1),2), 
            q2=round(quantile(taille_men,0.2),2), 
            q3=round(quantile(taille_men,0.3),2), 
            q4=round(quantile(taille_men,0.4),2), 
            q5=round(quantile(taille_men,0.5),2), 
            q6=round(quantile(taille_men,0.6),2), 
            q7=round(quantile(taille_men,0.7),2), 
            q8=round(quantile(taille_men,0.8),2), 
            q9=round(quantile(taille_men,0.9),2))
mu[,1]<-as.character(mu[,1])
head(mu)


p<-ggplot(surfp, aes(x=taille_men))+
  geom_density()+facet_grid(NOM ~ .) +
  geom_vline(aes(xintercept = q1, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q2, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q3, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q4, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q5, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q6, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q7, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q8, color = NOM),
             data = mu, linetype = "dashed") +
  geom_vline(aes(xintercept = q9, color = NOM),
             data = mu, linetype = "dashed")+
  scale_x_discrete(name ="D", limits=c((unlist(mu[1,])),unlist(mu[1,])))
p


mu1<-mu
for (i in 2:dim(mu)[2]){
  
  for (j in 1:dim(mu)[1]){
    mu[6+j,i]<-dim(surfp[which((surfp["NOM"]==mu[j,1]) & (surfp["taille_men"]<mu[j,i])), ])[1]
    mu[6+j,1]<-paste("nb_lower_q","_",mu[j,1])
  }
}


mu<-mu[1:12,]
mu[13,]<-c("q_probs","10%","20%","30%","40%","50%","60%","70%","80%","90%")
mu


















































































































































