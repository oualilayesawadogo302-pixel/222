#Creation du data frame

  ID<-c(1:15,15,31,34,17,32,17,35,36,37,38)
  Region<-c("Nord","Sud","Est","Ouest","Nord","Sud","Est","Ouest","Nord","Sud","Est","Ouest","Nord","Sud","Est","Ouest","Nord","Sud","Est","Ouest","Est","Ouest","Nord","Sud","Est")
  Culture<-c("Blé","Maïs","Orge","Colza","Tournesol","Blé","Maïs","Orge","Colza","Tournesol","Blé","Maïs","Orge","Colza","Tournesol","Blé","Maïs","Orge","Colza","Tournesol","Blé","Maïs","Orge","Colza","Tournesol")
  Surface<-round(c(15.5,22.3,18.7,12.8,25.1,14.2,20.8,17.3,11.5,23.7,16.8,21.5,19.2,13.1,24.8,15.1,22.7,18.1,12.3,25.5,18.1,12.3,14.9,21.2,24.1))
  Production<-round(c(6.2,8.5,7.1,4.8,9.3,5.8,7.9,6.7,4.2,8.8,6.5,8.2,7.3,4.9,9.1,6.1,8.4,7.0,4.5,9.0,7.0,4.5,5.9,8.1,8.9),1)
  Irrigation<-c("OUI","NON","OUI","NON","OUI","NON","OUI","NON","OUI","NON","OUI","NON","OUI","NON","OUI","NON","OUI","NON","OUI","NON","NON","OUI","NON","OUI","NON")
df_agri<-data.frame(ID,Region,Culture,Surface,Production,Irrigation)

def<-df_agri

#Decouvertes des donnees

def<- na.omit(def)
library(dplyr)
def<- unique(def)
str(def)
summary(def)
anyDuplicated(def$ID)
colSums(is.na(def))
def[!complete.cases(def),]

#Netoyage des donnees
  
def<- def[!duplicated(def$ID),]
mode_stat<- function(x){
ux<-unique(x)
ux[which.max(tabulate(match(x,ux)))]
}
def$Surface[is.na(def$Surface)]<- mean(def$Surface, na.rm=TRUE)
def$Production[is.na(def$Production)]<- mean(def$Production, na.rm=TRUE)
def$Irrigation[is.na(def$Irrigation)]<- mode_stat(def$Irrigation)
colSums(is.na(def))
      
        # Statistiques Descriptives
        
summary(def[,c("Region","Culture","Irrigation")],mode_stat)
sd(def$Region)
sd(def$Surface)
sd(def$Irrigation)
sapply(def[,c("Region","Culture","Irrigation")],mode_stat)
    
        # Visualisation
        
hist(def$Surface, main="Distribution des Surfaces", xlab="Surface(ha)")
boxplot(Production~Culture, data=def, main="Production selon la Culture", xlab="Culture", ylab="Production(tonnes)")
barplot(table(def$Region),main="Nombre de parcelle par Regions")
barplot(table(def$Irrigation),main="Irrigation des parcelles")
              
                # Analyse approfondie
                
cor(def$Surface,def$Production, use="complete.obs")
plot(def$Surface,def$Production, main="Surface vs Production", xlab="Surface", ylab="Production")
aggregate(Production~Culture, data=def,FUN=mean)
aggregate(Production~Region+Culture, data=def,FUN=mean)
