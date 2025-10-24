#===========
m=48
n=76
sy=0
for(i in 3:74){
  if(n%%i ==0){
    sy=sy+i
  }
}
sx=0
for(j in 2:46){
  if(m%%j ==0){
    sx=sx+j
  }
}
if( sx==n && sy==m){
  print(paste(" amis"))
}
if(sx!=n && sy!=m)
  print(" ne sont pas amis")





#===========
Nom=c("KABORE","OUEDRAOGO","GANAME","GORO","SAWADOGO","SAWADOGO","SAWADOGO","SAWADOGO","SAWADOGO","KABORE","COULIBALY","BARRY")
Prenom=c("Malachie","Jonathan","Guemilatou","Soumailla","Barkie Soumailla","Bilale","Rahim","Loukmane","Oualilaye","Oumar","Charly", "Yarbanga")
Python=c(14,16,13,14,18,11,10,19,16,17,11,12)
Pratiq_Logi=c(17,14,13,12,11,10,19,16,17,13,11,14)
Serie_tempo=c(19,18,15,13,16,18,10,11,18,11,13,15)
Etudiants<-data.frame(Nom,Prenom,Python,Pratiq_Logi,Serie_tempo)

mean(Etudiants$Python)
mean(Etudiants$Pratiq_Logi)
mean(Etudiants$Serie_tempo)
Etudiants$moyenne<-rowMeans(Etudiants[c("Python","Pratiq_Logi","Serie_tempo")])

#===========

multiplier<-function(a,b){
  if (b == 0 || a==0) {
    return(0)
  }
  else if(b%%2==1){
    return(a+multiplier(a,b-1))
  }
  else{
   return(multiplier(a*2,b/2))
  }
}
#test
multiplier(15,13)



#===========
Coeff<-function(n,p){
  if(n==p || p==0){
    return(1)
  }
  else if(p>n){
    return(0)
  }
  else{
    return(Coeff(n-1,p)+Coeff(n-1,p-1))
  }
}
#test
Coeff(15,3)
for(i in 1:5){
  cat(rep("",n-i))
for(j in 0:i){
  cat(Coeff(i,j))
}
  cat("\n")
  } 

AfficherTriangle <- function(n) {
  cat("Triangle de Pascal jusqu'à la ligne", n, "\n\n")
  
  for (i in 0:n) {  # Pour chaque ligne
    # Ajouter des espaces pour centrer un peu le triangle
    cat(rep(" ", n - i))
    
    for (j in 0:i) {  # Pour chaque élément de la ligne
      cat(Coeff(i, j), " ")
    }
    cat("\n")  # Saut de ligne après chaque rangée
  }
}

# Test : afficher le triangle jusqu'à la ligne 10
AfficherTriangle(10)

#===========
puissance <- function(a, n) {
  if (n == 0) return(1)
  else return(a * puissance(a, n - 1))
}
puissance(2, 5)

#===========
pgcd <- function(a, b) {
  if (b == 0) return(a)
  else return(pgcd(b, a %% b))
}
pgcd(45, 30)


#===========
produits<-c("Laptop","Souris","Clavier","Ecran","Casque")
prix_ht<-c(800,25,45,300,80)
quantiles<-c(10,50,30,15,40)
taux_tva<-0.20
prix_ttc<-prix_ht*(1+taux_tva)
valeur_stock<-prix_ht*quantiles
valeur_stock_ttc<-prix_ttc*quantiles
valeur_taxe<-valeur_stock_ttc-valeur_stock

#===========
etud<-data.frame(id=1:6,
                      prenom=c("alice","bob","charlie","Diana","Eve","Frank"),
                      age=c(20,22,21,23,20,24),
                      mats=c(15,12,18,14,16,11),
                      his=c(13,16,14,15,12,17),
                      admis=c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE))








#===========
library(dplyr)
Employe<-data.frame(NOM=c("Ali","Awa","Ali","Jean","NA"),
                    AGE=c(25,NA,25,22,28),
                    SEXE=c("M","F","M","M","F"))
is.na(Employe)
sum(is.na(Employe))
Employe_sans_na<-na.omit(Employe)
Employe1<-Employe
Employe1$AGE[is.na(Employe1$AGE)]<-mean(Employe1$AGE,na.rm = TRUE)
duplicated(Employe1)
sum(duplicated(Employe1))
Employe1_sans_doublons<-distinct(Employe1) #unique aussi marche
Employe1_sans_doublons$SEXE<-tolower(Employe1_sans_doublons$SEXE)

df<-data.frame(id=c("1","2","3"),
               age=c("25","30","28"),
               sexe=c("M","F","F"))
#CONVERSION
df$id <-as.numeric(df$id)
df$age <-as.numeric(df$age)
df$sexe <-as.factor(df$sexe)
str(df)
