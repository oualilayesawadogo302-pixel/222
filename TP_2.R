#=============

m=48
n=75
s1=0
for (i in 1:m) {
  if(m%%i==0){
    s1=s1+i
  } 
}
s2=0
for (j  in 1:n) {
 if(n%%j==0){
   s2=s2+j
 } 
}
if(s1==n & s2==m){
  cat(m,"et",n,"sont des amis")
}



#=============
etudiants <- data.frame( 
  nom = paste("Etudiant", 1:15), 
  sexe = c("H", "F", "H", "F", "H", "F", "H", "F", "F", "H", "F", "H", "H", "F", "F"), 
  age = c(20, 21, 19, 22, 20, 23, 19, 20, 21, 22, 19, 21, 20, NA, 22), 
  math = c(12, 14, 11, 15, 16, 17, NA, 13, 15, 12, 18, 11, 14, 16, 17), 
  physique = c(NA, 15, 9, 14, 16, 17, 8, 12, 14, 11, 18, 9, 13, 15, 16), 
  chimie = c(11, 13, 10, 14, 15, NA, 9, 12, 13, 10, 17, 9, 12, 14, 15) 
)
etu<- etudiants
etu[!complete.cases(etu),]
etu<-na.omit(etu)
mean(etu$math) 
median(etu$physique) 
var(etu$chimie) 
sd(etu$chimie) 
range(etu$age) 
summary(etu) =
table(etu$sexe) 
prop.table(table(etu$sexe)) * 100
tapply(etu$math, etu$sexe, mean) 
tapply(etu$chimie, etu$sexe, mean) 
etu$moyenne <- rowMeans(etu[, c("math", "physique", "chimie")]) 
head(etu)  

summary(etu$moyenne)
cor(etu$math, etu$physique) 
cor(etu [, c("math", "physique", "chimie")])

modele <- lm(math ~ physique, data = etu) 
modele
summary(modele)
etu[,(1:3)]
etu[1:3,1:3]
etu[1:3,c("math","physique")]
etu[etu$age>20,]
etu[etu$age>20 & etu$moyenne>14,]


 plot(etu$physique, etu$math, 
     main = "Relation Math vs Physique", 
     xlab = "Physique", ylab = "Mathématiques", 
     col = "blue", pch = 19) 
abline(modele, col = "red", lwd = 2) 
t.test(math ~ sexe, data = etu, var.equal = TRUE) 
hist(etu$moyenne, 
     main = "Distribution des moyennes", xlab = "Moyenne générale", 
     col = "lightblue", 
     border = "black")

boxplot(moyenne ~ sexe, data = etu, 
        col = c("orange", "lightgreen"), 
        main = "Comparaison des moyennes par sexe", 
        ylab = "Moyenne générale") 
barplot(tapply(etu$math, etu$sexe, mean), 
        col = c("skyblue", "pink"), 
        main = "Moyenne en Math par sexe", ylab = "Note moyenne")  


# Jeu de données initial
etudiants <- data.frame(
  Nom = c("Ali", "Awa", "Jean", "Ali ", "  Awa", "Binta", NA),
  Age = c(22, 23, NA, 22, 23, 24, 25),
  Sexe = c("M", "F", "M", "M", "f", "F", "F"),
  Note = c(14, 15, 12, 14, 15, NA, 16)
)     
dim(etudiants)
etudiants[,c("Age","Sexe")]       
etu<- etudiants
library(dplyr)
etu<- distinct(etu)
etu
etu<- na.omit(etu)
sum(is.na(etudiants))
duplicated(etudiants)
etudiants[!complete.cases(etudiants), ]
duplicated(etudiants)
etudiants$Nom <- trimws(etudiants$Nom)
etudiants$Nom <- toupper(etudiants$Nom)
etudiants <- mutate(etudiants,
                    Categorie = ifelse(Age < 23, "Jeune", "Adulte"))
etudiants<- etudiants[,-c(5)]
readline(prompt = "Entrer votre Nom")
print(paste"salut",+Nom)
cat("Salut Oualilaye")

#=============
library(openintro)
library(tidyverse)
library(caTools)
library(tseries)
ts.ar=arima.sim(list(order=c(1,0,0), ar=0.21), n=200)
ts.plot(ts.ar,tye="i")