#===============
# Création du jeu de données
maisons <- data.frame(
  surface = c(50, 60, 80, 100, 120, 150, 180, 200, 220, 250),
  chambres = c(2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
  distance = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),  # distance du centre-ville (en km)
  prix = c(80, 90, 110, 130, 150, 180, 210, 240, 270, 300)  # prix en millions de F CFA
)

# Affichage des premières lignes
print(maisons)

str(maisons)
summary(maisons)
is.na(maisons)
duplicated(maisons)

cor(maisons)

plot(maisons$prix,maisons$surface,main="representation",xlab="surface",ylab="prix",col="skyblue",pch=19)

mod<- lm(prix~ chambres+surface+distance,data=maisons)
mod
summary(mod)

new<- data.frame(surface=130, chambres=3,distance=7)

predict(mod,new)
prix_pre<- predict(mod)
plot(maisons$prix,prix_pre,xlab = "prix",ylab = "prix_pre",col="pink")

#==============
