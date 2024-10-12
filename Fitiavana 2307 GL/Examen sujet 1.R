# Chargement des packages nécessaires
library(FactoMineR)
library(factoextra)

# Données
data <- data.frame(
  Services = c(-2, -1, 2, 1),
  Qualite = c(3, 1, -1, -3),
  Prix = c(-1, 0, 1, 2)
)

# =====================1 Calcul de la matrice de covariance=====================
cov_matrix <- cov(data)

print(" Reponse 1 : Matrice de covariance :")
print(cov_matrix)

# =====================2 Calcul de la matrice de corrélation=====================
cor_matrix <- cor(data)
print(" Reponse 2 : Matrice de corrélation :")
print(cor_matrix)

# =====================3 Représentation des individus dans le plan principal=====================

# Réaliser une ACP
acp_result <- PCA(data, scale.unit = TRUE)

# Affichage des résultats principaux
print(acp_result)

# Affichage des valeurs propres (pourcentage d'inertie)
print(acp_result$eig)

# Charger le jeu de données mtcars
data(mtcars)

# Appliquer l'ACP aux données mtcars, en centrant et réduisant les données
# scale.unit = TRUE signifie que les données sont standardisées (centrées et réduites)
acp <- PCA(mtcars, scale.unit = TRUE, graph = FALSE)

# Représentation des variables dans le plan factoriel avec un cercle de corrélation
fviz_pca_var(acp, col.var = "cos2",  # Colorer les variables par la qualité de représentation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  # Palette de couleurs
             repel = TRUE) 


# ===================== 4 -Interpréter les résultats=====================

print("Reponse 4 : Interpréter les résultats
    1 - Longueur des vecteurs : Plus le vecteur d'une variable est long, plus cette variable est bien représentée dans le plan factoriel. Cela signifie que cette variable contribue fortement à la construction des composantes principales.

   2-  Angle entre les vecteurs :
         Si l'angle entre deux vecteurs est faible (près de 0°), cela signifie que les deux variables sont positivement corrélées.
 Si l'angle est proche de 90°, les deux variables sont indépendantes.
         Si l'angle est proche de 180°, les variables sont négativement corrélées.
 
   3 - Projection sur les axes : Une variable qui est bien alignée avec l'un des axes (composantes principales) a une forte contribution à cette composante.
   
   4 - Couleur des vecteurs : Dans l'exemple ci-dessus, les couleurs représentent la qualité de la représentation des variables (cos²). Plus une variable est colorée en rouge/orange, plus elle est bien représentée dans ce plan.
        ")

# Fait par :  Fitiavana 2307 GL
