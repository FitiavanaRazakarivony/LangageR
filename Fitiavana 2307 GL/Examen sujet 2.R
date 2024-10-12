# Créer la matrice donnée
T <- matrix(c(2, 2, 3, 3, 1, 2, 1, 0, 3, 2, 1, 4, 1, 3, 3), nrow = 5, byrow = TRUE)

# Afficher la matrice
print("Matrice T :")
print(T)

# ====================== Question 1 : Calcul de l'individu moyen, des écarts-types et des variables centrées-réduites ======================
mean_T <- colMeans(T) # Moyennes
sd_T <- apply(T, 2, sd) # Écarts-types

# Afficher les moyennes et écarts-types
print(paste("Moyennes :", toString(mean_T)))
print(paste("Écart-types :", toString(sd_T)))

# Centrer et réduire la matrice T
T_scaled <- scale(T)

# Afficher la matrice centrée-réduite
print("Matrice des variables centrées-réduites :")
print(T_scaled)

# ====================== Question 2 : Calcul de la matrice des corrélations ======================
cor_matrix <- cor(T_scaled) # Matrice de corrélation

# Afficher la matrice de corrélation
print("Matrice de corrélation :")
print(cor_matrix)

# ====================== Question 3 : Calcul des valeurs propres et des vecteurs propres ======================
eig <- eigen(cor_matrix) # Valeurs et vecteurs propres

# Afficher les valeurs et vecteurs propres
print("Valeurs propres :")
print(eig$values)
print("Vecteurs propres :")
print(eig$vectors)

# ====================== Question 4 : Déterminer les premiers vecteurs propres et leur interprétation ======================
# Extraire et afficher les deux premiers vecteurs propres
print("Premier vecteur propre v1 :")
print(eig$vectors[, 1])
print("Deuxième vecteur propre v2 :")
print(eig$vectors[, 2])

# ====================== Question 5 : Représenter les individus et les variables dans le plan factoriel (1,2) ======================
library(FactoMineR)
library(factoextra)

# Effectuer une analyse en composantes principales (ACP)
acp_result <- PCA(T_scaled, scale.unit = TRUE)

# Représenter les variables et les individus
fviz_pca_var(acp_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_ind(acp_result, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# ====================== Question 6 : Représenter l'individu supplémentaire dans le plan ======================
ind_supp <- scale(matrix(c(sqrt(10), sqrt(10), 2 * sqrt(10)), nrow = 1), center = mean_T, scale = sd_T)
T_with_supp <- rbind(T_scaled, ind_supp) # Ajouter l'individu supplémentaire
acp_result <- PCA(T_with_supp, scale.unit = TRUE) # Effectuer l'ACP

# Représenter les individus avec l'individu supplémentaire
fviz_pca_ind(acp_result, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Fait par Fitiavana 2307 GL
