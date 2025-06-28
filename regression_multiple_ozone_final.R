install.packages("dplyr")
install.packages("zoo")
install.packages("VIM")
install.packages("mice")
library(VIM)
library(dplyr)
library(zoo)
library(mice)


path_to_data = "C:\\Users\\lorai\\Desktop\\S8\\projet_pollution_par_lozone\\varaux_MERA_O3.csv"
path_to_data_MERA_O3 = "C:\\Users\\lorai\\Desktop\\S8\\projet_pollution_par_lozone\\MERA_O3.csv"


data_MERA_O3 = read.table(path_to_data_MERA_O3, sep=";", dec=".", header=TRUE, as.is=TRUE)
data_var = read.table(file=path_to_data, header = TRUE, sep = ";")

colnames(data_var)[colnames(data_var) == "date"] = "Date"

data = merge(data_var, data_MERA_O3[, c("Date", "MERA_O3")], by = "Date", all.x = TRUE)
names(data)


#methode1
#cette methode consiste a surpprimé les lignes contenant les NA ( suppression de 41% des données)
#Elle donne les meme resultats que les autres méhodes c'est a dire les même correlations et le a peu pres le meme R_squared
#data_scale =na.omit(data[, -1])
#data_scale = as.data.frame(data_scale)



# Nombre de regresseurs
#p = ncol(data_scale) -1
# Taille des donnees
#n = length(data_scale[,1])

# transformation en variable qualitative
# Fonction pour classifier un angle en degrés avec un décalage de 22.5°

classify_angle <- function(angle_deg) {
  
  if (is.na(angle_deg)) return(NA)  # Très important
  angle_mod <- angle_deg %% 360
  
  if (angle_mod >= 337.5 | angle_mod < 22.5) {
    return("Nord")
  } else if (angle_mod >= 22.5 & angle_mod < 67.5) {
    return("Nord-Est")
  } else if (angle_mod >= 67.5 & angle_mod < 112.5) {
    return("Est")
  } else if (angle_mod >= 112.5 & angle_mod < 157.5) {
    return("Sud-Est")
  } else if (angle_mod >= 157.5 & angle_mod < 202.5) {
    return("Sud")
  } else if (angle_mod >= 202.5 & angle_mod < 247.5) {
    return("Sud-Ouest")
  } else if (angle_mod >= 247.5 & angle_mod < 292.5) {
    return("Ouest")
  } else {
    return("Nord-Ouest")
  }
}




#methode2
# imputation des valeur manquantes
#Parcours des colonnes numériques et remplacement des NA par la moyenne
#Elle donne les meme resultats que les autres méhodes c'est a dire les même correlations et le a peu pres le meme R_squared
#numeric_vars = sapply(data_scale, is.numeric)
#for (var in names(data_scale)[numeric_vars]) {
# mean_val = mean(data_scale[[var]], na.rm = TRUE)
#  data_scale[[var]][is.na(data_scale[[var]])] <- mean_val
#}


#methode3
#Remplacer les NA par la dernière valeur observée dans chaque colonne
#Elle donne les meme resultats que les autres méhodes c'est a dire les même correlations et le a peu pres le meme R_squared
#data_scale <- as.data.frame(lapply(data_scale, function(x) na.locf(x, na.rm = FALSE)))

#methode 4
# methode knn pour predire la valeur de la valeur manquantes
# on remarque cette methode est tres lente car le KNN calcul des distances
#numeric_vars <- sapply(data[, -1], is.numeric)
#data_numeric <- data[, c(FALSE, numeric_vars)]  # exclut Date
#data_scale <- kNN(data_numeric, k = 3, imp_var = FALSE)


#creation des la variable qualitative

data_scale = data[,-1]

# On recrée la variable direction du vent AVANT l'imputation
data_scale$MERA_DV_cat = sapply(data_scale$MERA_DV, classify_angle)

# On enlève MERA_DV
data_scale = subset(data_scale, select = -MERA_DV)

# Transformer en facteur
data_scale$MERA_DV_cat <- factor(data_scale$MERA_DV_cat)
levels(data_scale$MERA_DV_cat)


#methode5 MICE

imputed_data <- mice(data_scale, m = 5, method = 'pmm', seed = 123)

# Récupérer les 5 jeux imputés
list_imputations <- lapply(1:5, function(i) complete(imputed_data, i))

# Séparer les parties numériques (sans la variable catégorielle)
list_num_only <- lapply(list_imputations, function(df) df[, sapply(df, is.numeric)])

# Calculer la moyenne des colonnes numériques
data_numeric_mean <- Reduce("+", list_num_only) / length(list_num_only)

# Récupérer la variable catégorielle depuis l’un des jeux (nous avons pris le premier)
data_numeric_mean$MERA_DV_cat <- list_imputations[[1]]$MERA_DV_cat

# Optionnel : retransformer en facteur (au cas où)
data_numeric_mean$MERA_DV_cat <- factor(data_numeric_mean$MERA_DV_cat)

# Résultat final
data_scale <- data_numeric_mean
na_count_per_column <- colSums(is.na(data_scale))

statbase = NULL
for (j in 1:(ncol(data_scale)-1)){
  statbase =rbind(statbase, summary(data_scale[,j]))
}

rownames(statbase) = names(data_scale[, -ncol(data_scale)])
statbase
# Ajout des ecart-types
ecart.type = apply(data_scale[, -ncol(data_scale)], 2, sd)
statbase = cbind(statbase, ecart.type)
round(statbase, 2)

#matrice de correlations
correlation_matrix <- cor(data_scale[, -ncol(data_scale)], use = "complete.obs")
round(correlation_matrix, 2)


# multicoliearité
X = data_scale[, -((ncol(data_scale)-1):ncol(data_scale))]
X = scale(X)
VIF = diag(solve(cor(X)))
round(VIF, 2)

# analyse de l’impact de MERA_DV_cat sur MERA_O3 avant la régression
boxplot(MERA_O3 ~ MERA_DV_cat, data = data_scale, 
        main = "Distribution de l’O3 selon la direction du vent", 
        xlab = "Direction du Vent", ylab = "O3 (centré-réduit)")


# Boucle pour tracer MERA_O3 en fonction de chaque variable
par(mfrow = c(2, 3))  # 2 lignes, 3 colonnes de graphiques
for (var in vars) {
  plot(data_scale[[var]], data_scale$MERA_O3,
       xlab = var,
       ylab = "MERA_O3",
       main = paste("O3 vs", var),
       pch = 15, col = "blue")
  abline(lm(MERA_O3 ~ data_scale[[var]], data = data_scale), col = "red")
}

# Boxplot pour la variable catégorielle
boxplot(MERA_O3 ~ MERA_DV_cat, data = data_scale,
        main = "MERA_O3 selon la direction du vent",
        xlab = "Direction du vent", ylab = "MERA_O3", col = "lightblue")

# Régression linéaire multiple
data_scale$MERA_DV_cat = relevel(data_scale$MERA_DV_cat, ref = "Nord")
levels(data_scale$MERA_DV_cat)

modele = lm(MERA_O3 ~ MERA_T + MERA_HR + MERA_VV + MERA_PA + MERA_RG + MERA_PL + MERA_DV_cat, data = data_scale)

# Résumé du modèle
summary(modele)

IC = confint(modele)
# Ajout de l’IC pour la variance
s2 = (summary(modele)$sigma)^2
qk = qchisq(c(0.025, 0.975), summary(modele)$df[2])
IC = rbind(IC, sigma2 = c(17*s2/qk[2] , 17*s2/qk[1] ))
round(IC , 4)

residus = residuals(modele)

#graphe residue
plot(residus)

#histogramme des residus
hist(residus, main="Histogramme des résidus", xlab="Résidus", col="lightblue", border="black")


# Test de Kolmogorov-Smirnov
ks.test(residus, "pnorm", mean(residus), sd(residus))

# Q-Q plot des résidus
qqnorm(residus)
qqline(residus, col = "red")

#Test de l’hypothese de non regression
#regarder la derniere ligne du summary de modele
# donc on rejette l'hypo de non regression


#Construction des ´echantillons d’apprentissage et de test.

set.seed(111) # initialisation du generateur
# Extraction des echantillons
test.ratio = 0.25 # part de l’echantillon test
npop = nrow(data_scale) # nombre de lignes dans les donnees
ntest = ceiling(npop*test.ratio) # taille de l’echantillon test
testi = sample(1:npop,ntest) # indices de l’echantillon test
appri = setdiff(1:npop,testi) # indices de l’echant. d’apprentissage
# Construction des ´echantillons avec les variables explicatives .
dataApp = data_scale[appri,] # construction de l’echantillon d’apprentissage
dataTest = data_scale[testi,] # construction de l’echantillon test


#Estimation du modele sur un echantillon d’apprentissage.
#On estime le modele avec les donnees de Eapp.
reslm = lm(MERA_O3 ~ MERA_T + MERA_HR + MERA_VV + MERA_PA + MERA_RG + MERA_PL + MERA_DV_cat, data = dataApp)
summary(reslm)

# Faire des prédictions sur l'échantillon de test
predictions = predict(reslm, newdata = dataTest)

# Afficher les premières prédictions
head(predictions)

# Calcul des erreurs (différence entre prédictions et vraies valeurs)
residuals <- predictions - dataTest$MERA_O3

# Erreur absolue moyenne (MAE)
mae <- mean(abs(residuals))
print(paste("Erreur absolue moyenne (MAE):", mae))

# Erreur quadratique moyenne (RMSE)
rmse <- sqrt(mean(residuals^2))
print(paste("Erreur quadratique moyenne (RMSE):", rmse))


# Utiliser la fonction step() avec direction = "forward"
reslm_forward = step(lm(MERA_O3 ~ 1, data = dataApp), 
                      scope = list(lower = ~1, upper = ~MERA_T + MERA_HR + MERA_VV + MERA_PA + MERA_RG + MERA_PL + MERA_DV_cat), 
                      direction = "forward")
summary(reslm_forward)


# Utiliser la fonction step() avec direction = "backward"
reslm_backward = step(reslm, direction = "backward")
summary(reslm_backward)



