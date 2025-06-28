# path_to_data = "~/Téléchargements/MERA_O3.csv"
# path_to_data = "~/Téléchargements/MERA_O3.csv"
path_to_data = "C:\\Users\\lorai\\Desktop\\S8\\projet_pollution_par_lozone\\MERA_O3.csv"

data = read.table(path_to_data, sep=";", dec=".", header=TRUE, as.is=TRUE)

max_concentration_journaliere = function(data, seuil = 0.75, nb_heures= 24) {
  
  # on converti la 1ere colone de sorte à ne garder que la date
  data$Date = as.Date(substr(data[, 1], 1, 10), format = "%Y-%m-%d")
  
  # Creation d'un vecteur vide pour y stocker les valeurs de concentrations max
  max_journalier = numeric() 
  
  # on garde les jours uniques
  date_unique = unique(data$Date)
  
  # Pour chaque jour, on calcule le max de concentration si au moins 75% des données sont valides
  for (day in date_unique) {
    # on Filtre les données correspondant à ce jour
    valeur_journaliere = data[data$Date == day, 2]
    # on compte le nombre de valeurs valides(non NA)
    nb_valeur_valide = sum(!is.na(valeur_journaliere))
    
    
    if (nb_valeur_valide >= seuil * nb_heures) { 
      #on rajoute le max jounalier
      max_journalier = c(max_journalier, max(valeur_journaliere, na.rm = TRUE))  
      
    } else {
      #on rajoute NA
      max_journalier = c(max_journalier, NA)  
    }
  }
  
  # On cree une dataframe avec les info qu'on a obtenu
  resultat = data.frame(Date = date_unique, Concentration_max = max_journalier)
  return(resultat)
}

new_data = max_concentration_journaliere(data)
new_data_sans_NA = na.omit(new_data)

par(mar = c(4, 4, 2, 1))
# Série chronologique
plot(new_data_sans_NA$Date, new_data_sans_NA$Concentration_max, 
     type = "l",
     main = "Série chronologique des maximums journaliers d'ozone",
     xlab = "Date",
     ylab = "Concentration O3 (µg/m³)",
     col = "blue")






#Statistiques descriptives

  cat("Statistiques descriptives des maximums journaliers d'ozone :\n")
  print(summary(new_data_sans_NA$Concentration_max))

  #Écart-type
  sd_max_daily <- sd(new_data_sans_NA$Concentration_max, na.rm = TRUE)
  cat("\nÉcart-type des maximums journaliers:", round(sd_max_daily, 2), "µg/m³\n")
  
  
  
  

# Visualisations

  # Histogramme
  hist(new_data_sans_NA$Concentration_max, 
       main = "Distribution des maximums journaliers d'ozone",
       xlab = "Concentration O3 (µg/m³)",
       ylab = "Fréquence",
       col = "skyblue",
       border = "black")
  
  # Boîte à moustaches
  boxplot(new_data_sans_NA$Concentration_max,
          main = "Boîte à moustaches des maximums journaliers d'ozone",
          ylab = "Concentration O3 (µg/m³)",
          col = "lightgreen")
  
  # Boîte à moustaches annuelle
  new_data_sans_NA$Date <- as.Date(new_data_sans_NA$Date, format="%Y/%m/%d")
  new_data_sans_NA$Annee = format(new_data_sans_NA$Date, "%Y")
  boxplot(new_data_sans_NA$Concentration_max ~ new_data_sans_NA$Annee, new_data_sans_NA = new_data_sans_NA, col = "lightblue",
          main = "Distribution des concentrations maximales par année",
          xlab = "Année", ylab = "Concentration maximale journalière")
  

  # Boîte à moustaches par heure de la journée
  data$Date = as.POSIXct(data$Date, format="%Y-%m-%d %H:%M", tz="UTC")
  data$Heure = format(data$Date, "%H")
  data = na.omit(data)
  head(data) # Regarde les premières valeurs
  boxplot(MERA_O3 ~ Heure, data = data, col = "lightblue",
          main = "Distribution des concentrations maximales par heure",
          xlab = "Heure de la journée", ylab = "Concentration maximale")
  
  # verification des fortes emissions dans la nuit 21H,22H, 23h
  subset(data, MERA_O3 > 180)$Date
  
  
  
  
  
  

# Installer et charger le package moments
if (!requireNamespace("moments", quietly = TRUE)) {
  install.packages("moments")
}
library(moments)

# Calculer le skewness
skew <- skewness(new_data_sans_NA$Concentration_max)
cat("Skewness:", skew, "\n")

# Calculer le kurtosis
kurt <- kurtosis(new_data_sans_NA$Concentration_max)
cat("Kurtosis:", kurt, "\n")




# Tendance
  # Calculer la moyenne mobile sur 30 jours pour voir la tendance
  install.packages("zoo")
  
  library(zoo)
  new_data_sans_NA$tendance <- rollmean(new_data_sans_NA$Concentration_max, k = 30, fill = NA)
  
  # Visualiser la tendance
  plot(new_data_sans_NA$Date, new_data_sans_NA$Concentration_max, type = "l", 
       main = "Tendance des maximums journaliers d'ozone",
       xlab = "Date", ylab = "Concentration O3 (µg/m³)")
  lines(new_data_sans_NA$Date, new_data_sans_NA$tendance, col = "red", lwd = 2)
  
  
  
  
  
  

# Saisonnalité

  # Extraire le mois de chaque date
  new_data_sans_NA$mois <- format(new_data_sans_NA$Date, "%m")
  
  # Calculer la moyenne par mois
  saisonnalite <- aggregate(Concentration_max ~ mois, data = new_data_sans_NA, FUN = max)
  
  # Visualiser la saisonnalité
  boxplot(Concentration_max ~ mois, data = new_data_sans_NA, col = "lightblue",
          main = "Distribution des maximums mensuels d'ozone",
          xlab = "Mois", ylab = "Concentration maximale O3 (µg/m³)")
  
# Épisodes de forte pollution

seuil_dinfo_recommandation = 180
episodes_pollution = new_data_sans_NA[new_data_sans_NA$Concentration_max > seuil_dinfo_recommandation, ]
plot(new_data_sans_NA$Date, new_data_sans_NA$Concentration_max, type = "l",
     main = "Épisodes de forte pollution à l'ozone",
     xlab = "Date", ylab = "Concentration O3 (µg/m³)")
points(episodes_pollution$Date, episodes_pollution$Concentration_max, col = "red", pch = 19)
abline(h = seuil_dinfo_recommandation, col = "blue", lty = 2)

# Afficher les dates des épisodes de pollution
print(episodes_pollution$Date)









moyenne_mobile = function(data_par_heure, period=8) {
  
  # Construction des moyennes mobiles
  taille_vecteur_moyenne <- nrow(data_par_heure) - period + 1
  vecteur_moyenne <- numeric(taille_vecteur_moyenne)
  minimum_valid_data = 0.75 * period
  
  for (i in 1:taille_vecteur_moyenne){
    data_period = data_par_heure$MERA_O3[i: (i+period -1)]
    valid_data_period = data_period[!is.na(data_period)]
    if (length(valid_data_period) >= minimum_valid_data) {
      vecteur_moyenne[i] = mean(valid_data_period)  
    } else {
      vecteur_moyenne[i] = NA
    }
  }
  
  # retrait des heures dont la moyenne est effectuee avec la concentration du lendemain
  selection = c()
  indice_heures_a_retirer = (24 - period + 1): 24
  for (i in 1:taille_vecteur_moyenne){
    if (!((i - 1) %% 24 %in% indice_heures_a_retirer)) {
      selection <- c(selection, i)
    }
  }
  
  resultat = data.frame(
    Date=data_par_heure$Date[1:taille_vecteur_moyenne][selection],
    Concentration=vecteur_moyenne[selection]
  )
  
  return(resultat)
}

concentration_moyennee = moyenne_mobile(data)
max_concentration_moyennee = max_concentration_journaliere(concentration_moyennee, 0.75, 17)

plot(max_concentration_moyennee$Date, max_concentration_moyennee$Concentration_max, 
     type = "l",
     main = "Série chronologique des maximums de la moyenne sur 8H",
     xlab = "Date",
     ylab = "Concentration O3 (µg/m³)",
     col = "blue")
abline(h = 120, col = "red", lwd = 2, lty = 2)
