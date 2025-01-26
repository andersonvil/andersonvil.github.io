# _________________________________________________________
#
#                  TUTORIEL ECONOMETRIE
# 
#             LES MOINDRES CARRES ORDINAIRES
#
# _________________________________________________________

# Ce tutoriel présente les bases pour effectuer des régressions de modèles linéaires
# simples sur R.
# L'exercice permet d'étudier quels sont les déterminants du nombre d'années 
# d'éducation d'une personne en fonction de variables disponibles dans un jeu
# de données.

## ==== PARAMETRES INITIAUX ====

# Pour effacer la mémoire
rm(list = ls())
gc()

# R repose sur des packages et bibliothèques (libraries) qui contiennent le code
# de différentes fonctions

# Installer les packages dont nous avons besoin pour travailler sur les données
# install.packages("wooldridge") # pour télécharger données de Wooldridge
# install.packages("dplyr") # pour manipuler les données

# Bibliothèques
library(dplyr) #pour faire des manipulations de données
library(wooldridge) # pour télécharger les données de Wooldridge


## ==== LES DONNEES ====

# Importer les données
data(htv) #pour détails sur les variables : https://rdrr.io/cran/wooldridge/man/htv.html
# "htv" est un object enregistré dans l'environnement des données

# Les dimensions de la base de données
dim(htv)
# 1230 observations et 23 variables

# Pour visualiser la base de données "htv"
View(htv) # ouvre une fenêtre sur l'ensemble des données
head(htv) # affiche les premières lignes de la base dans la console
names(htv) # affiche le noms des variables de la base dans la console

## ==== STATISTIQUES DESCRIPTIVES ====

# statistiques des varaibles qui nous intéressent (min, 1Q, médiane, moyenne, 3Q, max)
summary(htv[,c("educ","motheduc","fatheduc","abil","sibs","urban")])

# On peut référencer les années d'éducation en fonction des diplômes auxquelles
# elles correspondent.

# 6 années d'éducation correspond à la 6ème
# 12 années d'éducation correspond au bac
# 15 années = licence
# 17 = master
# 20 = doctorat

# On crée la variable dipl pour diplôme le plus élevé obtenu par l'individu observé
htv$dipl <- ifelse(htv$educ==20, "doctorat", NA) # Si educ == 20, dipl = "doctorat", sinon valeur manquante
htv$dipl <- ifelse(htv$educ<20&htv$educ>=17, "master", htv$dipl) # Si 17 <= educ < 20, dipl = "master", sinon valeur déjé enregistrée
htv$dipl <- ifelse(htv$educ<17&htv$educ>=15, "licence", htv$dipl)
htv$dipl <- ifelse(htv$educ<15&htv$educ>=12, "bac", htv$dipl)
htv$dipl <- ifelse(htv$educ<12&htv$educ>=9, "brevet", htv$dipl)
htv$dipl <- ifelse(htv$educ<9, "nondef", htv$dipl)

# on rapporte les statistiques pour chaque groupe de diplôme
diplome <- htv %>% group_by(dipl) %>% count()  # compte le nombre d'observations pour chaque type de diplôme
diplome$prop <- diplome$n/nrow(htv) # on l'exprime en proportion du nombre total d'individus
diplome # on affiche le tableau dans la console

# Résultats :
# 55% de la population a le bac comme diplôme le plus élevé
# 22% ont une licence comme diplôme le plus élevé
# 7% ont un master comme diplôme le plus élevé
# 1% ont un doctorat comme diplôme le plus élevé

# Histogramme de la variable abil
hist(htv$abil, col ="lightblue", xlab = "abil", main = "Histogramme des capacités cognitives")

## ==== REGRESSIONS ====

# On veut expliquer le niveau d'éducation des individus 
# et évaluer l'effet de différentes variables

# I) Modèle de régression educ = b0 + b1 motheduc + b2 fatheduc + u
# Estimer l'effet de l'éducation des parents sur l'éducation d'un individu
regression1 <- lm(educ ~ motheduc + fatheduc, data = htv) # lm = modèle linéaire
summary(regression1)

# Le résumé de la régression est organisé en 4 parties:
# 1) Call : rappelle la régression effectuée
# 2) Residuals : présente des statistiques sur les résidus
# 3) Coefficients : présente les coefficients estimés, ainsi que les écart-types, 
# les statistiques de Student (qui teste l'hypothèse nulle selon laquelle 
# le coefficient est égal à zéro (H0: beta=0), les p-valeur et les niveaux de
# significativité des estimateurs
# 4) Statistiques complémentaires : écart-types estimés des résidus, le R carré
# (ajusté et non-ajusté du nombre de variables), et les résultat du test de 
# Fisher selon lequel aucun des coefficient n'est statistiquement différent de zéro.

# Le modèle OLS est approprié si les termes d'erreurs sont normalement distribués
# On dessine l'histogram des termes d'erreurs u
hist(regression1$residuals,col='lightblue',main='Densité des résidus', xlab='résidus estimés')

# Modèle estimé : educ = 6.96 + 0.30 motheduc + 0.19 fatheduc
# Le R carré est de 0.25 : 25% de la variation de l'éducation d'un individu est expliqué par l'éducation de ses parents.
# Une année supplémentaire d'éducation chez la mère a pour effet d'augmenter de 0.3 années d'éducation chez un individu 
# (environ 4 mois).


# II) Modèle de régression educ = b0 + b1 motheduc + b2 fatheduc + b3 abil + u
regression2 <- lm(educ ~ motheduc + fatheduc + abil, data = htv)
summary(regression2)

# Modèle estimé : educ = 8.45 + 0.19 motheduc + 0.11 fatheduc + 0.50 abil
# Le R carré est de 0.43 : le pouvoir de prédiction du modèle a fortement augmenté. 
# La capacité cognitive explique une partie importante du niveau d'éducation.
# On remarque aussi que l'effet estimé de l'éducation des parents a diminué. 
# Les estimateurs du premier modèle incluaient un biais positif, sûrement dû à une corrélation positive entre éducation
# des parents et capacités cognitives. Plus des parents sont éduqués, plus ils vont stimuler leurs enfants intellectuellement,
# meilleures seront les capacités cognitives de l'enfant, plus il aura la facilité et l'envie de faire de longues études.


# III) Modèle de régression educ = b0 + b1 motheduc + b2 fatheduc + b3 abil + b4 urban + b5 sibs + u
regression3 <- lm(educ ~ motheduc + fatheduc + abil + sibs + urban, data = htv)
summary(regression3)

# Modèle estimé : educ = 8.44 + 0.17 motheduc + 0.11 fatheduc + 0.49 abil + 0.28 urban - 0.11 sibs
# Le R carré est de 0.44 : le pouvoir de prédiction du modèle n'a pas vraiment augmenté. 
# On remarque aussi que l'effet estimé de l'éducation de la mère a diminué,
# sûrement dû au fait que la variable est corrélée avec le nombre de frères et
# soeur qu'a l'enfant (nombre d'enfants -1 de la mère).
# L'estimateur du modèle précédent incluaient un biais positif, dû à une corrélation négative entre éducation
# de la mère et son nombre d'enfants et une corrélation négative entre éducation d'un enfant
# et le nombre de frères et soeurs qu'il a.
# En effet, on trouve qu'avec un frère ou une soeur supplémentaire dans la famille,
# un enfant aura un mois d'études en moins (-0.11 années d'éducation).


# IV) Modèle de régression motheduc = a0 + a1 abil + a2 sibs + a3 urban + e
regression4 <- lm(motheduc ~ abil + sibs + urban, data = htv)
summary(regression4)


## ==== PREDICTIONS ====

## Quelle est le nombre d'années d'éducation moyen pour un individu moyen habitant en ville ?

# Caractéristiques
moth1 <- mean(htv$motheduc)
fath1 <- mean(htv$fatheduc)
abil1 <- mean(htv$abil)
sibs1 <- mean(htv$sibs)
urban1 <- 1

# résultat
indiv1 <- regression3[["coefficients"]][["(Intercept)"]] + regression3[["coefficients"]][["motheduc"]]*moth1  + regression3[["coefficients"]][["fatheduc"]]*fath1 + regression3[["coefficients"]][["abil"]]*abil1 + regression3[["coefficients"]][["sibs"]]*sibs1 + regression3[["coefficients"]][["urban"]]*urban1
indiv1
# 13 années d'éducation (correspond au diplôme du bac comme diplôme le plus élevé)


# On compare l'éducation prédite de deux individus en fonction de leurs caractéristiques.
# On peut changer les valeurs enregistrées pour faire différents tests. 

# Individu 1
moth1 <- 20
fath1 <- 20
abil1 <- 1.8
sibs1 <- 1
urban1 <- 1

# Individu 2
moth2 <- 20
fath2 <- 20
abil2 <- 6
sibs2 <- 1
urban2 <- 1

# Résultats
indiv1 <- regression3[["coefficients"]][["(Intercept)"]] + regression3[["coefficients"]][["motheduc"]]*moth1  + regression3[["coefficients"]][["fatheduc"]]*fath1 + regression3[["coefficients"]][["abil"]]*abil1 + regression3[["coefficients"]][["sibs"]]*sibs1 + regression3[["coefficients"]][["urban"]]*urban1
indiv2 <- regression3[["coefficients"]][["(Intercept)"]] + regression3[["coefficients"]][["motheduc"]]*moth2  + regression3[["coefficients"]][["fatheduc"]]*fath2 + regression3[["coefficients"]][["abil"]]*abil2 + regression3[["coefficients"]][["sibs"]]*sibs2 + regression3[["coefficients"]][["urban"]]*urban2

# Pour afficher les résultats dans la console
indiv1
indiv2

# Pour calculer la différence d'années d'études entre les deux individus
diff <- indiv1 - indiv2
diff
# ou
diff2 <- regression3[["coefficients"]][["motheduc"]]*(moth1-moth2) + regression3[["coefficients"]][["fatheduc"]]*(fath1-fath2) + regression3[["coefficients"]][["abil"]]*(abil1-abil2) + regression3[["coefficients"]][["sibs"]]*(sibs1-sibs2) + regression3[["coefficients"]][["urban"]]*(urban1-urban2)
diff2

# L'individu 1 aurait en moyenne 2 années d'études de moins que l'individu 2.

# On vérifie que cela donne la même chose
diff==diff2 
round(diff,2)==round(diff2,2) # vrai en arrondissant les valeurs au centième près (deuxième décimale)