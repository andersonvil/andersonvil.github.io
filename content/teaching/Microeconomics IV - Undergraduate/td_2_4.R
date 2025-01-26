# _________________________________________________________
#
#                ECONOMETRIE : TD 2 et 4   
#
#                     17/10/2023
# _________________________________________________________

rm(list = ls())
gc()

## ======= LIBRARIES =======


## ======= DATA =======

install.packages("wooldridge")
library(wooldridge) # pour télécharger les données de Wooldridge

## ======= TD2 EX3 =======

## Importer les données
data(gpa2) 

## Visualiser les dix premières lignes des données dans la console
head(gpa2)

## Définition des variables

# sat : résultat à un test standardisé SAT avant l'entrée à l'université
# tothrs : temps d'étude (en heures)
# colgpa : résultat moyen aux examens (sur 4 points)
# athlete : variable binaire - égal à 1 si la personne est athlète de haut niveau, 0 sinon
# verbmath : ratio de la note de expression-compréhension verbale / note de maths du test SAT
# hsize : taille de la promotion (en centaine)
# hsrank : classement
# hsperc : percentile de classement de l'étudiant en classe de terminale
# female : variable binaire - égal à 1 si la personne est une femme, 0 sinon
# white : variable binaire - égal à 1 si la personne a la peau blanche, 0 sinon
# black : variable binaire - égal à 1 si la personne a la peau noire, 0 sinon
# hsizesq : taille de la classe au carré

## Statistiques descriptives des variables

# Variable dépendante : colgpa
summary(gpa2$colgpa)

# Variables explicatives : hsperc et sat
summary(gpa2$hsperc)
summary(gpa2$sat)

## Régression OLS
gpa_reg1 <- lm(colgpa ~ hsperc + sat, data = gpa2)
summary(gpa_reg1) # pour afficher les résultats de la régression

# Interprétez les coefficients et le R carré

# Quelle est la valeur prédite de colgpa quand perc=20 et sat = 1050
perc <- 20
sat <- 1050
b0_chap <- gpa_reg1[["coefficients"]][["(Intercept)"]]
b1_chap <- gpa_reg1[["coefficients"]][["hsperc"]]
b2_chap <- gpa_reg1[["coefficients"]][["sat"]]

colgpa_chap <- b0_chap + b1_chap*perc + b2_chap*sat
colgpa_chap # pour visualiser la valeur dans la console

# Quelle est la différence prédite de colgpa quand perc_A=perc_B et sat_A = 140 + sat_B
delta_perc <- 0 # la différence dans les perc
delta_sat <- 140 # la différence dans les résultats SAT

delta_colgpa <- b1_chap*delta_perc + b2_chap*delta_sat
delta_colgpa 

# Quelle différence dans la variable sat mène à une différence de 0.5 dans colgpa (avec delta_perc=0)? 
delta_colgpa2 <- 0.5

delta_sat2 <- (delta_colgpa2-b1_chap*delta_perc)/b2_chap
delta_sat2

# Est-ce que la distribution des termes d'erreurs est normale?
residus_chap <- data.frame(gpa_reg1$residuals)
head(residus_chap)
colnames(residus_chap) <- c("residus_chap")

hist(residus_chap$residus_chap, breaks = 20)

# Une plus jolie façon de présenter l'histogramme: avec la fonction ggplot
install.packages("ggplot2")
library(ggplot2)

ggplot(residus_chap, aes(x = residus_chap)) +
  geom_histogram(binwidth = 0.25, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogramme des résidus estimés", x = "Valeurs", y = "Fréquence") +
  theme_minimal()

## ======= TD2 EX4 =======

## Importer les données
data(bwght) 

## Description des variables (année d'étude 1998)

# faminc : revenu de la famille, en milliers de dollars
# cigtax : taxes sur cigarettes dans l'Etat de résidence (USA)
# cigprice: prix de la cigarette
# bwght: poids à la naissance, en ounces
# fatheduc: années d'éducation du père
# motheduc: années d'éducation de la mère
# parity: birth order of child
# male: =1 si homme, 0 sinon
# white: =1 si a la peau blanche, 0 sinon
# cigs: nombre de cigarettes fumées par jour pendant la grossesse
# lbwght: logarithm de bwght
# bwghtlbs: poids de naissance, en pounds
# packs: nombre de paquets de cigarettes fumées par jour pendant la grossesse
# lfaminc: log(faminc)

## Régression OLS

bwght_reg1 <- lm(bwght ~ cigs + faminc, data=bwght)
summary(bwght_reg1) # pour afficher les résultats de la régression

bwght_reg2 <- lm(bwght ~ cigs, data=bwght)
summary(bwght_reg2)

# l'effet du nombre de cigarettes fumées pendant la grossesse sur le poids du 
# bébé à la naissance est plus fort sans la variable du revenu
# Biais de variable omise - biais négatif (car beta1 est + négatif):
# vient du fait que faminc est corrélé à cigs mais se retrouve dans le
# terme d'erreur dans le deuxième modèle.
# le signe du biais = corrélation entre faminc et cigs * corrélation entre faminc et bwght

cor(bwght$faminc,bwght$cigs) # correlation négative
cor(bwght$faminc,bwght$bwght) # correlation positive (on le savait déjà du modèle 1)

## Est-ce qu'un changement d'unité de mesure de bwght modifie le R carré?
# bwght est exprimé en ounces
# 1 ounce = 28.3495231 grammes

# on veut exprimer bwght en grammes
bwght$bwght_g <- bwght$bwght*28.3495231

bwght_reg3 <- lm(bwght_g ~ cigs + faminc, data=bwght)
summary(bwght_reg3) # bwght en grammes
summary(bwght_reg1) # bwght en ounces

## Est-ce qu'un changement d'unité de mesure de faminc modifie le R carré?
# faminc est exprimé en milliers de dollars
# on veut l'exprimer en dollars
bwght$faminc_d <- bwght$faminc*1000

bwght_reg4 <- lm(bwght ~ cigs + faminc_d, data=bwght)
summary(bwght_reg4) # faminc en dollars
summary(bwght_reg1) # faminc en milliers de dollars

## ======= TD4 EX3 =======

## Importer les données
data(htv) 

## Description des variables (année d'étude 1991)

# wage: salaire horaire
# abil: measure de capacité cognitive
# educ: diplôme le plus haut obtenu
# ne: = 1 si habite dans le Nord-Ouest
# nc: =1 if si habite dans le Nord-Centre
# west: =1 si habite dans l'Ouest
# south: =1 si habite dans le Sud
# exper: expérience
# motheduc: diplôme le plus haut obtenu de la mère
# fatheduc: diplôme le plus haut obtenu du père
# sibs: nombre de frères et soeurs
# urban: =1 si habite dans une aire urbaine
# lwage: log(wage)
# expersq: exper^2

## Régression OLS

htv_reg1 <- lm(educ ~ motheduc + fatheduc, data=htv)
summary(htv_reg1) 

# Quelle est la proportion de la variation qui s'explique par la variation de l'éducation des parents ?

# Ajoutez la variable abil - interprétez les résultats
htv_reg2 <- lm(educ ~ motheduc + fatheduc + abil, data=htv)
summary(htv_reg2) 

