# _________________________________________________________
#
#                  TUTORIEL ECONOMETRIE
# 
#             LES MOINDRES CARRES ORDINAIRES
#
# _________________________________________________________

# Ce tutoriel pr�sente les bases pour effectuer des r�gressions de mod�les lin�aires
# simples sur R.
# L'exercice permet d'�tudier quels sont les d�terminants du nombre d'ann�es 
# d'�ducation d'une personne en fonction de variables disponibles dans un jeu
# de donn�es.

## ==== PARAMETRES INITIAUX ====

# Pour effacer la m�moire
rm(list = ls())
gc()

# R repose sur des packages et biblioth�ques (libraries) qui contiennent le code
# de diff�rentes fonctions

# Installer les packages dont nous avons besoin pour travailler sur les donn�es
# install.packages("wooldridge") # pour t�l�charger donn�es de Wooldridge
# install.packages("dplyr") # pour manipuler les donn�es

# Biblioth�ques
library(dplyr) #pour faire des manipulations de donn�es
library(wooldridge) # pour t�l�charger les donn�es de Wooldridge


## ==== LES DONNEES ====

# Importer les donn�es
data(htv) #pour d�tails sur les variables : https://rdrr.io/cran/wooldridge/man/htv.html
# "htv" est un object enregistr� dans l'environnement des donn�es

# Les dimensions de la base de donn�es
dim(htv)
# 1230 observations et 23 variables

# Pour visualiser la base de donn�es "htv"
View(htv) # ouvre une fen�tre sur l'ensemble des donn�es
head(htv) # affiche les premi�res lignes de la base dans la console
names(htv) # affiche le noms des variables de la base dans la console

## ==== STATISTIQUES DESCRIPTIVES ====

# statistiques des varaibles qui nous int�ressent (min, 1Q, m�diane, moyenne, 3Q, max)
summary(htv[,c("educ","motheduc","fatheduc","abil","sibs","urban")])

# On peut r�f�rencer les ann�es d'�ducation en fonction des dipl�mes auxquelles
# elles correspondent.

# 6 ann�es d'�ducation correspond � la 6�me
# 12 ann�es d'�ducation correspond au bac
# 15 ann�es = licence
# 17 = master
# 20 = doctorat

# On cr�e la variable dipl pour dipl�me le plus �lev� obtenu par l'individu observ�
htv$dipl <- ifelse(htv$educ==20, "doctorat", NA) # Si educ == 20, dipl = "doctorat", sinon valeur manquante
htv$dipl <- ifelse(htv$educ<20&htv$educ>=17, "master", htv$dipl) # Si 17 <= educ < 20, dipl = "master", sinon valeur d�j� enregistr�e
htv$dipl <- ifelse(htv$educ<17&htv$educ>=15, "licence", htv$dipl)
htv$dipl <- ifelse(htv$educ<15&htv$educ>=12, "bac", htv$dipl)
htv$dipl <- ifelse(htv$educ<12&htv$educ>=9, "brevet", htv$dipl)
htv$dipl <- ifelse(htv$educ<9, "nondef", htv$dipl)

# on rapporte les statistiques pour chaque groupe de dipl�me
diplome <- htv %>% group_by(dipl) %>% count()  # compte le nombre d'observations pour chaque type de dipl�me
diplome$prop <- diplome$n/nrow(htv) # on l'exprime en proportion du nombre total d'individus
diplome # on affiche le tableau dans la console

# R�sultats :
# 55% de la population a le bac comme dipl�me le plus �lev�
# 22% ont une licence comme dipl�me le plus �lev�
# 7% ont un master comme dipl�me le plus �lev�
# 1% ont un doctorat comme dipl�me le plus �lev�

# Histogramme de la variable abil
hist(htv$abil, col ="lightblue", xlab = "abil", main = "Histogramme des capacit�s cognitives")

## ==== REGRESSIONS ====

# On veut expliquer le niveau d'�ducation des individus 
# et �valuer l'effet de diff�rentes variables

# I) Mod�le de r�gression educ = b0 + b1 motheduc + b2 fatheduc + u
# Estimer l'effet de l'�ducation des parents sur l'�ducation d'un individu
regression1 <- lm(educ ~ motheduc + fatheduc, data = htv) # lm = mod�le lin�aire
summary(regression1)

# Le r�sum� de la r�gression est organis� en 4 parties:
# 1) Call : rappelle la r�gression effectu�e
# 2) Residuals : pr�sente des statistiques sur les r�sidus
# 3) Coefficients : pr�sente les coefficients estim�s, ainsi que les �cart-types, 
# les statistiques de Student (qui teste l'hypoth�se nulle selon laquelle 
# le coefficient est �gal � z�ro (H0: beta=0), les p-valeur et les niveaux de
# significativit� des estimateurs
# 4) Statistiques compl�mentaires : �cart-types estim�s des r�sidus, le R carr�
# (ajust� et non-ajust� du nombre de variables), et les r�sultat du test de 
# Fisher selon lequel aucun des coefficient n'est statistiquement diff�rent de z�ro.

# Le mod�le OLS est appropri� si les termes d'erreurs sont normalement distribu�s
# On dessine l'histogram des termes d'erreurs u
hist(regression1$residuals,col='lightblue',main='Densit� des r�sidus', xlab='r�sidus estim�s')

# Mod�le estim� : educ = 6.96 + 0.30 motheduc + 0.19 fatheduc
# Le R carr� est de 0.25 : 25% de la variation de l'�ducation d'un individu est expliqu� par l'�ducation de ses parents.
# Une ann�e suppl�mentaire d'�ducation chez la m�re a pour effet d'augmenter de 0.3 ann�es d'�ducation chez un individu 
# (environ 4 mois).


# II) Mod�le de r�gression educ = b0 + b1 motheduc + b2 fatheduc + b3 abil + u
regression2 <- lm(educ ~ motheduc + fatheduc + abil, data = htv)
summary(regression2)

# Mod�le estim� : educ = 8.45 + 0.19 motheduc + 0.11 fatheduc + 0.50 abil
# Le R carr� est de 0.43 : le pouvoir de pr�diction du mod�le a fortement augment�. 
# La capacit� cognitive explique une partie importante du niveau d'�ducation.
# On remarque aussi que l'effet estim� de l'�ducation des parents a diminu�. 
# Les estimateurs du premier mod�le incluaient un biais positif, s�rement d� � une corr�lation positive entre �ducation
# des parents et capacit�s cognitives. Plus des parents sont �duqu�s, plus ils vont stimuler leurs enfants intellectuellement,
# meilleures seront les capacit�s cognitives de l'enfant, plus il aura la facilit� et l'envie de faire de longues �tudes.


# III) Mod�le de r�gression educ = b0 + b1 motheduc + b2 fatheduc + b3 abil + b4 urban + b5 sibs + u
regression3 <- lm(educ ~ motheduc + fatheduc + abil + sibs + urban, data = htv)
summary(regression3)

# Mod�le estim� : educ = 8.44 + 0.17 motheduc + 0.11 fatheduc + 0.49 abil + 0.28 urban - 0.11 sibs
# Le R carr� est de 0.44 : le pouvoir de pr�diction du mod�le n'a pas vraiment augment�. 
# On remarque aussi que l'effet estim� de l'�ducation de la m�re a diminu�,
# s�rement d� au fait que la variable est corr�l�e avec le nombre de fr�res et
# soeur qu'a l'enfant (nombre d'enfants -1 de la m�re).
# L'estimateur du mod�le pr�c�dent incluaient un biais positif, d� � une corr�lation n�gative entre �ducation
# de la m�re et son nombre d'enfants et une corr�lation n�gative entre �ducation d'un enfant
# et le nombre de fr�res et soeurs qu'il a.
# En effet, on trouve qu'avec un fr�re ou une soeur suppl�mentaire dans la famille,
# un enfant aura un mois d'�tudes en moins (-0.11 ann�es d'�ducation).


# IV) Mod�le de r�gression motheduc = a0 + a1 abil + a2 sibs + a3 urban + e
regression4 <- lm(motheduc ~ abil + sibs + urban, data = htv)
summary(regression4)


## ==== PREDICTIONS ====

## Quelle est le nombre d'ann�es d'�ducation moyen pour un individu moyen habitant en ville ?

# Caract�ristiques
moth1 <- mean(htv$motheduc)
fath1 <- mean(htv$fatheduc)
abil1 <- mean(htv$abil)
sibs1 <- mean(htv$sibs)
urban1 <- 1

# r�sultat
indiv1 <- regression3[["coefficients"]][["(Intercept)"]] + regression3[["coefficients"]][["motheduc"]]*moth1  + regression3[["coefficients"]][["fatheduc"]]*fath1 + regression3[["coefficients"]][["abil"]]*abil1 + regression3[["coefficients"]][["sibs"]]*sibs1 + regression3[["coefficients"]][["urban"]]*urban1
indiv1
# 13 ann�es d'�ducation (correspond au dipl�me du bac comme dipl�me le plus �lev�)


# On compare l'�ducation pr�dite de deux individus en fonction de leurs caract�ristiques.
# On peut changer les valeurs enregistr�es pour faire diff�rents tests. 

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

# R�sultats
indiv1 <- regression3[["coefficients"]][["(Intercept)"]] + regression3[["coefficients"]][["motheduc"]]*moth1  + regression3[["coefficients"]][["fatheduc"]]*fath1 + regression3[["coefficients"]][["abil"]]*abil1 + regression3[["coefficients"]][["sibs"]]*sibs1 + regression3[["coefficients"]][["urban"]]*urban1
indiv2 <- regression3[["coefficients"]][["(Intercept)"]] + regression3[["coefficients"]][["motheduc"]]*moth2  + regression3[["coefficients"]][["fatheduc"]]*fath2 + regression3[["coefficients"]][["abil"]]*abil2 + regression3[["coefficients"]][["sibs"]]*sibs2 + regression3[["coefficients"]][["urban"]]*urban2

# Pour afficher les r�sultats dans la console
indiv1
indiv2

# Pour calculer la diff�rence d'ann�es d'�tudes entre les deux individus
diff <- indiv1 - indiv2
diff
# ou
diff2 <- regression3[["coefficients"]][["motheduc"]]*(moth1-moth2) + regression3[["coefficients"]][["fatheduc"]]*(fath1-fath2) + regression3[["coefficients"]][["abil"]]*(abil1-abil2) + regression3[["coefficients"]][["sibs"]]*(sibs1-sibs2) + regression3[["coefficients"]][["urban"]]*(urban1-urban2)
diff2

# L'individu 1 aurait en moyenne 2 ann�es d'�tudes de moins que l'individu 2.

# On v�rifie que cela donne la m�me chose
diff==diff2 
round(diff,2)==round(diff2,2) # vrai en arrondissant les valeurs au centi�me pr�s (deuxi�me d�cimale)