# =============================================================
# Économétrie L3, séance 1. Série d'exercices, code R du corrigé
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# (environnement d'exécution R) ou dans RStudio et l'exécuter.
# =============================================================

# ---------- [ex1] Exercice 1, calculs à la main vérifiés ----------
x <- c(2, 3, 4, 6, 7, 8)                          # prix du panier (euros)
y <- c(14, 12, 13, 9, 8, 4)                       # paniers vendus en une heure
dx <- x - mean(x); dy <- y - mean(y)              # écarts à la moyenne
c(mean(x), mean(y))                               # moyennes
c(sum(dx * dy), sum(dx^2), sum(dy^2))             # sommes du tableau
c(var(x), var(y), cov(x, y))                      # s2_x, s2_y, s_xy
round(cor(x, y), 4)                               # r
b1 <- sum(dx * dy) / sum(dx^2); b0 <- mean(y) - b1 * mean(x)
c(b0, b1)
yhat <- b0 + b1 * x; uhat <- y - yhat
yhat; uhat                                        # valeurs prédites et résidus
c(sum(uhat), sum(x * uhat))                       # propriétés 1 et 2
sct <- sum(dy^2); sce <- sum((yhat - mean(y))^2); scr <- sum(uhat^2)
c(sct, sce, scr, sce / sct)                       # SCT, SCE, SCR, R2
b0 + b1 * c(5, 15)                                # prévisions à 5 et 15 euros
coef(lm(y ~ I(100 * x)))                          # prix en centimes

# ---------- [ex2] Exercice 2, les deux droites de régression ----------
x <- c(2, 3, 4, 6, 7, 8); y <- c(14, 12, 13, 9, 8, 4)
b1 <- cov(x, y) / var(x)                          # régression de y sur x
g1 <- cov(x, y) / var(y)                          # régression de x sur y
c(b1, g1)
c(round(b1 * g1, 4), round(cor(x, y)^2, 4))       # produit des pentes et r au carré
round(1 / g1, 4)                                  # pente de la seconde droite dans (x, y)

# ---------- [ex3] Exercice 3, table de régression, ATTEND ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("attend")                                    # 680 étudiants
m1 <- lm(final ~ attend, data = attend)
m2 <- lm(final ~ priGPA, data = attend)
m3 <- lm(attend ~ priGPA, data = attend)
for (m in list(m1, m2, m3)) print(c(coef(summary(m))[, c(1, 2, 4)], nobs(m), summary(m)$r.squared))   # coefficients, erreurs-types, p-valeurs
range(attend$attend)                              # min et max
round(predict(m1, data.frame(attend = c(32, 16))), 4)
round(predict(m3, data.frame(priGPA = 3)), 4)
m4 <- lm(final ~ atndrte, data = attend)          # présence en %
round(c(coef(m4), summary(m4)$r.squared), 4)

# ---------- [ex4] Exercice 4, résultats potentiels et tirage au sort ----------
y0 <- c(8, 9, 7, 10, 14, 13, 15, 12)              # notes sans tutorat, A à H
y1 <- c(11, 11, 10, 12, 15, 15, 16, 14)           # notes avec tutorat
d <- c(1, 1, 1, 1, 0, 0, 0, 0)                    # participation choisie
y1 - y0; mean(y1 - y0)                            # effets individuels et effet moyen
mean((y1 - y0)[d == 1])                           # effet moyen chez les participants
ecart <- mean(y1[d == 1]) - mean(y0[d == 0])
c(ecart, mean((y1 - y0)[d == 1]), mean(y0[d == 1]) - mean(y0[d == 0]))
t <- c(1, 0, 1, 0, 1, 0, 1, 0)                    # un tirage au sort (A, C, E, G)
mean(y1[t == 1]) - mean(y0[t == 0])

# ---------- [ex4-tirages] Exercice 4, les 70 tirages possibles ----------
y0 <- c(8, 9, 7, 10, 14, 13, 15, 12)              # notes sans tutorat, A à H
y1 <- c(11, 11, 10, 12, 15, 15, 16, 14)           # notes avec tutorat
tirages <- combn(8, 4)                            # 4 étudiants tirés parmi 8
ecarts <- apply(tirages, 2, function(g) mean(y1[g]) - mean(y0[-g]))
c(ncol(tirages), round(mean(ecarts), 10), min(ecarts), max(ecarts))

# ---------- [ex5-depart] Exercice 5, charger les données WINE ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("wine")                                      # 21 pays
wine

# ---------- [ex5-mco] Exercice 5, statistiques et MCO ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("wine")
summary(wine[, c("alcohol", "heart")]); round(c(sd(wine$alcohol), sd(wine$heart)), 3)
round(c(cov(wine$alcohol, wine$heart), cor(wine$alcohol, wine$heart)), 4)
m <- lm(heart ~ alcohol, data = wine)
round(c(coef(m), summary(m)$r.squared, nobs(m)), 4)
wine$prevu <- fitted(m); wine$residu <- resid(m)
subset(wine, country %in% c("France", "Japan"), c(country, alcohol, heart, prevu, residu))
sct <- sum((wine$heart - mean(wine$heart))^2)
round(c(sum(resid(m)), sct, sum((fitted(m) - mean(wine$heart))^2), sum(resid(m)^2)), 2)
round(c(coef(lm(liver ~ alcohol, data = wine)), summary(lm(liver ~ alcohol, data = wine))$r.squared), 4)
round(c(coef(lm(deaths ~ alcohol, data = wine)), summary(lm(deaths ~ alcohol, data = wine))$r.squared), 4)

# ---------- [ex5-figure] Exercice 5, nuage de points et droite ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("wine")
plot(wine$alcohol, wine$heart, pch = 16, col = "#1F4E79", bty = "l",
     xlab = "Alcool issu du vin (litres par habitant)",
     ylab = "Décès cardiaques pour 100 000")
abline(lm(heart ~ alcohol, data = wine), col = "#C0502B")
noms <- c(France = "France", Japan = "Japon", Italy = "Italie")   # pays à nommer
k <- wine$country %in% names(noms)
text(wine$alcohol[k], wine$heart[k], noms[as.character(wine$country[k])], pos = 4, cex = 0.8)
