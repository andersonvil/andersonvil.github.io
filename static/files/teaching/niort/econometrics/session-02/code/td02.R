# =============================================================
# Économétrie L3, séance 2. Série d'exercices, code R du corrigé
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# (environnement d'exécution R) ou dans RStudio et l'exécuter.
# Les simulations fixent la graine (2026). R, Python et Stata utilisent
# des générateurs différents. Les dernières décimales diffèrent.
# =============================================================

# ---------- [ex1] Exercice 1, une loi jointe à la main ----------
xv <- c(0, 2, 4); uv <- c(-2, 0, 2)          # valeurs de x et de u
P <- matrix(c(0.10, 0.15, 0.05,              # lignes : x = 0, 2, 4
              0.10, 0.20, 0.10,              # colonnes : u = -2, 0, 2
              0.05, 0.15, 0.10), nrow = 3, byrow = TRUE)
px <- rowSums(P); pu <- colSums(P)           # lois marginales
Ex <- sum(px * xv); Eu <- sum(pu * uv)
c(somme = sum(P)); px; pu
c(Ex = Ex, Varx = sum(px * xv^2) - Ex^2, Eu = round(Eu, 10))
Eu_x <- as.vector(P %*% uv) / px             # E(u | x) pour x = 0, 2, 4
round(Eu_x, 4); round(sum(px * Eu_x), 10)    # espérances itérées
covxu <- sum(P * outer(xv, uv)) - Ex * Eu    # Cov(x, u) = E(xu) - E(x)E(u)
round(covxu, 4)
round(8 + xv + Eu_x, 4)                      # E(y | x) = 8 + x + E(u | x)
round(c(pente = (Eu_x[3] + 4 - Eu_x[1]) / 4, formule = 1 + covxu / 2.4), 4)
round(as.vector(P %*% uv^2) / px - Eu_x^2, 4)   # Var(u | x)
Q <- outer(px, pu)                           # loi jointe si x et u sont indépendants
Q; round(as.vector(Q %*% uv) / px, 4)         # E(u | x) = 0 pour tout x

# ---------- [ex1-simul] Exercice 1, la pente des MCO sur des échantillons tirés ----------
xv <- c(0, 2, 4); uv <- c(-2, 0, 2)
P <- matrix(c(0.10, 0.15, 0.05, 0.10, 0.20, 0.10, 0.05, 0.15, 0.10), nrow = 3, byrow = TRUE)
set.seed(2026)
pente <- function() {                        # un échantillon de 100 couples (x, u)
  k <- sample(0:8, 100, replace = TRUE, prob = as.vector(t(P)))
  x <- xv[k %/% 3 + 1]; u <- uv[k %% 3 + 1]  # case k : ligne k %/% 3, colonne k %% 3
  y <- 8 + 1 * x + u                         # vraie pente 1
  sum((x - mean(x)) * y) / sum((x - mean(x))^2)
}
b1 <- replicate(5000, pente())
round(c(moyenne = mean(b1), sept_sixiemes = 7 / 6), 4)

# ---------- [ex2] Exercice 2, la pente des extrêmes ----------
x <- c(2, 4, 6, 8, 10); y <- c(7, 10, 11, 14, 13)   # exemple à la main
c(extremes = (y[5] - y[1]) / (x[5] - x[1]), mco = 32 / 40)
sct <- sum((x - mean(x))^2)
c(var_extremes = 2 / (x[5] - x[1])^2, var_mco = 1 / sct, SCT = sct, borne = (x[5] - x[1])^2 / 2)
set.seed(2026)
yy <- matrix(6 + x, 10000, 5, byrow = TRUE) + matrix(rnorm(50000), nrow = 10000)   # sigma = 1
ext <- (yy[, 5] - yy[, 1]) / 8; mco <- as.vector(yy %*% ((x - mean(x)) / sct))
round(c(mean(ext), mean(mco)), 4)
round(c(var(ext), var(mco)), 4)              # théoriques 1/32 et 1/40

# ---------- [ex3] Exercice 3, table de régression, NBASAL ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("nbasal")                               # 269 joueurs de NBA
for (f in c("wage ~ points", "points ~ exper", "wage ~ exper")) {
  s <- summary(lm(as.formula(f), data = nbasal))
  print(f); print(round(s$coefficients[, c(1, 2, 4)], 4)); print(round(c(R2 = s$r.squared, sigma = s$sigma), 3))
}
m1 <- lm(wage ~ points, data = nbasal)
quart <- cut(nbasal$points, quantile(nbasal$points), include.lowest = TRUE)
round(tapply(resid(m1), quart, sd))          # écart-type des résidus par quart de points
round(c(SCT = sum((nbasal$points - mean(nbasal$points))^2), s = sd(nbasal$points)), 3)
round(287.101 + 111.330 * c(10, 20), 1)      # prévisions pour 10 et 20 points

# ---------- [ex5-depart] Exercice 5, charger les données HTV ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("htv")                                  # 1 230 hommes de 26 à 34 ans, en 1991
summary(htv[, c("wage", "educ", "abil")])

# ---------- [ex5-mco] Exercice 5, MCO, sigma chapeau et erreur-type ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("htv")
m <- lm(wage ~ educ, data = htv)
round(summary(m)$coefficients[, 1:2], 4); round(summary(m)$r.squared, 4); nobs(m)
n <- nobs(m); x <- htv$educ
scr <- sum(resid(m)^2); sct <- sum((x - mean(x))^2)
s <- sqrt(scr / (n - 2))                     # sigma chapeau
round(c(SCR = scr, sigma_chapeau = s, SCT_educ = sct, se_b1 = s / sqrt(sct)), 4)

# ---------- [ex5-residus] Exercice 5, résidus et dispersion ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("htv")
res <- resid(lm(wage ~ educ, data = htv))
groupe <- cut(htv$educ, c(0, 12, 14, 20), labels = c("12 ou moins", "13 ou 14", "15 ou plus"))
round(cbind(ecart_type = tapply(res, groupe, sd), n = table(groupe)), 2)
plot(htv$educ, res, pch = 16, cex = 0.5, col = adjustcolor("#1F4E79", 0.35),
     xlab = "Années d'études (educ)", ylab = "Résidu")
abline(h = 0, col = "#C0502B")

# ---------- [ex5-simul] Exercice 5, la population des 1 230 hommes ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("htv")
x <- htv$educ; y <- htv$wage
set.seed(2026)
un_echantillon <- function() {               # 100 hommes tirés avec remise
  e <- sample.int(1230, 100, replace = TRUE)
  m <- lm(y[e] ~ x[e])
  c(b1 = coef(m)[[2]], se = summary(m)$coefficients[2, 2])
}
r <- replicate(2000, un_echantillon())
round(c(moyenne_pentes = mean(r["b1", ]), pente_1230 = 1.392), 3)
round(c(ecart_type_pentes = sd(r["b1", ]), moyenne_se = mean(r["se", ])), 3)

# ---------- [ex5-abil] Exercice 5, l'aptitude, un facteur de u observé ici ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("htv")
round(c(educ_abil = cor(htv$educ, htv$abil), wage_abil = cor(htv$wage, htv$abil)), 3)
round(tapply(htv$abil, htv$educ >= 15, mean), 2)   # aptitude moyenne, moins de 15 ans d'études ou plus
