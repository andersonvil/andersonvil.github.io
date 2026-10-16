# =============================================================
# Économétrie L3, séance 2. Code R des figures, tables et calculs
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# (environnement d'exécution R) ou dans RStudio et l'exécuter.
# Les simulations fixent la graine (2026). R, Python et Stata utilisent
# des générateurs différents. Les tirages, donc les dernières décimales
# des résultats simulés, diffèrent d'un langage à l'autre.
# =============================================================

# ---------- [ech-quatre] Quatre échantillons, quatre droites ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")                              # les 856 étudiants, vus comme la population
x <- econmath$colgpa; y <- econmath$score
pente <- function(a, b) cov(a, b) / var(a)    # pente des MCO
set.seed(2026)                                # graine : mêmes tirages à chaque exécution
ech <- replicate(4, sample.int(856, 50, replace = TRUE))   # 4 échantillons de 50, avec remise
round(pente(x, y), 2)                         # pente sur les 856
round(apply(ech, 2, function(e) pente(x[e], y[e])), 2)     # pentes des 4 échantillons
par(mfrow = c(1, 4), mar = c(4, 4, 2, 1))
for (k in 1:4) {
  e <- ech[, k]
  plot(x[e], y[e], pch = 16, cex = 0.6, col = "#1F4E79", xlim = c(0.8, 4.1), ylim = c(20, 100),
       xlab = "colgpa", ylab = "score", main = paste("pente =", round(pente(x[e], y[e]), 1)))
  abline(lm(y ~ x), col = "#8C8C8C", lty = 2)  # droite des 856
  abline(lm(y[e] ~ x[e]), col = "#C0502B")     # droite de l'échantillon
}

# ---------- [ech-distrib] Mille échantillons, la distribution des pentes ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
x <- econmath$colgpa; y <- econmath$score
pente <- function(a, b) cov(a, b) / var(a)    # pente des MCO
set.seed(2026)
b1 <- replicate(1000, {e <- sample.int(856, 50, replace = TRUE); pente(x[e], y[e])})
round(c(moyenne = mean(b1), ecart_type = sd(b1)), 2)
round(range(b1), 2)                           # plus petite et plus grande pente
hist(b1, breaks = 30, col = "#1F4E79", border = "white",
     xlab = "Pente estimée sur un échantillon de 50 étudiants",
     ylab = "Nombre d'échantillons", main = "")
abline(v = pente(x, y), col = "#C0502B", lwd = 2)   # pente calculée sur les 856

# ---------- [va] Un étudiant tiré au hasard ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")                              # population de 856 étudiants
X <- as.integer(econmath$work >= 15)          # X = 1 si 15 heures ou plus de travail salarié
table(X)                                      # loi de X, en effectifs
round(c(P_X1 = mean(X), P_X0 = 1 - mean(X)), 4)
round(1 / 856, 5)                             # probabilité de chaque note

# ---------- [esp-prop] Calculer avec l'espérance ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
Y <- econmath$score                           # Y = note finale, chaque note a la probabilité 1/856
mu <- mean(Y)                                 # E(Y), moyenne de la population
round(mu, 2)
round(c(mean(0.2 * Y), 0.2 * mu), 2)          # E(0,2 Y) = 0,2 E(Y)
round(mean(Y - mu), 10)                       # E(Y - mu) = 0
round(c(EY2 = mean(Y^2), EY_carre = mu^2), 1)

# ---------- [variance] Variance et écart-type ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
X <- as.integer(econmath$work >= 15); Y <- econmath$score
p <- mean(X)
round(c(p * (1 - p), mean(X^2) - p^2), 4)     # Var(X) = p(1 - p)
VarY <- mean((Y - mean(Y))^2)                 # division par N = 856
round(c(VarY = VarY, ecart_type = sqrt(VarY), autre = mean(Y^2) - mean(Y)^2), 2)
round(c(mean((0.2 * Y - mean(0.2 * Y))^2), 0.04 * VarY), 3)   # Var(0,2 Y) = 0,04 Var(Y)

# ---------- [lgn] La moyenne empirique se rapproche de l'espérance ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
y <- econmath$score
set.seed(2026)
tirages <- sample(y, 2000, replace = TRUE)    # 2 000 étudiants tirés au hasard, avec remise
moy <- cumsum(tirages) / (1:2000)             # moyenne des n premiers tirages
round(moy[c(10, 100, 2000)], 2); round(mean(y), 2)
plot(1:2000, moy, type = "l", log = "x", col = "#1F4E79", ylim = c(60, 85),
     xlab = "Nombre de tirages n (échelle logarithmique)", ylab = "Moyenne des n notes")
abline(h = mean(y), col = "#C0502B", lty = 2)

# ---------- [cov-pop] Covariance et corrélation dans la population ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
X <- as.integer(econmath$work >= 15); Y <- econmath$score
covXY <- mean(X * Y) - mean(X) * mean(Y)      # Cov(X, Y) = E(XY) - E(X)E(Y)
round(c(covXY, mean((X - mean(X)) * (Y - mean(Y)))), 3)
sdX <- sqrt(mean((X - mean(X))^2)); sdY <- sqrt(mean((Y - mean(Y))^2))
round(covXY / (sdX * sdY), 3)                 # Corr(X, Y)

# ---------- [independance] Covariance nulle sans indépendance ----------
x <- c(-1, 0, 1); prob <- c(1, 1, 1) / 3      # X prend -1, 0, 1 avec probabilité 1/3
y <- x^2                                      # Y = X^2, entièrement déterminée par X
EX <- sum(prob * x); EY <- sum(prob * y); EXY <- sum(prob * x * y)
c(EX = EX, EY = round(EY, 4), EXY = EXY, Cov = EXY - EX * EY)
c(P_X0_Y1 = 0, produit = round(prob[2] * (prob[1] + prob[3]), 4))

# ---------- [cond-cov] Covariance nulle, espérance conditionnelle non nulle ----------
x <- c(-1, 0, 1); prob <- c(1, 1, 1) / 3      # x prend -1, 0, 1 avec probabilité 1/3
u <- x^2 - 2 / 3                              # u dépend de x
round(c(Eu = sum(prob * u), Cov = sum(prob * x * u)), 10)
round(u, 4)                                   # E(u | x) pour x = -1, 0, 1

# ---------- [var-moyenne] La variance d'une moyenne ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
y <- econmath$score
sigma <- sqrt(mean((y - mean(y))^2))          # écart-type de la note dans la population
set.seed(2026)
moyennes <- replicate(5000, mean(sample(y, 50, replace = TRUE)))   # 5 000 moyennes de 50 notes
round(c(theorique = sigma / sqrt(50), simule = sd(moyennes)), 3)
round(c(mean(moyennes), mean(y)), 2)

# ---------- [esp-cond] Espérance conditionnelle ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
X <- as.integer(econmath$work >= 15); Y <- econmath$score
m <- tapply(Y, X, mean); p <- mean(X)         # E(Y | X = 0), E(Y | X = 1), P(X = 1)
round(m, 2); round(p, 4)
classe <- floor(econmath$colgpa / 0.25) * 0.25 + 0.125   # classes de colgpa de largeur 0,25
t <- aggregate(score ~ classe, data = cbind(econmath, classe), FUN = mean)
t$n <- as.vector(table(classe)); t <- t[t$n >= 10, ]
round(t, 2)
plot(econmath$colgpa, Y, pch = 16, cex = 0.5, col = adjustcolor("#1F4E79", 0.15),
     xlab = "Moyenne universitaire (colgpa, sur 4)", ylab = "Note finale (score, %)")
points(t$classe, t$score, pch = 16, col = "#C0502B")     # moyennes par classe

# ---------- [iterees] La loi des espérances itérées ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
X <- as.integer(econmath$work >= 15); Y <- econmath$score
m <- tapply(Y, X, mean); p <- mean(X)         # E(Y | X = 0), E(Y | X = 1), P(X = 1)
round(c(iterees = (1 - p) * m[[1]] + p * m[[2]], EY = mean(Y), simple = mean(m)), 2)
round(p * (1 - p) * (m[[2]] - m[[1]]), 3)     # Cov(X, Y) = p(1 - p)[E(Y|X=1) - E(Y|X=0)]

# ---------- [var-cond] Variance conditionnelle ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
bornes <- quantile(econmath$colgpa, c(0, 0.25, 0.5, 0.75, 1))
quart <- cut(econmath$colgpa, bornes, include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))
et_pop <- function(v) sqrt(mean((v - mean(v))^2))      # écart-type de la population
round(cbind(moyenne = tapply(econmath$score, quart, mean),
            ecart_type = tapply(econmath$score, quart, et_pop), n = table(quart)), 2)
bornes                                        # bornes des quarts

# ---------- [rls1] Moyennes par classe et droite des MCO ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
m1 <- lm(score ~ colgpa, data = econmath)     # droite des MCO
classe <- floor(econmath$colgpa / 0.25) * 0.25 + 0.125
t <- aggregate(score ~ classe, data = cbind(econmath, classe), FUN = mean)
t$n <- as.vector(table(classe)); t <- t[t$n >= 10, ]
t$droite <- coef(m1)[1] + coef(m1)[2] * t$classe
round(t, 2)                                   # moyennes par classe et droite
plot(econmath$colgpa, econmath$score, pch = 16, cex = 0.5, col = adjustcolor("#1F4E79", 0.15),
     xlab = "Moyenne universitaire (colgpa, sur 4)", ylab = "Note finale (score, %)")
points(t$classe, t$score, pch = 16, col = "#C0502B")     # moyennes par classe
abline(m1, col = "#8C8C8C", lty = 2)

# ---------- [rls3] Variation de x dans l'échantillon ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
summary(econmath$colgpa); round(sd(econmath$colgpa), 3)   # écart-type positif : RLS.3
length(unique(econmath$colgpa))               # nombre de valeurs différentes

# ---------- [equilibre] Comparer les groupes sur ce qui est observé ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath"); data("jtrain2")
econmath$w15 <- as.integer(econmath$work >= 15)
moy <- function(v) tapply(v, econmath$w15, mean, na.rm = TRUE)   # na.rm : actmth a 42 manquants
round(sapply(econmath[c("colgpa", "mothcoll", "actmth")], moy), 2)
round(aggregate(cbind(educ, age, re75) ~ train, data = jtrain2, FUN = mean), 2)   # re75 : 1975

# ---------- [rls5] Résidus et dispersion selon colgpa ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
res <- resid(lm(score ~ colgpa, data = econmath))      # résidus des MCO
bornes <- quantile(econmath$colgpa, c(0, 0.25, 0.5, 0.75, 1))
quart <- cut(econmath$colgpa, bornes, include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))
round(tapply(res, quart, sd), 2)              # écart-type des résidus par quart de colgpa
plot(econmath$colgpa, res, pch = 16, cex = 0.5, col = adjustcolor("#1F4E79", 0.35),
     xlab = "Moyenne universitaire (colgpa, sur 4)", ylab = "Résidu")
abline(h = 0, col = "#C0502B")

# ---------- [jouet-bruit] Exemple à la main, l'estimation est la vérité plus du bruit ----------
x <- c(2, 4, 6, 8, 10); y <- c(7, 10, 11, 14, 13)
u <- y - (6 + 1 * x)                          # erreurs, si la vraie droite est 6 + x
w <- (x - mean(x)) / sum((x - mean(x))^2)     # poids w_i
u; w
b1 <- 1 + sum(w * u)                          # b1 = vraie pente + somme w_i u_i
b1
1 + sum(w * c(1, -1, 2, 0, 1))                # avec d'autres erreurs
par(mfrow = c(1, 2))
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(6, 18), main = "Erreurs u")
abline(a = 6, b = 1, col = "#8C8C8C", lty = 2); segments(x, y, x, 6 + x, col = "#8C8C8C")
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(6, 18), main = "Résidus")
abline(a = 6.2, b = 0.8, col = "#C0502B"); segments(x, y, x, 6.2 + 0.8 * x, col = "#C0502B")

# ---------- [jouet-b0] Exemple à la main, la constante ----------
x <- c(2, 4, 6, 8, 10); y <- c(7, 10, 11, 14, 13)
u <- y - (6 + 1 * x)                          # erreurs, si la vraie droite est 6 + x
b1 <- 1 + sum((x - mean(x)) * u) / sum((x - mean(x))^2)
c(moyenne_u = mean(u), b1_moins_1 = b1 - 1)
6 + mean(u) - (b1 - 1) * mean(x)              # b0 = 6 + moyenne(u) - (b1 - 1) x moyen

# ---------- [mc-sansbiais] Simulation, l'absence de biais ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
x <- econmath$colgpa[1:50]                    # x fixés : 50 premiers étudiants
w <- (x - mean(x)) / sum((x - mean(x))^2)     # poids w_i, pente = somme w_i y_i
set.seed(2026)
u <- matrix(rnorm(5000 * 50, 0, 11), nrow = 5000)          # une ligne par échantillon
y <- matrix(32 + 14 * x, 5000, 50, byrow = TRUE) + u       # vraie pente 14
b1 <- as.vector(y %*% w)                      # pente des MCO de chaque échantillon
round(c(moyenne = mean(b1), ecart_type = sd(b1)), 3)
round(11 / sqrt(sum((x - mean(x))^2)), 3)     # écart-type théorique
hist(b1, breaks = 40, col = "#1F4E79", border = "white",
     xlab = "Pente estimée", ylab = "Nombre d'échantillons", main = "")
abline(v = 14, col = "#C0502B", lwd = 2)      # vraie pente

# ---------- [mc-biais] Simulation, un biais quand RLS.4 échoue ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
x50 <- econmath$colgpa[1:50]
x500 <- rep(x50, 10)                          # 500 observations, même dispersion de x
set.seed(2026)
pentes <- function(x) {
  w <- (x - mean(x)) / sum((x - mean(x))^2)
  e <- matrix(rnorm(5000 * length(x), 0, 11), nrow = 5000)
  y <- sweep(e, 2, 32 + 14 * x + 4 * (x - 2.8), "+")   # E(u | x) = 4 (x - 2,8)
  as.vector(y %*% w)
}
b50 <- pentes(x50); b500 <- pentes(x500)
round(rbind(n50 = c(mean(b50), sd(b50)), n500 = c(mean(b500), sd(b500))), 3)
hist(b500, breaks = 40, freq = FALSE, col = adjustcolor("#1F4E79", 0.7), border = "white",
     xlim = c(8, 28), xlab = "Pente estimée", main = "")
hist(b50, breaks = 40, freq = FALSE, col = adjustcolor("#8C8C8C", 0.5), border = "white", add = TRUE)
abline(v = 14, col = "#C0502B", lwd = 2)      # vraie pente

# ---------- [mc-variance] Simulation, ce qui rend la pente précise ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
x50 <- econmath$colgpa[1:50]
plans <- list(reference = list(x50, 11), n200 = list(rep(x50, 4), 11),
              x_resserre = list(mean(x50) + 0.5 * (x50 - mean(x50)), 11), sigma5.5 = list(x50, 5.5))
set.seed(2026)
simul <- function(x, s) {
  w <- (x - mean(x)) / sum((x - mean(x))^2)
  as.vector(sweep(matrix(rnorm(5000 * length(x), 0, s), nrow = 5000), 2, 32 + 14 * x, "+") %*% w)
}
b <- lapply(plans, function(p) simul(p[[1]], p[[2]]))
theo <- sapply(plans, function(p) p[[2]] / sqrt(sum((p[[1]] - mean(p[[1]]))^2)))
round(rbind(theorique = theo, simule = sapply(b, sd)), 3)
par(mfrow = c(1, 3))
for (k in 1:3) {
  hist(b[[k]], breaks = 40, xlim = c(-6, 34), col = "#1F4E79", border = "white",
       main = names(plans)[k], xlab = "Pente estimée")
  abline(v = 14, col = "#C0502B", lwd = 1.5)
}

# ---------- [eventail] Pourquoi la dispersion de x aide ----------
set.seed(2026)
xl <- seq(1, 5, length.out = 20); xe <- 3 + 0.25 * (xl - 3)   # x dispersés, puis x resserrés
e <- matrix(rnorm(20 * 20), nrow = 20)        # 20 échantillons d'erreurs, les mêmes à gauche et à droite
par(mfrow = c(1, 2))
for (x in list(xl, xe)) {
  plot(NA, xlim = c(0, 6), ylim = c(-4, 12), xlab = "x", ylab = "y",
       main = paste("x entre", min(x), "et", max(x)))
  rect(min(x), -5, max(x), 13, col = adjustcolor("#8C8C8C", 0.15), border = NA)
  for (r in 1:20) abline(lm(2 + x + e[r, ] ~ x), col = adjustcolor("#1F4E79", 0.35))
  abline(a = 2, b = 1, col = "#C0502B", lwd = 2)   # vraie droite 2 + x
}
round(c(1 / sqrt(sum((xl - 3)^2)), 1 / sqrt(sum((xe - 3)^2))), 3)   # écarts-types théoriques

# ---------- [mc-hetero] Simulation, quand RLS.5 échoue ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
x <- econmath$colgpa[1:50]
w <- (x - mean(x)) / sum((x - mean(x))^2)
s <- 3 + 14 * abs(x - 2.8)                    # écart-type de u, variable selon x
round(mean(s^2), 1)                           # moyenne des variances de u
set.seed(2026)
u <- sweep(matrix(rnorm(5000 * 50), nrow = 5000), 2, s, "*")
b1 <- as.vector(sweep(u, 2, 32 + 14 * x, "+") %*% w)
round(c(moyenne = mean(b1), ecart_type_simule = sd(b1)), 3)
round(c(vrai = sqrt(sum(w^2 * s^2)), formule_usuelle = sqrt(mean(s^2) / sum((x - mean(x))^2))), 3)

# ---------- [mc-sigma] Simulation, estimer la variance de l'erreur ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
x <- econmath$colgpa[1:50]
sct <- sum((x - mean(x))^2); w <- (x - mean(x)) / sct
set.seed(2026)
y <- matrix(32 + 14 * x, 5000, 50, byrow = TRUE) + matrix(rnorm(5000 * 50, 0, 11), nrow = 5000)
b1 <- as.vector(y %*% w); b0 <- rowMeans(y) - b1 * mean(x)
scr <- rowSums((y - b0 - outer(b1, x))^2)     # SCR de chaque échantillon
round(c(SCR_sur_n_moins_2 = mean(scr / 48), SCR_sur_n = mean(scr / 50)), 2)
c(sigma2 = 121, biais_attendu = 121 * 48 / 50)

# ---------- [mc-se] Simulation, l'erreur-type ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
x <- econmath$colgpa[1:50]
sct <- sum((x - mean(x))^2); w <- (x - mean(x)) / sct
set.seed(2026)
y <- matrix(32 + 14 * x, 5000, 50, byrow = TRUE) + matrix(rnorm(5000 * 50, 0, 11), nrow = 5000)
b1 <- as.vector(y %*% w); b0 <- rowMeans(y) - b1 * mean(x)
scr <- rowSums((y - b0 - outer(b1, x))^2)     # SCR de chaque échantillon
se <- sqrt(scr / 48 / sct)                    # erreur-type de la pente, un par échantillon
round(c(moyenne_se = mean(se), ecart_type_pentes = sd(b1)), 3)
round(11 / sqrt(sct), 3)                      # écart-type théorique

# ---------- [erreur-type] D'où viennent les erreurs-types de la table ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
m1 <- lm(score ~ colgpa, data = econmath)
n <- nobs(m1); x <- econmath$colgpa; sct <- sum((x - mean(x))^2)
s2 <- sum(resid(m1)^2) / (n - 2)              # sigma chapeau au carré = SCR / (n - 2)
round(c(SCR = sum(resid(m1)^2), sigma_chapeau = sqrt(s2), SCT_x = sct), 3)
round(mean(x^2), 3)                          # moyenne des x au carré
round(c(se_b1 = sqrt(s2 / sct), se_b0 = sqrt(s2 * sum(x^2) / (n * sct))), 4)
round(summary(m1)$coefficients[, 2], 4); round(summary(m1)$sigma, 3)   # logiciel
