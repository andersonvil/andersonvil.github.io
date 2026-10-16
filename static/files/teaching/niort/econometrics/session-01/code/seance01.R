# =============================================================
# Économétrie L3, séance 1. Code R des figures, tables et calculs
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# (environnement d'exécution R) ou dans RStudio et l'exécuter.
# =============================================================

# ---------- [etude] Heures d'étude et note ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")                                  # 856 étudiants
round(cor(econmath$study, econmath$score), 3)     # corrélation

# ---------- [outils] Charger des données de Wooldridge ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")                                  # charge ECONMATH
dim(econmath)                                     # lignes, colonnes
head(econmath)                                    # premières lignes
# ?econmath affiche la description des variables

# ---------- [ext-ct] Coupe transversale, ECONMATH ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
head(econmath[, c("score", "colgpa", "work", "study", "econhs")], 5)
nrow(econmath)

# ---------- [ext-st] Série temporelle, PHILLIPS ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("phillips")
head(phillips[, c("year", "unem", "inf")], 3)
tail(phillips[, c("year", "unem", "inf")], 2)
plot(phillips$year, phillips$unem, type = "l", col = "#1F4E79", lwd = 1.5,
     ylim = range(phillips$unem, phillips$inf), xlab = "Année", ylab = "", yaxt = "n")
axis(2, at = axTicks(2), labels = sub(".", ",", axTicks(2), fixed = TRUE), las = 1)
lines(phillips$year, phillips$inf, col = "#8C8C8C", lty = 2)
abline(h = 0, lwd = 0.5)
legend("topleft", c("Taux de chômage (%)", "Inflation (%)"),
       col = c("#1F4E79", "#8C8C8C"), lty = c(1, 2), bty = "n")

# ---------- [ext-cte] Coupes transversales empilées, CPS78_85 ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("cps78_85")
var <- c("year", "educ", "exper", "female", "lwage")
head(cps78_85[cps78_85$year == 78, var], 2)       # enquête de 1978
head(cps78_85[cps78_85$year == 85, var], 2)       # enquête de 1985
table(cps78_85$year)

# ---------- [ext-panel] Données de panel, WAGEPAN ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("wagepan")
subset(wagepan, nr %in% c(13, 17) & year <= 1981,
       select = c(nr, year, educ, exper, hours, lwage))
length(unique(wagepan$nr)); length(unique(wagepan$year))

# ---------- [jtrain] Données expérimentales, JTRAIN2 ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("jtrain2")
round(aggregate(cbind(re78, educ, age) ~ train, data = jtrain2, FUN = mean), 2)
table(jtrain2$train)                              # effectifs

# ---------- [travail] Travailler 15 h ou plus, comparaison de groupes ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
econmath$w15 <- as.integer(econmath$work >= 15)  # 1 si 15 h ou plus
round(aggregate(cbind(score, colgpa, mothcoll) ~ w15, data = econmath, FUN = mean), 2)
table(econmath$w15)                               # effectifs

# ---------- [variance] Construire la variance ----------
a <- c(2, 4, 6, 8, 10)       # série A, dispersée
b <- c(4, 5, 6, 7, 8)        # série B, même moyenne, plus resserrée
a - mean(a); sum(a - mean(a))                     # écarts, de somme nulle
(a - mean(a))^2; var(a)                           # carrés et variance de A
(b - mean(b))^2; var(b)                           # carrés et variance de B
layout(matrix(1:2, 1), widths = c(1, 1.6)); par(mar = c(3, 1, 2, 1))
ha <- c(1.9, 2.2, 2.5, 2.8, 3.1); hb <- c(0.2, 0.5, 0.8, 1.1, 1.4)
plot(a, ha, pch = 16, col = "#1F4E79", xlim = c(0, 11.5), ylim = c(0, 3.7),
     yaxt = "n", xlab = "", ylab = "", main = "Écarts à la moyenne, de somme nulle",
     cex.main = 0.9, font.main = 1)
points(b, hb, pch = 16, col = "#1F4E79")
segments(6, ha, a, ha, col = "#8C8C8C"); segments(6, hb, b, hb, col = "#8C8C8C")
abline(v = 6, col = "#C0502B", lty = 2)
text(c(0.3, 0.3), c(2.4, 0.7), c("A", "B"), font = 2)
cotes <- c(abs(a - 6), abs(b - 6))                # côtés des carrés, A puis B
pos <- c(0, 4.6, 7.2, 7.8, 10.4, 16.5, 19.1, 20.7, 21.3, 22.9)
plot.new(); plot.window(xlim = c(-0.5, 25.5), ylim = c(-0.3, 5.2), asp = 1)
title("Carrés des écarts, d'aire (x - moyenne) au carré", cex.main = 0.9, font.main = 1)
rect(pos, 0, pos + cotes, cotes, col = adjustcolor("#1F4E79", 0.25), border = "#1F4E79")
k <- cotes > 0; text(pos[k] + cotes[k] / 2, cotes[k] / 2, cotes[k]^2, cex = 0.8)
text(c(7.2, 20.7), c(4.4, 4.4), c("A", "B"), font = 2)

# ---------- [quadrants] Signe de la covariance, ECONMATH ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
plot(econmath$colgpa, econmath$score, pch = 16, cex = 0.5,
     col = adjustcolor("#1F4E79", 0.35), xaxt = "n",
     xlab = "Moyenne universitaire (colgpa, sur 4)", ylab = "Note finale (score, %)")
axis(1, at = axTicks(1), labels = sub(".", ",", axTicks(1), fixed = TRUE))
abline(v = mean(econmath$colgpa), h = mean(econmath$score), col = "#8C8C8C", lty = 2)
text(c(3.8, 1.1, 3.8, 1.1), c(97, 25, 25, 97), c("+", "+", "\u2212", "\u2212"),
     col = "#C0502B", cex = 1.6)
round(cov(econmath$colgpa, econmath$score), 3)
round(cor(econmath$colgpa, econmath$score), 3)

# ---------- [deplacer] Déplacer des points change le signe ----------
x <- 1:10
yA <- c(2, 4, 3, 5, 4, 6, 5, 7, 6, 8)             # nuage de départ
yB <- c(7, 4, 3, 5, 4, 6, 5, 7, 6, 3)             # on déplace 2 points
yC <- c(7, 8, 6, 5, 4, 6, 5, 4, 2, 3)             # on en déplace 4 autres
round(c(cor(x, yA), cor(x, yB), cor(x, yC)), 3)
round(c(cov(x, yA), cov(x, yB), cov(x, yC)), 3)
par(mfrow = c(1, 3))
plot(x, yA, pch = 16, col = "#1F4E79", ylim = c(2, 8), ylab = "y",
     main = paste("r =", sub(".", ",", format(round(cor(x, yA), 2), nsmall = 2), fixed = TRUE)))
abline(v = 5.5, h = 5, col = "#8C8C8C", lty = 2)
plot(x, yB, pch = 16, col = "#1F4E79", ylim = c(2, 8), ylab = "y",
     main = paste("r =", sub(".", ",", format(round(abs(cor(x, yB)), 2), nsmall = 2), fixed = TRUE)))
points(x[c(1, 10)], yA[c(1, 10)], pch = 16, col = "#C8C8C8")     # ancienne place
abline(v = 5.5, h = 5, col = "#8C8C8C", lty = 2)
plot(x, yC, pch = 16, col = "#1F4E79", ylim = c(2, 8), ylab = "y",
     main = paste("r =", sub(".", ",", format(round(cor(x, yC), 2), nsmall = 2), fixed = TRUE)))
points(x[c(2, 3, 8, 9)], yB[c(2, 3, 8, 9)], pch = 16, col = "#C8C8C8")
abline(v = 5.5, h = 5, col = "#8C8C8C", lty = 2)

# ---------- [jouet-stats] Exemple jouet, statistiques descriptives ----------
x <- c(2, 4, 6, 8, 10)       # heures de travail personnel par semaine
y <- c(7, 10, 11, 14, 13)    # note sur 20
c(mean(x), mean(y))
c(var(x), var(y))            # var() divise par n - 1
cov(x, y)
round(cor(x, y), 4)

# ---------- [nuage] Nuage de points du fil rouge ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
plot(econmath$colgpa, econmath$score, pch = 16, cex = 0.5,
     col = adjustcolor("#1F4E79", 0.35), xaxt = "n",
     xlab = "Moyenne universitaire (colgpa, sur 4)", ylab = "Note finale (score, %)")
axis(1, at = axTicks(1), labels = sub(".", ",", axTicks(1), fixed = TRUE))

# ---------- [mco-illus] Quelle droite choisir ? ----------
x <- c(2, 4, 6, 8, 10)
y <- c(7, 10, 11, 14, 13)
scr_1 <- sum((y - (4 + 1.2 * x))^2)               # droite 1 : 4 + 1,2 x
scr_2 <- sum((y - (6.2 + 0.8 * x))^2)             # droite 2 : 6,2 + 0,8 x
round(c(scr_1, scr_2), 2)
par(mfrow = c(1, 2))
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(4, 18),
     xlab = "x (heures)", ylab = "y (note sur 20)",
     main = paste0("y = 4 + 1,2 x\nSCR = ", sub(".", ",", format(round(scr_1, 1), nsmall = 1), fixed = TRUE)))
abline(a = 4, b = 1.2, col = "#8C8C8C", lwd = 1.5)
segments(x, y, x, 4 + 1.2 * x, col = "#8C8C8C")   # résidus
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(4, 18),
     xlab = "x (heures)", ylab = "y (note sur 20)",
     main = paste0("y = 6,2 + 0,8 x\nSCR = ", sub(".", ",", format(round(scr_2, 1), nsmall = 1), fixed = TRUE)))
abline(a = 6.2, b = 0.8, col = "#C0502B", lwd = 1.5)
segments(x, y, x, 6.2 + 0.8 * x, col = "#8C8C8C")

# ---------- [carres] Trois critères pour choisir la droite ----------
x <- c(2, 4, 6, 8, 10)
y <- c(7, 10, 11, 14, 13)
u_a <- y - (17 - x)                               # droite fausse, pente -1
u_b1 <- y - (5 + x); u_b2 <- y - (8 + 0.5 * x)    # deux droites
u_c <- y - (6.2 + 0.8 * x)                        # droite des MCO
c(sum(u_a), round(sum(u_c), 10))                  # a) sommes des résidus
c(sum(abs(u_b1)), sum(abs(u_b2)))                 # b) sommes des valeurs absolues
round(sum(u_c^2), 2)                              # c) somme des carrés
par(mfrow = c(1, 3))
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(5, 16), asp = 1,
     main = "a) y = 17 - x\nsomme des résidus = 0")
abline(a = 17, b = -1, col = "#8C8C8C"); segments(x, y, x, 17 - x, col = "#8C8C8C")
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(5, 16), asp = 1,
     main = "b) y = 5 + x et y = 8 + 0,5 x\nsomme des |résidus| = 4 et 4")
abline(a = 5, b = 1, col = "#8C8C8C"); abline(a = 8, b = 0.5, col = "#8C8C8C", lty = 2)
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(5, 16), asp = 1,
     main = "c) y = 6,2 + 0,8 x\nsomme des carrés = 4,4")
abline(a = 6.2, b = 0.8, col = "#C0502B")
rect(x, pmin(y, y - u_c), x + abs(u_c), pmin(y, y - u_c) + abs(u_c),
     col = adjustcolor("#1F4E79", 0.25), border = NA)

# ---------- [jouet-mco] Exemple jouet, MCO à la main et par le code ----------
x <- c(2, 4, 6, 8, 10)
y <- c(7, 10, 11, 14, 13)
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)
c(b0, b1)                    # à la main
coef(lm(y ~ x))              # par le code

# ---------- [jouet-proprietes] Valeurs prédites, résidus, propriétés ----------
x <- c(2, 4, 6, 8, 10)
y <- c(7, 10, 11, 14, 13)
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)
yhat <- b0 + b1 * x          # valeurs prédites
uhat <- y - yhat             # résidus
rbind(yhat, uhat)
round(sum(uhat), 10)         # somme des résidus
round(sum(x * uhat), 10)     # somme x * résidus
c(b0 + b1 * mean(x), mean(y))  # la droite passe par le point moyen

# ---------- [decomp] Décomposition pour une observation ----------
x <- c(2, 4, 6, 8, 10)
y <- c(7, 10, 11, 14, 13)
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)
yhat <- b0 + b1 * x
plot(x, y, pch = 16, col = "#1F4E79", xlim = c(0, 12), ylim = c(4, 18),
     xlab = "x (heures)", ylab = "y (note sur 20)")
abline(a = b0, b = b1, col = "#C0502B", lwd = 1.5)
abline(h = mean(y), col = "#8C8C8C", lty = 2)
arrows(8.25, mean(y), 8.25, yhat[4], code = 3, length = 0.05, col = "#C0502B")  # 4e étudiant
arrows(7.75, yhat[4], 7.75, y[4], code = 3, length = 0.05, col = "#8C8C8C")
text(8.45, (yhat[4] + mean(y)) / 2, expression(hat(y)[i] - bar(y)), col = "#C0502B", adj = 0)
text(7.55, (y[4] + yhat[4]) / 2, expression(hat(u)[i]), col = "#8C8C8C", adj = 1)

# ---------- [jouet-r2] SCT, SCE, SCR et R2 ----------
x <- c(2, 4, 6, 8, 10)
y <- c(7, 10, 11, 14, 13)
b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)
yhat <- b0 + b1 * x
SCT <- sum((y - mean(y))^2); SCE <- sum((yhat - mean(y))^2); SCR <- sum((y - yhat)^2)
c(SCT, SCE, SCR)
round(c(R2 = SCE / SCT, r2 = cor(x, y)^2), 4)

# ---------- [jtrain-r2] Un R2 faible dans une expérience, JTRAIN2 ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("jtrain2")
m_jt <- lm(re78 ~ train, data = jtrain2)
round(coef(m_jt), 2); round(summary(m_jt)$r.squared, 3)

# ---------- [reg-table] Première estimation sur ECONMATH ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
m1 <- lm(score ~ colgpa, data = econmath)
m2 <- lm(score ~ actmth, data = econmath)         # 42 valeurs manquantes écartées
summary(m1)$coefficients[, c(1, 2, 4)]; round(summary(m1)$r.squared, 3); nobs(m1)   # 4e colonne : p-valeurs
summary(m2)$coefficients[, c(1, 2, 4)]; round(summary(m2)$r.squared, 3); nobs(m2)
round(predict(m1, newdata = data.frame(colgpa = 3)), 2)
range(econmath$colgpa)

# ---------- [droite] Droite des MCO sur le nuage ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
m1 <- lm(score ~ colgpa, data = econmath)
plot(econmath$colgpa, econmath$score, pch = 16, cex = 0.5,
     col = adjustcolor("#1F4E79", 0.35), xaxt = "n",
     xlab = "Moyenne universitaire (colgpa, sur 4)", ylab = "Note finale (score, %)")
axis(1, at = axTicks(1), labels = sub(".", ",", axTicks(1), fixed = TRUE))
abline(m1, col = "#C0502B", lwd = 2)
points(mean(econmath$colgpa), mean(econmath$score), pch = 15, col = "#C0502B", cex = 1.2)
round(c(mean(econmath$colgpa), mean(econmath$score)), 3)

# ---------- [residus] Valeurs prédites et résidus sur ECONMATH ----------
if (!requireNamespace("wooldridge", quietly = TRUE)) install.packages("wooldridge")
library(wooldridge)
data("econmath")
m1 <- lm(score ~ colgpa, data = econmath)
econmath$score_chap <- fitted(m1)                 # valeurs prédites
econmath$u_chap <- resid(m1)                      # résidus
round(head(econmath[, c("colgpa", "score", "score_chap", "u_chap")], 5), 2)
round(sum(resid(m1)), 8)
SCT <- sum((econmath$score - mean(econmath$score))^2)
round(c(SCT = SCT, SCE = sum((fitted(m1) - mean(econmath$score))^2), SCR = sum(resid(m1)^2)), 1)
