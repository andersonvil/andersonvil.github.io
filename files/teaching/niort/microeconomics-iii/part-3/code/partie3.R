# =============================================================
# Microéconomie III, Partie III (séances V à VIII). Code R
# des figures, tables et calculs du support.
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# (environnement d'exécution R) ou dans RStudio et l'exécuter.
# R de base suffit, aucun paquet n'est nécessaire.
# =============================================================

# ---------- [fil-loterie] Fil rouge, espérance et écart-type ----------
W  <- c(3600, 10000)                 # patrimoine avec et sans sinistre (euros)
pi <- c(0.1, 0.9)                    # probabilités des deux états
E  <- sum(pi * W)                    # espérance
V  <- sum(pi * (W - E)^2)            # variance
cat("E[W] =", E, " Var(W) =", V, " sigma =", sqrt(V), "\n")
# avec deux issues, Var(W) = pi1 * pi2 * (W2 - W1)^2
cat("formule à deux issues :", pi[1] * pi[2] * (W[2] - W[1])^2, "\n")

# ---------- [petrole] Espérance, l'exemple du pétrole ----------
pi <- c(1/4, 3/4)                    # succès, échec
X  <- c(40, 20)                      # cours de l'action (dollars)
cat("Espérance du cours :", sum(pi * X), "dollars par action\n")

# ---------- [emplois] Deux emplois d'été, variance et écart-type ----------
emplois <- list("Emploi 1, commission"   = list(pi = c(0.5, 0.5),   X = c(2000, 1000)),
                "Emploi 2, salaire fixe" = list(pi = c(0.99, 0.01), X = c(1510, 510)))
for (nom in names(emplois)) {
  pi <- emplois[[nom]]$pi; X <- emplois[[nom]]$X
  E  <- sum(pi * X)
  ec <- X - E                        # écarts à l'espérance
  V  <- sum(pi * ec^2)
  cat(nom, ": E =", round(E, 2), " écarts =", round(ec, 2),
      " moyenne pondérée des écarts =", round(sum(pi * ec), 10), "\n")
  cat("   carrés des écarts =", round(ec^2, 2), " variance =", round(V, 2),
      " écart-type =", round(sqrt(V), 2), "\n")
}

# ---------- [emplois-mod] Emploi 1 augmenté de 100 dollars ----------
emplois <- list("Emploi 1 modifié" = list(pi = c(0.5, 0.5),   X = c(2100, 1100)),
                "Emploi 2"         = list(pi = c(0.99, 0.01), X = c(1510, 510)))
for (nom in names(emplois)) {
  pi <- emplois[[nom]]$pi; X <- emplois[[nom]]$X
  E  <- sum(pi * X)
  cat(nom, ": revenu espéré =", round(E, 2), " écart-type =",
      round(sqrt(sum(pi * (X - E)^2)), 2), "\n")
}

# ---------- [fig-uniformes] Deux distributions uniformes ----------
x1 <- seq(1000, 2000, by = 100)               # emploi 1, 11 revenus
x2 <- seq(1300, 1700, by = 100)               # emploi 2, 5 revenus
for (nom in c("Emploi 1", "Emploi 2")) {
  x <- if (nom == "Emploi 1") x1 else x2
  p <- rep(1 / length(x), length(x)); E <- sum(p * x)
  cat(nom, ": E =", round(E, 2), " écart-type =", round(sqrt(sum(p * (x - E)^2)), 2), "\n")
}
# chaque distribution tracée en plateau, comme le livre
plot(c(1000, 1000, 2000, 2000), c(0, 1/11, 1/11, 0), type = "l", lwd = 2, col = "#8C8C8C",
     xlim = c(800, 2200), ylim = c(0, 0.25), xlab = "Revenu (dollars)", ylab = "Probabilité",
     xaxt = "n", yaxt = "n", bty = "l")
lines(c(1300, 1300, 1700, 1700), c(0, 1/5, 1/5, 0), lwd = 2, col = "#1F4E79")
axis(1, at = c(1000, 1500, 2000), labels = c("1 000", "1 500", "2 000"))
axis(2, at = c(0.1, 0.2), labels = c("0,1", "0,2"), las = 1)
legend("topright", c("Emploi 1", "Emploi 2"), col = c("#8C8C8C", "#1F4E79"), lwd = 2, bty = "n")

# ---------- [amende] Stationnement en double file, amende espérée ----------
amende <- c(6, 50, 500); p <- c(1, 0.1, 0.01)     # amende, probabilité d'être pris
for (k in 1:3)
  cat("amende", amende[k], "dollars, probabilité", p[k], ": amende espérée =",
      sprintf("%.2f", amende[k] * p[k]), "\n")

# ---------- [ticket] Le billet de loterie, consommation contingente ----------
richesse <- 100; prix <- 5; gain <- 200
cat("sans billet :", richesse, richesse,                 # (tiré, pas tiré)
    " avec billet :", richesse - prix + gain, richesse - prix, "\n")

# ---------- [plan-contingent] Le plan de la consommation contingente ----------
W1 <- 3600; W2 <- 10000; g <- 0.1                # dotation, taux de prime
K  <- seq(0, 9000, length.out = 50)               # montant assuré
plot(W1 + (1 - g) * K, W2 - g * K, type = "l", lwd = 2, col = "#1F4E79",
     xlim = c(3000, 11000), ylim = c(3000, 11000), bty = "l",
     xlab = "c1, richesse si sinistre (euros)", ylab = "c2, richesse sans sinistre (euros)")
abline(0, 1, lty = 2, col = "#8C8C8C")            # diagonale c1 = c2
points(W1, W2, pch = 19, col = "#1F4E79"); points(9360, 9360, pch = 19, col = "#C0502B")
text(W1, W2, "dotation, K = 0", pos = 4, cex = 0.8)
text(9360, 9360, "assurance complète, K = 6 400", pos = 1, cex = 0.8)

# ---------- [courbes-eu] Courbes d'indifférence de l'utilité espérée ----------
pi <- 0.1                                         # probabilité du sinistre
c1 <- seq(500, 11000, length.out = 300)
plot(NA, xlim = c(3000, 11500), ylim = c(7000, 11000), bty = "l",
     xlab = "c1, richesse si sinistre (euros)", ylab = "c2, sans sinistre (euros)")
for (U in c(94, 96, 98)) {                         # pi*sqrt(c1) + (1-pi)*sqrt(c2) = U
  lines(c1, ((U - pi * sqrt(c1)) / (1 - pi))^2, lwd = ifelse(U == 96, 2, 1),
        col = ifelse(U == 96, "#1F4E79", "#8C8C8C"))
  text(10700, ((U - pi * sqrt(10700)) / (1 - pi))^2, paste("EU =", U), pos = 3, cex = 0.7)
}
abline(0, 1, lty = 2, col = "#8C8C8C")
points(3600, 10000, pch = 19, col = "#1F4E79")
points(9216, 9216, pch = 19, col = "#C0502B")
text(9216, 9216, "(9 216 ; 9 216), pente -1/9", pos = 1, cex = 0.8)

# ---------- [eu-fil] Utilité espérée, fil rouge et exemple de la Partie I ----------
u  <- sqrt                                        # utilité de von Neumann-Morgenstern
EU <- 0.1 * u(3600) + 0.9 * u(10000)
cat("Inès : EU =", round(EU, 2), " u(E[W]) = u(9 360) =", round(u(9360), 2), "\n")
TMS_dot <- (0.1 / (2 * sqrt(3600))) / (0.9 / (2 * sqrt(10000)))
cat("TMS en la dotation =", round(TMS_dot, 4),
    " sur la diagonale =", round(0.1 / 0.9, 4), "\n")
# exemple de la Partie I : 85 000 euros certains ou (100 000 ; 40 000) avec (3/4 ; 1/4)
cat("85 000 certains :", round(u(85000), 2),
    " loterie :", round(0.75 * u(100000) + 0.25 * u(40000), 2), "\n")

# ---------- [affine] Transformations affines et non affines ----------
pi <- c(0.1, 0.9); X <- c(3600, 10000)            # loterie d'Inès sans assurance
certain <- 9300                                    # 9 300 euros certains
fonctions <- list("u = racine(W)"           = function(w) sqrt(w),
                  "v = 2 racine(W) + 5"     = function(w) 2 * sqrt(w) + 5,
                  "w = (racine(W))^4 = W^2" = function(w) sqrt(w)^4)
for (nom in names(fonctions)) {
  f <- fonctions[[nom]]; EL <- sum(pi * f(X))
  choix <- ifelse(f(certain) > EL, "certain", "loterie")
  f2 <- function(v) format(round(v, 2), big.mark = ",", nsmall = 2)
  cat(sprintf("%-24s EU(loterie) = %s  u(9 300) = %s  -> %s\n", nom,
      f2(EL), f2(f(certain)), choix))
}

# ---------- [attitudes-pr] Trois attitudes face au risque ----------
courbe <- function(x, y) {           # courbe lisse (Hermite) passant par les points du livre
  n <- length(x); h <- diff(x); s <- diff(y) / h
  d <- c(0, 2 / (1 / s[-(n - 1)] + 1 / s[-1]), 0)
  d[1] <- max(0, (3 * s[1] - d[2]) / 2); d[n] <- max(0, (3 * s[n - 1] - d[n - 1]) / 2)
  X <- seq(x[1], x[n], length.out = 300); k <- pmin(findInterval(X, x), n - 1); t <- (X - x[k]) / h[k]
  list(x = X, y = (2*t^3 - 3*t^2 + 1)*y[k] + (t^3 - 2*t^2 + t)*h[k]*d[k] + (3*t^2 - 2*t^3)*y[k+1] + (t^3 - t^2)*h[k]*d[k+1])
}
panneaux <- list("(a) averse au risque" = list(c(0, 10, 15, 16, 20, 30), c(0, 10, 13.5, 14, 16, 18), c("A", "B", "C", "D", "E")),
                 "(b) goût pour le risque" = list(c(0, 10, 20, 30), c(0, 3, 8, 18), c("A", "C", "E")),
                 "(c) neutre au risque" = list(c(0, 10, 20, 30), c(0, 6, 12, 18), c("A", "C", "E")))
layout(matrix(1:3, 1), widths = c(1.6, 1, 1))
for (nom in names(panneaux)) {
  W <- panneaux[[nom]][[1]]; u <- panneaux[[nom]][[2]]; m <- length(u)
  EU <- 0.5 * u[W == 10] + 0.5 * u[m]                     # loterie 10 ou 30, proba 1/2
  cat(nom, ": EU =", EU, " u(20) =", u[W == 20], "\n")
  plot(courbe(W, u), type = "l", lwd = 2, col = "#1F4E79", xlim = c(0, 33), ylim = c(0, 20), xaxs = "i", yaxs = "i",
       axes = FALSE, main = nom, xlab = "Revenu (milliers de dollars)", ylab = "Utilité")
  axis(1, at = W[-1]); axis(2, at = u[-1], labels = sub(".", ",", u[-1], fixed = TRUE), las = 1); box(bty = "l")
  segments(0, u[-1], W[-1], u[-1], lty = 2, col = "#8C8C8C"); segments(W[-1], 0, W[-1], u[-1], lty = 2, col = "#8C8C8C")
  lines(c(10, 30), c(u[W == 10], u[m]), lty = 2, col = "#8C8C8C")   # pointillés et corde, comme le livre
  points(W[-1], u[-1], pch = 19, col = "#1F4E79"); points(20, EU, pch = 15, col = "#C0502B")
  text(W[-1], u[-1], panneaux[[nom]][[3]], pos = 2, cex = 0.8)
  if (nom == "(a) averse au risque") { text(20, 14, "F", pos = 4, cex = 0.8); segments(16, 14, 20, 14, lty = 2, col = "#8C8C8C") }
}
layout(1)
cat("emploi actuel, 15 000 dollars certains : u = 13,5 < EU = 14\n")

# ---------- [prime-pr] Prime de risque, deux emplois risqués ----------
courbe <- function(x, y) {           # courbe lisse (Hermite) passant par les points du livre
  n <- length(x); h <- diff(x); s <- diff(y) / h
  d <- c(0, 2 / (1 / s[-(n - 1)] + 1 / s[-1]), 0)
  d[1] <- max(0, (3 * s[1] - d[2]) / 2); d[n] <- max(0, (3 * s[n - 1] - d[n - 1]) / 2)
  X <- seq(x[1], x[n], length.out = 300); k <- pmin(findInterval(X, x), n - 1); t <- (X - x[k]) / h[k]
  list(x = X, y = (2*t^3 - 3*t^2 + 1)*y[k] + (t^3 - 2*t^2 + t)*h[k]*d[k] + (3*t^2 - 2*t^3)*y[k+1] + (t^3 - t^2)*h[k]*d[k+1])
}
W <- c(0, 10, 16, 30, 40); u <- c(0, 10, 14, 18, 20)      # points O, A, C, E, G du livre (milliers de dollars)
for (cas in list(list("10 ou 30", 0.5 * 10 + 0.5 * 18), list("0 ou 40", 0.5 * 0 + 0.5 * 20))) {
  EU <- cas[[2]]; EC <- approx(u, W, xout = EU)$y          # revenu certain de même utilité
  cat(cas[[1]], ": EU =", EU, " équivalent certain =", EC, " prime =", 20 - EC, "\n")
}
px <- c(10, 16, 20, 30, 40); py <- c(10, 14, 14, 18, 20)   # A, C, F, E, G
plot(courbe(W, u), type = "l", lwd = 2, col = "#1F4E79", xlim = c(0, 42), ylim = c(0, 22), xaxs = "i", yaxs = "i",
     axes = FALSE, xlab = "Revenu (milliers de dollars)", ylab = "Utilité")
axis(1, at = c(10, 16, 20, 30, 40)); axis(2, at = c(10, 14, 18, 20), las = 1); box(bty = "l")
segments(0, py, px, py, lty = 2, col = "#8C8C8C"); segments(px, 0, px, py, lty = 2, col = "#8C8C8C")
lines(c(10, 30), c(10, 18), lty = 2, col = "#8C8C8C")     # corde AE, F en est le milieu
segments(16, 14, 20, 14, lwd = 3, col = "#C0502B"); text(18, 14, "prime 4", pos = 1, col = "#C0502B", cex = 0.8)
points(px, py, pch = c(19, 19, 15, 19, 19), col = c(rep("#1F4E79", 2), "#C0502B", rep("#1F4E79", 2)))
text(px, py, c("A", "C", "F", "E", "G"), pos = 3, cex = 0.8)

# ---------- [prime-fil] Équivalent certain et prime de risque d'Inès ----------
W1 <- 3600; W2 <- 10000; pi <- 0.1
E  <- pi * W1 + (1 - pi) * W2                   # 9 360
EU <- pi * sqrt(W1) + (1 - pi) * sqrt(W2)
EC <- EU^2                                      # u(EC) = EU, donc EC = EU^2
cat("E[W] =", E, " EU =", EU, " EC =", EC, " prime rho =", E - EC, "\n")
par(mfrow = c(1, 2))
for (lim in list(c(2500, 10600), c(9100, 9420))) {
  w <- seq(lim[1], lim[2], length.out = 300)
  plot(w, sqrt(w), type = "l", lwd = 2, col = "#1F4E79", bty = "l",
       xlab = "Richesse W (euros)", ylab = expression(u(W) == sqrt(W)))
  lines(c(W1, W2), sqrt(c(W1, W2)), lty = 2, col = "#8C8C8C")
  segments(EC, EU, E, EU, lwd = 3, col = "#C0502B")
  points(E, sqrt(E), pch = 19, col = "#1F4E79"); points(E, EU, pch = 15, col = "#C0502B")
}
text(9250, 96.1, expression(rho == 144), col = "#C0502B"); par(mfrow = c(1, 1))

# ---------- [trois-agents] Trois attitudes sur la loterie d'Inès ----------
W <- c(3600, 10000); pi <- c(0.1, 0.9); E <- sum(pi * W)
agents <- list("averse, u = racine(W)"   = list(u = sqrt, inv = function(v) v^2),
               "neutre, u = W"           = list(u = function(w) w, inv = function(v) v),
               "goût du risque, u = W^2" = list(u = function(w) w^2, inv = sqrt))
for (nom in names(agents)) {
  EU <- sum(pi * agents[[nom]]$u(W))
  EC <- agents[[nom]]$inv(EU)                     # équivalent certain
  cat(sprintf("%-24s EC = %9.1f   prime rho = E[W] - EC = %7.1f\n", nom, EC, E - EC))
}

# ---------- [courbes-mv] Courbes d'indifférence espérance et écart-type ----------
s <- seq(0, 5, length.out = 100)                 # écart-type du revenu
par(mfrow = c(1, 2))
for (cas in list(list(0.40, "(a) forte aversion au risque"),
                 list(0.08, "(b) faible aversion au risque"))) {
  plot(NA, xlim = c(0, 5.6), ylim = c(9, 25), xaxt = "n", yaxt = "n", bty = "l",
       main = cas[[2]], xlab = "Écart-type du revenu", ylab = "Revenu espéré")
  for (k in c(10, 12, 14)) {
    lines(s, k + cas[[1]] * s^2, lwd = 1.6, col = "#1F4E79")   # E = k + c sigma^2
    text(5.3, k + cas[[1]] * 25, paste0("U", (k - 8) / 2), cex = 0.8)
  }
}
par(mfrow = c(1, 1))

# ---------- [climatiseurs] Diversifier ses ventes ----------
pi    <- c(0.5, 0.5)                          # été chaud, été froid
clim  <- c(30000, 12000)                      # revenu si l'on ne vend que des climatiseurs
chauf <- c(12000, 30000)                      # revenu si l'on ne vend que des radiateurs
cas <- list("climatiseurs seuls" = clim, "radiateurs seuls" = chauf,
            "moitié-moitié" = 0.5 * clim + 0.5 * chauf)
for (nom in names(cas)) {
  R <- cas[[nom]]; E <- sum(pi * R)
  cat(sprintf("%-20s revenus = %s  E = %.0f  écart-type = %.0f\n", nom,
      paste(R, collapse = " "), E, sqrt(sum(pi * (R - E)^2))))
}

# ---------- [correlation] Diversification et corrélation ----------
rho   <- seq(-1, 1, length.out = 201)
sigma <- 9000 * sqrt((1 + rho) / 2)           # écart-type du revenu moitié-moitié
for (r in c(-1, -0.5, 0, 0.5, 1))
  cat(sprintf("rho = %5.1f   écart-type = %7.1f\n", r, 9000 * sqrt((1 + r) / 2)))
cat("baisse de l'écart-type pour rho = 0 :", round(1 - sqrt(0.5), 3), "\n")
plot(rho, sigma, type = "l", lwd = 2, col = "#1F4E79", bty = "l",
     xlab = "Corrélation entre les deux activités",
     ylab = "Écart-type du revenu (dollars)")
points(c(-1, 0, 1), c(0, 9000 * sqrt(0.5), 9000), pch = 19, col = "#C0502B")
abline(h = 9000, lty = 2, col = "#8C8C8C")
text(-0.6, 8600, "une seule activité : 9 000", col = "#8C8C8C", cex = 0.8)

# ---------- [cambriolage] La décision de s'assurer ----------
pi  <- c(0.1, 0.9)                            # cambriolage, pas de cambriolage
cas <- list("sans assurance" = c(40000, 50000),
            "avec assurance (prime 1 000)" = c(49000, 49000))
for (nom in names(cas)) {
  W <- cas[[nom]]; E <- sum(pi * W)
  cat(sprintf("%-30s E = %.0f   écart-type = %.0f\n", nom, E, sqrt(sum(pi * (W - E)^2))))
}
cat("perte espérée =", 0.1 * 10000, " = prime actuariellement équitable\n")

# ---------- [assurance-equitable] Prime équitable, assurance complète ----------
W1 <- 3600; W2 <- 10000; pi <- 0.1; g <- 0.1
K  <- seq(0, 9000, length.out = 50)
c1 <- seq(3000, 11500, length.out = 300)
plot(W1 + (1 - g) * K, W2 - g * K, type = "l", lwd = 2, col = "#1F4E79", bty = "l",
     xlim = c(3000, 11500), ylim = c(8900, 10250),
     xlab = "c1, richesse si sinistre (euros)", ylab = "c2, sans sinistre (euros)")
lines(c1, ((96 - pi * sqrt(c1)) / (1 - pi))^2, col = "#8C8C8C")
lines(c1, ((sqrt(9360) - pi * sqrt(c1)) / (1 - pi))^2, col = "#C0502B")
abline(0, 1, lty = 2, col = "#8C8C8C")
points(W1, W2, pch = 19, col = "#1F4E79"); points(9360, 9360, pch = 19, col = "#C0502B")
text(9360, 9360, "optimum (9 360 ; 9 360)", pos = 2, cex = 0.8)
cat("EU sans assurance = 96, EU assurance complète =", round(sqrt(9360), 3), "\n")

# ---------- [assurance-chargee] Prime chargée, assurance partielle ----------
W1 <- 3600; W2 <- 10000; pi <- 0.1
K  <- seq(0, 8000, length.out = 801)
plot(NA, xlim = c(0, 8000), ylim = c(95.9, 96.8), bty = "l",
     xlab = "Montant assuré K (euros)", ylab = "Utilité espérée")
for (g in c(0.10, 0.12)) {
  col <- ifelse(g == 0.10, "#1F4E79", "#C0502B")
  lines(K, pi * sqrt(W1 + (1 - g) * K) + (1 - pi) * sqrt(W2 - g * K), lwd = 2, col = col)
  k2  <- ((1 - pi) * g / (pi * (1 - g)))^2         # c2 / c1 à l'optimum
  Ks  <- (W2 - k2 * W1) / (g + k2 * (1 - g))       # montant assuré optimal
  EUs <- pi * sqrt(W1 + (1 - g) * Ks) + (1 - pi) * sqrt(W2 - g * Ks)
  cat(sprintf("gamma = %.2f   K* = %7.1f   EU(K*) = %.3f   EU(0) = 96\n", g, Ks, EUs))
  points(Ks, EUs, pch = 19, col = col)
}
legend("bottomright", c("gamma = 0,10", "gamma = 0,12"),
       col = c("#1F4E79", "#C0502B"), lwd = 2, bty = "n")

# ---------- [mutualisation] Coût moyen par assuré, loi exacte ----------
pi <- 0.1; L <- 6400
par(mfrow = c(1, 4))
for (N in c(1, 10, 100, 1000)) {
  S <- 0:N                                       # nombre de sinistres
  cout <- L * S / N; prob <- dbinom(S, N, pi)    # coût par assuré et probabilité
  s <- sqrt(sum(prob * (cout - 640)^2))
  queue <- 1 - pbinom(floor(800 * N / L), N, pi)  # P(coût > 800)
  cat(sprintf("N = %5d  moyenne = %6.1f  écart-type = %7.1f  P(coût > 800) = %.4f\n",
      N, sum(cout * prob), s, queue))
  plot(cout, prob, type = "h", lwd = 1.5, col = "#1F4E79", xlim = c(-300, 6900),
       yaxt = "n", bty = "n", main = paste("N =", N),
       xlab = "Coût moyen par assuré (euros)", ylab = "")
  abline(v = 640, lty = 2, col = "#C0502B")
}
par(mfrow = c(1, 1))
entre <- pbinom(125, 1000, pi) - pbinom(78, 1000, pi)
cat("N = 1000, P(500 <= coût <= 800) =", round(entre, 4), "\n")

# ---------- [titre] Assurance de titre, enchère d'un acheteur neutre ----------
valeur <- 300000; p_probleme <- 1 / 20
enchere <- (1 - p_probleme) * valeur + p_probleme * 0
cat("enchère maximale d'un acheteur neutre au risque :", enchere, "dollars\n")
cat("perte espérée sans assurance :", p_probleme * valeur, " frais d'assurance : 1 500\n")

# ---------- [costumes] Valeur de l'information ----------
profit <- list("commander 50" = c(5000, 5000),          # ventes de 50, ventes de 100
               "commander 100" = c(1500, 12000))
pi <- c(0.5, 0.5)
for (nom in names(profit))
  cat(sprintf("%-14s profit espéré = %.0f\n", nom, sum(pi * profit[[nom]])))
info <- sum(pi * c(5000, 12000))                # on commande juste dans chaque état
cat("avec information complète :", info, "  valeur de l'information :", info - 6750, "\n")

# ---------- [rendements] Rendement nominal et réel ----------
actifs <- list("obligation" = c(1000, 1000, 100),        # (prix début, prix fin, flux)
               "immeuble (millions)" = c(10, 11, 0.5),
               "action GM" = c(80, 72, 4))
inflation <- 0.05
for (nom in names(actifs)) {
  a <- actifs[[nom]]; r <- (a[2] - a[1] + a[3]) / a[1]
  cat(sprintf("%-20s rendement = %5.1f%%   rendement réel = %5.1f%%\n",
      nom, 100 * r, 100 * (r - inflation)))
}

# ---------- [table58] Risque et rendement, 1926 à 2010 ----------
t <- data.frame("rendement moyen" = c(11.9, 6.2, 3.7),
                "rendement réel moyen" = c(8.7, 3.3, 0.7),
                "écart-type" = c(20.4, 8.3, 3.1), check.names = FALSE,
                row.names = c("actions (S&P 500)", "obligations d'entreprise",
                              "bons du Trésor"))
print(t)
cat("supplément de rendement réel sur les bons du Trésor :\n")
ex <- round(t[["rendement réel moyen"]] - t["bons du Trésor", "rendement réel moyen"], 1)
cat(paste(rownames(t), format(ex, nsmall = 1)), sep = "\n")

# ---------- [premier-euro] Combien investir dans l'actif risqué ----------
w <- 100                                          # richesse, u = ln
x <- seq(0, 60, length.out = 300)                 # montant investi
par(mfrow = c(1, 2))
for (cas in list(list(0.3, -0.5, "A. rendement espéré -0,1"),
                 list(0.5, -0.4, "B. rendement espéré +0,05"))) {
  rg <- cas[[1]]; rb <- cas[[2]]
  EU <- 0.5 * log(w + x * rg) + 0.5 * log(w + x * rb)
  xs <- max(0, -w * (rg + rb) / (2 * rg * rb))    # condition du premier ordre
  cat(cas[[3]], ": x* =", round(xs, 2), "\n")
  plot(x, EU, type = "l", lwd = 2, col = "#1F4E79", yaxt = "n", bty = "l", main = cas[[3]],
       xlab = "Montant investi x", ylab = "Utilité espérée")
  points(xs, 0.5 * log(w + xs * rg) + 0.5 * log(w + xs * rb), pch = 19, col = "#C0502B")
}
par(mfrow = c(1, 1))

# ---------- [droite-budget] La droite budgétaire risque et rendement ----------
rf <- 4; rm <- 12; sm <- 20                     # en %, sigma_m = 20 est une hypothèse
p  <- (rm - rf) / sm                            # prix du risque
cat("prix du risque p =", p, "\n")
for (x in c(0, 0.5, 1, 1.5))
  cat(sprintf("x = %3.1f  r_x = %5.1f %%  sigma_x = %5.1f %%\n",
      x, x * rm + (1 - x) * rf, x * sm))
s <- seq(0, 32, length.out = 100)
plot(s, rf + p * s, type = "l", lwd = 2, col = "#1F4E79", bty = "l",
     xlab = "Écart-type du rendement (%)", ylab = "Rendement espéré (%)")
points(c(0, 10, 20, 30), c(4, 8, 12, 16), pch = 19, col = "#1F4E79")
text(c(0, 10, 20, 30), c(4, 8, 12, 16), paste("x =", c("0", "0,5", "1", "1,5")),
     pos = 4, cex = 0.8)

# ---------- [deux-investisseurs] Deux investisseurs, deux portefeuilles ----------
rf <- 0.04; rm <- 0.12; sm <- 0.20
s <- seq(0, 0.5, length.out = 200)
plot(100 * s, 100 * (rf + (rm - rf) / sm * s), type = "l", lwd = 2, col = "#1F4E79",
     xlim = c(0, 50), ylim = c(0, 30), bty = "l",
     xlab = "Écart-type du rendement (%)", ylab = "Rendement espéré (%)")
for (cas in list(list(4, "A", "#8C8C8C"), list(1, "B", "#C0502B"))) {
  a <- cas[[1]]
  x <- (rm - rf) / (a * sm^2)                     # part optimale de l'actif risqué
  r <- rf + x * (rm - rf); sig <- x * sm
  U <- r - a / 2 * sig^2                          # utilité atteinte
  cat(sprintf("investisseur %s (a = %d) : x* = %.2f  r = %.1f %%  sigma = %.1f %%\n",
      cas[[2]], a, x, 100 * r, 100 * sig))
  lines(100 * s, 100 * (U + a / 2 * s^2), col = cas[[3]], lwd = 1.4)
  points(100 * sig, 100 * r, pch = 19, col = cas[[3]])
  text(100 * sig, 100 * r, cas[[2]], pos = 4)
}
points(20, 12, pch = 15, col = "#1F4E79"); text(20, 12, "marché", pos = 4, cex = 0.8)

# ---------- [actifs-xy] Choisir l'actif risqué à combiner ----------
rf <- 0.04; a <- 5                                 # actif sûr, aversion au risque
actifs <- list(X = c(0.08, 0.10), Y = c(0.14, 0.20))  # (rendement espéré, écart-type)
for (nom in names(actifs)) {
  r <- actifs[[nom]][1]; s <- actifs[[nom]][2]
  tout <- r - a / 2 * s^2                          # tout placer dans l'actif risqué
  x <- (r - rf) / (a * s^2)                        # meilleur mélange avec l'actif sûr
  melange <- rf + x * (r - rf) - a / 2 * (x * s)^2
  cat(sprintf("%s : prix du risque = %.2f  U(tout) = %.4f  x* = %.2f  U(mélange) = %.4f\n",
      nom, (r - rf) / s, tout, x, melange))
}

# ---------- [actifs-ab] Deux actifs corrélés négativement ----------
pi <- c(0.5, 0.5)
A <- c(10, -5); B <- c(-5, 10)                     # valeurs dans les deux états
cas <- list("A seul" = A, "B seul" = B, "A et B" = A + B)
for (nom in names(cas)) {
  X <- cas[[nom]]; E <- sum(pi * X)
  cat(sprintf("%-7s valeurs = %s  espérance = %5.2f  écart-type = %.2f\n", nom,
      paste(X, collapse = " "), E, sqrt(sum(pi * (X - E)^2))))
}

# ---------- [beta] Calculer un bêta ----------
rm <- c(-10, 0, 10, 20)                       # marché (%), 4 états équiprobables
actifs <- list(D = c(-2, -1, 4, 13), C = c(-13, -4, 11, 32), Z = c(12, -8, -8, 12))
covp <- function(x, y) mean((x - mean(x)) * (y - mean(y)))   # covariance (division par 4)
cat(sprintf("marché : moyenne = %.1f  écart-type = %.2f\n", mean(rm), sqrt(covp(rm, rm))))
for (nom in names(actifs)) {
  r <- actifs[[nom]]
  cat(sprintf("%s : moyenne = %5.2f  écart-type = %6.2f  bêta = cov/var = %.2f\n",
      nom, mean(r), sqrt(covp(r, r)), covp(r, rm) / covp(rm, rm)))
}

# ---------- [droite-marche] La droite de marché ----------
rf <- 2; rm <- 5                                 # en %
b <- seq(0, 2, length.out = 50)
plot(b, rf + b * (rm - rf), type = "l", lwd = 2, col = "#1F4E79", bty = "l",
     xlab = "Bêta", ylab = "Rendement espéré (%)", ylim = c(1.5, 8))
points(c(0, 0.5, 1, 1.5), c(2, 3.5, 5, 6.5), pch = 19, col = "#1F4E79")
text(c(0, 0.5, 1, 1.5), c(2, 3.5, 5, 6.5), c("Z", "D", "M", "C"), pos = 4)
points(1, 7, pch = 19, col = "#C0502B"); text(1, 7, "S", pos = 4, col = "#C0502B")
arrows(1, 6.8, 1, 5.2, length = 0.08, col = "#C0502B")
for (beta in c(0, 0.5, 1, 1.5))
  cat(sprintf("bêta = %3.1f  rendement d'équilibre = %.1f %%\n",
      beta, rf + beta * (rm - rf)))

# ---------- [ajustement] Comment le prix s'ajuste ----------
rf <- 0.02; rm <- 0.05; beta <- 1.0
p1 <- 107                                        # prix attendu dans un an (euros)
r_requis <- rf + beta * (rm - rf)
cat(sprintf("rendement requis = %.1f%%\n", 100 * r_requis))
cat(sprintf("au prix de 100 euros, rendement espéré = %.1f%%\n", 100 * (p1 - 100) / 100))
cat(sprintf("prix d'équilibre = %.2f euros\n", p1 / (1 + r_requis)))

# ---------- [fonds] Fonds gérés et fonds indiciel ----------
x <- 2026; y <- 10; z <- 9; u <- numeric(60)      # Wichmann et Hill, graine fixée
for (k in 1:60) {
  x <- (171 * x) %% 30269; y <- (172 * y) %% 30307; z <- (170 * z) %% 30323
  u[k] <- (x / 30269 + y / 30307 + z / 30323) %% 1
}
rf <- 2; rm <- 6                                 # en %
beta <- 0.6 + 0.8 * u[1:30]                      # 30 fonds fictifs
r <- rf + beta * (rm - rf) - 1.2 + 3 * (u[31:60] - 0.5)   # frais et hasard
cat("fonds sous la droite :", sum(r < rf + beta * (rm - rf)), "sur 30\n")
plot(beta, r, pch = 19, col = "#8C8C8C", xlim = c(0, 1.6), ylim = c(1.5, 8.6), bty = "l",
     xlab = "Bêta", ylab = "Rendement moyen (%)")
lines(c(0, 1.6), c(rf, rf + 1.6 * (rm - rf)), lwd = 2, col = "#1F4E79")
points(1, rm, pch = 15, col = "#C0502B")
text(1, rm, "fonds indiciel", pos = 4, col = "#C0502B")

# ---------- [cascade] Cascades informationnelles ----------
q <- seq(0.5, 1, length.out = 101)
faux <- (1 - q)^2 / (q^2 + (1 - q)^2)            # probabilité de cascade erronée
x <- 2026; y <- 10; z <- 9; freq <- c()           # Wichmann et Hill, graine fixée
for (qq in c(0.55, 0.6, 0.7, 0.8, 0.9)) {
  erreurs <- 0
  for (s in 1:10000) {                           # 10 000 files de 20 personnes
    d <- 0                                       # avance de A parmi les signaux révélés
    for (i in 1:20) {
      x <- (171 * x) %% 30269; y <- (172 * y) %% 30307; z <- (170 * z) %% 30323
      u <- (x / 30269 + y / 30307 + z / 30323) %% 1
      s_ <- ifelse(u < qq, 1, -1)                # signal, +1 pour A (le bon choix)
      choix <- if (abs(d) < 2) s_ else sign(d)   # au-delà de 2, on imite
      if (abs(d) < 2) d <- d + s_
    }
    erreurs <- erreurs + (choix < 0)             # la dernière personne choisit B
  }
  freq <- c(freq, erreurs / 10000)
  cat(sprintf("q = %.2f  simulation = %.4f  théorie = %.4f\n", qq, erreurs / 10000,
      (1 - qq)^2 / (qq^2 + (1 - qq)^2)))
}
plot(q, faux, type = "l", lwd = 2, col = "#1F4E79", bty = "l",
     xlab = "Précision du signal privé q", ylab = "Proba. de cascade erronée")
points(c(0.55, 0.6, 0.7, 0.8, 0.9), freq, pch = 19, col = "#C0502B")

# ---------- [pelles] Demande de pelles à neige et équité ----------
P <- seq(0, 50, length.out = 100)
plot(100 - 2 * P, P, type = "l", lwd = 2, col = "#8C8C8C", xlim = c(0, 165),
     ylim = c(0, 50), xaxt = "n", yaxt = "n", bty = "l",
     xlab = "Quantité de pelles", ylab = "Prix (dollars)")
lines(c(160, 110, 0), c(0, 25, 30), lwd = 2, col = "#1F4E79")       # D2, après la tempête
abline(h = c(20, 25, 40), lty = 3, col = "#8C8C8C"); axis(2, at = c(20, 25, 40), las = 1)
points(c(60, 120), c(20, 20), pch = 19, col = "#C0502B")
text(c(60, 120), c(20, 20), c("Q1", "Q2"), pos = 3)
text(c(5, 4), c(46, 31.5), c("D1", "D2"))

# ---------- [emploi-risque] Un emploi sûr ou un emploi risqué ----------
u <- c("10" = 10, "15" = 13.5, "20" = 16, "30" = 18)  # revenus en milliers de dollars
EU <- 0.5 * u[["10"]] + 0.5 * u[["30"]]               # offre risquée, 10 ou 30
cat("offre risquée : E =", 0.5 * 10 + 0.5 * 30, " EU =", EU, "\n")
cat("15 000 certains : u =", u[["15"]], "  20 000 certains : u =", u[["20"]], "\n")

# ---------- [avous] Racine ou logarithme ----------
pi <- c(0.1, 0.9); W <- c(3600, 10000)
for (nom in c("racine", "logarithme")) {
  u <- if (nom == "racine") sqrt else log
  EU <- sum(pi * u(W))
  choix <- ifelse(EU > u(9200), "garde le risque", "prend 9 200 certains")
  cat(sprintf("%-10s EU = %.3f  u(9 200) = %.3f  -> %s\n", nom, EU, u(9200), choix))
}

# ---------- [decomposition] Prime maximale pour une assurance complète ----------
EC <- (0.1 * sqrt(3600) + 0.9 * sqrt(10000))^2      # équivalent certain
cat("prime maximale =", round(10000 - EC, 2), "\n")
cat("dont perte espérée =", 0.1 * 6400,
    " et prime de risque =", round(9360 - EC, 2), "\n")

# ---------- [portefeuille-pr] Rendement et risque du portefeuille ----------
rf <- 0.04; rm <- 0.12; sm <- 0.20; x <- 0.5    # sigma_m = 20 % est une hypothèse
cat(sprintf("r_x = %.1f %%   sigma_x = %.1f %%\n",
            100 * (x * rm + (1 - x) * rf), 100 * x * sm))

# ---------- [mv-simple] Part optimale de l'actif risqué ----------
rf <- 0.04; rm <- 0.12; sm <- 0.20
for (a in c(1, 2, 4, 8)) cat(sprintf("a = %d   x* = %.3f\n", a, (rm - rf) / (a * sm^2)))

# ---------- [res-portefeuille] Probabilité d'une forte perte annuelle ----------
cas <- list(A = c(0.08, 0.10), B = c(0.20, 0.40))   # rendements supposés normaux
for (nom in names(cas)) {
  z_ <- (-0.40 - cas[[nom]][1]) / cas[[nom]][2]
  cat(sprintf("%s : P(rendement < -40 %%) = %.4f\n", nom, pnorm(z_)))
}

# ---------- [formule-cascade] Probabilité d'une cascade erronée ----------
for (q in c(0.6, 0.7, 0.8))
  cat(sprintf("q = %.1f   P(cascade erronée) = %.4f\n", q, (1 - q)^2 / (q^2 + (1 - q)^2)))

# ---------- [alt-petrole] Exemple alternatif, action de biotechnologie ----------
cat("espérance du cours :", round(0.3 * 80 + 0.7 * 35, 2), "euros\n")

# ---------- [alt-emplois] Exemple alternatif, deux emplois étudiants ----------
emplois <- list("A" = list(c(0.5, 0.5), c(1800, 1000)),
                "B" = list(c(0.9, 0.1), c(1500, 500)),
                "A + 100" = list(c(0.5, 0.5), c(1900, 1100)))
for (nom in names(emplois)) {
  pi <- emplois[[nom]][[1]]; X <- emplois[[nom]][[2]]
  E <- sum(pi * X); V <- sum(pi * (X - E)^2)
  cat(sprintf("%-8s E = %.0f  variance = %.0f  écart-type = %.0f\n", nom, E, V, sqrt(V)))
}

# ---------- [alt-amende] Exemple alternatif, fraude dans les transports ----------
p <- c(0.05, 0.05, 0.02, 0.02); amende <- c(30, 50, 50, 120)
for (k in 1:4) cat(sprintf("p = %.2f  amende = %d  amende espérée = %.2f  seuil = %.0f\n",
                         p[k], amende[k], p[k] * amende[k], 2 / p[k]))

# ---------- [alt-ticket] Exemple alternatif, jeu à gratter ----------
richesse <- 50; prix <- 2; gain <- 100
cat("avec ticket : gagnant", richesse - prix + gain, " perdant", richesse - prix, "\n")

# ---------- [alt-attitudes] Exemple alternatif, le pari de Varian ----------
EU <- 0.5 * sqrt(5) + 0.5 * sqrt(15)
cat(sprintf("racine : EU = %.3f  u(10) = %.3f  EC = %.2f  rho = %.2f\n",
            EU, sqrt(10), EU^2, 10 - EU^2))
cat("carré  : EU =", 0.5 * 25 + 0.5 * 225, " u(10) =", 100, "\n")

# ---------- [alt-prime-pr] Exemple alternatif, CDD ou indépendant ----------
for (cas in list(c(1000, 3000), c(0, 4000))) {
  EU <- 0.5 * sqrt(cas[1]) + 0.5 * sqrt(cas[2])
  cat(sprintf("%d ou %d : EU = %.2f  EC = %.0f  rho = %.0f\n",
      cas[1], cas[2], EU, EU^2, 2000 - EU^2))
}

# ---------- [alt-divers] Exemple alternatif, lunettes et imperméables ----------
pluie <- c(20, 5); soleil <- c(5, 20)           # valeur d'une action (pluie, soleil)
parts <- list("imperméables" = c(10, 0), "lunettes" = c(0, 10),
              "moitié-moitié" = c(5, 5))
for (nom in names(parts)) {
  v <- parts[[nom]][1] * pluie + parts[[nom]][2] * soleil
  cat(nom, ": valeur =", v, " espérance =", mean(v), "\n")
}

# ---------- [alt-cambriolage] Exemple alternatif, l'assurance selon Varian ----------
W <- 35000; perte <- 10000; pi <- 0.01
cat("sans assurance : E =", round((1 - pi) * W + pi * (W - perte), 2), "\n")
cat("assurance de 10 000, prime 100 :", W - 100, "dans les deux états\n")
cat("1 000 ménages : sinistres attendus =", 1000 * pi,
    " coût moyen par ménage =", 1000 * pi * perte / 1000, "\n")

# ---------- [alt-titre] Exemple alternatif, scooter d'occasion ----------
cat("offre maximale d'une acheteuse neutre :", round(0.95 * 3000, 2), "euros\n")

# ---------- [alt-info] Exemple alternatif, le glacier ----------
rachat <- 0.40
p200 <- rep(3 * 200 - 1.2 * 200, 2)                    # commander 200 (demande 200, 400)
p400 <- c(3 * 200 + rachat * 200 - 400, 3 * 400 - 400) # commander 400
cat("commander 200 :", p200, " espérance =", round(mean(p200), 2), "\n")
cat("commander 400 :", p400, " espérance =", round(mean(p400), 2), "\n")
info <- 0.5 * 360 + 0.5 * 800
cat("information parfaite :", info, " valeur =", round(info - 540, 2), "\n")

# ---------- [alt-rendements] Exemple alternatif, trois placements ----------
actifs <- list(livret = c(1000, 1000, 30), appartement = c(200000, 206000, 8000),
               action = c(40, 36, 1.2))                # (prix début, prix fin, flux)
for (nom in names(actifs)) {
  a <- actifs[[nom]]; r <- (a[2] - a[1] + a[3]) / a[1]
  cat(sprintf("%-12s rendement = %5.1f %%   réel = %5.1f %%\n",
      nom, 100 * r, 100 * (r - 0.02)))
}

# ---------- [alt-portefeuille] Exemple alternatif, livret et fonds indiciel ----------
rf <- 0.03; rm <- 0.08; sm <- 0.15
cat(sprintf("prix du risque = %.4f   x = 0,4 : r = %.1f %%  sigma = %.1f %%\n",
            (rm - rf) / sm, 100 * (rf + 0.4 * (rm - rf)), 100 * 0.4 * sm))
for (a in c(5, 2)) cat(sprintf("a = %d   x* = %.2f\n", a, (rm - rf) / (a * sm^2)))

# ---------- [alt-ab] Exemple alternatif, vélos et cinéma ----------
cas <- list("vélos" = c(12, -4), "cinéma" = c(-4, 12), "les deux" = c(8, 8))
for (nom in names(cas)) {
  X <- cas[[nom]]; E <- mean(X)
  cat(sprintf("%-9s espérance = %.0f  écart-type = %.0f\n", nom, E, sqrt(mean((X - E)^2))))
}

# ---------- [alt-fonds] Exemple alternatif, fonds géré et fonds indiciel ----------
rf <- 2.5; rm <- 6.5; beta <- 1.2
cat(sprintf("fonds indiciel à %.0f %% : rendement = %.1f %%\n",
            100 * beta, rf + beta * (rm - rf)))
cat("fonds géré : rendement = 7 %\n")

# ---------- [alt-bulle] Exemple alternatif, rendement locatif ----------
for (prix in c(120000, 200000))
  cat(sprintf("prix = %d   rendement locatif = %.1f %%\n", prix, 100 * 6000 / prix))

# ---------- [alt-cascade] Exemple alternatif, deux bouchons lyonnais ----------
q <- 0.7
cat("deux premiers avis faux :", round((1 - q)^2, 4), "\n")
cat("cascade sur B :", round((1 - q)^2 / (q^2 + (1 - q)^2), 4), "\n")

# ---------- [alt-taxi] Exemple alternatif, chauffeur de VTC ----------
for (gain_horaire in c(40, 20))
  cat(sprintf("gain horaire %d : heures pour gagner 200 = %.0f\n",
      gain_horaire, 200 / gain_horaire))
