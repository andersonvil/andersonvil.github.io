# simulate_and_test.R -- Base-R companion to python/simulate_and_test.py.
#
#   log q1 = b0 + b1 log p1 + b2 log p2 + b3 log p3 + b4 log Y + u           (1)
#   H0: theta = b1 + b2 + b3 + b4 = 0
#
# Part A  simulates its own data (same DGP, R's RNG) and reproduces every step of the note:
#         imposing (restricted OLS on (3), recovery of b3, numeraire invariance), testing
#         (t on theta, reparametrisation, F from SSRs = t^2, matrix Wald, LR) and the three
#         Monte Carlo experiments (size, power, restricted vs unrestricted b1).
# Part B  re-estimates everything on the common file data/sim_data.csv (written by Python)
#         and checks coefficients, standard errors, t and F against Python to 1e-6.
#
# Run from the project root, after the Python script:  Rscript R/simulate_and_test.R

dir.create("output/figures_R", recursive = TRUE, showWarnings = FALSE)
logf <- file("output/log_R.txt", open = "wt")
say <- function(...) { s <- sprintf(...); cat(s, "\n"); writeLines(s, logf) }
say("%s; ggplot2 available: %s (base graphics used)", R.version.string,
    requireNamespace("ggplot2", quietly = TRUE))

# =============================================================================
# 0. Parameters (identical to the Python script)
# =============================================================================
SEED <- 20261015
R <- 50; T <- 10; J <- 3; K <- J + 2
BETA0 <- 1.0
BETA_H0 <- c(-0.6, 0.2, 0.1, 0.3)        # b1, b2, b3, b4: theta = 0
THETA_H1 <- 0.1                          # under H1 only b4 changes: b4 = 0.3 + theta
SIG_PI <- 0.20; SIG_MU <- 0.15; SIG_EPS <- 0.05; SIG_OM <- 0.30
MEAN_LOGY <- log(2000); SIG_U <- 0.20; ALPHA <- 0.05
N_MC <- 2000; THETA_GRID <- round(seq(-0.15, 0.15, by = 0.01), 2); N_POWER <- 1000
R_POWER <- c(50, 200); SIG_PI_FIG4 <- c(no_inflation = 0, strong_inflation = 1); N_FIG4 <- 2000
C_THETA <- c(0, 1, 1, 1, 1)
INK <- "#0b0b0b"; INK2 <- "#52514e"; BLUE <- "#2a78d6"; ORANGE <- "#eb6834"

beta_true <- function(theta) BETA_H0 + c(0, 0, 0, theta)

# =============================================================================
# 1. Data-generating process
# =============================================================================
simulate_regressors <- function(R, T, sig_pi = SIG_PI) {
  pi_t <- rnorm(T, 0, sig_pi)
  X <- matrix(NA_real_, R * T, J + 1)
  rt <- expand.grid(t = 1:T, r = 1:R)                       # row order (r, t), t fastest
  for (j in 1:J) {
    mu <- rnorm(R, 0, SIG_MU)
    X[, j] <- pi_t[rt$t] + mu[rt$r] + rnorm(R * T, 0, SIG_EPS)
  }
  X[, J + 1] <- MEAN_LOGY + pi_t[rt$t] + rnorm(R * T, 0, SIG_OM)
  colnames(X) <- c("logp1", "logp2", "logp3", "logY")
  X
}
design <- function(X) {
  lpJ <- X[, J]
  list(XU = cbind(1, X),
       XR = cbind(1, X[, 1] - lpJ, X[, 2] - lpJ, X[, J + 1] - lpJ))
}

# =============================================================================
# 2-3. Imposing and testing (lm, anova, matrix Wald) on a data frame
# =============================================================================
estimate_all <- function(d, y) {
  d$y <- d[[y]]
  d$r1 <- d$logp1 - d$logp3; d$r2 <- d$logp2 - d$logp3; d$yr <- d$logY - d$logp3
  mU <- lm(y ~ logp1 + logp2 + logp3 + logY, data = d)       # unrestricted (1)
  mR <- lm(y ~ r1 + r2 + yr, data = d)                       # restricted (3)
  mP <- lm(y ~ r1 + r2 + yr + logp3, data = d)               # reparametrised
  bU <- coef(mU); VU <- vcov(mU); bR <- coef(mR); VR <- vcov(mR); bP <- coef(mP)
  n <- nrow(d)
  # recovery of b3 in the restricted model, Var = a' V a
  a <- c(0, -1, -1, -1)
  b3R <- sum(a * bR); se_b3R <- sqrt(drop(t(a) %*% VR %*% a))
  # t test on theta = c'beta
  theta <- sum(C_THETA * bU); se_theta <- sqrt(drop(t(C_THETA) %*% VU %*% C_THETA))
  tstat <- theta / se_theta
  # reparametrisation: coefficient on log p3 is theta
  stopifnot(abs(bP["logp3"] - theta) < 1e-12)
  # F from the SSRs and via anova()
  ssrU <- sum(resid(mU)^2); ssrR <- sum(resid(mR)^2)
  F <- (ssrR - ssrU) / (ssrU / (n - K))
  av <- anova(mR, mU)
  stopifnot(abs(av$F[2] - F) < 1e-9, abs(F - tstat^2) < 1e-9)
  # matrix Wald statistic (finite-sample form, = F for one restriction)
  Wf <- drop(t(sum(C_THETA * bU)) %*% solve(t(C_THETA) %*% VU %*% C_THETA) %*% sum(C_THETA * bU))
  stopifnot(abs(Wf - F) < 1e-9)
  W <- n * (ssrR - ssrU) / ssrU; LR <- n * log(ssrR / ssrU)
  tc <- qt(1 - ALPHA / 2, n - K)
  # numeraire invariance
  inv <- 0
  for (k in 1:J) {
    lk <- d[[paste0("logp", k)]]
    Z <- sapply(setdiff(1:J, k), function(j) d[[paste0("logp", j)]] - lk)
    m <- lm(d$y ~ Z + I(d$logY - lk))
    b <- numeric(J + 2); b[1] <- coef(m)[1]; b[J + 2] <- coef(m)[J + 1]
    b[setdiff(1:J, k) + 1] <- coef(m)[2:J]
    b[k + 1] <- -(sum(b[setdiff(1:J, k) + 1]) + b[J + 2])
    inv <- max(inv, abs(b - c(bR[1:3], b3R, bR[4])))
  }
  list(bU = unname(bU), seU = unname(sqrt(diag(VU))),
       bR = c(unname(bR[1:3]), b3R, unname(bR[4])),
       seR = c(unname(sqrt(diag(VR)))[1:3], se_b3R, unname(sqrt(VR[4, 4]))),
       theta = theta, se_theta = se_theta, t = tstat, F = F, p = pf(F, 1, n - K, lower.tail = FALSE),
       ci = theta + c(-1, 1) * tc * se_theta, ssrU = ssrU, ssrR = ssrR, W = W, LR = LR,
       invariance = inv)
}

report <- function(tag, e) {
  say("[%s] theta = %.6f (se %.6f), t = %.6f, F = %.6f, t^2 = %.6f, p = %.6f, CI [%.4f, %.4f]",
      tag, e$theta, e$se_theta, e$t, e$F, e$t^2, e$p, e$ci[1], e$ci[2])
  say("[%s] W = %.4f, LR = %.4f, numeraire invariance max diff = %.1e", tag, e$W, e$LR, e$invariance)
}

# ---------------------------------------------------------------- Part A: own sample
set.seed(SEED)
X <- simulate_regressors(R, T)
u <- rnorm(R * T, 0, SIG_U)
own <- data.frame(X, logq_H0 = BETA0 + drop(X %*% beta_true(0)) + u,
                  logq_H1 = BETA0 + drop(X %*% beta_true(THETA_H1)) + u)
say("\n=== Part A: R's own simulated sample (n = %d) ===", nrow(own))
for (h in c("H0", "H1")) report(paste("own", h), estimate_all(own, paste0("logq_", h)))

# =============================================================================
# Fast OLS helpers for the Monte Carlo
# =============================================================================
ssr <- function(Xd, Y) colSums(as.matrix(qr.resid(qr(Xd), Y))^2)
F_stats <- function(XU, XR, Y) { sU <- ssr(XU, Y); (ssr(XR, Y) - sU) / (sU / (nrow(XU) - K)) }

# 4. Size ---------------------------------------------------------------------
set.seed(SEED + 1)
n <- R * T
F_mc <- replicate(N_MC, {
  X <- simulate_regressors(R, T); D <- design(X)
  F_stats(D$XU, D$XR, BETA0 + drop(X %*% beta_true(0)) + rnorm(n, 0, SIG_U))
})
Fcrit <- qf(1 - ALPHA, 1, n - K)
size <- mean(F_mc > Fcrit)
say("\n[size] %d reps: rejection rate = %.4f (MC s.e. %.4f)", N_MC, size,
    sqrt(ALPHA * (1 - ALPHA) / N_MC))

# 5. Power ----------------------------------------------------------------------
set.seed(SEED + 2)
power <- list()
for (Rp in R_POWER) {
  np <- Rp * T; crit <- qf(1 - ALPHA, 1, np - K); rej <- numeric(length(THETA_GRID))
  for (s in 1:N_POWER) {
    X <- simulate_regressors(Rp, T); D <- design(X); uu <- rnorm(np, 0, SIG_U)
    Y <- sapply(THETA_GRID, function(th) BETA0 + drop(X %*% beta_true(th)) + uu)
    rej <- rej + (F_stats(D$XU, D$XR, Y) > crit)
  }
  power[[as.character(np)]] <- rej / N_POWER
  say("[power] n = %d: power at theta = 0.10 -> %.3f; at theta = 0 -> %.3f", np,
      power[[as.character(np)]][THETA_GRID == 0.10], power[[as.character(np)]][THETA_GRID == 0])
}

# 6. Restricted vs unrestricted b1 ------------------------------------------------
set.seed(SEED + 3)
fig4 <- list()
for (nm in names(SIG_PI_FIG4)) {
  out <- t(replicate(N_FIG4, {
    X <- simulate_regressors(R, T, SIG_PI_FIG4[[nm]]); D <- design(X); uu <- rnorm(n, 0, SIG_U)
    Y <- cbind(BETA0 + drop(X %*% beta_true(0)) + uu, BETA0 + drop(X %*% beta_true(THETA_H1)) + uu)
    c(qr.coef(qr(D$XU), Y)[2, ], qr.coef(qr(D$XR), Y)[2, ])   # U_H0, U_H1, R_H0, R_H1
  }))
  fig4[[nm]] <- out
  say("[fig4:%s] H0 sd U = %.4f, sd R = %.4f, ratio = %.3f; H1 mean U = %.4f, mean R = %.4f, mean(R-U) = %.4f",
      nm, sd(out[, 1]), sd(out[, 3]), sd(out[, 3]) / sd(out[, 1]), mean(out[, 2]), mean(out[, 4]),
      mean(out[, 4] - out[, 2]))
}

# Figures (base graphics) -----------------------------------------------------------
png("output/figures_R/fig2_size_R.png", width = 3.4, height = 2.5, units = "in", res = 200)
par(mar = c(4, 4, 1, 1), family = "serif", cex = 0.75)
hist(F_mc, breaks = seq(0, ceiling(max(F_mc)) + 0.25, 0.25), freq = FALSE, col = "#9ec1ec",
     border = "white", xlim = c(0, 12), ylim = c(0, 1.2), main = "", xlab = "F statistic")
curve(df(x, 1, n - K), from = 0.02, to = 12, add = TRUE, lwd = 1.5, col = INK)
abline(v = Fcrit, col = ORANGE, lty = 2, lwd = 1.3)
text(Fcrit + 0.3, 0.8, sprintf("5%% critical value %.2f\nrejection rate %.1f%%", Fcrit, 100 * size),
     adj = 0)
invisible(dev.off())

png("output/figures_R/fig3_power_R.png", width = 3.4, height = 2.5, units = "in", res = 200)
par(mar = c(4, 4, 1, 1), family = "serif", cex = 0.75)
plot(THETA_GRID, power[[1]], type = "o", pch = 19, cex = 0.5, col = BLUE, lwd = 2, ylim = c(0, 1),
     xlab = expression(theta), ylab = "rejection rate at 5%", bty = "l")
lines(THETA_GRID, power[[2]], type = "o", pch = 19, cex = 0.5, col = ORANGE, lwd = 2)
abline(h = ALPHA, lty = 3, col = INK2)
legend("bottomright", bty = "n", legend = paste("n =", names(power)), col = c(BLUE, ORANGE), lwd = 2)
invisible(dev.off())

png("output/figures_R/fig4_restricted_vs_unrestricted_R.png", width = 6.6, height = 3.6,
    units = "in", res = 200)
par(mfrow = c(2, 2), mar = c(3.5, 1, 2, 1), family = "serif", cex = 0.7)
for (nm in names(fig4)) for (k in 1:2) {
  dU <- density(fig4[[nm]][, k]); dR <- density(fig4[[nm]][, k + 2])
  plot(dU, col = BLUE, lwd = 1.5, xlim = c(-0.85, -0.35), ylim = c(0, 9.5), yaxt = "n",
       main = sprintf("sigma_pi = %g, %s", SIG_PI_FIG4[[nm]], c("H0", "H1")[k]),
       xlab = "b1 hat", font.main = 1)
  lines(dR, col = ORANGE, lwd = 1.8); abline(v = BETA_H0[1], lty = 2)
}
invisible(dev.off())

# =============================================================================
# Part B: common file data/sim_data.csv -- cross-check with Python to 1e-6
# =============================================================================
say("\n=== Part B: data/sim_data.csv (written by Python) ===")
csv <- read.csv("data/sim_data.csv")
py <- read.csv("output/python_estimates.csv")
pyv <- setNames(py$value, py$name)
nm5 <- c("b0", "b1", "b2", "b3", "b4")
rv <- c()
for (h in c("H0", "H1")) {
  e <- estimate_all(csv, paste0("logq_", h)); report(paste("csv", h), e)
  rv <- c(rv, setNames(e$bU, paste0(h, "_U_", nm5)), setNames(e$seU, paste0(h, "_U_se_", nm5)),
          setNames(e$bR, paste0(h, "_R_", nm5)), setNames(e$seR, paste0(h, "_R_se_", nm5)),
          setNames(c(e$theta, e$se_theta, e$t, e$F, e$p, e$ssrU, e$ssrR, e$W, e$LR),
                   paste0(h, c("_theta", "_se_theta", "_t", "_F", "_p", "_ssrU", "_ssrR", "_W", "_LR"))))
}
common <- intersect(names(rv), names(pyv))
diffs <- abs(rv[common] - pyv[common])
cc <- data.frame(name = common, R = sprintf("%.12f", rv[common]),
                 Python = sprintf("%.12f", pyv[common]), abs_diff = sprintf("%.2e", diffs))
write.csv(cc, "output/crosscheck_R_vs_Python.csv", row.names = FALSE)
verdict <- if (max(diffs) < 1e-6) "PASS" else "FAIL"
say("Cross-check on %d quantities (coefficients, s.e., theta, t, F, p, SSR, W, LR): max |R - Python| = %.2e -> %s (tolerance 1e-6)",
    length(common), max(diffs), verdict)

# Monte Carlo agreement with Python (different RNGs: compare within MC error)
pm <- read.csv("output/python_mc.csv"); pm <- setNames(pm$value, pm$name)
se_size <- sqrt(ALPHA * (1 - ALPHA) / N_MC)
say("MC size: R %.4f vs Python %.4f; z of the difference = %.2f", size, pm[["size"]],
    (size - pm[["size"]]) / (sqrt(2) * se_size))
for (np in names(power)) {
  pr <- power[[np]][THETA_GRID == 0.10]; pp <- pm[[paste0("power_", np, "_0.10")]]
  say("MC power at theta = 0.10, n = %s: R %.3f vs Python %.3f; z = %.2f", np, pr, pp,
      (pr - pp) / sqrt(2 * pp * (1 - pp) / N_POWER))
}
for (nm in names(fig4)) say("Fig 4 [%s]: sd ratio H0 R %.3f vs Python %.3f; shift R-U under H1 R %.4f vs Python %.4f",
  nm, sd(fig4[[nm]][, 3]) / sd(fig4[[nm]][, 1]), pm[[paste0("fig4_", nm, "_sd_ratio_H0")]],
  mean(fig4[[nm]][, 4] - fig4[[nm]][, 2]), pm[[paste0("fig4_", nm, "_shift_H1")]])
close(logf)
if (verdict != "PASS") quit(status = 1)
