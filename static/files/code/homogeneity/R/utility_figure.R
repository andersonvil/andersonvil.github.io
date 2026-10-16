# utility_figure.R -- Figure 1 of the note, base R version (standalone).
#
# Left : Cobb-Douglas indifference curves, budget line at (p, Y), budget line at (2p, 2Y)
#        (identical), optimal basket; dotted: income doubles alone.
# Right: surface of U(x1, x2) with the budget constraint traced on it and the maximum.
#
# Outputs output/figures_R/fig1_utility_R.pdf and .png
# Run from the project root:  Rscript R/utility_figure.R

dir.create("output/figures_R", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------ parameters
ALPHA <- 0.4                  # U(x1, x2) = ALPHA log x1 + (1 - ALPHA) log x2
P1 <- 40; P2 <- 25; Y <- 2000
LAM <- 2
INK <- "#0b0b0b"; INK2 <- "#52514e"; BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; AQUA <- "#1baf7a"

U <- function(x1, x2) ALPHA * log(x1) + (1 - ALPHA) * log(x2)
demand <- function(p1, p2, y) c(ALPHA * y / p1, (1 - ALPHA) * y / p2)

xs <- demand(P1, P2, Y)
xd <- demand(LAM * P1, LAM * P2, LAM * Y)
stopifnot(isTRUE(all.equal(xs, xd)))          # homogeneity of degree zero
u_star <- U(xs[1], xs[2])

draw <- function() {
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), family = "serif", cex = 0.8)
  on.exit(par(op))
  # ---- left panel
  g1 <- seq(1, 110, length.out = 400)
  plot(NA, xlim = c(0, 105), ylim = c(0, 170), xlab = expression(x[1]),
       ylab = expression(x[2]), main = "(a) Budget sets and optimal basket",
       font.main = 1, bty = "l", xaxs = "i", yaxs = "i")
  for (du in c(-0.35, 0, 0.35)) {
    lines(g1, exp((u_star + du - ALPHA * log(g1)) / (1 - ALPHA)),
          col = if (du == 0) INK else INK2, lwd = if (du == 0) 1.6 else 1)
  }
  b <- seq(0, Y / P1, length.out = 50)
  lines(b, (Y - P1 * b) / P2, col = BLUE, lwd = 5)
  lines(b, (LAM * Y - LAM * P1 * b) / (LAM * P2), col = ORANGE, lwd = 1.5, lty = 2)
  b2 <- seq(0, LAM * Y / P1, length.out = 50)
  lines(b2, (LAM * Y - P1 * b2) / P2, col = INK2, lty = 3)
  points(xs[1], xs[2], pch = 21, bg = AQUA, col = "white", cex = 1.6)
  legend("topright", bty = "n", cex = 0.85,
         legend = c("indifference curves", "budget at (p, Y)",
                    "budget at (2p, 2Y) (identical)", "budget at (p, 2Y)",
                    sprintf("x* = (%g, %g) under both budgets", xs[1], xs[2])),
         col = c(INK2, BLUE, ORANGE, INK2, AQUA), lwd = c(1, 4, 1.5, 1, NA),
         lty = c(1, 1, 2, 3, NA), pch = c(NA, NA, NA, NA, 19))
  # ---- right panel: utility surface with the budget path
  gx <- seq(2, 60, length.out = 40); gy <- seq(2, 90, length.out = 40)
  z <- outer(gx, gy, U)
  zf <- (z[-1, -1] + z[-1, -40] + z[-40, -1] + z[-40, -40]) / 4
  pal <- colorRampPalette(c("#e8f0fb", BLUE))(100)
  cols <- pal[cut(zf, 100)]
  pm <- persp(gx, gy, z, theta = -38, phi = 24, col = cols, border = NA, shade = 0.3,
              xlab = "x1", ylab = "x2", zlab = "U(x1, x2)", ticktype = "detailed",
              main = "(b) Utility surface and budget path", font.main = 1, cex.axis = 0.7)
  bb <- seq(2, Y / P1 - 0.6, length.out = 200)
  bx2 <- (Y - P1 * bb) / P2
  keep <- bx2 <= 90
  lines(trans3d(bb[keep], bx2[keep], U(bb[keep], bx2[keep]), pm), col = ORANGE, lwd = 2.5)
  points(trans3d(xs[1], xs[2], u_star, pm), pch = 21, bg = AQUA, col = "white", cex = 1.6)
}

pdf("output/figures_R/fig1_utility_R.pdf", width = 7, height = 3.2); draw(); invisible(dev.off())
png("output/figures_R/fig1_utility_R.png", width = 7, height = 3.2, units = "in", res = 200)
draw(); invisible(dev.off())
cat(sprintf("Optimum at (p,Y): (%.4f, %.4f); at (2p,2Y): (%.4f, %.4f)\n", xs[1], xs[2], xd[1], xd[2]))
