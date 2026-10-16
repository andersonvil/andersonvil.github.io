# =============================================================
# Économétrie L3, séance 2. Série d'exercices, code Python du corrigé
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# ou dans Spyder et l'exécuter.
# Les simulations fixent la graine (2026). Python, R et Stata utilisent
# des générateurs différents. Les dernières décimales diffèrent.
# =============================================================

# ---------- [ex1] Exercice 1, une loi jointe à la main ----------
import numpy as np
xv = np.array([0, 2, 4]); uv = np.array([-2, 0, 2])          # valeurs de x et de u
P = np.array([[0.10, 0.15, 0.05],                             # lignes : x = 0, 2, 4
              [0.10, 0.20, 0.10],                             # colonnes : u = -2, 0, 2
              [0.05, 0.15, 0.10]])
px, pu = P.sum(axis=1), P.sum(axis=0)                         # lois marginales
Ex = (px * xv).sum(); Eu = (pu * uv).sum()
print("somme des probabilités :", P.sum(), "| loi de x :", px, "| loi de u :", pu)
print("E(x) :", Ex, "| Var(x) :", round((px * xv**2).sum() - Ex**2, 4), "| E(u) :", round(Eu, 10))
Eu_x = (P * uv).sum(axis=1) / px                              # E(u | x) pour x = 0, 2, 4
print("E(u | x) :", Eu_x.round(4), "| espérances itérées :", round((px * Eu_x).sum(), 10))
cov = (P * np.outer(xv, uv)).sum() - Ex * Eu                  # Cov(x, u) = E(xu) - E(x)E(u)
print("Cov(x, u) :", round(cov, 4))
print("E(y | x) = 8 + x + E(u | x) :", (8 + xv + Eu_x).round(4))
print("pente de E(y | x) :", round((Eu_x[2] + 4 - Eu_x[0]) / 4, 4), "| 1 + Cov(x,u)/Var(x) :", round(1 + cov / 2.4, 4))
print("Var(u | x) :", ((P * uv**2).sum(axis=1) / px - Eu_x**2).round(4))
Q = np.outer(px, pu)                                          # loi jointe si x et u sont indépendants
print("loi jointe indépendante :", Q.round(3).tolist(), "| E(u | x) :", ((Q * uv).sum(axis=1) / px).round(4))

# ---------- [ex1-simul] Exercice 1, la pente des MCO sur des échantillons tirés ----------
import numpy as np
xv = np.array([0, 2, 4]); uv = np.array([-2, 0, 2])
P = np.array([[0.10, 0.15, 0.05], [0.10, 0.20, 0.10], [0.05, 0.15, 0.10]])
rng = np.random.default_rng(2026)
k = rng.choice(9, p=P.ravel(), size=(5000, 100))              # 5 000 échantillons de 100 couples (x, u)
x, u = xv[k // 3], uv[k % 3]                                  # case k : ligne k // 3, colonne k % 3
y = 8 + 1 * x + u                                             # vraie pente 1
xc = x - x.mean(axis=1, keepdims=True)
b1 = (xc * y).sum(axis=1) / (xc**2).sum(axis=1)               # pente des MCO de chaque échantillon
print("moyenne des 5 000 pentes :", round(b1.mean(), 4), "| vraie pente 1, 1 + 1/6 =", round(7 / 6, 4))

# ---------- [ex2] Exercice 2, la pente des extrêmes ----------
import numpy as np
x = np.array([2, 4, 6, 8, 10]); y = np.array([7, 10, 11, 14, 13])   # exemple à la main
print("pente des extrêmes :", (y[-1] - y[0]) / (x[-1] - x[0]), "| pente des MCO :", 32 / 40)
sct = ((x - x.mean())**2).sum()
print("Var pente des extrêmes / sigma^2 :", 2 / (x[-1] - x[0])**2, "| Var MCO / sigma^2 :", 1 / sct)
print("SCT_x :", sct, " >= (x_n - x_1)^2 / 2 :", (x[-1] - x[0])**2 / 2)
rng = np.random.default_rng(2026)
yy = 6 + 1 * x + rng.normal(0, 1, size=(10000, 5))           # vraie droite 6 + x, sigma = 1
ext = (yy[:, -1] - yy[:, 0]) / 8; mco = yy @ ((x - x.mean()) / sct)
print("moyennes :", round(ext.mean(), 4), round(mco.mean(), 4))
print("variances :", round(ext.var(ddof=1), 4), round(mco.var(ddof=1), 4), "| théoriques 1/32 et 1/40")

# ---------- [ex3] Exercice 3, table de régression, NBASAL ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])   # installe si absent
import wooldridge as wd
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
nba = wd.data("nbasal")                        # 269 joueurs de NBA
mod = {f: smf.ols(f, data=nba).fit() for f in ["wage ~ points", "points ~ exper", "wage ~ exper"]}
[print(f, m.params.round(3).tolist(), m.bse.round(3).tolist(), "R2", round(m.rsquared, 3),
    "sigma", round(np.sqrt(m.scale), 2), "p", m.pvalues.round(4).tolist()) for f, m in mod.items()]
m1 = mod["wage ~ points"]
quart = pd.qcut(nba["points"], 4)
print("écart-type des résidus par quart de points :", m1.resid.groupby(quart, observed=True).std().round(0).tolist())
print("SCT_points :", round(((nba["points"] - nba["points"].mean())**2).sum(), 1), " s_points :", round(nba["points"].std(), 3))
print("prévisions pour 10 et 20 points :", (287.101 + 111.330 * np.array([10, 20])).round(1))

# ---------- [ex5-depart] Exercice 5, charger les données HTV ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
htv = wd.data("htv")                           # 1 230 hommes de 26 à 34 ans, en 1991
print(htv[["wage", "educ", "abil"]].describe())

# ---------- [ex5-mco] Exercice 5, MCO, sigma chapeau et erreur-type ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])   # installe si absent
import wooldridge as wd
import numpy as np
import statsmodels.formula.api as smf
htv = wd.data("htv")
m = smf.ols("wage ~ educ", data=htv).fit()
print(m.params.round(4).tolist(), m.bse.round(4).tolist(), "R2", round(m.rsquared, 4), "n", int(m.nobs))
n = int(m.nobs); x = htv["educ"]
scr = (m.resid**2).sum(); sct = ((x - x.mean())**2).sum()
s = np.sqrt(scr / (n - 2))                     # sigma chapeau
print("SCR :", round(scr, 1), "| sigma chapeau :", round(s, 4), "| SCT_educ :", round(sct, 2))
print("se(b1) = sigma chapeau / racine(SCT) :", round(s / np.sqrt(sct), 4))

# ---------- [ex5-residus] Exercice 5, résidus et dispersion ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])   # installe si absent
import wooldridge as wd
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
htv = wd.data("htv")
res = smf.ols("wage ~ educ", data=htv).fit().resid
groupe = pd.cut(htv["educ"], [0, 12, 14, 20], labels=["12 ou moins", "13 ou 14", "15 ou plus"])
print(res.groupby(groupe, observed=True).agg(["std", "size"]).round(2))
fig, ax = plt.subplots(figsize=(5.5, 3.4))
ax.scatter(htv["educ"], res, s=8, color="#1F4E79", alpha=0.35)
ax.axhline(0, color="#C0502B")
ax.set_xlabel("Années d'études (educ)"); ax.set_ylabel("Résidu")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_td_htv.pdf"); plt.show()

# ---------- [ex5-simul] Exercice 5, la population des 1 230 hommes ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
htv = wd.data("htv")
x, y = htv["educ"].to_numpy(), htv["wage"].to_numpy()
rng = np.random.default_rng(2026)
e = rng.integers(0, 1230, size=(2000, 100))    # 2 000 échantillons de 100 hommes, avec remise
xs, ys = x[e], y[e]
xc = xs - xs.mean(axis=1, keepdims=True); sct = (xc**2).sum(axis=1)
b1 = (xc * ys).sum(axis=1) / sct; b0 = ys.mean(axis=1) - b1 * xs.mean(axis=1)
scr = ((ys - b0[:, None] - b1[:, None] * xs)**2).sum(axis=1)
se = np.sqrt(scr / 98 / sct)                   # erreur-type usuelle de chaque échantillon
print("moyenne des pentes :", round(b1.mean(), 3), "| pente sur les 1 230 :", 1.392)
print("écart-type des pentes :", round(b1.std(ddof=1), 3), "| moyenne des erreurs-types :", round(se.mean(), 3))

# ---------- [ex5-abil] Exercice 5, l'aptitude, un facteur de u observé ici ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
htv = wd.data("htv")
print("corrélation educ-abil :", round(htv["educ"].corr(htv["abil"]), 3))
print("corrélation wage-abil :", round(htv["wage"].corr(htv["abil"]), 3))
print(htv.groupby(htv["educ"] >= 15)["abil"].mean().round(2))   # aptitude moyenne, moins de 15 ans d'études ou plus
