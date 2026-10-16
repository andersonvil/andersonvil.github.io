# =============================================================
# Économétrie L3, séance 1. Série d'exercices, code Python du corrigé
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# ou dans Spyder et l'exécuter.
# =============================================================

# ---------- [ex1] Exercice 1, calculs à la main vérifiés ----------
import numpy as np
x = np.array([2, 3, 4, 6, 7, 8])               # prix du panier (euros)
y = np.array([14, 12, 13, 9, 8, 4])            # paniers vendus en une heure
n = len(x)
dx = x - x.mean(); dy = y - y.mean()           # écarts à la moyenne
print("moyennes :", x.mean(), y.mean())
print("sommes :", (dx * dy).sum(), (dx**2).sum(), (dy**2).sum())
print("s2_x, s2_y, s_xy :", x.var(ddof=1), y.var(ddof=1), np.cov(x, y)[0, 1])
print("r :", round(np.corrcoef(x, y)[0, 1], 4))
b1 = (dx * dy).sum() / (dx**2).sum(); b0 = y.mean() - b1 * x.mean()
print("b0, b1 :", b0, b1)
yhat = b0 + b1 * x; uhat = y - yhat
print("valeurs prédites :", yhat); print("résidus :", uhat)
print("somme des résidus :", uhat.sum(), " somme x*résidus :", (x * uhat).sum())
sct = (dy**2).sum(); sce = ((yhat - y.mean())**2).sum(); scr = (uhat**2).sum()
print("SCT, SCE, SCR :", sct, sce, scr, " R2 :", sce / sct)
print("prévisions à 5 et 15 euros :", b0 + b1 * 5, b0 + b1 * 15)
xc = 100 * x                                   # prix en centimes
b1c = ((xc - xc.mean()) * dy).sum() / ((xc - xc.mean())**2).sum()
print("prix en centimes, b0 et b1 :", y.mean() - b1c * xc.mean(), b1c)

# ---------- [ex2] Exercice 2, les deux droites de régression ----------
import numpy as np
x = np.array([2, 3, 4, 6, 7, 8]); y = np.array([14, 12, 13, 9, 8, 4])
dx = x - x.mean(); dy = y - y.mean()
b1 = (dx * dy).sum() / (dx**2).sum()           # régression de y sur x
g1 = (dx * dy).sum() / (dy**2).sum()           # régression de x sur y
r = np.corrcoef(x, y)[0, 1]
print("pente de y sur x :", b1, " pente de x sur y :", g1)
print("produit des pentes :", round(b1 * g1, 4), " r au carré :", round(r**2, 4))
print("pente de la seconde droite dans le plan (x, y) :", round(1 / g1, 4))

# ---------- [ex3] Exercice 3, table de régression, ATTEND ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])   # installe si absent
import wooldridge as wd
import statsmodels.formula.api as smf
att = wd.data("attend")                        # 680 étudiants
m1 = smf.ols("final ~ attend", data=att).fit()
m2 = smf.ols("final ~ priGPA", data=att).fit()
m3 = smf.ols("attend ~ priGPA", data=att).fit()
[print(m.params.round(4).to_dict(), m.bse.round(4).to_dict(), "p-valeurs :", m.pvalues.tolist(),
    "n =", int(m.nobs), "R2 =", round(m.rsquared, 4)) for m in (m1, m2, m3)]
print("min et max de attend :", att["attend"].min(), att["attend"].max())
print("note prédite pour 32 et 16 séances :", m1.predict({"attend": [32, 16]}).round(4).tolist())
print("séances prédites pour priGPA = 3 :", round(m3.params.iloc[0] + 3 * m3.params.iloc[1], 4))
m4 = smf.ols("final ~ atndrte", data=att).fit()          # présence en %
print("présence en % :", m4.params.round(4).to_dict(), "R2 =", round(m4.rsquared, 4))

# ---------- [ex4] Exercice 4, résultats potentiels et tirage au sort ----------
import numpy as np
y0 = np.array([8, 9, 7, 10, 14, 13, 15, 12])   # notes sans tutorat, A à H
y1 = np.array([11, 11, 10, 12, 15, 15, 16, 14])  # notes avec tutorat
d = np.array([1, 1, 1, 1, 0, 0, 0, 0])         # participation choisie
print("effets individuels :", y1 - y0, " effet moyen :", (y1 - y0).mean())
print("effet moyen chez les participants :", (y1 - y0)[d == 1].mean())
ecart = y1[d == 1].mean() - y0[d == 0].mean()
print("écart observé :", ecart, " = ", (y1 - y0)[d == 1].mean(), "+",
    y0[d == 1].mean() - y0[d == 0].mean())
t = np.array([1, 0, 1, 0, 1, 0, 1, 0])         # un tirage au sort (A, C, E, G)
print("écart avec ce tirage :", y1[t == 1].mean() - y0[t == 0].mean())

# ---------- [ex4-tirages] Exercice 4, les 70 tirages possibles ----------
import itertools
import numpy as np
y0 = np.array([8, 9, 7, 10, 14, 13, 15, 12])   # notes sans tutorat, A à H
y1 = np.array([11, 11, 10, 12, 15, 15, 16, 14])  # notes avec tutorat
tirages = list(itertools.combinations(range(8), 4))   # 4 étudiants tirés parmi 8
ecarts = [y1[list(g)].mean() - np.delete(y0, g).mean() for g in tirages]
print("nombre de tirages :", len(tirages), " moyenne des écarts :", round(np.mean(ecarts), 10))
print("plus petit et plus grand écart :", min(ecarts), max(ecarts))

# ---------- [ex5-depart] Exercice 5, charger les données WINE ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
wine = wd.data("wine")                         # 21 pays
print(wine)

# ---------- [ex5-mco] Exercice 5, statistiques et MCO ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])   # installe si absent
import wooldridge as wd
import statsmodels.formula.api as smf
wine = wd.data("wine")
print(wine[["alcohol", "heart"]].describe().round(3))
print("covariance :", round(wine["alcohol"].cov(wine["heart"]), 4))
print("corrélation :", round(wine["alcohol"].corr(wine["heart"]), 4))
m = smf.ols("heart ~ alcohol", data=wine).fit()
print(m.params.round(4).to_dict(), "R2 =", round(m.rsquared, 4), "n =", int(m.nobs))
wine["prevu"] = m.fittedvalues; wine["residu"] = m.resid
print(wine.loc[wine["country"].isin(["France", "Japan"]), ["country", "alcohol", "heart", "prevu", "residu"]].round(2))
sct = ((wine["heart"] - wine["heart"].mean())**2).sum()
print("somme des résidus :", round(m.resid.sum(), 8) + 0)
print("SCT, SCE, SCR :", round(sct, 2), round(m.ess, 2), round(m.ssr, 2))
[print(f, smf.ols(f, data=wine).fit().params.round(4).to_dict(), "R2 =",
    round(smf.ols(f, data=wine).fit().rsquared, 4)) for f in ["liver ~ alcohol", "deaths ~ alcohol"]]

# ---------- [ex5-figure] Exercice 5, nuage de points et droite ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
wine = wd.data("wine")
b1, b0 = np.polyfit(wine["alcohol"], wine["heart"], 1)     # pente et constante des MCO
g = np.linspace(0, 10, 50)
fig, ax = plt.subplots(figsize=(5.5, 3.6))
ax.scatter(wine["alcohol"], wine["heart"], color="#1F4E79")
ax.plot(g, b0 + b1 * g, color="#C0502B")
noms = {"France": "France", "Japan": "Japon", "Italy": "Italie"}   # pays à nommer
[ax.annotate(noms[p], (a, h), xytext=(4, 3), textcoords="offset points", fontsize=8)
    for p, a, h in zip(wine["country"], wine["alcohol"], wine["heart"]) if p in noms]
ax.set_xlabel("Alcool issu du vin (litres par habitant)")
ax.set_ylabel("Décès cardiaques pour 100 000")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_td_wine.pdf"); plt.show()
