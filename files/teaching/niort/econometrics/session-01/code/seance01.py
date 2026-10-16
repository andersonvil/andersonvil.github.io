# =============================================================
# Économétrie L3, séance 1. Code Python des figures, tables et calculs
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# ou dans Spyder et l'exécuter.
# =============================================================

# ---------- [etude] Heures d'étude et note ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")                         # 856 étudiants
r = econ["study"].corr(econ["score"])              # corrélation
print("corrélation study-score :", round(r, 3))

# ---------- [outils] Charger des données de Wooldridge ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")                         # charge ECONMATH
print(econ.shape)                                  # lignes, colonnes
print(econ.head())                                 # premières lignes
wd.data("econmath", description=True)              # description des variables

# ---------- [ext-ct] Coupe transversale, ECONMATH ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")
print(econ[["score", "colgpa", "work", "study", "econhs"]].head(5))
print("n =", len(econ))

# ---------- [ext-st] Série temporelle, PHILLIPS ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import matplotlib.pyplot as plt
phil = wd.data("phillips")
print(phil[["year", "unem", "inf"]].head(3))
print(phil[["year", "unem", "inf"]].tail(2))
fig, ax = plt.subplots(figsize=(6, 3))
ax.plot(phil["year"], phil["unem"], color="#1F4E79", label="Taux de chômage (%)")
ax.plot(phil["year"], phil["inf"], color="#8C8C8C", ls="--", label="Inflation (%)")
ax.axhline(0, color="black", lw=0.5)
ax.set_xlabel("Année")
ax.legend(frameon=False)
ax.spines[["top", "right"]].set_visible(False)
ax.yaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout()
fig.savefig("fig_phillips.pdf")
plt.show()

# ---------- [ext-cte] Coupes transversales empilées, CPS78_85 ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
cps = wd.data("cps78_85")
var = ["year", "educ", "exper", "female", "lwage"]
print(cps[cps["year"] == 78][var].head(2))         # enquête de 1978
print(cps[cps["year"] == 85][var].head(2))         # enquête de 1985
print(cps["year"].value_counts())

# ---------- [ext-panel] Données de panel, WAGEPAN ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
pan = wd.data("wagepan")
choix = pan["nr"].isin([13, 17]) & (pan["year"] <= 1981)
print(pan[choix][["nr", "year", "educ", "exper", "hours", "lwage"]])
print("individus :", pan["nr"].nunique(), " années :", pan["year"].nunique())

# ---------- [jtrain] Données expérimentales, JTRAIN2 ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
jt = wd.data("jtrain2")
print(jt.groupby("train")[["re78", "educ", "age"]].mean().round(2))
print(jt["train"].value_counts())                  # effectifs

# ---------- [travail] Travailler 15 h ou plus, comparaison de groupes ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")
econ["w15"] = (econ["work"] >= 15).astype(int)    # 1 si 15 h ou plus
print(econ.groupby("w15")[["score", "colgpa", "mothcoll"]].mean().round(2))
print(econ["w15"].value_counts())                  # effectifs

# ---------- [variance] Construire la variance ----------
import numpy as np
import matplotlib.pyplot as plt
a = np.array([2, 4, 6, 8, 10]); b = np.array([4, 5, 6, 7, 8])      # même moyenne, 6
print("écarts A :", a - 6, " somme :", (a - 6).sum())
print("carrés A :", (a - 6)**2, " variance A :", a.var(ddof=1))
print("carrés B :", (b - 6)**2, " variance B :", b.var(ddof=1))
fig, (g, d) = plt.subplots(1, 2, figsize=(8, 2.2), width_ratios=[1, 1.6])
ha = [1.9, 2.2, 2.5, 2.8, 3.1]; hb = [0.2, 0.5, 0.8, 1.1, 1.4]      # hauteurs des points
g.hlines(ha, 6, a, color="#8C8C8C"); g.hlines(hb, 6, b, color="#8C8C8C")  # écarts
g.scatter(a, ha, color="#1F4E79", zorder=3); g.scatter(b, hb, color="#1F4E79", zorder=3)
g.axvline(6, color="#C0502B", ls="--", lw=1); g.text(6.15, 3.3, "moyenne = 6", color="#C0502B")
g.text(0.3, 2.4, "A", fontweight="bold"); g.text(0.3, 0.7, "B", fontweight="bold")
g.set_xlim(0, 11.5); g.set_ylim(0, 3.7); g.set_yticks([])
g.spines[["top", "right", "left"]].set_visible(False)
g.set_title("Écarts à la moyenne, de somme nulle")
cotes = list(np.abs(a - 6)) + list(np.abs(b - 6))                  # côtés des carrés, A puis B
pos = [0, 4.6, 7.2, 7.8, 10.4, 16.5, 19.1, 20.7, 21.3, 22.9]         # positions horizontales
[d.add_patch(plt.Rectangle((p, 0), c, c, fc="#1F4E79", alpha=0.25)) for p, c in zip(pos, cotes)]
[d.text(p + c / 2, c / 2, c * c, ha="center", va="center") for p, c in zip(pos, cotes) if c > 0]
d.text(7.2, 4.4, "A", fontweight="bold"); d.text(20.7, 4.4, "B", fontweight="bold")
d.set_xlim(-0.5, 25.5); d.set_ylim(-0.3, 5.2); d.set_aspect("equal"); d.axis("off")
d.set_title("Carrés des écarts, d'aire (x - moyenne) au carré")
fig.tight_layout(); fig.savefig("fig_variance.pdf"); plt.show()

# ---------- [quadrants] Signe de la covariance, ECONMATH ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import matplotlib.pyplot as plt
econ = wd.data("econmath")
fig, ax = plt.subplots(figsize=(5.2, 3.4))
ax.scatter(econ["colgpa"], econ["score"], s=8, color="#1F4E79", alpha=0.35)
ax.axvline(econ["colgpa"].mean(), color="#8C8C8C", ls="--", lw=1)
ax.axhline(econ["score"].mean(), color="#8C8C8C", ls="--", lw=1)
ax.text(3.8, 97, "+", fontsize=18, color="#C0502B", ha="center", va="center")
ax.text(1.1, 25, "+", fontsize=18, color="#C0502B", ha="center", va="center")
ax.text(3.8, 25, "\u2212", fontsize=18, color="#C0502B", ha="center", va="center")
ax.text(1.1, 97, "\u2212", fontsize=18, color="#C0502B", ha="center", va="center")
ax.set_xlabel("Moyenne universitaire (colgpa, sur 4)")
ax.set_ylabel("Note finale (score, %)")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout()
fig.savefig("fig_quadrants.pdf")
plt.show()
print("covariance :", round(econ["colgpa"].cov(econ["score"]), 3))
print("corrélation :", round(econ["colgpa"].corr(econ["score"]), 3))

# ---------- [deplacer] Déplacer des points change le signe ----------
import numpy as np
import matplotlib.pyplot as plt
x = np.arange(1, 11)                                     # x = 1, 2, ..., 10
yA = np.array([2, 4, 3, 5, 4, 6, 5, 7, 6, 8])            # nuage de départ
yB = np.array([7, 4, 3, 5, 4, 6, 5, 7, 6, 3])            # on déplace 2 points
yC = np.array([7, 8, 6, 5, 4, 6, 5, 4, 2, 3])            # on en déplace 4 autres
rA = np.corrcoef(x, yA)[0, 1]; rB = np.corrcoef(x, yB)[0, 1]; rC = np.corrcoef(x, yC)[0, 1]
print("r :", round(rA, 3), round(rB, 3) + 0, round(rC, 3))
print("s_xy :", np.cov(x, yA)[0, 1].round(3), np.cov(x, yB)[0, 1].round(3) + 0, np.cov(x, yC)[0, 1].round(3))
fig, axs = plt.subplots(1, 3, figsize=(9, 3), sharey=True)
axs[1].scatter(x[[0, 9]], yA[[0, 9]], color="#C8C8C8", s=40)                # anciennes places
axs[2].scatter(x[[1, 2, 7, 8]], yB[[1, 2, 7, 8]], color="#C8C8C8", s=40)
axs[0].scatter(x, yA, color="#1F4E79", s=40); axs[1].scatter(x, yB, color="#1F4E79", s=40)
axs[2].scatter(x, yC, color="#1F4E79", s=40)
[a.set_title(f"r = {round(r, 2) + 0:.2f}".replace(".", ",").replace("-", "\u2212")) for a, r in zip(axs, [rA, rB, rC])]
[a.axvline(5.5, color="#8C8C8C", ls="--", lw=1) for a in axs]              # moyenne de x
[a.axhline(5, color="#8C8C8C", ls="--", lw=1) for a in axs]                # moyenne de y
[a.set_xlabel("x") for a in axs]; axs[0].set_ylabel("y")
[a.spines[["top", "right"]].set_visible(False) for a in axs]
fig.tight_layout(); fig.savefig("fig_deplacer.pdf"); plt.show()

# ---------- [jouet-stats] Exemple jouet, statistiques descriptives ----------
import numpy as np
x = np.array([2, 4, 6, 8, 10])        # heures de travail personnel par semaine
y = np.array([7, 10, 11, 14, 13])     # note sur 20
print("moyennes :", x.mean(), y.mean())
print("variances :", x.var(ddof=1), y.var(ddof=1))   # ddof=1, division par n-1
print("covariance :", np.cov(x, y, ddof=1)[0, 1])
print("corrélation :", round(np.corrcoef(x, y)[0, 1], 4))

# ---------- [nuage] Nuage de points du fil rouge ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import matplotlib.pyplot as plt
econ = wd.data("econmath")
fig, ax = plt.subplots(figsize=(5.2, 3.4))
ax.scatter(econ["colgpa"], econ["score"], s=8, color="#1F4E79", alpha=0.35)
ax.set_xlabel("Moyenne universitaire (colgpa, sur 4)")
ax.set_ylabel("Note finale (score, %)")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout()
fig.savefig("fig_nuage.pdf")
plt.show()

# ---------- [mco-illus] Quelle droite choisir ? ----------
import numpy as np
import matplotlib.pyplot as plt
x = np.array([2, 4, 6, 8, 10]); y = np.array([7, 10, 11, 14, 13])
g = np.linspace(0, 12, 50)                                     # grille pour tracer
scr_1 = ((y - (4 + 1.2 * x))**2).sum()                         # droite 1, 4 + 1,2 x
scr_2 = ((y - (6.2 + 0.8 * x))**2).sum()                       # droite 2, 6,2 + 0,8 x
print("SCR :", round(scr_1, 2), round(scr_2, 2))
fig, axs = plt.subplots(1, 2, figsize=(6.4, 3), sharey=True)
axs[0].plot(g, 4 + 1.2 * g, color="#8C8C8C"); axs[0].vlines(x, y, 4 + 1.2 * x, color="#8C8C8C", lw=1)
axs[1].plot(g, 6.2 + 0.8 * g, color="#C0502B"); axs[1].vlines(x, y, 6.2 + 0.8 * x, color="#8C8C8C", lw=1)
axs[0].set_title(f"y = 4 + 1,2 x\nSCR = {scr_1:.1f}".replace(".", ","), fontsize=10)
axs[1].set_title(f"y = 6,2 + 0,8 x\nSCR = {scr_2:.1f}".replace(".", ","), fontsize=10)
[a.scatter(x, y, color="#1F4E79", zorder=3) for a in axs]
[a.set_xlim(0, 12) for a in axs]; [a.set_ylim(4, 18) for a in axs]
[a.set_xlabel("x (heures)") for a in axs]; axs[0].set_ylabel("y (note sur 20)")
[a.spines[["top", "right"]].set_visible(False) for a in axs]
[a.yaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ",")) for a in axs]
fig.tight_layout(); fig.savefig("fig_mco_illus.pdf"); plt.show()

# ---------- [carres] Trois critères pour choisir la droite ----------
import numpy as np
import matplotlib.pyplot as plt
x = np.array([2, 4, 6, 8, 10]); y = np.array([7, 10, 11, 14, 13])
g = np.linspace(0, 12, 50)
u_a = y - (17 - x)                                          # a) droite fausse, pente -1
u_b1 = y - (5 + x); u_b2 = y - (8 + 0.5 * x)                # b) deux droites
u_c = y - (6.2 + 0.8 * x)                                   # c) droite des MCO
print("a) somme des résidus :", u_a.sum(), " MCO :", round(u_c.sum(), 10) + 0)
print("b) sommes des valeurs absolues :", np.abs(u_b1).sum(), np.abs(u_b2).sum())
print("c) somme des carrés :", round((u_c**2).sum(), 2))
fig, axs = plt.subplots(1, 3, figsize=(8, 2.6))
axs[0].plot(g, 17 - g, color="#8C8C8C"); axs[0].vlines(x, y, 17 - x, color="#8C8C8C", lw=1)
axs[1].plot(g, 5 + g, color="#8C8C8C"); axs[1].plot(g, 8 + 0.5 * g, color="#8C8C8C", ls="--")
axs[2].plot(g, 6.2 + 0.8 * g, color="#C0502B")
[axs[2].add_patch(plt.Rectangle((xi, min(yi, yi - ui)), abs(ui), abs(ui),
    fc="#1F4E79", alpha=0.25)) for xi, yi, ui in zip(x, y, u_c)]       # carrés des résidus
axs[0].set_title("a) y = 17 \u2212 x\nsomme des résidus = 0")
axs[1].set_title("b) y = 5 + x et y = 8 + 0,5 x\nsomme des |résidus| = 4 et 4")
axs[2].set_title("c) y = 6,2 + 0,8 x\nsomme des carrés = 4,4")
[a.scatter(x, y, color="#1F4E79", zorder=3) for a in axs]
[a.set_xlim(0, 12) for a in axs]; [a.set_ylim(5, 16) for a in axs]
[a.set_aspect("equal") for a in axs]                                    # carrés non déformés
[a.set_xlabel("x (heures)") for a in axs]; axs[0].set_ylabel("y (note sur 20)")
[a.spines[["top", "right"]].set_visible(False) for a in axs]
[a.yaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ",")) for a in axs]
fig.tight_layout(); fig.savefig("fig_carres.pdf"); plt.show()

# ---------- [jouet-mco] Exemple jouet, MCO à la main et par le code ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "statsmodels"])    # installe si absent
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
x = np.array([2, 4, 6, 8, 10])
y = np.array([7, 10, 11, 14, 13])
b1 = ((x - x.mean()) * (y - y.mean())).sum() / ((x - x.mean())**2).sum()
b0 = y.mean() - b1 * x.mean()
print("à la main :", round(b0, 4), round(b1, 4))
jouet = pd.DataFrame({"x": x, "y": y})
print("par le code :", smf.ols("y ~ x", data=jouet).fit().params.round(4).tolist())

# ---------- [jouet-proprietes] Valeurs prédites, résidus, propriétés ----------
import numpy as np
x = np.array([2, 4, 6, 8, 10])
y = np.array([7, 10, 11, 14, 13])
b1 = ((x - x.mean()) * (y - y.mean())).sum() / ((x - x.mean())**2).sum()
b0 = y.mean() - b1 * x.mean()
yhat = b0 + b1 * x                                     # valeurs prédites
uhat = y - yhat                                        # résidus
print("prédites :", yhat.round(2), " résidus :", uhat.round(2))
print("somme des résidus :", round(uhat.sum(), 10))
print("somme x * résidus :", round((x * uhat).sum(), 10))
print("droite en x moyen :", round(b0 + b1 * x.mean(), 10), " y moyen :", y.mean())

# ---------- [decomp] Décomposition pour une observation ----------
import numpy as np
import matplotlib.pyplot as plt
x = np.array([2, 4, 6, 8, 10])
y = np.array([7, 10, 11, 14, 13])
b1 = ((x - x.mean()) * (y - y.mean())).sum() / ((x - x.mean())**2).sum()
b0 = y.mean() - b1 * x.mean()
yhat = b0 + b1 * x
g = np.linspace(0, 12, 50)
fig, ax = plt.subplots(figsize=(4.6, 3.2))
ax.scatter(x, y, color="#1F4E79", zorder=3)
ax.plot(g, b0 + b1 * g, color="#C0502B")
ax.axhline(y.mean(), color="#8C8C8C", ls="--", lw=1)
ax.annotate("", xy=(8.25, yhat[3]), xytext=(8.25, y.mean()),
    arrowprops=dict(arrowstyle="<->", color="#C0502B"))  # 4e étudiant, x = 8
ax.annotate("", xy=(7.75, y[3]), xytext=(7.75, yhat[3]),
    arrowprops=dict(arrowstyle="<->", color="#8C8C8C"))
ax.text(8.45, (yhat[3] + y.mean()) / 2, r"$\hat y_i-\bar y$", color="#C0502B")
ax.text(7.55, (y[3] + yhat[3]) / 2, r"$\hat u_i$", color="#8C8C8C", ha="right")
ax.text(0.3, y.mean() + 0.3, r"$\bar y$", color="#8C8C8C")
ax.set_xlim(0, 12); ax.set_ylim(4, 18)
ax.set_xlabel("x (heures)"); ax.set_ylabel("y (note sur 20)")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig("fig_decomp.pdf")
plt.show()

# ---------- [jouet-r2] SCT, SCE, SCR et R2 ----------
import numpy as np
x = np.array([2, 4, 6, 8, 10])
y = np.array([7, 10, 11, 14, 13])
b1 = ((x - x.mean()) * (y - y.mean())).sum() / ((x - x.mean())**2).sum()
b0 = y.mean() - b1 * x.mean()
yhat = b0 + b1 * x
sct = ((y - y.mean())**2).sum()
sce = ((yhat - y.mean())**2).sum()
scr = ((y - yhat)**2).sum()
print("SCT, SCE, SCR :", sct, round(sce, 4), round(scr, 4))
print("R2 :", round(sce / sct, 4), " r2 :", round(np.corrcoef(x, y)[0, 1]**2, 4))

# ---------- [jtrain-r2] Un R2 faible dans une expérience, JTRAIN2 ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])
import wooldridge as wd
import statsmodels.formula.api as smf
jt = wd.data("jtrain2")
m_jt = smf.ols("re78 ~ train", data=jt).fit()
print(m_jt.params.round(2).tolist(), " R2 :", round(m_jt.rsquared, 3))

# ---------- [reg-table] Première estimation sur ECONMATH ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])
import wooldridge as wd
import statsmodels.formula.api as smf
econ = wd.data("econmath")
m1 = smf.ols("score ~ colgpa", data=econ).fit()
m2 = smf.ols("score ~ actmth", data=econ).fit()       # 42 valeurs manquantes écartées
print(m1.params.round(3).tolist(), m1.bse.round(3).tolist(), round(m1.rsquared, 3), int(m1.nobs))
print(m2.params.round(3).tolist(), m2.bse.round(3).tolist(), round(m2.rsquared, 3), int(m2.nobs))
print("p-valeurs (étoiles) :", m1.pvalues.tolist(), m2.pvalues.tolist())
print("prédiction pour colgpa = 3 :", round(m1.params["Intercept"] + 3 * m1.params["colgpa"], 2))
print("colgpa min et max :", econ["colgpa"].min(), econ["colgpa"].max())

# ---------- [droite] Droite des MCO sur le nuage ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
econ = wd.data("econmath")
m1 = smf.ols("score ~ colgpa", data=econ).fit()
g = np.linspace(econ["colgpa"].min(), econ["colgpa"].max(), 50)
fig, ax = plt.subplots(figsize=(5.2, 3.4))
ax.scatter(econ["colgpa"], econ["score"], s=8, color="#1F4E79", alpha=0.35)
ax.plot(g, m1.params["Intercept"] + m1.params["colgpa"] * g, color="#C0502B", lw=2)
ax.scatter(econ["colgpa"].mean(), econ["score"].mean(), color="#C0502B", marker="s", s=40, zorder=4)
ax.set_xlabel("Moyenne universitaire (colgpa, sur 4)")
ax.set_ylabel("Note finale (score, %)")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout()
fig.savefig("fig_droite.pdf")
plt.show()
print("point moyen :", round(econ["colgpa"].mean(), 3), round(econ["score"].mean(), 3))

# ---------- [residus] Valeurs prédites et résidus sur ECONMATH ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])
import wooldridge as wd
import statsmodels.formula.api as smf
econ = wd.data("econmath")
m1 = smf.ols("score ~ colgpa", data=econ).fit()
econ["score_chap"] = m1.fittedvalues                  # valeurs prédites
econ["u_chap"] = m1.resid                             # résidus
print(econ[["colgpa", "score", "score_chap", "u_chap"]].head(5).round(2))
print("somme des résidus :", round(m1.resid.sum(), 8))
sct = ((econ["score"] - econ["score"].mean())**2).sum()
print("SCT, SCE, SCR :", round(sct, 1), round(m1.ess, 1), round(m1.ssr, 1))
