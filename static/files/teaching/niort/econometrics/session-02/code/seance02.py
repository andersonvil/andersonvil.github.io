# =============================================================
# Économétrie L3, séance 2. Code Python des figures, tables et calculs
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# ou dans Spyder et l'exécuter.
# Les simulations fixent la graine (2026). Python, R et Stata utilisent
# des générateurs différents. Les tirages, donc les dernières décimales
# des résultats simulés, diffèrent d'un langage à l'autre.
# =============================================================

# ---------- [ech-quatre] Quatre échantillons, quatre droites ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
econ = wd.data("econmath")                     # les 856 étudiants, vus comme la population
x, y = econ["colgpa"].to_numpy(), econ["score"].to_numpy()
pente = lambda a, b: np.cov(a, b)[0, 1] / np.var(a, ddof=1)   # pente des MCO
rng = np.random.default_rng(2026)              # graine : mêmes tirages à chaque exécution
ech = rng.integers(0, 856, size=(4, 50))       # 4 échantillons de 50 étudiants, avec remise
print("pente sur les 856 :", round(pente(x, y), 2))
print("pentes des 4 échantillons :", np.round([pente(x[e], y[e]) for e in ech], 2))
g = np.linspace(0.8, 4.1, 50)
fig, axs = plt.subplots(1, 4, figsize=(9, 2.6), sharey=True)
[a.scatter(x[e], y[e], s=8, color="#1F4E79", alpha=0.6) for a, e in zip(axs, ech)]
droite = lambda e: y[e].mean() + pente(x[e], y[e]) * (g - x[e].mean())   # droite des MCO
[a.plot(g, droite(np.arange(856)), color="#8C8C8C", ls="--") for a in axs]   # les 856
[a.plot(g, droite(e), color="#C0502B") for a, e in zip(axs, ech)]
[a.set_title(f"pente = {pente(x[e], y[e]):.1f}".replace(".", ","), fontsize=10)
    for a, e in zip(axs, ech)]
[a.set_xlabel("colgpa") for a in axs]; axs[0].set_ylabel("score")
[a.spines[["top", "right"]].set_visible(False) for a in axs]
fig.tight_layout(); fig.savefig("fig_echantillons.pdf"); plt.show()

# ---------- [ech-distrib] Mille échantillons, la distribution des pentes ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
econ = wd.data("econmath")
x, y = econ["colgpa"].to_numpy(), econ["score"].to_numpy()
pente = lambda a, b: np.cov(a, b)[0, 1] / np.var(a, ddof=1)   # pente des MCO
rng = np.random.default_rng(2026)
ech = rng.integers(0, 856, size=(1000, 50))    # 1 000 échantillons de 50, avec remise
b1 = np.array([pente(x[e], y[e]) for e in ech])
print("moyenne et écart-type des 1 000 pentes :", round(b1.mean(), 2), round(b1.std(ddof=1), 2))
print("plus petite et plus grande pente :", round(b1.min(), 2), round(b1.max(), 2))
fig, ax = plt.subplots(figsize=(5.2, 2.9))
ax.hist(b1, bins=30, color="#1F4E79", alpha=0.7)
ax.axvline(pente(x, y), color="#C0502B", lw=2)          # pente calculée sur les 856
ax.set_xlabel("Pente estimée sur un échantillon de 50 étudiants")
ax.set_ylabel("Nombre d'échantillons")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout(); fig.savefig("fig_distrib_echant.pdf"); plt.show()

# ---------- [va] Un étudiant tiré au hasard ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")                     # population de 856 étudiants
X = (econ["work"] >= 15).astype(int)           # X = 1 si 15 heures ou plus de travail salarié
print("loi de X :", X.value_counts().sort_index().tolist(), "étudiants sur", len(X))
print("P(X = 1) :", round(X.mean(), 4), "| P(X = 0) :", round(1 - X.mean(), 4))
print("chaque note a la probabilité 1/856 :", round(1 / 856, 5))

# ---------- [esp-prop] Calculer avec l'espérance ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")
Y = econ["score"]                              # Y = note finale, chaque note a la probabilité 1/856
mu = Y.mean()                                  # E(Y), moyenne de la population
print("E(Y) :", round(mu, 2))
print("E(0,2 Y) :", round((0.2 * Y).mean(), 2), "| 0,2 E(Y) :", round(0.2 * mu, 2))
print("E(Y - mu) :", round((Y - mu).mean(), 10) + 0)
print("E(Y^2) :", round((Y**2).mean(), 1), "| [E(Y)]^2 :", round(mu**2, 1))

# ---------- [variance] Variance et écart-type ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")
X = (econ["work"] >= 15).astype(int); Y = econ["score"]
p = X.mean()
print("Var(X) = p(1 - p) :", round(p * (1 - p), 4), "| E(X^2) - p^2 :", round((X**2).mean() - p**2, 4))
print("Var(Y) :", round(Y.var(ddof=0), 2), " (division par N = 856) | écart-type :", round(Y.std(ddof=0), 2))
print("E(Y^2) - [E(Y)]^2 :", round((Y**2).mean() - Y.mean()**2, 2))
print("Var(0,2 Y) = 0,04 Var(Y) :", round((0.2 * Y).var(ddof=0), 3), round(0.04 * Y.var(ddof=0), 3))

# ---------- [lgn] La moyenne empirique se rapproche de l'espérance ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
econ = wd.data("econmath")
y = econ["score"].to_numpy()
rng = np.random.default_rng(2026)
tirages = rng.choice(y, size=2000)             # 2 000 étudiants tirés au hasard, avec remise
moy = np.cumsum(tirages) / np.arange(1, 2001)  # moyenne des n premiers tirages
print("moyenne après 10, 100, 2 000 tirages :", moy[[9, 99, 1999]].round(2))
print("E(Y) :", round(y.mean(), 2))
fig, ax = plt.subplots(figsize=(5.2, 2.9))
ax.plot(np.arange(1, 2001), moy, color="#1F4E79")
ax.axhline(y.mean(), color="#C0502B", ls="--")
ax.set_xscale("log"); ax.set_ylim(60, 85)
ax.set_xticks([1, 10, 100, 1000], ["1", "10", "100", "1 000"])
ax.set_xlabel("Nombre de tirages n (échelle logarithmique)"); ax.set_ylabel("Moyenne des n notes")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_lgn.pdf"); plt.show()

# ---------- [cov-pop] Covariance et corrélation dans la population ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
econ = wd.data("econmath")
X = (econ["work"] >= 15).astype(int); Y = econ["score"]
cov = (X * Y).mean() - X.mean() * Y.mean()     # Cov(X, Y) = E(XY) - E(X)E(Y)
print("Cov(X, Y) :", round(cov, 3))
print("autre écriture, E[(X - E X)(Y - E Y)] :", round(((X - X.mean()) * (Y - Y.mean())).mean(), 3))
print("Corr(X, Y) :", round(cov / (X.std(ddof=0) * Y.std(ddof=0)), 3))

# ---------- [independance] Covariance nulle sans indépendance ----------
import numpy as np
x = np.array([-1, 0, 1]); prob = np.array([1, 1, 1]) / 3   # X prend -1, 0, 1 avec probabilité 1/3
y = x**2                                       # Y = X^2, entièrement déterminée par X
EX, EY, EXY = (prob * x).sum(), (prob * y).sum(), (prob * x * y).sum()
print("E(X), E(Y), E(XY) :", EX, round(EY, 4), EXY, "| Cov(X, Y) :", EXY - EX * EY)
print("P(X = 0 et Y = 1) :", 0, "| P(X = 0) P(Y = 1) :", round(prob[1] * (prob[0] + prob[2]), 4))

# ---------- [cond-cov] Covariance nulle, espérance conditionnelle non nulle ----------
import numpy as np
x = np.array([-1, 0, 1]); prob = np.array([1, 1, 1]) / 3   # x prend -1, 0, 1 avec probabilité 1/3
u = x**2 - 2 / 3                               # u dépend de x
print("E(u) :", round((prob * u).sum(), 10), "| Cov(x, u) = E(xu) - E(x)E(u) :", round((prob * x * u).sum(), 10))
print("E(u | x) pour x = -1, 0, 1 :", u.round(4))

# ---------- [var-moyenne] La variance d'une moyenne ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
econ = wd.data("econmath")
y = econ["score"].to_numpy()
sigma = y.std(ddof=0)                          # écart-type de la note dans la population
rng = np.random.default_rng(2026)
moyennes = rng.choice(y, size=(5000, 50)).mean(axis=1)   # 5 000 moyennes de 50 notes
print("écart-type théorique de la moyenne, sigma / racine(50) :", round(sigma / np.sqrt(50), 3))
print("écart-type des 5 000 moyennes simulées :", round(moyennes.std(ddof=1), 3))
print("moyenne des 5 000 moyennes :", round(moyennes.mean(), 2), "| E(Y) :", round(y.mean(), 2))

# ---------- [esp-cond] Espérance conditionnelle ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
econ = wd.data("econmath")
X = (econ["work"] >= 15).astype(int); Y = econ["score"]
m = Y.groupby(X).mean(); p = X.mean()          # E(Y | X = 0), E(Y | X = 1), P(X = 1)
print("E(Y|X=0), E(Y|X=1) :", m.round(2).tolist(), "| P(X=1) :", round(p, 4))
classe = (econ["colgpa"] // 0.25) * 0.25 + 0.125           # classes de colgpa de largeur 0,25
t = econ.groupby(classe)["score"].agg(["mean", "size"]); t = t[t["size"] >= 10]
print(t.round(2))
fig, ax = plt.subplots(figsize=(5.2, 3.2))
ax.scatter(econ["colgpa"], Y, s=8, color="#1F4E79", alpha=0.15)
ax.scatter(t.index, t["mean"], color="#C0502B", s=30, zorder=3)     # moyennes par classe
ax.set_xlabel("Moyenne universitaire (colgpa, sur 4)"); ax.set_ylabel("Note finale (score, %)")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout(); fig.savefig("fig_esp_cond.pdf"); plt.show()

# ---------- [iterees] La loi des espérances itérées ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")
X = (econ["work"] >= 15).astype(int); Y = econ["score"]
m = Y.groupby(X).mean(); p = X.mean()          # E(Y | X = 0), E(Y | X = 1), P(X = 1)
print("(1 - p) E(Y|X=0) + p E(Y|X=1) :", round((1 - p) * m[0] + p * m[1], 2))
print("E(Y) :", round(Y.mean(), 2), "| moyenne simple des deux :", round(m.mean(), 2))
print("Cov(X, Y) = p(1 - p)[E(Y|X=1) - E(Y|X=0)] :", round(p * (1 - p) * (m[1] - m[0]), 3))

# ---------- [var-cond] Variance conditionnelle ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import pandas as pd
econ = wd.data("econmath")
quart = pd.qcut(econ["colgpa"], 4, labels=["Q1", "Q2", "Q3", "Q4"])   # quarts de colgpa
t = econ.groupby(quart, observed=True)["score"].agg(["mean", "std", "size"])
t["std"] = econ.groupby(quart, observed=True)["score"].std(ddof=0)  # écart-type de la population
print(t.round(2))
print("bornes des quarts :", pd.qcut(econ["colgpa"], 4).cat.categories.tolist())

# ---------- [rls1] Moyennes par classe et droite des MCO ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
econ = wd.data("econmath")
b = smf.ols("score ~ colgpa", data=econ).fit().params          # droite des MCO
classe = (econ["colgpa"] // 0.25) * 0.25 + 0.125
t = econ.groupby(classe)["score"].agg(["mean", "size"]); t = t[t["size"] >= 10]
print(t.assign(droite=b["Intercept"] + b["colgpa"] * t.index).round(2))   # moyennes et droite
fig, ax = plt.subplots(figsize=(5.2, 3.2))
ax.scatter(econ["colgpa"], econ["score"], s=8, color="#1F4E79", alpha=0.15)
ax.scatter(t.index, t["mean"], color="#C0502B", s=30, zorder=3)     # moyennes par classe
ax.plot([0.9, 4], [b["Intercept"] + b["colgpa"] * v for v in (0.9, 4)], color="#8C8C8C", ls="--")
ax.set_xlabel("Moyenne universitaire (colgpa, sur 4)"); ax.set_ylabel("Note finale (score, %)")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout(); fig.savefig("fig_rls1.pdf"); plt.show()

# ---------- [rls3] Variation de x dans l'échantillon ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath")
print(econ["colgpa"].describe().round(3))      # écart-type positif : RLS.3 est satisfaite
print("nombre de valeurs différentes :", econ["colgpa"].nunique())

# ---------- [equilibre] Comparer les groupes sur ce qui est observé ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
econ = wd.data("econmath"); jt = wd.data("jtrain2")
econ["w15"] = (econ["work"] >= 15).astype(int)
print(econ.groupby("w15")[["colgpa", "mothcoll", "actmth"]].mean().round(2))
print(jt.groupby("train")[["educ", "age", "re75"]].mean().round(2))   # re75 : revenu de 1975

# ---------- [rls5] Résidus et dispersion selon colgpa ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])
import wooldridge as wd
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
econ = wd.data("econmath")
res = smf.ols("score ~ colgpa", data=econ).fit().resid       # résidus des MCO
quart = pd.qcut(econ["colgpa"], 4, labels=["Q1", "Q2", "Q3", "Q4"])
print("écart-type des résidus par quart de colgpa :")
print(res.groupby(quart, observed=True).std().round(2))
fig, ax = plt.subplots(figsize=(5.2, 3))
ax.scatter(econ["colgpa"], res, s=8, color="#1F4E79", alpha=0.35)
ax.axhline(0, color="#C0502B")
ax.set_xlabel("Moyenne universitaire (colgpa, sur 4)"); ax.set_ylabel("Résidu")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout(); fig.savefig("fig_rls5.pdf"); plt.show()

# ---------- [jouet-bruit] Exemple à la main, l'estimation est la vérité plus du bruit ----------
import numpy as np
import matplotlib.pyplot as plt
x = np.array([2, 4, 6, 8, 10]); y = np.array([7, 10, 11, 14, 13])
u = y - (6 + 1 * x)                            # erreurs, si la vraie droite est 6 + x
w = (x - x.mean()) / ((x - x.mean())**2).sum() # poids w_i
print("erreurs :", u, "| poids :", w)
b1 = 1 + (w * u).sum()                         # b1 = vraie pente + somme w_i u_i
print("b1 :", round(b1, 4))
print("avec d'autres erreurs (1, -1, 2, 0, 1), b1 :", 1 + (w * np.array([1, -1, 2, 0, 1])).sum())
g = np.linspace(0, 12, 50)
fig, axs = plt.subplots(1, 2, figsize=(6.4, 2.9), sharey=True)
axs[0].plot(g, 6 + g, color="#8C8C8C", ls="--"); axs[0].vlines(x, y, 6 + x, color="#8C8C8C")
axs[1].plot(g, 6.2 + 0.8 * g, color="#C0502B"); axs[1].vlines(x, y, 6.2 + 0.8 * x, color="#C0502B")
axs[0].set_title("Erreurs u (vraie droite 6 + x)", fontsize=10)
axs[1].set_title("Résidus (droite des MCO 6,2 + 0,8 x)", fontsize=10)
[a.scatter(x, y, color="#1F4E79", zorder=3) for a in axs]
[a.set_xlabel("x (heures)") for a in axs]; axs[0].set_ylabel("y (note sur 20)")
[a.set_xticks(range(0, 13, 2)) for a in axs]
[a.spines[["top", "right"]].set_visible(False) for a in axs]
fig.tight_layout(); fig.savefig("fig_jouet_vraie.pdf"); plt.show()

# ---------- [jouet-b0] Exemple à la main, la constante ----------
import numpy as np
x = np.array([2, 4, 6, 8, 10]); y = np.array([7, 10, 11, 14, 13])
u = y - (6 + 1 * x)                            # erreurs, si la vraie droite est 6 + x
b1 = 1 + ((x - x.mean()) * u).sum() / ((x - x.mean())**2).sum()
print("moyenne des erreurs :", u.mean(), "| b1 - 1 :", round(b1 - 1, 4))
print("b0 = 6 + moyenne(u) - (b1 - 1) x moyen :", round(6 + u.mean() - (b1 - 1) * x.mean(), 4))

# ---------- [mc-sansbiais] Simulation, l'absence de biais ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
x = wd.data("econmath")["colgpa"].head(50).to_numpy()   # x fixés : 50 premiers étudiants
w = (x - x.mean()) / ((x - x.mean())**2).sum()          # poids w_i, pente = somme w_i y_i
rng = np.random.default_rng(2026)
y = 32 + 14 * x + rng.normal(0, 11, size=(5000, 50))    # 5 000 échantillons, vraie pente 14
b1 = y @ w                                              # pente des MCO de chaque échantillon
print("moyenne des pentes :", round(b1.mean(), 3), "| écart-type :", round(b1.std(ddof=1), 3))
print("écart-type théorique 11 / racine(SCT_x) :", round(11 / np.sqrt(((x - x.mean())**2).sum()), 3))
fig, ax = plt.subplots(figsize=(5.2, 2.9))
ax.hist(b1, bins=40, color="#1F4E79", alpha=0.7)
ax.axvline(14, color="#C0502B", lw=2)                   # vraie pente
ax.set_xlabel("Pente estimée"); ax.set_ylabel("Nombre d'échantillons")
ax.spines[["top", "right"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout(); fig.savefig("fig_mc_sansbiais.pdf"); plt.show()

# ---------- [mc-biais] Simulation, un biais quand RLS.4 échoue ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
x50 = wd.data("econmath")["colgpa"].head(50).to_numpy()
x500 = np.tile(x50, 10)                        # 500 observations, même dispersion de x
rng = np.random.default_rng(2026)
poids = lambda x: (x - x.mean()) / ((x - x.mean())**2).sum()
tirer = lambda x: 32 + 14 * x + 4 * (x - 2.8) + rng.normal(0, 11, size=(5000, len(x)))
b50, b500 = tirer(x50) @ poids(x50), tirer(x500) @ poids(x500)   # E(u | x) = 4 (x - 2,8)
print("n = 50 : moyenne", round(b50.mean(), 3), " écart-type", round(b50.std(ddof=1), 3))
print("n = 500 : moyenne", round(b500.mean(), 3), " écart-type", round(b500.std(ddof=1), 3))
fig, ax = plt.subplots(figsize=(5.6, 2.9))
ax.hist(b50, bins=40, color="#8C8C8C", alpha=0.6, density=True, label="n = 50")
ax.hist(b500, bins=40, color="#1F4E79", alpha=0.7, density=True, label="n = 500")
ax.axvline(14, color="#C0502B", lw=2); ax.text(13.8, 0.4, "vraie pente 14", color="#C0502B", ha="right")
ax.set_xlabel("Pente estimée"); ax.set_yticks([]); ax.legend(frameon=False)
ax.spines[["top", "right", "left"]].set_visible(False)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
fig.tight_layout(); fig.savefig("fig_mc_biais.pdf"); plt.show()

# ---------- [mc-variance] Simulation, ce qui rend la pente précise ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
import matplotlib.pyplot as plt
x50 = wd.data("econmath")["colgpa"].head(50).to_numpy()
plans = {"référence": (x50, 11), "n = 200": (np.tile(x50, 4), 11),
    "x deux fois moins dispersé": (x50.mean() + 0.5 * (x50 - x50.mean()), 11), "sigma = 5,5": (x50, 5.5)}
rng = np.random.default_rng(2026)
sct = lambda x: ((x - x.mean())**2).sum()
simul = lambda x, s: (32 + 14 * x + rng.normal(0, s, size=(5000, len(x)))) @ ((x - x.mean()) / sct(x))
b = {k: simul(x, s) for k, (x, s) in plans.items()}
[print(f"{k:27s} théorique {s / np.sqrt(sct(x)):.3f} | simulé {b[k].std(ddof=1):.3f}")
    for k, (x, s) in plans.items()]
fig, axs = plt.subplots(1, 3, figsize=(9, 2.6), sharex=True, sharey=True)
[a.hist(b[k], bins=40, range=(-6, 34), color="#1F4E79", alpha=0.7) for a, k in zip(axs, list(plans)[:3])]
[a.axvline(14, color="#C0502B", lw=1.5) for a in axs]
[a.set_title(k, fontsize=10) for a, k in zip(axs, list(plans)[:3])]
[a.set_xlabel("Pente estimée") for a in axs]; [a.set_yticks([]) for a in axs]
[a.spines[["top", "right", "left"]].set_visible(False) for a in axs]
fig.tight_layout(); fig.savefig("fig_mc_variance.pdf"); plt.show()

# ---------- [eventail] Pourquoi la dispersion de x aide ----------
import numpy as np
import matplotlib.pyplot as plt
rng = np.random.default_rng(2026)
xl = np.linspace(1, 5, 20); xe = 3 + 0.25 * (xl - 3)    # x dispersés, puis x resserrés
e = rng.normal(0, 1, size=(20, 20))            # 20 échantillons d'erreurs, les mêmes à gauche et à droite
g = np.linspace(0, 6, 20)
droite = lambda x, u: np.polyval(np.polyfit(x, 2 + x + u, 1), g)   # droite des MCO
fig, axs = plt.subplots(1, 2, figsize=(7, 2.8), sharey=True)
[axs[0].plot(g, droite(xl, u), color="#1F4E79", alpha=0.35) for u in e]
[axs[1].plot(g, droite(xe, u), color="#1F4E79", alpha=0.35) for u in e]
[a.plot(g, 2 + g, color="#C0502B", lw=2) for a in axs]   # vraie droite 2 + x
axs[0].set_title("x entre 1 et 5", fontsize=10); axs[1].set_title("x entre 2,5 et 3,5", fontsize=10)
[a.axvspan(x.min(), x.max(), color="#8C8C8C", alpha=0.15) for a, x in zip(axs, [xl, xe])]
[a.set_xlabel("x") for a in axs]; axs[0].set_ylabel("y"); axs[0].set_ylim(-4, 12)
[a.spines[["top", "right"]].set_visible(False) for a in axs]
fig.tight_layout(); fig.savefig("fig_eventail.pdf"); plt.show()
print("écarts-types théoriques :", round(1 / np.sqrt(((xl - 3)**2).sum()), 3),
    round(1 / np.sqrt(((xe - 3)**2).sum()), 3))

# ---------- [mc-hetero] Simulation, quand RLS.5 échoue ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
x = wd.data("econmath")["colgpa"].head(50).to_numpy()
w = (x - x.mean()) / ((x - x.mean())**2).sum()
s = 3 + 14 * np.abs(x - 2.8)                   # écart-type de u, variable selon x
print("moyenne des variances de u :", round((s**2).mean(), 1))
rng = np.random.default_rng(2026)
b1 = (32 + 14 * x + rng.normal(0, 1, size=(5000, 50)) * s) @ w
print("moyenne des pentes :", round(b1.mean(), 3), "| écart-type simulé :", round(b1.std(ddof=1), 3))
print("vrai écart-type, racine(somme w_i^2 s_i^2) :", round(np.sqrt((w**2 * s**2).sum()), 3))
sct = ((x - x.mean())**2).sum()
print("formule usuelle, racine(moyenne des s_i^2 / SCT_x) :", round(np.sqrt((s**2).mean() / sct), 3))

# ---------- [mc-sigma] Simulation, estimer la variance de l'erreur ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
x = wd.data("econmath")["colgpa"].head(50).to_numpy()
sct = ((x - x.mean())**2).sum(); w = (x - x.mean()) / sct
rng = np.random.default_rng(2026)
y = 32 + 14 * x + rng.normal(0, 11, size=(5000, 50))
b1 = y @ w; b0 = y.mean(axis=1) - b1 * x.mean()
scr = ((y - b0[:, None] - b1[:, None] * x)**2).sum(axis=1)   # SCR de chaque échantillon
print("moyenne de SCR / (n - 2) :", round((scr / 48).mean(), 2))
print("moyenne de SCR / n :", round((scr / 50).mean(), 2))
print("vraie variance sigma^2 :", 121, "| 121 x 48 / 50 :", 121 * 48 / 50)

# ---------- [mc-se] Simulation, l'erreur-type ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge"])     # installe si absent
import wooldridge as wd
import numpy as np
x = wd.data("econmath")["colgpa"].head(50).to_numpy()
sct = ((x - x.mean())**2).sum(); w = (x - x.mean()) / sct
rng = np.random.default_rng(2026)
y = 32 + 14 * x + rng.normal(0, 11, size=(5000, 50))
b1 = y @ w; b0 = y.mean(axis=1) - b1 * x.mean()
scr = ((y - b0[:, None] - b1[:, None] * x)**2).sum(axis=1)   # SCR de chaque échantillon
se = np.sqrt(scr / 48 / sct)                   # erreur-type de la pente, un par échantillon
print("moyenne des erreurs-types :", round(se.mean(), 3), "| écart-type des pentes :", round(b1.std(ddof=1), 3))
print("écart-type théorique 11 / racine(SCT_x) :", round(11 / np.sqrt(sct), 3))

# ---------- [erreur-type] D'où viennent les erreurs-types de la table ----------
import subprocess, sys
subprocess.run([sys.executable, "-m", "pip", "install", "-q",
    "--disable-pip-version-check", "wooldridge", "statsmodels"])
import wooldridge as wd
import numpy as np
import statsmodels.formula.api as smf
econ = wd.data("econmath")
m1 = smf.ols("score ~ colgpa", data=econ).fit()
n = int(m1.nobs); x = econ["colgpa"]; sct = ((x - x.mean())**2).sum()
s2 = m1.ssr / (n - 2)                          # sigma chapeau au carré = SCR / (n - 2)
print("SCR :", round(m1.ssr, 1), "| sigma chapeau :", round(np.sqrt(s2), 3), "| SCT_x :", round(sct, 2))
print("se(b1) = sigma chapeau / racine(SCT_x) :", f"{np.sqrt(s2 / sct):.4f}")
print("moyenne des x au carré :", round((x**2).mean(), 3))
print("se(b0) :", f"{np.sqrt(s2 * (x**2).sum() / (n * sct)):.4f}")
print("logiciel :", m1.bse.round(3).tolist(), "| sigma chapeau :", round(np.sqrt(m1.scale), 3))
