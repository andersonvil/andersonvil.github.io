# =============================================================
# Microéconomie III, Partie III (séances V à VIII). Code Python
# des figures, tables et calculs du support.
# Chaque bloc est autonome. On peut le copier seul dans Google Colab
# ou dans Spyder et l'exécuter. Les figures sont enregistrées en PDF
# dans le dossier courant.
# =============================================================

# ---------- [fil-loterie] Fil rouge, espérance et écart-type ----------
import numpy as np
W = np.array([3600, 10000])          # patrimoine avec et sans sinistre (euros)
pi = np.array([0.1, 0.9])            # probabilités des deux états
E = (pi * W).sum()                   # espérance
V = (pi * (W - E) ** 2).sum()        # variance
print("E[W] =", round(E, 2), " Var(W) =", round(V, 2), " sigma =", round(np.sqrt(V), 2))
# avec deux issues, Var(W) = pi1 * pi2 * (W2 - W1)^2
print("formule à deux issues :", round(pi[0] * pi[1] * (W[1] - W[0]) ** 2, 2))

# ---------- [petrole] Espérance, l'exemple du pétrole ----------
pi = [1 / 4, 3 / 4]                  # succès, échec
X = [40, 20]                         # cours de l'action (dollars)
E = sum(p * x for p, x in zip(pi, X))
print("Espérance du cours :", E, "dollars par action")

# ---------- [emplois] Deux emplois d'été, variance et écart-type ----------
import numpy as np
emplois = {"Emploi 1, commission": ([0.5, 0.5], [2000, 1000]),
           "Emploi 2, salaire fixe": ([0.99, 0.01], [1510, 510])}
for nom, (pi, X) in emplois.items():
    pi, X = np.array(pi), np.array(X)
    E = (pi * X).sum()
    ec = X - E                        # écarts à l'espérance
    V = (pi * ec ** 2).sum()
    print(nom, ": E =", round(E, 2), " écarts =", np.round(ec, 2),
          " moyenne pondérée des écarts =", round((pi * ec).sum(), 10))
    print("   carrés des écarts =", (ec ** 2).tolist(), " variance =",
          round(V, 2), " écart-type =", round(np.sqrt(V), 2))

# ---------- [emplois-mod] Emploi 1 augmenté de 100 dollars ----------
import numpy as np
emplois = {"Emploi 1 modifié": ([0.5, 0.5], [2100, 1100]),
           "Emploi 2": ([0.99, 0.01], [1510, 510])}
for nom, (pi, X) in emplois.items():
    pi, X = np.array(pi), np.array(X)
    E = (pi * X).sum()
    s = np.sqrt((pi * (X - E) ** 2).sum())
    print(nom, ": revenu espéré =", round(E, 2), " écart-type =", round(s, 2))

# ---------- [fig-uniformes] Deux distributions uniformes ----------
import numpy as np
import matplotlib.pyplot as plt
x1 = np.arange(1000, 2001, 100)              # emploi 1, 11 revenus
x2 = np.arange(1300, 1701, 100)              # emploi 2, 5 revenus
for nom, x in (("Emploi 1", x1), ("Emploi 2", x2)):
    p = np.full(len(x), 1 / len(x))
    E = (p * x).sum()
    s = np.sqrt((p * (x - E) ** 2).sum())
    print(nom, ": E =", round(E, 2), " écart-type =", round(s, 2))
fig, ax = plt.subplots(figsize=(6.4, 2.9))   # chaque distribution tracée en plateau, comme le livre
ax.plot([1000, 1000, 2000, 2000], [0, 1 / 11, 1 / 11, 0], color="#8C8C8C", lw=2, label="Emploi 1")
ax.plot([1300, 1300, 1700, 1700], [0, 1 / 5, 1 / 5, 0], color="#1F4E79", lw=2, label="Emploi 2")
ax.set(xlim=(800, 2200), ylim=(0, 0.25), xticks=[1000, 1500, 2000], yticks=[0.1, 0.2])
ax.set_xlabel("Revenu (dollars)"); ax.set_ylabel("Probabilité")
ax.xaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.yaxis.set_major_formatter(lambda v, p: f"{v:.1f}".replace(".", ","))
ax.spines[["top", "right"]].set_visible(False)
ax.legend(frameon=False, loc="upper right")
fig.tight_layout()
fig.savefig("fig_uniformes.pdf")
plt.show()

# ---------- [amende] Stationnement en double file, amende espérée ----------
politiques = [(6, 1.0), (50, 0.1), (500, 0.01)]   # (amende, probabilité d'être pris)
for amende, p in politiques:
    print(f"amende {amende}, probabilité {p} : amende espérée = {amende * p:.2f}")

# ---------- [ticket] Le billet de loterie, consommation contingente ----------
richesse, prix, gain = 100, 5, 200
sans_billet = (richesse, richesse)                     # (tiré, pas tiré)
avec_billet = (richesse - prix + gain, richesse - prix)
print("sans billet :", sans_billet, " avec billet :", avec_billet)

# ---------- [plan-contingent] Le plan de la consommation contingente ----------
import numpy as np
import matplotlib.pyplot as plt
W1, W2, pi, g = 3600, 10000, 0.1, 0.1       # dotation, probabilité, taux de prime
K = np.linspace(0, 9000, 50)                 # montant assuré
c1, c2 = W1 + (1 - g) * K, W2 - g * K        # consommation avec et sans sinistre
fig, ax = plt.subplots(figsize=(5.2, 3.6))
ax.plot([3000, 11000], [3000, 11000], color="#8C8C8C", lw=1, ls="--")
ax.plot(c1, c2, color="#1F4E79", lw=2)
ax.plot(W1, W2, "o", color="#1F4E79")
ax.plot(9360, 9360, "o", color="#C0502B")
ax.annotate("dotation, K = 0", (W1, W2), (4300, 10500), fontsize=9)
ax.annotate("assurance complète\nK = 6 400", (9360, 9360), (6900, 7500), fontsize=9,
            arrowprops=dict(arrowstyle="->", color="#8C8C8C"))
ax.text(10100, 10800, "c1 = c2", color="#8C8C8C", fontsize=9)
ax.set_xlabel("c1, richesse si sinistre (euros)")
ax.set_ylabel("c2, richesse sans sinistre (euros)")
ax.set_xlim(3000, 11000); ax.set_ylim(3000, 11000)
ax.xaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.yaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_plan.pdf"); plt.show()

# ---------- [courbes-eu] Courbes d'indifférence de l'utilité espérée ----------
import numpy as np
import matplotlib.pyplot as plt
pi = 0.1                                     # probabilité du sinistre
c1 = np.linspace(500, 11000, 300)
fig, ax = plt.subplots(figsize=(5.2, 3.6))
for U, col in ((94, "#8C8C8C"), (96, "#1F4E79"), (98, "#8C8C8C")):
    c2 = ((U - pi * np.sqrt(c1)) / (1 - pi)) ** 2    # pi*sqrt(c1) + (1-pi)*sqrt(c2) = U
    ax.plot(c1, c2, color=col, lw=2 if U == 96 else 1)
    y = ((U - pi * np.sqrt(10700)) / (1 - pi)) ** 2
    ax.text(10700, y + 120, f"EU = {U}", fontsize=8)
ax.plot([3000, 11000], [3000, 11000], color="#8C8C8C", lw=1, ls="--")
ax.plot(3600, 10000, "o", color="#1F4E79"); ax.plot(9216, 9216, "o", color="#C0502B")
ax.annotate("dotation", (3600, 10000), (4100, 10450), fontsize=9)
ax.annotate("(9 216 ; 9 216)\npente $= -\\pi_1/\\pi_2 = -1/9$", (9216, 9216), (5200, 7600),
            fontsize=9, arrowprops=dict(arrowstyle="->", color="#8C8C8C"))
ax.set_xlim(3000, 11500); ax.set_ylim(7000, 11000)
ax.set_xlabel("c1, richesse si sinistre (euros)")
ax.set_ylabel("c2, sans sinistre (euros)")
ax.xaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.yaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_courbes_eu.pdf"); plt.show()

# ---------- [eu-fil] Utilité espérée, fil rouge et exemple de la Partie I ----------
import numpy as np
u = np.sqrt                                   # utilité de von Neumann-Morgenstern
EU = 0.1 * u(3600) + 0.9 * u(10000)
print("Inès : EU =", round(EU, 2), " u(E[W]) = u(9 360) =", round(u(9360), 2))
TMS_dot = 0.1 / (2 * np.sqrt(3600)) / (0.9 / (2 * np.sqrt(10000)))
print("TMS en la dotation =", round(TMS_dot, 4), " sur la diagonale =", round(0.1 / 0.9, 4))
# exemple de la Partie I : 85 000 euros certains ou (100 000 ; 40 000) avec (3/4 ; 1/4)
print("85 000 certains :", round(u(85000), 2),
      " loterie :", round(0.75 * u(100000) + 0.25 * u(40000), 2))

# ---------- [affine] Transformations affines et non affines ----------
import numpy as np
L = ([0.1, 0.9], [3600, 10000])              # loterie d'Inès sans assurance
certain = 9300                               # 9 300 euros certains
fonctions = {"u = racine(W)": lambda w: np.sqrt(w),
             "v = 2 racine(W) + 5": lambda w: 2 * np.sqrt(w) + 5,
             "w = (racine(W))^4 = W^2": lambda w: np.sqrt(w) ** 4}
for nom, f in fonctions.items():
    EL = sum(p * f(x) for p, x in zip(*L))
    choix = "certain" if f(certain) > EL else "loterie"
    print(f"{nom:24s} EU(loterie) = {EL:,.2f}  u(9 300) = {f(certain):,.2f}  -> {choix}")

# ---------- [attitudes-pr] Trois attitudes face au risque ----------
import numpy as np, matplotlib.pyplot as plt
def courbe(x, y):                    # courbe lisse (Hermite) passant par les points du livre
    x, y = np.array(x, float), np.array(y, float); h = np.diff(x); s = np.diff(y) / h
    d = np.r_[0, 2 / (1 / s[:-1] + 1 / s[1:]), 0]
    d[0], d[-1] = max(0, (3 * s[0] - d[1]) / 2), max(0, (3 * s[-1] - d[-2]) / 2)
    X = np.linspace(x[0], x[-1], 300); k = np.clip(np.searchsorted(x, X) - 1, 0, len(h) - 1); t = (X - x[k]) / h[k]
    return X, (2*t**3 - 3*t**2 + 1)*y[k] + (t**3 - 2*t**2 + t)*h[k]*d[k] + (3*t**2 - 2*t**3)*y[k+1] + (t**3 - t**2)*h[k]*d[k+1]
panneaux = {"(a) averse au risque": ([0, 10, 15, 16, 20, 30], [0, 10, 13.5, 14, 16, 18], "ABCDE"),
            "(b) goût pour le risque": ([0, 10, 20, 30], [0, 3, 8, 18], "ACE"),
            "(c) neutre au risque": ([0, 10, 20, 30], [0, 6, 12, 18], "ACE")}
fig, axs = plt.subplots(1, 3, figsize=(9.6, 3.1), gridspec_kw={"width_ratios": [1.6, 1, 1]})
for ax, (nom, (W, u, lettres)) in zip(axs, panneaux.items()):
    EU = 0.5 * u[W.index(10)] + 0.5 * u[-1]                 # loterie 10 ou 30, proba 1/2
    print(nom, ": EU =", EU, " u(20) =", u[W.index(20)])
    for x, y, n in zip(W[1:], u[1:], lettres):              # pointillés vers les axes, comme le livre
        ax.plot([0, x, x], [y, y, 0], color="#8C8C8C", ls="--", lw=0.6); ax.annotate(n, (x - 1.6, y + 0.7), fontsize=8)
    ax.plot([10, 30], [u[W.index(10)], u[-1]], color="#8C8C8C", ls="--", lw=1); ax.plot(*courbe(W, u), color="#1F4E79", lw=2)
    ax.plot(W[1:], u[1:], "o", color="#1F4E79", ms=3.5); ax.plot(20, EU, "s", color="#C0502B", ms=5)
    ax.set(title=nom, xlabel="Revenu (milliers de dollars)", xlim=(0, 33), ylim=(0, 20), xticks=W[1:], yticks=u[1:])
    ax.set_yticklabels([f"{v:g}".replace(".", ",") for v in u[1:]]); ax.tick_params(labelsize=7)
    ax.title.set_fontsize(10); ax.spines[["top", "right"]].set_visible(False)
a = axs[0]; a.set_ylabel("Utilité"); a.annotate("F", (20.6, 12.7), size=8); a.plot([16, 20], [14, 14], "--", c="#8C8C8C", lw=.6)
a.get_xticklabels()[1].set_ha("right"); a.get_xticklabels()[2].set_ha("left")      # 15 et 16 côte à côte
a.get_yticklabels()[1].set_va("top"); a.get_yticklabels()[2].set_va("bottom")     # 13,5 et 14 aussi
print("emploi actuel, 15 000 dollars certains : u = 13,5 < EU = 14")
fig.tight_layout(); fig.savefig("fig_attitudes.pdf"); plt.show()

# ---------- [prime-pr] Prime de risque, deux emplois risqués ----------
import numpy as np, matplotlib.pyplot as plt
def courbe(x, y):                    # courbe lisse (Hermite) passant par les points du livre
    x, y = np.array(x, float), np.array(y, float); h = np.diff(x); s = np.diff(y) / h
    d = np.r_[0, 2 / (1 / s[:-1] + 1 / s[1:]), 0]
    d[0], d[-1] = max(0, (3 * s[0] - d[1]) / 2), max(0, (3 * s[-1] - d[-2]) / 2)
    X = np.linspace(x[0], x[-1], 300); k = np.clip(np.searchsorted(x, X) - 1, 0, len(h) - 1); t = (X - x[k]) / h[k]
    return X, (2*t**3 - 3*t**2 + 1)*y[k] + (t**3 - 2*t**2 + t)*h[k]*d[k] + (3*t**2 - 2*t**3)*y[k+1] + (t**3 - t**2)*h[k]*d[k+1]
W, u = [0, 10, 16, 30, 40], [0, 10, 14, 18, 20]            # points O, A, C, E, G du livre (milliers de dollars)
for nom, EU in (("10 ou 30", 0.5 * 10 + 0.5 * 18), ("0 ou 40", 0.5 * 0 + 0.5 * 20)):
    EC = np.interp(EU, u, W)                                # revenu certain de même utilité
    print(nom, ": EU =", EU, " équivalent certain =", EC, " prime =", 20 - EC)
fig, ax = plt.subplots(figsize=(5.6, 3.3))
pts = {"A": (10, 10), "C": (16, 14), "F": (20, 14), "E": (30, 18), "G": (40, 20)}
for n, (x, y) in pts.items():                               # pointillés vers les axes, comme le livre
    ax.plot([0, x, x], [y, y, 0], color="#8C8C8C", ls="--", lw=0.6); ax.annotate(n, (x - 1.4, y + 0.6), fontsize=9)
ax.plot([10, 30], [10, 18], color="#8C8C8C", ls="--", lw=1)  # corde AE, F en est le milieu
ax.plot(*courbe(W, u), color="#1F4E79", lw=2)
ax.plot([16, 20], [14, 14], color="#C0502B", lw=2.5); ax.text(16.4, 12.3, "prime 4", color="#C0502B", fontsize=9)
for n, (x, y) in pts.items():
    ax.plot(x, y, "s" if n == "F" else "o", color="#C0502B" if n == "F" else "#1F4E79", ms=4.5)
ax.set(xlim=(0, 42), ylim=(0, 22), xticks=[10, 16, 20, 30, 40], yticks=[10, 14, 18, 20])
ax.set_xlabel("Revenu (milliers de dollars)"); ax.set_ylabel("Utilité")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_prime_pr.pdf"); plt.show()

# ---------- [prime-fil] Équivalent certain et prime de risque d'Inès ----------
import numpy as np
import matplotlib.pyplot as plt
W1, W2, pi = 3600, 10000, 0.1
E = pi * W1 + (1 - pi) * W2                   # 9 360
EU = pi * np.sqrt(W1) + (1 - pi) * np.sqrt(W2)
EC = EU ** 2                                  # u(EC) = EU, donc EC = EU^2
print("E[W] =", E, " EU =", EU, " EC =", EC, " prime rho =", E - EC)
w = np.linspace(2500, 10600, 300)
fig, axs = plt.subplots(1, 2, figsize=(8.4, 3.1))
for ax, (a, b) in zip(axs, ((2500, 10600), (9100, 9420))):
    ax.plot(w, np.sqrt(w), color="#1F4E79", lw=2)
    ax.plot([W1, W2], [np.sqrt(W1), np.sqrt(W2)], color="#8C8C8C", ls="--", lw=1)
    ax.plot([EC, E], [EU, EU], color="#C0502B", lw=2.5)
    ax.plot(E, np.sqrt(E), "o", color="#1F4E79"); ax.plot(E, EU, "s", color="#C0502B")
    ax.set_xlim(a, b); ax.set_ylim(np.sqrt(a) - 0.3, np.sqrt(b) + 0.3)
    ax.xaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
    ax.spines[["top", "right"]].set_visible(False); ax.set_xlabel("Richesse W (euros)")
axs[0].set_ylabel("$u(W) = \\sqrt{W}$"); axs[1].set_title("Zoom", fontsize=10)
axs[1].text(9225, 96.15, "$\\rho = 144$", color="#C0502B")
fig.tight_layout(); fig.savefig("fig_prime_fil.pdf"); plt.show()

# ---------- [trois-agents] Trois attitudes sur la loterie d'Inès ----------
import numpy as np
W, pi = np.array([3600, 10000]), np.array([0.1, 0.9])
E = (pi * W).sum()
agents = {"averse, u = racine(W)": (np.sqrt, lambda v: v ** 2),
          "neutre, u = W": (lambda w: w, lambda v: v),
          "goût du risque, u = W^2": (lambda w: w ** 2.0, np.sqrt)}
for nom, (u, u_inv) in agents.items():
    EU = (pi * u(W)).sum()
    EC = u_inv(EU)                            # équivalent certain
    print(f"{nom:24s} EC = {EC:9.1f}   prime rho = E[W] - EC = {E - EC:7.1f}")

# ---------- [courbes-mv] Courbes d'indifférence espérance et écart-type ----------
import numpy as np
import matplotlib.pyplot as plt
s = np.linspace(0, 5, 100)                     # écart-type du revenu
fig, axs = plt.subplots(1, 2, figsize=(8.0, 3.0), sharey=True)
for ax, (c, titre) in zip(axs, ((0.40, "(a) forte aversion au risque"),
                                 (0.08, "(b) faible aversion au risque"))):
    for k, nom in zip((10, 12, 14), ("U1", "U2", "U3")):
        ax.plot(s, k + c * s ** 2, color="#1F4E79", lw=1.6)   # espérance = k + c * sigma^2
        ax.text(5.05, k + c * 25, nom, fontsize=9, va="center")
    ax.set_title(titre, fontsize=10); ax.set_xticks([]); ax.set_yticks([])
    ax.set_xlabel("Écart-type du revenu"); ax.set_xlim(0, 5.6); ax.set_ylim(9, 25)
    ax.spines[["top", "right"]].set_visible(False)
axs[0].set_ylabel("Revenu espéré")
fig.tight_layout(); fig.savefig("fig_courbes_mv.pdf"); plt.show()

# ---------- [climatiseurs] Diversifier ses ventes ----------
import numpy as np
pi = np.array([0.5, 0.5])                    # été chaud, été froid
clim = np.array([30000, 12000])              # revenu si l'on ne vend que des climatiseurs
chauf = np.array([12000, 30000])             # revenu si l'on ne vend que des radiateurs
for nom, R in (("climatiseurs seuls", clim), ("radiateurs seuls", chauf),
               ("moitié-moitié", 0.5 * clim + 0.5 * chauf)):
    E = (pi * R).sum()
    s = np.sqrt((pi * (R - E) ** 2).sum())
    print(f"{nom:20s} revenus = {R}  E = {E:.0f}  écart-type = {s:.0f}")

# ---------- [correlation] Diversification et corrélation ----------
import numpy as np
import matplotlib.pyplot as plt
rho = np.linspace(-1, 1, 201)
sigma = 9000 * np.sqrt((1 + rho) / 2)        # écart-type du revenu moitié-moitié
for r in (-1, -0.5, 0, 0.5, 1):
    print(f"rho = {r:5.1f}   écart-type = {9000 * np.sqrt((1 + r) / 2):7.1f}")
print("baisse de l'écart-type pour rho = 0 :", round(1 - np.sqrt(0.5), 3))
fig, ax = plt.subplots(figsize=(5.6, 3.0))
ax.plot(rho, sigma, color="#1F4E79", lw=2)
ax.plot([-1, 0, 1], [0, 9000 * np.sqrt(0.5), 9000], "o", color="#C0502B")
ax.axhline(9000, color="#8C8C8C", ls="--", lw=1)
ax.text(-0.98, 8400, "une seule activité : 9 000", color="#8C8C8C", fontsize=9)
ax.set_xlabel("Corrélation entre les deux activités")
ax.set_ylabel("Écart-type du revenu (dollars)")
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
ax.yaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_correlation.pdf"); plt.show()

# ---------- [cambriolage] La décision de s'assurer ----------
import numpy as np
pi = np.array([0.1, 0.9])                    # cambriolage, pas de cambriolage
cas = {"sans assurance": np.array([40000, 50000]),
       "avec assurance (prime 1 000)": np.array([49000, 49000])}
for nom, W in cas.items():
    E = (pi * W).sum()
    print(f"{nom:30s} E = {E:.0f}   écart-type = {np.sqrt((pi * (W - E) ** 2).sum()):.0f}")
print("perte espérée =", 0.1 * 10000, " = prime actuariellement équitable")

# ---------- [assurance-equitable] Prime équitable, assurance complète ----------
import numpy as np
import matplotlib.pyplot as plt
W1, W2, pi, g = 3600, 10000, 0.1, 0.1
K = np.linspace(0, 9000, 50)
c1 = np.linspace(3000, 11500, 300)
fig, ax = plt.subplots(figsize=(5.2, 3.6))
ax.plot(W1 + (1 - g) * K, W2 - g * K, color="#1F4E79", lw=2)          # droite de budget
for U, col in ((96, "#8C8C8C"), (np.sqrt(9360), "#C0502B")):
    ax.plot(c1, ((U - pi * np.sqrt(c1)) / (1 - pi)) ** 2, color=col, lw=1.4)
ax.plot([3000, 11500], [3000, 11500], color="#8C8C8C", lw=1, ls="--")
ax.plot(W1, W2, "o", color="#1F4E79"); ax.plot(9360, 9360, "o", color="#C0502B")
ax.annotate("optimum (9 360 ; 9 360)", (9360, 9360), (5200, 9050), fontsize=9,
            arrowprops=dict(arrowstyle="->", color="#8C8C8C"))
ax.set_xlim(3000, 11500); ax.set_ylim(8900, 10250)
ax.set_xlabel("c1, richesse si sinistre (euros)")
ax.set_ylabel("c2, sans sinistre (euros)")
ax.xaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.yaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_assurance_equitable.pdf"); plt.show()
print("EU sans assurance = 96, EU assurance complète =", round(np.sqrt(9360), 3))

# ---------- [assurance-chargee] Prime chargée, assurance partielle ----------
import numpy as np
import matplotlib.pyplot as plt
W1, W2, pi = 3600, 10000, 0.1
K = np.linspace(0, 8000, 801)
fig, ax = plt.subplots(figsize=(5.6, 3.1))
for g, col in ((0.10, "#1F4E79"), (0.12, "#C0502B")):
    EU = pi * np.sqrt(W1 + (1 - g) * K) + (1 - pi) * np.sqrt(W2 - g * K)
    k2 = ((1 - pi) * g / (pi * (1 - g))) ** 2          # c2 / c1 à l'optimum
    Ks = (W2 - k2 * W1) / (g + k2 * (1 - g))           # montant assuré optimal
    EUs = pi * np.sqrt(W1 + (1 - g) * Ks) + (1 - pi) * np.sqrt(W2 - g * Ks)
    print(f"gamma = {g:.2f}   K* = {Ks:7.1f}   EU(K*) = {EUs:.3f}   EU(0) = 96")
    ax.plot(K, EU, color=col, lw=2, label=f"$\\gamma$ = {g:.2f}".replace(".", ","))
    ax.plot(Ks, EUs, "o", color=col)
ax.set_xlabel("Montant assuré K (euros)"); ax.set_ylabel("Utilité espérée")
ax.xaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
ax.yaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
ax.spines[["top", "right"]].set_visible(False); ax.legend(frameon=False)
fig.tight_layout(); fig.savefig("fig_assurance_chargee.pdf"); plt.show()

# ---------- [mutualisation] Coût moyen par assuré, loi exacte ----------
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import binom
pi, L = 0.1, 6400
fig, axs = plt.subplots(1, 4, figsize=(8.6, 2.5), sharey=False)
for ax, N in zip(axs, (1, 10, 100, 1000)):
    S = np.arange(N + 1)                       # nombre de sinistres
    cout, prob = L * S / N, binom.pmf(S, N, pi)  # coût par assuré et probabilité
    s = np.sqrt((prob * (cout - 640) ** 2).sum())
    queue = binom.sf(800 * N / L, N, pi)         # P(coût > 800)
    print(f"N = {N:5d}  moyenne = {(cout * prob).sum():6.1f}  écart-type = {s:7.1f}"
          f"  P(coût > 800) = {queue:.4f}")
    ax.vlines(cout, 0, prob, color="#1F4E79", lw=1.5)
    ax.axvline(640, color="#C0502B", lw=1, ls="--")
    ax.set_title(f"N = {N}", fontsize=10); ax.set_xlim(-300, 6900); ax.set_yticks([])
    ax.xaxis.set_major_formatter(lambda v, p: f"{v:,.0f}".replace(",", " "))
    ax.spines[["top", "right", "left"]].set_visible(False)
entre = binom.cdf(125, 1000, pi) - binom.cdf(78, 1000, pi)
print("N = 1000, P(500 <= coût <= 800) =", round(entre, 4))
fig.supxlabel("Coût moyen par assuré (euros)", fontsize=10)
fig.tight_layout(); fig.savefig("fig_mutualisation.pdf"); plt.show()

# ---------- [titre] Assurance de titre, enchère d'un acheteur neutre ----------
valeur, p_probleme = 300000, 1 / 20
enchere = (1 - p_probleme) * valeur + p_probleme * 0
print("enchère maximale d'un acheteur neutre au risque :", enchere, "dollars")
print("perte espérée sans assurance :", p_probleme * valeur, " frais d'assurance : 1 500")

# ---------- [costumes] Valeur de l'information ----------
import numpy as np
profit = {"commander 50": np.array([5000, 5000]),       # ventes de 50, ventes de 100
          "commander 100": np.array([1500, 12000])}
pi = np.array([0.5, 0.5])
for nom, x in profit.items():
    print(f"{nom:14s} profit espéré = {(pi * x).sum():.0f}")
info = (pi * np.array([5000, 12000])).sum()            # on commande juste dans chaque état
print("avec information complète :", info, "  valeur de l'information :", info - 6750)

# ---------- [rendements] Rendement nominal et réel ----------
actifs = {"obligation": (1000, 1000, 100),              # (prix début, prix fin, flux)
          "immeuble (millions)": (10, 11, 0.5),
          "action GM": (80, 72, 4)}
inflation = 0.05
for nom, (p0, p1, flux) in actifs.items():
    r = (p1 - p0 + flux) / p0
    print(f"{nom:20s} rendement = {r:6.1%}   rendement réel = {r - inflation:6.1%}")

# ---------- [table58] Risque et rendement, 1926 à 2010 ----------
import pandas as pd
t = pd.DataFrame({"rendement moyen": [11.9, 6.2, 3.7],
                  "rendement réel moyen": [8.7, 3.3, 0.7],
                  "écart-type": [20.4, 8.3, 3.1]},
                 index=["actions (S&P 500)", "obligations d'entreprise", "bons du Trésor"])
print(t)
ex = t["rendement réel moyen"] - t.loc["bons du Trésor", "rendement réel moyen"]
print("supplément de rendement réel sur les bons du Trésor :")
print(ex.round(1).to_string())

# ---------- [premier-euro] Combien investir dans l'actif risqué ----------
import numpy as np
import matplotlib.pyplot as plt
w = 100                                         # richesse, u = ln
x = np.linspace(0, 60, 300)                     # montant investi
fig, axs = plt.subplots(1, 2, figsize=(8.0, 2.8))
for ax, (rg, rb, titre) in zip(axs, ((0.3, -0.5, "A. rendement espéré -0,1"),
                                      (0.5, -0.4, "B. rendement espéré +0,05"))):
    EU = 0.5 * np.log(w + x * rg) + 0.5 * np.log(w + x * rb)
    xs = max(0, -w * (rg + rb) / (2 * rg * rb))   # condition du premier ordre
    print(titre, ": x* =", round(xs, 2))
    ax.plot(x, EU, color="#1F4E79", lw=2)
    ax.plot(xs, 0.5 * np.log(w + xs * rg) + 0.5 * np.log(w + xs * rb), "o", color="#C0502B")
    ax.set_title(titre, fontsize=10); ax.set_xlabel("Montant investi x")
    ax.set_yticks([]); ax.spines[["top", "right"]].set_visible(False)
axs[0].set_ylabel("Utilité espérée")
fig.tight_layout(); fig.savefig("fig_premier_euro.pdf"); plt.show()

# ---------- [droite-budget] La droite budgétaire risque et rendement ----------
import numpy as np
import matplotlib.pyplot as plt
rf, rm, sm = 4, 12, 20                         # en %, sigma_m = 20 est une hypothèse
p = (rm - rf) / sm                             # prix du risque
print("prix du risque p =", p)
for x in (0, 0.5, 1, 1.5):
    print(f"x = {x:3.1f}  r_x = {x * rm + (1 - x) * rf:5.1f} %  sigma_x = {x * sm:5.1f} %")
s = np.linspace(0, 32, 100)
fig, ax = plt.subplots(figsize=(5.4, 3.1))
ax.plot(s, rf + p * s, color="#1F4E79", lw=2)
ax.plot([0, 10, 20, 30], [4, 8, 12, 16], "o", color="#1F4E79")
for sx, rx, t in ((0, 4, "0"), (10, 8, "0,5"), (20, 12, "1"), (30, 16, "1,5")):
    ax.annotate("x = " + t, (sx, rx), (sx + 0.8, rx - 1.6), fontsize=9)
ax.set_xlabel("Écart-type du rendement (%)"); ax.set_ylabel("Rendement espéré (%)")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_droite_budget.pdf"); plt.show()

# ---------- [deux-investisseurs] Deux investisseurs, deux portefeuilles ----------
import numpy as np
import matplotlib.pyplot as plt
rf, rm, sm = 0.04, 0.12, 0.20
s = np.linspace(0, 0.5, 200)
fig, ax = plt.subplots(figsize=(5.4, 3.3))
ax.plot(100 * s, 100 * (rf + (rm - rf) / sm * s), color="#1F4E79", lw=2)
for a, nom, col in ((4, "A", "#8C8C8C"), (1, "B", "#C0502B")):
    x = (rm - rf) / (a * sm ** 2)                 # part optimale de l'actif risqué
    r, sig = rf + x * (rm - rf), x * sm
    U = r - a / 2 * sig ** 2                      # utilité atteinte
    print(f"investisseur {nom} (a = {a}) : x* = {x:.2f}  r = {100 * r:.1f} %"
          f"  sigma = {100 * sig:.1f} %")
    ax.plot(100 * s, 100 * (U + a / 2 * s ** 2), color=col, lw=1.4)
    ax.plot(100 * sig, 100 * r, "o", color=col); ax.text(100 * sig + 1, 100 * r - 1.6, nom)
ax.plot(20, 12, "s", color="#1F4E79"); ax.text(20.8, 11.2, "marché", fontsize=9)
ax.set_xlim(0, 50); ax.set_ylim(0, 30)
ax.set_xlabel("Écart-type du rendement (%)"); ax.set_ylabel("Rendement espéré (%)")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_deux_investisseurs.pdf"); plt.show()

# ---------- [actifs-xy] Choisir l'actif risqué à combiner ----------
import numpy as np
rf, a = 0.04, 5                                 # actif sûr, aversion au risque
actifs = {"X": (0.08, 0.10), "Y": (0.14, 0.20)}  # (rendement espéré, écart-type)
for nom, (r, s) in actifs.items():
    tout = r - a / 2 * s ** 2                    # tout placer dans l'actif risqué
    x = (r - rf) / (a * s ** 2)                  # meilleur mélange avec l'actif sûr
    melange = rf + x * (r - rf) - a / 2 * (x * s) ** 2
    print(f"{nom} : prix du risque = {(r - rf) / s:.2f}  U(tout) = {tout:.4f}"
          f"  x* = {x:.2f}  U(mélange) = {melange:.4f}")

# ---------- [actifs-ab] Deux actifs corrélés négativement ----------
import numpy as np
pi = np.array([0.5, 0.5])
A, B = np.array([10, -5]), np.array([-5, 10])     # valeurs dans les deux états
for nom, X in (("A seul", A), ("B seul", B), ("A et B", A + B)):
    E = (pi * X).sum()
    s = np.sqrt((pi * (X - E) ** 2).sum())
    print(f"{nom:7s} valeurs = {X}  espérance = {E:5.2f}  écart-type = {s:.2f}")

# ---------- [beta] Calculer un bêta ----------
import numpy as np
rm = np.array([-10, 0, 10, 20])              # marché (%), 4 états équiprobables
actifs = {"D": np.array([-2, -1, 4, 13]),
          "C": np.array([-13, -4, 11, 32]),
          "Z": np.array([12, -8, -8, 12])}
cov = lambda x, y: ((x - x.mean()) * (y - y.mean())).mean()
print(f"marché : moyenne = {rm.mean():.1f}  écart-type = {np.sqrt(cov(rm, rm)):.2f}")
for nom, r in actifs.items():
    print(f"{nom} : moyenne = {r.mean():5.2f}  écart-type = {np.sqrt(cov(r, r)):6.2f}"
          f"  bêta = cov/var = {cov(r, rm) / cov(rm, rm):.2f}")

# ---------- [droite-marche] La droite de marché ----------
import numpy as np
import matplotlib.pyplot as plt
rf, rm = 2, 5                                  # en %
b = np.linspace(0, 2, 50)
fig, ax = plt.subplots(figsize=(5.4, 3.1))
ax.plot(b, rf + b * (rm - rf), color="#1F4E79", lw=2)
for beta, r, nom in ((0, 2, "Z"), (0.5, 3.5, "D"), (1, 5, "M"), (1.5, 6.5, "C")):
    ax.plot(beta, r, "o", color="#1F4E79"); ax.text(beta + 0.04, r - 0.55, nom)
ax.plot(1, 7, "o", color="#C0502B"); ax.text(1.05, 7.1, "S", color="#C0502B")
ax.annotate("", (1, 5.2), (1, 6.8), arrowprops=dict(arrowstyle="->", color="#C0502B"))
ax.set_xlabel("Bêta"); ax.set_ylabel("Rendement espéré (%)")
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_droite_marche.pdf"); plt.show()
for beta in (0, 0.5, 1, 1.5):
    print(f"bêta = {beta:3.1f}  rendement d'équilibre = {rf + beta * (rm - rf):.1f} %")

# ---------- [ajustement] Comment le prix s'ajuste ----------
rf, rm, beta = 0.02, 0.05, 1.0
p1 = 107                                       # prix attendu dans un an (euros)
r_requis = rf + beta * (rm - rf)
print(f"rendement requis = {r_requis:.1%}")
print(f"au prix de 100 euros, rendement espéré = {(p1 - 100) / 100:.1%}")
print(f"prix d'équilibre = {p1 / (1 + r_requis):.2f} euros")

# ---------- [fonds] Fonds gérés et fonds indiciel ----------
import numpy as np
import matplotlib.pyplot as plt
x, y, z, u = 2026, 10, 9, []                   # Wichmann et Hill, graine fixée
for k in range(60):
    x, y, z = 171 * x % 30269, 172 * y % 30307, 170 * z % 30323
    u.append((x / 30269 + y / 30307 + z / 30323) % 1)
u = np.array(u)
rf, rm = 2, 6                                  # en %
beta = 0.6 + 0.8 * u[:30]                      # 30 fonds fictifs
r = rf + beta * (rm - rf) - 1.2 + 3 * (u[30:] - 0.5)   # frais et hasard
print("fonds sous la droite :", (r < rf + beta * (rm - rf)).sum(), "sur 30")
fig, ax = plt.subplots(figsize=(5.4, 3.0))
ax.plot([0, 1.6], [rf, rf + 1.6 * (rm - rf)], color="#1F4E79", lw=2)
ax.plot(beta, r, "o", color="#8C8C8C", ms=4); ax.plot(1, rm, "s", color="#C0502B")
ax.text(1.03, rm - 0.6, "fonds indiciel", color="#C0502B", fontsize=9)
ax.set_xlabel("Bêta"); ax.set_ylabel("Rendement moyen (%)")
ax.xaxis.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_fonds.pdf"); plt.show()

# ---------- [cascade] Cascades informationnelles ----------
import numpy as np
import matplotlib.pyplot as plt
x, y, z, freq = 2026, 10, 9, []                 # Wichmann et Hill, graine fixée
Q = (0.55, 0.6, 0.7, 0.8, 0.9)
for qq in Q:
    erreurs = 0
    for s in range(10000):                      # 10 000 files de 20 personnes
        d = 0                                   # avance de A parmi les signaux révélés
        for i in range(20):
            x, y, z = 171 * x % 30269, 172 * y % 30307, 170 * z % 30323
            u = (x / 30269 + y / 30307 + z / 30323) % 1
            s_ = 1 if u < qq else -1            # signal, +1 pour A (le bon choix)
            choix = s_ if abs(d) < 2 else (1 if d > 0 else -1)   # au-delà de 2, on imite
            d = d + s_ if abs(d) < 2 else d
        erreurs += choix < 0                    # la dernière personne choisit B
    freq.append(erreurs / 10000)
    print(f"q = {qq:.2f}  simulation = {freq[-1]:.4f}  théorie = {(1 - qq) ** 2 / (qq ** 2 + (1 - qq) ** 2):.4f}")
q = np.linspace(0.5, 1, 101); fig, ax = plt.subplots(figsize=(5.4, 3.0))
ax.plot(q, (1 - q) ** 2 / (q ** 2 + (1 - q) ** 2), color="#1F4E79", lw=2, label="théorie")
ax.plot(Q, freq, "o", color="#C0502B", label="simulation")
ax.set(xlabel="Précision du signal privé q", ylabel="Proba. de cascade erronée")
for a in (ax.xaxis, ax.yaxis): a.set_major_formatter(lambda v, p: f"{v:g}".replace(".", ","))
ax.spines[["top", "right"]].set_visible(False); ax.legend(frameon=False)
fig.tight_layout(); fig.savefig("fig_cascade.pdf"); plt.show()

# ---------- [pelles] Demande de pelles à neige et équité ----------
import numpy as np
import matplotlib.pyplot as plt
P = np.linspace(0, 50, 100)
fig, ax = plt.subplots(figsize=(5.0, 3.2))
ax.plot(100 - 2 * P, P, color="#8C8C8C", lw=2)                     # D1, temps normal
ax.plot([160, 110, 0], [0, 25, 30], color="#1F4E79", lw=2)         # D2, après la tempête
ax.text(5, 46, "D1", color="#8C8C8C"); ax.text(4, 31.5, "D2", color="#1F4E79")
for p in (20, 25, 40):
    ax.axhline(p, color="#8C8C8C", lw=0.6, ls=":")
ax.plot([60, 120], [20, 20], "o", color="#C0502B")
ax.text(61, 21, "Q1", fontsize=9); ax.text(121, 21, "Q2", fontsize=9)
ax.set_yticks([20, 25, 40]); ax.set_xticks([])
ax.set_xlabel("Quantité de pelles"); ax.set_ylabel("Prix (dollars)")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout(); fig.savefig("fig_pelles.pdf"); plt.show()

# ---------- [emploi-risque] Un emploi sûr ou un emploi risqué ----------
u = {10: 10, 15: 13.5, 20: 16, 30: 18}       # utilité, revenus en milliers de dollars
EU = 0.5 * u[10] + 0.5 * u[30]                # offre risquée, 10 ou 30
print("offre risquée : E =", 0.5 * 10 + 0.5 * 30, " EU =", EU)
print("15 000 certains : u =", u[15], "  20 000 certains : u =", u[20])

# ---------- [avous] Racine ou logarithme ----------
import numpy as np
pi, W = np.array([0.1, 0.9]), np.array([3600, 10000])
for nom, u in (("racine", np.sqrt), ("logarithme", np.log)):
    EU = (pi * u(W)).sum()
    choix = "garde le risque" if EU > u(9200) else "prend 9 200 certains"
    print(f"{nom:10s} EU = {EU:.3f}  u(9 200) = {u(9200):.3f}  -> {choix}")

# ---------- [decomposition] Prime maximale pour une assurance complète ----------
import numpy as np
EC = (0.1 * np.sqrt(3600) + 0.9 * np.sqrt(10000)) ** 2   # équivalent certain
print("prime maximale =", round(10000 - EC, 2))
print("dont perte espérée =", 0.1 * 6400, " et prime de risque =", round(9360 - EC, 2))

# ---------- [portefeuille-pr] Rendement et risque du portefeuille ----------
rf, rm, sm, x = 0.04, 0.12, 0.20, 0.5        # sigma_m = 20 % est une hypothèse
print(f"r_x = {100 * (x * rm + (1 - x) * rf):.1f} %   sigma_x = {100 * x * sm:.1f} %")

# ---------- [mv-simple] Part optimale de l'actif risqué ----------
rf, rm, sm = 0.04, 0.12, 0.20
for a in (1, 2, 4, 8):
    print(f"a = {a}   x* = {(rm - rf) / (a * sm ** 2):.3f}")

# ---------- [res-portefeuille] Probabilité d'une forte perte annuelle ----------
from scipy.stats import norm                  # rendements supposés normaux
for nom, r, s in (("A", 0.08, 0.10), ("B", 0.20, 0.40)):
    print(f"{nom} : P(rendement < -40 %) = {norm.cdf((-0.40 - r) / s):.4f}")

# ---------- [formule-cascade] Probabilité d'une cascade erronée ----------
for q in (0.6, 0.7, 0.8):
    print(f"q = {q}   P(cascade erronée) = {(1 - q) ** 2 / (q ** 2 + (1 - q) ** 2):.4f}")

# ---------- [alt-petrole] Exemple alternatif, action de biotechnologie ----------
print("espérance du cours :", round(0.3 * 80 + 0.7 * 35, 2), "euros")

# ---------- [alt-emplois] Exemple alternatif, deux emplois étudiants ----------
import numpy as np
emplois = {"A": ([0.5, 0.5], [1800, 1000]), "B": ([0.9, 0.1], [1500, 500]),
           "A + 100": ([0.5, 0.5], [1900, 1100])}
for nom, (pi, X) in emplois.items():
    pi, X = np.array(pi), np.array(X)
    E = (pi * X).sum(); V = (pi * (X - E) ** 2).sum()
    print(f"{nom:8s} E = {E:.0f}  variance = {V:.0f}  écart-type = {np.sqrt(V):.0f}")

# ---------- [alt-amende] Exemple alternatif, fraude dans les transports ----------
for p, amende in ((0.05, 30), (0.05, 50), (0.02, 50), (0.02, 120)):
    print(f"p = {p}  amende = {amende}  espérée = {p * amende:.2f}  seuil = {2 / p:.0f}")

# ---------- [alt-ticket] Exemple alternatif, jeu à gratter ----------
richesse, prix, gain = 50, 2, 100
print("avec ticket : gagnant", richesse - prix + gain, " perdant", richesse - prix)

# ---------- [alt-attitudes] Exemple alternatif, le pari de Varian ----------
import numpy as np
EU = 0.5 * np.sqrt(5) + 0.5 * np.sqrt(15)
print(f"racine : EU = {EU:.3f}  u(10) = {np.sqrt(10):.3f}  EC = {EU ** 2:.2f}"
      f"  rho = {10 - EU ** 2:.2f}")
print("carré  : EU =", 0.5 * 25 + 0.5 * 225, " u(10) =", 100)

# ---------- [alt-prime-pr] Exemple alternatif, CDD ou indépendant ----------
import numpy as np
for bas, haut in ((1000, 3000), (0, 4000)):
    EU = 0.5 * np.sqrt(bas) + 0.5 * np.sqrt(haut)
    print(f"{bas} ou {haut} : EU = {EU:.2f}  EC = {EU ** 2:.0f}"
          f"  rho = {2000 - EU ** 2:.0f}")

# ---------- [alt-divers] Exemple alternatif, lunettes et imperméables ----------
pluie, soleil = (20, 5), (5, 20)              # valeur d'une action (pluie, soleil)
parts = {"imperméables": (10, 0), "lunettes": (0, 10), "moitié-moitié": (5, 5)}
for nom, (a, b) in parts.items():
    v = [a * pluie[k] + b * soleil[k] for k in range(2)]
    print(f"{nom:14s} valeur = {v}  espérance = {0.5 * v[0] + 0.5 * v[1]}")

# ---------- [alt-cambriolage] Exemple alternatif, l'assurance selon Varian ----------
W, perte, pi = 35000, 10000, 0.01
print("sans assurance : E =", round((1 - pi) * W + pi * (W - perte), 2))
print("assurance de 10 000, prime 100 :", W - 100, "dans les deux états")
print("1 000 ménages : sinistres attendus =", 1000 * pi,
      " coût moyen par ménage =", 1000 * pi * perte / 1000)

# ---------- [alt-titre] Exemple alternatif, scooter d'occasion ----------
print("offre maximale d'une acheteuse neutre :", round(0.95 * 3000, 2), "euros")

# ---------- [alt-info] Exemple alternatif, le glacier ----------
prix, rachat = 3, 0.40
p200 = [3 * 200 - 1.2 * 200] * 2                     # commander 200 (demande 200, 400)
p400 = [3 * 200 + rachat * 200 - 400, 3 * 400 - 400]  # commander 400
print("commander 200 :", p200, " espérance =", round(sum(p200) / 2, 2))
print("commander 400 :", p400, " espérance =", round(sum(p400) / 2, 2))
info = 0.5 * 360 + 0.5 * 800
print("information parfaite :", info, " valeur =", round(info - 540, 2))

# ---------- [alt-rendements] Exemple alternatif, trois placements ----------
actifs = {"livret": (1000, 1000, 30), "appartement": (200000, 206000, 8000),
          "action": (40, 36, 1.2)}                      # (prix début, prix fin, flux)
for nom, (p0, p1, flux) in actifs.items():
    r = (p1 - p0 + flux) / p0
    print(f"{nom:12s} rendement = {100 * r:5.1f} %   réel = {100 * (r - 0.02):5.1f} %")

# ---------- [alt-portefeuille] Exemple alternatif, livret et fonds indiciel ----------
rf, rm, sm = 0.03, 0.08, 0.15
print(f"prix du risque = {(rm - rf) / sm:.4f}")
print(f"x = 0,4 : r = {100 * (rf + 0.4 * (rm - rf)):.1f} %  sigma = {100 * 0.4 * sm:.1f} %")
for a in (5, 2):
    print(f"a = {a}   x* = {(rm - rf) / (a * sm ** 2):.2f}")

# ---------- [alt-ab] Exemple alternatif, vélos et cinéma ----------
import numpy as np
parts = {"vélos": [12, -4], "cinéma": [-4, 12], "les deux": [8, 8]}  # soleil, pluie
for nom, X in parts.items():
    X = np.array(X); E = X.mean()
    print(f"{nom:9s} espérance = {E:.0f}  écart-type = {np.sqrt(((X - E) ** 2).mean()):.0f}")

# ---------- [alt-fonds] Exemple alternatif, fonds géré et fonds indiciel ----------
rf, rm, beta = 2.5, 6.5, 1.2
print(f"fonds indiciel à {100 * beta:.0f} % : rendement = {rf + beta * (rm - rf):.1f} %")
print("fonds géré : rendement = 7 %")

# ---------- [alt-bulle] Exemple alternatif, rendement locatif ----------
for prix in (120000, 200000):
    print(f"prix = {prix}   rendement locatif = {100 * 6000 / prix:.1f} %")

# ---------- [alt-cascade] Exemple alternatif, deux bouchons lyonnais ----------
q = 0.7
print("deux premiers avis faux :", round((1 - q) ** 2, 4))
print("cascade sur B :", round((1 - q) ** 2 / (q ** 2 + (1 - q) ** 2), 4))

# ---------- [alt-taxi] Exemple alternatif, chauffeur de VTC ----------
for gain_horaire in (40, 20):
    print(f"gain horaire {gain_horaire} : heures pour gagner 200 = {200 / gain_horaire:.0f}")
