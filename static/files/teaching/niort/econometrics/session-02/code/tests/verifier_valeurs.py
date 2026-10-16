# Vérifie les valeurs des documents de la séance 2 qui ne figurent pas telles
# quelles dans les sorties des blocs de code (repérées par audit_valeurs.py).
# Chaque ligne affiche la valeur calculée et la valeur écrite dans le texte.
import numpy as np
import wooldridge as wd
import statsmodels.formula.api as smf

def controle(libelle, calcule, ecrit, tol):
    ok = abs(calcule - ecrit) <= tol
    print(f"{'OK ' if ok else 'ÉCHEC'} {libelle:60s} calculé {calcule:12.5f}  texte {ecrit}")
    return ok

e = wd.data("econmath"); x, y = e["colgpa"], e["score"]
m = smf.ols("score ~ colgpa", data=e).fit()
x50 = x.to_numpy()[:50]; sct50 = ((x50 - x50.mean())**2).sum()
sct = ((x - x.mean())**2).sum()
res = [
    controle("écart entre moyenne des 1 000 pentes et 14,32", 14.38 - 14.32, 0.06, 1e-9),
    controle("écart-type de la moyenne de 1 000 pentes, 3,19/racine(1000)", 3.19 / np.sqrt(1000), 0.10, 0.005),
    controle("borne de Tchebychev, Var(Y)/(n eps^2), n = 2 000, eps = 1", y.var(ddof=0) / 2000, 0.09, 0.005),
    controle("constante des MCO, ECONMATH", m.params["Intercept"], 32.31, 0.005),
    controle("SCR, ECONMATH", m.ssr, 103036.6, 0.05),
    controle("sigma chapeau carré = SCR/854", m.ssr / 854, 120.65, 0.005),
    controle("racine de SCT_x", np.sqrt(sct), 15.70, 0.005),
    controle("SCT_x des 50 premiers colgpa", sct50, 17.81, 0.005),
    controle("4 SCT_x (chaque x répété 4 fois)", 4 * sct50, 71.24, 0.005),
    controle("SCT_x / 4 (dispersion divisée par 2)", sct50 / 4, 4.45, 0.005),
    controle("121 x 2 / 50", 121 * 2 / 50, 4.8, 0.05),
    controle("sigma divisé par 2", 11 / 2, 5.5, 1e-12),
    controle("1/n somme x^2 = moyenne^2 + SCT_x/n", x.mean()**2 + sct / 856, 8.21, 0.005),
]
# RLS.1, écarts entre moyennes de classe et droite
classe = (x // 0.25) * 0.25 + 0.125
t = e.groupby(classe)["score"].agg(["mean", "size"]); t = t[t["size"] >= 10]
ecart = t["mean"] - (m.params["Intercept"] + m.params["colgpa"] * t.index)
res += [controle("RLS.1, écart de la classe la plus basse", ecart.iloc[0], 2.7, 0.05),
        controle("RLS.1, écart de la deuxième classe", ecart.iloc[1], 4.3, 0.05),
        controle("RLS.1, effectif des deux classes inférieures", t["size"].iloc[:2].sum(), 55, 0),
        controle("RLS.1, plus grand écart des autres classes", ecart.iloc[2:].abs().max(), 1.3, 0.05)]
# Éventail, x régulièrement espacés de 1 à 5 (20 points), puis resserrés de 2,5 à 3,5
xa = np.linspace(1, 5, 20); xb = 3 + (xa - 3) / 4
res += [controle("éventail, sd théorique à gauche", 1 / np.sqrt(((xa - xa.mean())**2).sum()), 0.18, 0.005),
        controle("éventail, sd théorique à droite", 1 / np.sqrt(((xb - xb.mean())**2).sum()), 0.74, 0.005)]
# Quiz et exercices
res += [controle("quiz 6, moyenne pondérée", 0.4 * 12 + 0.6 * 10, 10.8, 1e-9),
        controle("quiz 6, poids inversés", 0.6 * 12 + 0.4 * 10, 11.2, 1e-9),
        controle("ex. 1, accroissement de E(y|x) pour 2 heures", 2 * 7 / 6, 2.333, 0.0005),
        controle("ex. 2, rapport des variances 40/32", 40 / 32, 1.25, 1e-12)]
n = wd.data("nbasal"); mn = smf.ols("wage ~ points", data=n).fit()
res += [controle("ex. 3, (sigma chapeau / se) au carré", (np.sqrt(mn.scale) / 7.82)**2, 9323, 0.5),
        controle("ex. 3, s_x", np.sqrt(9323 / 268), 5.90, 0.005),
        controle("ex. 3, SCT_points exact", ((n["points"] - n["points"].mean())**2).sum(), 9331, 0.5),
        controle("ex. 3, 111,33 x 10", 111.33 * 10, 1113.30, 1e-9),
        controle("ex. 3, prévision pour 10 points", 287.10 + 1113.30, 1400.4, 0.05),
        controle("ex. 3, prévision pour 20 points", 287.10 + 2226.6, 2513.7, 0.05)]
print(sum(res), "contrôles réussis sur", len(res))
