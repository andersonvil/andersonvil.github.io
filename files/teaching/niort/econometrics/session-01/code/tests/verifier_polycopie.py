# Vérifie les valeurs numériques du polycopié qui ne figurent pas déjà dans
# les sorties des blocs de code du support. Chaque ligne affiche la valeur
# calculée et la valeur écrite dans le texte.
import itertools
import numpy as np
import wooldridge as wd

def controle(libelle, calcule, ecrit, tol):
    ok = abs(calcule - ecrit) <= tol
    print(f"{'OK ' if ok else 'ÉCHEC'} {libelle:55s} calculé {calcule:12.5f}  texte {ecrit}")
    return ok

e = wd.data("econmath"); x, y = e["colgpa"], e["score"]
res = [
    controle("ECONMATH s_xy", x.cov(y), 4.13, 0.005),
    controle("ECONMATH r_xy", x.corr(y), 0.5735, 0.00005),
    controle("ECONMATH s_y / s_x", y.std() / x.std(), 24.96, 0.005),
    controle("ECONMATH r s_y / s_x = pente", x.corr(y) * y.std() / x.std(), 14.32, 0.005),
    controle("ECONMATH s_xy avec la note sur 20", x.cov(0.2 * y), 0.83, 0.005),
    controle("ECONMATH r_xy avec la note sur 20", x.corr(0.2 * y), 0.57, 0.005),
    controle("ECONMATH minimum de colgpa", x.min(), 0.875, 1e-9),
    controle("ECONMATH moyenne de colgpa", x.mean(), 2.815, 0.0005),
    controle("ECONMATH moyenne de score", y.mean(), 72.60, 0.005),
    controle("n/(n-1) - 1 en %, n = 856", 100 * (856 / 855 - 1), 0.12, 0.005),
    controle("n/(n-1) - 1 en %, n = 5", 100 * (5 / 4 - 1), 25, 1e-9),
]
a, b = np.array([2, 4, 6, 8, 10]), np.array([4, 5, 6, 7, 8])
res += [controle("écart absolu moyen de A", np.abs(a - 6).mean(), 2.4, 1e-9),
        controle("écart absolu moyen de B", np.abs(b - 6).mean(), 1.2, 1e-9)]
z = np.array([-2, -1, 0, 1, 2]); res.append(controle("r entre x et x au carré", np.corrcoef(z, z**2)[0, 1], 0, 1e-12))
xj, yj = np.array([2, 4, 6, 8, 10]), np.array([7, 10, 11, 14, 13])
sa = lambda b0, b1: np.abs(yj - b0 - b1 * xj).sum()
sc = lambda b0, b1: ((yj - b0 - b1 * xj)**2).sum()
res += [controle("somme des résidus de 17 - x", (yj - 17 + xj).sum(), 0, 1e-12),
        controle("somme |résidus| de 5 + x", sa(5, 1), 4, 1e-12),
        controle("somme |résidus| de 8 + 0,5 x", sa(8, 0.5), 4, 1e-12),
        controle("somme |résidus| de 6,5 + 0,75 x", sa(6.5, 0.75), 4, 1e-12),
        controle("minimum de la somme |résidus| (droites par 2 points)",
                 min(sa(yj[i] - (yj[j] - yj[i]) / (xj[j] - xj[i]) * xj[i], (yj[j] - yj[i]) / (xj[j] - xj[i]))
                     for i, j in itertools.combinations(range(5), 2)), 4, 1e-12),
        controle("SCR de 4 + 1,2 x", sc(4, 1.2), 11.0, 1e-9),
        controle("somme des d_i au carré", ((2.2 - 0.4 * xj)**2).sum(), 6.6, 1e-9),
        controle("0,924 x racine(7,5/10)", 0.9238 * np.sqrt(0.75), 0.8, 0.0005)]
j = wd.data("jtrain2"); m = j.groupby("train")["re78"].mean()
res += [controle("JTRAIN2 revenu moyen des formés", m[1], 6.349, 0.0005),
        controle("JTRAIN2 revenu moyen des non formés", m[0], 4.555, 0.0005),
        controle("JTRAIN2 écart", m[1] - m[0], 1.79, 0.005)]
w = wd.data("wagepan"); res.append(controle("WAGEPAN personnes x années", w["nr"].nunique() * w["year"].nunique(), 4360, 0))
print(sum(res), "contrôles réussis sur", len(res))
