# Génère, à partir des trois scripts (source unique du code) :
#   annexes_code.tex         slides de code, avec onglets et bouton Retour
#   seance01_python.ipynb    notebook Python pour Google Colab
#   seance01_R.ipynb         notebook R pour Google Colab
import json, re, pathlib

ICI = pathlib.Path(__file__).parent
SCRIPTS = {"py": ("Python", ICI / "seance01.py"),
           "r": ("R", ICI / "seance01.R"),
           "stata": ("Stata", ICI / "seance01.do")}

# identifiant du bloc, titre, étiquette de la slide appelante (ordre des slides)
ITEMS = [
    ("etude", "heures d'étude et note", "f-correlation"),
    ("outils", "charger des données de Wooldridge", "f-outils"),
    ("ext-ct", "coupe transversale", "f-ct"),
    ("ext-st", "série temporelle", "f-st"),
    ("ext-cte", "coupes transversales empilées", "f-cte"),
    ("ext-panel", "données de panel", "f-panel"),
    ("jtrain", "données expérimentales", "f-exp"),
    ("travail", "travailler 15 heures ou plus", "f-travail"),
    ("variance", "construire la variance", "f-variance"),
    ("quadrants", "signe de la covariance", "f-covariance"),
    ("deplacer", "déplacer des points", "f-deplacer"),
    ("jouet-stats", "exemple jouet, statistiques descriptives", "f-jouet-stats"),
    ("nuage", "nuage de points du fil rouge", "f-question"),
    ("mco-illus", "quelle droite choisir", "f-quelle-droite"),
    ("carres", "trois critères", "f-carres"),
    ("jouet-mco", "exemple jouet, estimateurs des MCO", "f-jouet-mco"),
    ("jouet-proprietes", "exemple jouet, propriétés algébriques", "f-proprietes"),
    ("decomp", "décomposition d'une observation", "f-decomp"),
    ("jouet-r2", "exemple jouet, SCT, SCE, SCR et R2", "f-r2"),
    ("jtrain-r2", "un R2 faible dans une expérience", "f-r2-limites"),
    ("reg-table", "première estimation", "f-table"),
    ("droite", "droite des MCO sur le nuage", "f-droite"),
    ("residus", "valeurs prédites et résidus", "f-residus"),
]

MARQUE = re.compile(r"^[#*] -{10} \[([a-z0-9-]+)\]")

def decouper(chemin):
    """Découpe un script en blocs, d'après les lignes de repère [identifiant]."""
    blocs, cle = {}, None
    for ligne in chemin.read_text(encoding="utf-8").splitlines():
        m = MARQUE.match(ligne)
        if m:
            cle = m.group(1)
            blocs[cle] = []
        elif cle:
            blocs[cle].append(ligne)
    return {k: "\n".join(v).strip("\n") for k, v in blocs.items()}

code = {lang: decouper(p) for lang, (_, p) in SCRIPTS.items()}
for lang in code:
    manquants = [i for i, _, _ in ITEMS if i not in code[lang]]
    en_trop = [i for i in code[lang] if i not in [j for j, _, _ in ITEMS]]
    if manquants or en_trop:
        raise SystemExit(f"{lang} : manquants {manquants}, en trop {en_trop}")

def taille(texte):
    lignes = texte.split("\n")
    n, w = len(lignes), max(len(l) for l in lignes)
    if n <= 16 and w <= 82:
        return r"\footnotesize"
    if n <= 19 and w <= 92:
        return r"\scriptsize"
    if n > 26 or w > 120:
        raise SystemExit(f"bloc trop long ({n} lignes, {w} caractères)")
    return r"\tiny"

# ---------- annexes_code.tex ----------
sortie = []
for ident, titre, appelant in ITEMS:
    for lang, (nom, _) in SCRIPTS.items():
        texte = code[lang][ident]
        sortie.append(
            f"\\begin{{frame}}[fragile,t,label=code-{ident}-{lang}]"
            f"{{Code {nom}, {titre}}}\n"
            f"\\navcode{{{ident}}}{{{lang}}}{{{appelant}}}\n"
            f"\\lstset{{basicstyle={taille(texte)}\\ttfamily}}\n"
            f"\\begin{{lstlisting}}[style={lang}]\n{texte}\n"
            "\\end{lstlisting}\n\\end{frame}\n")
(ICI.parent / "annexes_code.tex").write_text("\n".join(sortie), encoding="utf-8")

# ---------- notebooks ----------
compteur = iter(range(1000))
def cellule(type_, texte):
    c = {"cell_type": type_, "id": f"c{next(compteur):03d}", "metadata": {},
         "source": texte.splitlines(keepends=True)}
    if type_ == "code":
        c.update({"execution_count": None, "outputs": []})
    return c

NOYAUX = {"py": {"name": "python3", "display_name": "Python 3", "language": "python"},
          "r": {"name": "ir", "display_name": "R", "language": "R"}}
for lang in ("py", "r"):
    nom = SCRIPTS[lang][0]
    cells = [cellule("markdown",
        f"# Économétrie L3, séance 1. Code {nom}\n\n"
        "Chaque cellule est autonome. Elle installe ce qui manque, charge les données "
        "et reproduit un résultat du support. On peut l'exécuter seule, dans n'importe quel ordre.")]
    for ident, titre, _ in ITEMS:
        cells.append(cellule("markdown", f"## {titre[0].upper() + titre[1:]}"))
        cells.append(cellule("code", code[lang][ident]))
    nb = {"cells": cells, "metadata": {"kernelspec": NOYAUX[lang],
          "language_info": {"name": NOYAUX[lang]["language"]}},
          "nbformat": 4, "nbformat_minor": 5}
    suffixe = "python" if lang == "py" else "R"
    (ICI / f"seance01_{suffixe}.ipynb").write_text(
        json.dumps(nb, ensure_ascii=False, indent=1), encoding="utf-8")

print(len(sortie), "slides de code, 2 notebooks")


# =================================================================
# Polycopié et série d'exercices (documents de classe article)
# =================================================================
SCRIPTS_TD = {"py": ("Python", ICI / "td01.py"), "r": ("R", ICI / "td01.R"),
              "stata": ("Stata", ICI / "td01.do")}
ITEMS_TD = [
    ("ex1", "exercice 1, calculs à la main"),
    ("ex2", "exercice 2, les deux droites de régression"),
    ("ex3", "exercice 3, table de régression"),
    ("ex4", "exercice 4, résultats potentiels"),
    ("ex4-tirages", "exercice 4, les 70 tirages possibles"),
    ("ex5-depart", "exercice 5, charger les données"),
    ("ex5-mco", "exercice 5, statistiques et MCO"),
    ("ex5-figure", "exercice 5, nuage de points et droite"),
]
code_td = {lang: decouper(p) for lang, (_, p) in SCRIPTS_TD.items()}
for lang in code_td:
    if sorted(code_td[lang]) != sorted(i for i, _ in ITEMS_TD):
        raise SystemExit(f"td {lang} : blocs {sorted(code_td[lang])}")

def taille_article(texte):
    lignes = texte.split("\n")
    w = max(len(l) for l in lignes)
    if len(lignes) > 40:
        raise SystemExit(f"bloc trop long ({len(lignes)} lignes)")
    if w <= 95:
        return r"\footnotesize"
    if w <= 107:
        return r"\scriptsize"
    return r"\tiny"

def listing(lang, nom, texte, ident):
    # Étiquette « [Langage] identifiant » : repère visible, sert aussi aux
    # tests d'extraction du code depuis le PDF.
    return ("\\begin{minipage}{\\linewidth}\n"
            f"\\etiquettecode{{{nom}}}{{{ident}}}\n"
            f"\\lstset{{basicstyle={taille_article(texte)}\\ttfamily}}\n"
            f"\\begin{{lstlisting}}[style={lang}]\n{texte}\n\\end{{lstlisting}}\n"
            "\\end{minipage}\\par\\medskip\n")

def annexe_article(items, code, scripts, prefixe):
    sortie = []
    for ident, titre in items:
        sortie.append(f"\\subsection{{{titre[0].upper() + titre[1:]}}}\\label{{{prefixe}-{ident}}}\n")
        for lang, (nom, _) in scripts.items():
            sortie.append(listing(lang, nom, code[lang][ident], ident))
    return "\n".join(sortie)

(ICI.parent / "polycopie" / "annexe_code.tex").write_text(
    annexe_article([(i, t) for i, t, _ in ITEMS], code, SCRIPTS, "code"), encoding="utf-8")
(ICI.parent / "td" / "corrige_code.tex").write_text(
    annexe_article(ITEMS_TD, code_td, SCRIPTS_TD, "code"), encoding="utf-8")
(ICI.parent / "td" / "depart_code.tex").write_text(
    "".join(listing(lang, nom, code_td[lang]["ex5-depart"], "ex5-depart")
            for lang, (nom, _) in SCRIPTS_TD.items()), encoding="utf-8")

for lang in ("py", "r"):
    nom = SCRIPTS_TD[lang][0]
    cells = [cellule("markdown",
        f"# Économétrie L3, séance 1. Série d'exercices, code {nom} du corrigé\n\n"
        "Chaque cellule est autonome. Elle installe ce qui manque, charge les données "
        "et reproduit un résultat du corrigé.")]
    for ident, titre in ITEMS_TD:
        cells.append(cellule("markdown", f"## {titre[0].upper() + titre[1:]}"))
        cells.append(cellule("code", code_td[lang][ident]))
    nb = {"cells": cells, "metadata": {"kernelspec": NOYAUX[lang],
          "language_info": {"name": NOYAUX[lang]["language"]}},
          "nbformat": 4, "nbformat_minor": 5}
    suffixe = "python" if lang == "py" else "R"
    (ICI / f"td01_{suffixe}.ipynb").write_text(json.dumps(nb, ensure_ascii=False, indent=1), encoding="utf-8")
print("annexes du polycopié et du corrigé, 2 notebooks de TD")
