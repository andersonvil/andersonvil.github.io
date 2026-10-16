# Génère, à partir des trois scripts (source unique du code) :
#   annexes_code.tex         slides de code, avec onglets et bouton Retour
#   seance02_python.ipynb    notebook Python pour Google Colab
#   seance02_R.ipynb         notebook R pour Google Colab
import json, re, pathlib

ICI = pathlib.Path(__file__).parent
SCRIPTS = {"py": ("Python", ICI / "seance02.py"),
           "r": ("R", ICI / "seance02.R"),
           "stata": ("Stata", ICI / "seance02.do")}

# identifiant du bloc, titre, étiquette de la slide appelante (ordre des slides)
ITEMS = [
    ("ech-quatre", "quatre échantillons, quatre droites", "f-quatre"),
    ("ech-distrib", "la distribution des pentes", "f-distrib"),
    ("va", "un étudiant tiré au hasard", "f-va"),
    ("lgn", "la loi des grands nombres", "f-esperance"),
    ("esp-prop", "calculer avec l'espérance", "f-prop-esp"),
    ("variance", "variance et écart-type", "f-variance"),
    ("cov-pop", "covariance et corrélation", "f-cov"),
    ("independance", "covariance nulle sans indépendance", "f-indep"),
    ("var-moyenne", "la variance d'une moyenne", "f-varsomme"),
    ("esp-cond", "espérance conditionnelle", "f-espcond"),
    ("iterees", "les espérances itérées", "f-prop-cond"),
    ("cond-cov", "covariance nulle et espérance conditionnelle", "f-cond-cov"),
    ("var-cond", "variance conditionnelle", "f-varcond"),
    ("rls1", "moyennes par classe et droite des MCO", "f-rls1"),
    ("rls3", "variation de x", "f-rls3"),
    ("equilibre", "comparer les groupes", "f-rls4-examiner"),
    ("rls5", "résidus et dispersion", "f-rls5"),
    ("jouet-bruit", "la vérité plus du bruit", "f-jouet-bruit"),
    ("jouet-b0", "la constante de l'exemple à la main", "f-preuve-b0"),
    ("mc-sansbiais", "simulation, absence de biais", "f-mc-sansbiais"),
    ("mc-biais", "simulation, un biais", "f-mc-biais"),
    ("mc-variance", "simulation, leviers de précision", "f-leviers"),
    ("eventail", "la dispersion de x", "f-eventail"),
    ("mc-hetero", "simulation, hétéroscédasticité", "f-mc-hetero"),
    ("mc-sigma", "simulation, estimer sigma au carré", "f-mc-sigma"),
    ("mc-se", "simulation, l'erreur-type", "f-erreur-type"),
    ("erreur-type", "les erreurs-types de la table", "f-table2"),
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
    if n <= 18 and w <= 92:
        return r"\scriptsize"
    if n > 25 or w > 125:
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
        f"# Économétrie L3, séance 2. Code {nom}\n\n"
        "Chaque cellule est autonome. Elle installe ce qui manque, charge les données "
        "et reproduit un résultat du support. On peut l'exécuter seule, dans n'importe quel ordre.")]
    for ident, titre, _ in ITEMS:
        cells.append(cellule("markdown", f"## {titre[0].upper() + titre[1:]}"))
        cells.append(cellule("code", code[lang][ident]))
    nb = {"cells": cells, "metadata": {"kernelspec": NOYAUX[lang],
          "language_info": {"name": NOYAUX[lang]["language"]}},
          "nbformat": 4, "nbformat_minor": 5}
    suffixe = "python" if lang == "py" else "R"
    (ICI / f"seance02_{suffixe}.ipynb").write_text(
        json.dumps(nb, ensure_ascii=False, indent=1), encoding="utf-8")

print(len(sortie), "slides de code, 2 notebooks")


# =================================================================
# Polycopié et série d'exercices (documents de classe article)
# =================================================================
if not (ICI / "td02.py").exists():
    raise SystemExit("td02 absent, annexes du support seulement")
SCRIPTS_TD = {"py": ("Python", ICI / "td02.py"), "r": ("R", ICI / "td02.R"),
              "stata": ("Stata", ICI / "td02.do")}
ITEMS_TD = [
    ("ex1", "exercice 1, une loi jointe à la main"),
    ("ex1-simul", "exercice 1, la pente des MCO sur des échantillons tirés"),
    ("ex2", "exercice 2, la pente des extrêmes"),
    ("ex3", "exercice 3, table de régression"),
    ("ex5-depart", "exercice 5, charger les données"),
    ("ex5-mco", "exercice 5, MCO, sigma chapeau et erreur-type"),
    ("ex5-residus", "exercice 5, résidus et dispersion"),
    ("ex5-simul", "exercice 5, la population des 1 230 hommes"),
    ("ex5-abil", "exercice 5, l'aptitude"),
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
        f"# Économétrie L3, séance 2. Série d'exercices, code {nom} du corrigé\n\n"
        "Chaque cellule est autonome. Elle installe ce qui manque, charge les données "
        "et reproduit un résultat du corrigé.")]
    for ident, titre in ITEMS_TD:
        cells.append(cellule("markdown", f"## {titre[0].upper() + titre[1:]}"))
        cells.append(cellule("code", code_td[lang][ident]))
    nb = {"cells": cells, "metadata": {"kernelspec": NOYAUX[lang],
          "language_info": {"name": NOYAUX[lang]["language"]}},
          "nbformat": 4, "nbformat_minor": 5}
    suffixe = "python" if lang == "py" else "R"
    (ICI / f"td02_{suffixe}.ipynb").write_text(json.dumps(nb, ensure_ascii=False, indent=1), encoding="utf-8")
print("annexes du polycopié et du corrigé, 2 notebooks de TD")
