# Génère annexes_code.tex à partir des deux scripts, source unique du code.
# Les blocs sont repérés par les lignes « # ---------- [identifiant] Titre ---------- ».
# L'ordre des annexes et la slide de retour sont lus dans les fichiers .tex :
# chaque \code{identifiant} appartient au frame dont il porte l'étiquette.
import re, pathlib

ICI = pathlib.Path(__file__).parent
RACINE = ICI.parent
SCRIPTS = {"py": ("Python", ICI / "partie3.py"), "r": ("R", ICI / "partie3.R")}
SOURCES_TEX = ["seance5.tex", "seance6.tex", "seance7.tex", "seance8.tex", "alternatifs.tex"]
MARQUE = re.compile(r"^# -{10} \[([a-z0-9-]+)\] (.*?) -{10}$")

def decouper(chemin):
    blocs, titres, cle = {}, {}, None
    for ligne in chemin.read_text(encoding="utf-8").splitlines():
        m = MARQUE.match(ligne)
        if m:
            cle = m.group(1); blocs[cle] = []; titres[cle] = m.group(2)
        elif cle:
            blocs[cle].append(ligne)
    return {k: "\n".join(v).strip("\n") for k, v in blocs.items()}, titres

def appels():
    """Liste ordonnée (identifiant, étiquette du frame appelant)."""
    out, label = [], None
    for f in SOURCES_TEX:
        for ligne in (RACINE / f).read_text(encoding="utf-8").splitlines():
            m = re.search(r"\\begin\{frame\}\[[^\]]*label=([A-Za-z0-9-]+)", ligne)
            if m:
                label = m.group(1)
            elif ligne.startswith("\\begin{frame}"):
                label = None
            for ident in re.findall(r"\\code\{([a-z0-9-]+)\}", ligne):
                if label is None:
                    raise SystemExit(f"\\code{{{ident}}} dans un frame sans étiquette ({f})")
                out.append((ident, label))
    return out

code, titres = {}, {}
for lang, (_, p) in SCRIPTS.items():
    code[lang], titres[lang] = decouper(p)
ITEMS = appels()
ids = [i for i, _ in ITEMS]
doublons = {i for i in ids if ids.count(i) > 1}
if doublons:
    raise SystemExit(f"identifiants appelés plusieurs fois : {doublons}")
for lang in code:
    manquants = [i for i in ids if i not in code[lang]]
    orphelins = [i for i in code[lang] if i not in ids]
    if manquants or orphelins:
        raise SystemExit(f"{lang} : manquants {manquants}, sans slide {orphelins}")

def taille(texte):
    # Classe 11pt : \footnotesize 9 pt, \scriptsize 8 pt, \tiny 6 pt. Avec basewidth = 0.5em,
    # un caractère occupe 4,5, 4 ou 3 pt, pour une largeur de texte de 398 pt.
    lignes = texte.split("\n")
    n, w = len(lignes), max(len(l) for l in lignes)
    if n <= 15 and w <= 82:
        return r"\footnotesize"
    if n <= 17 and w <= 93:
        return r"\scriptsize"
    if n > 26 or w > 130:
        raise SystemExit(f"bloc trop long ({n} lignes, {w} caractères)")
    if n > 24:
        return r"\fontsize{5.6}{6.3}\selectfont"
    return r"\tiny"

sortie = []
for ident, appelant in ITEMS:
    titre = titres["py"][ident]
    titre = titre[0].lower() + titre[1:]
    for lang, (nom, _) in SCRIPTS.items():
        texte = code[lang][ident]
        sortie.append(
            f"\\begin{{frame}}[fragile,t,label=code-{ident}-{lang}]"
            f"{{Code {nom}, {titre}}}\n"
            f"\\navcode{{{ident}}}{{{lang}}}{{{appelant}}}\n"
            f"\\lstset{{basicstyle={taille(texte)}\\ttfamily}}\n"
            f"\\begin{{lstlisting}}[style={lang}]\n{texte}\n"
            "\\end{lstlisting}\n\\end{frame}\n")
(RACINE / "annexes_code.tex").write_text("\n".join(sortie), encoding="utf-8")
print(len(ITEMS), "blocs,", len(sortie), "slides de code")
