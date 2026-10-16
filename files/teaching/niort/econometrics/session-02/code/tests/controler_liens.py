# Contrôle automatique des liens listés par verifier_liens.py.
# Usage : python3 verifier_liens.py seance02.pdf > liens_verifies.txt ; python3 controler_liens.py liens_verifies.txt
import re, sys, collections
L = re.compile(r"^p\.\s*(\d+)\s+\[(.*?)\s*\] -> (\S+)\s+p\.(\d+)")
liens = [(int(m[1]), m[2], m[3], int(m[4])) for m in map(L.match, open(sys.argv[1], encoding="utf-8")) if m]
page_de = {}                                 # destination nommée -> page
for _, _, nom, cible in liens:
    page_de[nom] = cible
erreurs, n = [], collections.Counter()
code_depuis = collections.defaultdict(set)   # bloc -> pages qui portent un bouton « Code »
ann_depuis = collections.defaultdict(set)
for p, txt, nom, cible in liens:
    m = re.match(r"code-(.+)-(py|r|stata)$", nom)
    if m and txt == "Code":
        n["Code"] += 1
        code_depuis[m[1]].add(p)
        if m[2] != "py": erreurs.append(f"p.{p} bouton Code vers {nom}, pas Python")
    elif nom.startswith("ann-"):
        n["renvoi annexe"] += 1; ann_depuis[nom[4:]].add(p)
codepage = {}                                # page de code -> (bloc, langage)
for nom, cible in page_de.items():
    m = re.match(r"code-(.+)-(py|r|stata)$", nom)
    if m: codepage[cible] = (m[1], m[2])
for p, txt, nom, cible in liens:
    if p in codepage:
        bloc, lang = codepage[p]
        if txt in ("Python", "R", "Stata"):
            n["onglet"] += 1
            attendu = f"code-{bloc}-{ {'Python': 'py', 'R': 'r', 'Stata': 'stata'}[txt] }"
            if nom != attendu: erreurs.append(f"p.{p} onglet {txt} vers {nom}, attendu {attendu}")
        elif txt == "Retour":
            n["Retour de code"] += 1
            if cible not in code_depuis[bloc]:
                erreurs.append(f"p.{p} Retour du bloc {bloc} vers p.{cible}, boutons Code en {sorted(code_depuis[bloc])}")
    elif nom.startswith("src-"):
        n["Retour d'annexe"] += 1
        if cible not in ann_depuis[nom[4:]]:
            erreurs.append(f"p.{p} Retour de l'annexe {nom[4:]} vers p.{cible}, renvois en {sorted(ann_depuis[nom[4:]])}")
for bloc in code_depuis:
    for lang in ("py", "r", "stata"):
        if f"code-{bloc}-{lang}" not in page_de: erreurs.append(f"bloc {bloc} sans page {lang}")
autres = [l for l in liens if not (l[1] in ("Code", "Python", "R", "Stata", "Retour") or l[2].startswith(("ann-", "src-")))]
for p, txt, nom, cible in autres:
    n["autre"] += 1; print(f"autre lien, p.{p} [{txt}] -> {nom} p.{cible}")
print(dict(n), "total", len(liens))
print("\n".join(erreurs) if erreurs else "aucune erreur")
