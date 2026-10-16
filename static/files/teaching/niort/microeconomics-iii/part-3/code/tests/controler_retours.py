# Vérifie que chaque bouton « Retour » (slides de code et exemples alternatifs) ramène
# à la slide qui contient le bouton « Code » ou « Exemple alternatif » correspondant.
# Usage : python3 verifier_liens.py partie3.pdf > liens_verifies.txt ; python3 controler_retours.py liens_verifies.txt
import re, sys
L = []
for l in open(sys.argv[1], encoding="utf-8").read().splitlines():
    m = re.match(r"p\.\s*(\d+)\s+\[(.*?)\s*\] -> (\S+)\s+p\.(\d+)", l)
    if m:
        L.append((int(m.group(1)), m.group(2), m.group(3), int(m.group(4))))
appel = {}
for src, txt, nom, cible in L:
    if txt.startswith(("Code", "Exemple alternatif")):
        appel.setdefault(cible, set()).add(src)
nom_page = {cible: nom for _, _, nom, cible in L}
pages_py = {nom: cible for _, _, nom, cible in L if nom.startswith("code-")}
erreurs = n = 0
for src, txt, nom, cible in L:
    if txt != "Retour":
        continue
    n += 1
    nom_src = nom_page.get(src, "")
    attendu = appel.get(pages_py.get(nom_src[:-2] + "-py") if nom_src.endswith("-r") else src, set())
    if cible not in attendu:
        erreurs += 1
        print("Retour suspect", src, nom_src, "->", cible, "attendu", attendu)
print(n, "boutons Retour contrôlés,", erreurs, "erreur(s)")
