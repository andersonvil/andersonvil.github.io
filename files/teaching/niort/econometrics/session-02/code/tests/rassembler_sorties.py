# Rassemble les sorties des blocs exécutés depuis la source dans un fichier texte.
# Usage : python3 rassembler_sorties.py source dossier_sorties extension fichier
import re, sys, pathlib
src, dossier, ext, cible = sys.argv[1:5]
MARQUE = re.compile(r"^[#*] -{10} \[([a-z0-9-]+)\] (.*?) -+$")
morceaux = []
for l in pathlib.Path(src).read_text(encoding="utf-8").splitlines():
    m = MARQUE.match(l)
    if m:
        sortie = (pathlib.Path(dossier) / f"{m[1]}.{ext}.out").read_text(encoding="utf-8")
        sortie = sortie.split("\n--- stderr ---\n")[0].rstrip()
        morceaux.append(f"========== [{m[1]}] {m[2]}\n{sortie}\n")
pathlib.Path(cible).write_text("\n".join(morceaux), encoding="utf-8")
print(cible, len(morceaux), "blocs")
