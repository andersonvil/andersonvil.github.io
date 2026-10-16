# Associe chaque « Overfull \vbox » du journal LaTeX à la page qui le contient
# et imprime le titre de cette page.
# Usage : python3 debordements.py partie3.log partie3.pdf
import re, subprocess, sys

log = open(sys.argv[1], encoding="latin-1").read()
pdf = sys.argv[2]
evenements = []
for m in re.finditer(r"Overfull \\vbox \(([\d.]+)pt too high\)|\[(\d+)(?=[\s\]<{])", log):
    if m.group(1):
        evenements.append(("debord", float(m.group(1))))
    else:
        evenements.append(("page", int(m.group(2))))
en_attente, resultat = [], []
for type_, v in evenements:
    if type_ == "debord":
        en_attente.append(v)
    else:
        for d in en_attente:
            resultat.append((v, d))
        en_attente = []
for page, d in resultat:
    titre = subprocess.run(["pdftotext", "-f", str(page), "-l", str(page), pdf, "-"],
                           capture_output=True, text=True).stdout.strip().split("\n")[0]
    print(f"p.{page:3d}  {d:6.1f} pt  {titre[:70]}")
print(len(resultat), "débordements")
