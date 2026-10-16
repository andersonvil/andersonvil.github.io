# Vérifie un à un les liens internes du PDF : pour chaque lien, page de départ,
# destination nommée, page d'arrivée et titre de la page d'arrivée.
# Usage : python3 verifier_liens.py seance02.pdf
import sys, warnings, subprocess
warnings.filterwarnings("ignore")
import pypdf

pdf = sys.argv[1]
r = pypdf.PdfReader(pdf)
idx = {p.indirect_reference.idnum: i + 1 for i, p in enumerate(r.pages)}
dest = {k: idx[v.page.idnum] for k, v in r.named_destinations.items()}

def titre(n):
    t = subprocess.run(["pdftotext", "-f", str(n), "-l", str(n), pdf, "-"],
                       capture_output=True, text=True).stdout.strip().split("\n")
    return t[0] if t else ""

def texte_lien(page, rect):
    x0, y0, x1, y1 = [float(v) for v in rect]
    h = float(r.pages[page - 1].mediabox.height)
    out = subprocess.run(["pdftotext", "-f", str(page), "-l", str(page), "-x", str(int(x0)),
                          "-y", str(int(h - y1)), "-W", str(int(x1 - x0) + 1), "-H", str(int(y1 - y0) + 1),
                          pdf, "-"], capture_output=True, text=True).stdout
    return " ".join(out.split())

liens = []
for n, p in enumerate(r.pages, 1):
    for a in p.get("/Annots", []) or []:
        a = a.get_object()
        if a.get("/Subtype") != "/Link":
            continue
        act = a.get("/A")
        if act is None or act.get("/S") != "/GoTo":
            continue
        d = act["/D"]
        nom = d if isinstance(d, str) else None
        if nom is None or nom.startswith("Navigation") or nom == "Doc-Start":
            continue                      # barre de navigation de beamer (masquée)
        liens.append((n, nom, dest.get(nom), texte_lien(n, a["/Rect"])))

for n, nom, cible, txt in liens:
    print(f"p.{n:3d}  [{txt[:40]:40s}] -> {nom:22s} p.{cible}  « {titre(cible)[:60]} »")
print(len(liens), "liens,", sum(c is None for _, _, c, _ in liens), "sans destination")
