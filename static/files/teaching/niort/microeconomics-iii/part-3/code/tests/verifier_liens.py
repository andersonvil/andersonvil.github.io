# Vérifie un à un les liens du PDF.
# Liens internes : page de départ, texte du bouton, destination, page et titre d'arrivée.
# Liens vers les simulateurs : le fichier visé doit exister à côté du PDF.
# Contrôles automatiques : toute destination existe, chaque bouton « Code » mène à une
# slide de code, chaque « Retour » d'une slide de code ou d'une annexe ramène à la slide
# qui y envoie, chaque « Exemple alternatif » mène à une slide « Exemple alternatif ».
# Usage : python3 verifier_liens.py partie3.pdf
import sys, subprocess, pathlib, warnings
warnings.filterwarnings("ignore")
import pypdf

pdf = pathlib.Path(sys.argv[1])
r = pypdf.PdfReader(str(pdf))
idx = {p.indirect_reference.idnum: i + 1 for i, p in enumerate(r.pages)}
dest = {k: idx[v.page.idnum] for k, v in r.named_destinations.items()}

def titre(n):
    t = subprocess.run(["pdftotext", "-f", str(n), "-l", str(n), str(pdf), "-"],
                       capture_output=True, text=True).stdout.strip().split("\n")
    return t[0] if t else ""

def texte_lien(page, rect):
    x0, y0, x1, y1 = [float(v) for v in rect]
    h = float(r.pages[page - 1].mediabox.height)
    out = subprocess.run(["pdftotext", "-f", str(page), "-l", str(page), "-x", str(int(x0)),
                          "-y", str(int(h - y1)), "-W", str(int(x1 - x0) + 1), "-H", str(int(y1 - y0) + 1),
                          str(pdf), "-"], capture_output=True, text=True).stdout
    return " ".join(out.split())

internes, externes, problemes = [], [], []
for n, p in enumerate(r.pages, 1):
    for a in p.get("/Annots", []) or []:
        a = a.get_object()
        if a.get("/Subtype") != "/Link":
            continue
        act = a.get("/A")
        if act is None:
            continue
        if act.get("/S") == "/URI":
            externes.append((n, str(act["/URI"])))
            continue
        if act.get("/S") != "/GoTo":
            continue
        d = act["/D"]
        nom = d if isinstance(d, str) else None
        if nom is None or nom.startswith("Navigation") or nom == "Doc-Start":
            continue
        internes.append((n, nom, dest.get(nom), texte_lien(n, a["/Rect"])))

retours_attendus = {}            # destination d'un bouton Code ou alternatif -> page appelante
for n, nom, cible, txt in internes:
    if cible is None:
        problemes.append(f"p.{n} lien vers {nom} sans destination")
        continue
    t = titre(cible)
    if "Code" in txt and nom.startswith("code-") and not t.startswith("Code"):
        problemes.append(f"p.{n} bouton Code vers « {t} »")
    if "alternatif" in txt and not t.startswith("Exemple alternatif"):
        problemes.append(f"p.{n} bouton Exemple alternatif vers « {t} »")
    if ("Code" in txt or "alternatif" in txt) and nom.startswith(("code-", "alt-")) and nom.endswith(("-py", "")):
        retours_attendus.setdefault(cible, set()).add(n)
for n, nom, cible, txt in internes:
    if "Retour" in txt and cible is not None:
        appelants = retours_attendus.get(n)
        if appelants and cible not in appelants and not titre(n).startswith("Code R"):
            problemes.append(f"p.{n} Retour vers p.{cible}, appelée depuis {sorted(appelants)}")

for n, nom, cible, txt in internes:
    print(f"p.{n:3d}  [{txt[:28]:28s}] -> {nom:28s} p.{cible}  « {titre(cible)[:55] if cible else '?'} »")
for n, uri in externes:
    existe = (pdf.parent / uri).exists() if not uri.startswith("http") else None
    print(f"p.{n:3d}  lien externe {uri}  {'fichier présent' if existe else ('ABSENT' if existe is False else '')}")
    if existe is False:
        problemes.append(f"p.{n} fichier absent {uri}")
print(f"\n{len(internes)} liens internes, {len(externes)} liens vers les simulateurs, {len(problemes)} problème(s)")
for pb in problemes:
    print("  PROBLÈME", pb)
