# Compare le code lisible dans le PDF (texte extrait de chaque slide de code)
# au code source des scripts, aux blancs près.
# Usage : python3 extraire_code_pdf.py partie3.pdf partie3.py partie3.R
import re, subprocess, sys, pathlib
MARQUE = re.compile(r"^# -{10} \[([a-z0-9-]+)\] (.*?) -{10}$")
def blocs(chemin):
    out, cle = {}, None
    for l in pathlib.Path(chemin).read_text(encoding="utf-8").splitlines():
        m = MARQUE.match(l)
        if m: cle = m.group(1); out[cle] = []
        elif cle: out[cle].append(l)
    return {k: "\n".join(v).strip() for k, v in out.items()}
pdf = sys.argv[1]
src = {"Python": blocs(sys.argv[2]), "R": blocs(sys.argv[3])}
titres = {}
for l in pathlib.Path(sys.argv[2]).read_text(encoding="utf-8").splitlines():
    m = MARQUE.match(l)
    if m: titres[m.group(2)[0].lower() + m.group(2)[1:]] = m.group(1)
n = int(re.search(r"Pages:\s+(\d+)", subprocess.run(["pdfinfo", pdf], capture_output=True, text=True).stdout).group(1))
norm = lambda t: re.sub(r"\s+", "", t.replace("−", "-").replace("’", "'"))
ok = ko = 0
for p in range(1, n + 1):
    t = subprocess.run(["pdftotext", "-raw", "-f", str(p), "-l", str(p), pdf, "-"], capture_output=True, text=True).stdout
    t = re.sub(r"^Anderson VIL, Dr en Économie Microéconomie III Année universitaire \S+( \d+/\d+)?\n", "", t)  # barre de signature
    m = re.match(r"Code (Python|R), (.*)\n", t)
    if not m: continue
    lang, titre = m.group(1), re.sub(r"\s*Python\s*R\s*$", "", m.group(2).strip()).replace("’", "'")
    ident = titres.get(titre)
    corps = t[m.end():]
    corps = re.sub(r"^\s*Python\s*R\s*\n", "", corps)
    corps = re.sub(r"^Retour\s*$", "", corps, count=1, flags=re.M)
    if ident is None:
        print("titre inconnu", p, titre); ko += 1; continue
    if norm(src[lang][ident]) in norm(corps) or norm(corps) == norm(src[lang][ident]):
        ok += 1
    else:
        ko += 1; print(f"p.{p} {lang} {ident} : texte extrait différent du source")
print(ok, "slides de code conformes,", ko, "écart(s)")
