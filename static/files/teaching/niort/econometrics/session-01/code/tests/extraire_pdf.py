# Extrait le code des slides d'annexe du PDF compilé, comme le ferait un
# copier-coller, puis le compare au code source.
# Deux modes : "layout" (pdftotext -layout, disposition conservée)
#               "brut"   (pdftotext simple, ordre de lecture du PDF)
# Usage : python3 extraire_pdf.py seance01.pdf dossier_sortie
import json, re, subprocess, sys, pathlib, textwrap

ICI = pathlib.Path(__file__).parent
sys.path.insert(0, str(ICI.parent))
import importlib.util
spec = importlib.util.spec_from_file_location("gen", ICI.parent / "generer_annexes.py")
gen = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gen)                      # régénère aussi annexes et notebooks

pdf, sortie = sys.argv[1], pathlib.Path(sys.argv[2])
sortie.mkdir(parents=True, exist_ok=True)
NOMS = {"py": "Python", "r": "R", "stata": "Stata"}

def texte(page, layout):
    cmd = ["pdftotext", "-f", str(page), "-l", str(page)] + (["-layout"] if layout else []) + [pdf, "-"]
    return subprocess.run(cmd, capture_output=True, text=True).stdout

n_pages = int(re.search(r"Pages:\s+(\d+)", subprocess.run(["pdfinfo", pdf],
              capture_output=True, text=True).stdout).group(1))
premiere = next(p for p in range(1, n_pages + 1)
                if texte(p, False).startswith(("Code Python, " + gen.ITEMS[0][1]).replace("'", "\u2019")))

extraits = {"layout": {l: {} for l in NOMS}, "brut": {l: {} for l in NOMS}}
page = premiere
for ident, titre, _ in gen.ITEMS:
    for lang in NOMS:
        attendu = f"Code {NOMS[lang]}, {titre}".replace("'", "’")
        # mode layout
        lignes = texte(page, True).replace("\f", "").split("\n")
        if not lignes[0].startswith(attendu):
            raise SystemExit(f"page {page} : titre inattendu {lignes[0]!r}, attendu {attendu!r}")
        corps = [l.rstrip() for l in lignes[1:] if l.strip() != "Retour"]
        extraits["layout"][lang][ident] = textwrap.dedent("\n".join(corps)).strip("\n") + "\n"
        # mode brut
        lignes = texte(page, False).replace("\f", "").split("\n")
        corps = [l.rstrip() for l in lignes[1:] if l.strip() not in ("Python", "R", "Stata", "Retour")]
        extraits["brut"][lang][ident] = "\n".join(corps).strip("\n") + "\n"
        page += 1

rapport = []
for lang in NOMS:
    for mode in ("layout", "brut"):
        (sortie / f"extrait_{mode}_{lang}.json").write_text(
            json.dumps(extraits[mode][lang], ensure_ascii=False, indent=1), encoding="utf-8")
    for ident, _, _ in gen.ITEMS:
        src = [l.rstrip() for l in gen.code[lang][ident].split("\n")]
        ext = extraits["layout"][lang][ident].rstrip("\n").split("\n")
        ok = src == ext
        norm = lambda ls: [re.sub(r"(?<=\S) {2,}", " ", l) for l in ls]
        presque = norm(src) == norm(ext)       # seuls diffèrent des blancs internes
        etat = "identique" if ok else ("identique aux blancs internes près" if presque else "DIFFÉRENT")
        rapport.append(f"{lang:6s} {ident:18s} {etat}")
        if not presque:
            for i, (a, b) in enumerate(zip(src, ext)):
                if a != b:
                    rapport.append(f"    ligne {i + 1}\n    source : {a!r}\n    extrait: {b!r}")
                    break
            if len(src) != len(ext):
                rapport.append(f"    {len(src)} lignes dans la source, {len(ext)} extraites")
(sortie / "comparaison_layout.txt").write_text("\n".join(rapport) + "\n", encoding="utf-8")
print("\n".join(rapport))
