# Extrait le code d'un document de classe article (polycopié, série,
# corrigé), comme le ferait un copier-coller, et le compare au code source.
# Chaque bloc est précédé d'une étiquette « [Langage] identifiant ».
# Usage : python3 extraire_article.py document.pdf {support|td} dossier_sortie
import json, re, subprocess, sys, pathlib, textwrap, importlib.util, contextlib, io

ICI = pathlib.Path(__file__).parent
spec = importlib.util.spec_from_file_location("gen", ICI.parent / "generer_annexes.py")
gen = importlib.util.module_from_spec(spec)
with contextlib.redirect_stdout(io.StringIO()):
    spec.loader.exec_module(gen)

pdf, source, sortie = sys.argv[1], sys.argv[2], pathlib.Path(sys.argv[3])
sortie.mkdir(parents=True, exist_ok=True)
code = gen.code if source == "support" else gen.code_td
LANGS = {"Python": "py", "R": "r", "Stata": "stata"}
ETIQUETTE = re.compile(r"^\s*\[(Python|R|Stata)\] (\S+)\s*$")
TITRE = re.compile(r"^\s*[A-Z]\.\d+\s+\S")          # titres de sous-section d'annexe
PAGE = re.compile(r"^\s*\d+\s*$")                    # numéros de page

def extraire(layout):
    cmd = ["pdftotext"] + (["-layout"] if layout else []) + [pdf, "-"]
    lignes = subprocess.run(cmd, capture_output=True, text=True).stdout.replace("\f", "\n").split("\n")
    blocs, cle, courant = {}, None, []
    def fermer():
        if cle:
            texte = "\n".join(l.rstrip() for l in courant).strip("\n")
            blocs[cle] = (textwrap.dedent(texte) if layout else texte) + "\n"
    for l in lignes:
        m = ETIQUETTE.match(l)
        if m:
            fermer(); cle, courant = (LANGS[m.group(1)], m.group(2)), []
        elif cle and (TITRE.match(l) or re.match(r"^\s*(\d+\.\s+[A-ZÉ]|Exercice\b)", l)):
            fermer(); cle, courant = None, []
        elif cle and not PAGE.match(l):
            courant.append(l)
    fermer()
    return blocs

rapport = []
for mode, layout in (("layout", True), ("brut", False)):
    blocs = extraire(layout)
    for lang in ("py", "r", "stata"):
        d = {i: t for (l, i), t in blocs.items() if l == lang}
        (sortie / f"extrait_{mode}_{lang}.json").write_text(json.dumps(d, ensure_ascii=False, indent=1), encoding="utf-8")
    if mode == "layout":
        norm = lambda ls: [re.sub(r"(?<=\S) {2,}", " ", l) for l in ls]
        for (lang, ident), texte in sorted(blocs.items()):
            src = [l.rstrip() for l in code[lang][ident].split("\n")]
            ext = texte.rstrip("\n").split("\n")
            etat = ("identique" if src == ext else
                    "identique aux blancs internes près" if norm(src) == norm(ext) else "DIFFÉRENT")
            rapport.append(f"{lang:6s} {ident:18s} {etat}")
            if etat == "DIFFÉRENT":
                for i, (a, b) in enumerate(zip(src, ext)):
                    if a != b:
                        rapport.append(f"    ligne {i + 1}\n    source : {a!r}\n    extrait: {b!r}"); break
                if len(src) != len(ext):
                    rapport.append(f"    {len(src)} lignes dans la source, {len(ext)} extraites")
(sortie / "comparaison_layout.txt").write_text("\n".join(rapport) + "\n", encoding="utf-8")
print("\n".join(rapport)); print(len(rapport and [r for r in rapport if not r.startswith(" ")]), "blocs")
