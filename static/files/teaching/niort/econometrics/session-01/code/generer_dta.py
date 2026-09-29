# Produit les fichiers .dta du cours à partir du paquet Python wooldridge
# (données de la 7e édition de Wooldridge, identiques à celles du paquet R).
# Stata charge ensuite ces fichiers, ce qui garantit les mêmes données dans
# les trois langages. Format 114 (lisible par Stata 10 et toutes les versions
# ultérieures). Usage : python3 generer_dta.py dossier_sortie
import contextlib, io, pathlib, re, sys
import wooldridge as wd

JEUX = ["econmath", "phillips", "cps78_85", "wagepan", "jtrain2", "attend", "wine"]
sortie = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else ".")
sortie.mkdir(parents=True, exist_ok=True)

def libelles(nom):
    """Libellés des variables, lus dans la description du paquet."""
    tampon = io.StringIO()
    with contextlib.redirect_stdout(tampon):
        wd.data(nom, description=True)
    lib = {}
    for ligne in tampon.getvalue().splitlines():
        m = re.match(r"^\|\s*(\S+)\s*\|\s*(.*?)\s*\|$", ligne)
        if m and m.group(1) != "variable":
            lib[m.group(1)] = m.group(2).encode("ascii", "ignore").decode()[:80]
    return lib

for nom in JEUX:
    d = wd.data(nom)
    lib = {k: v for k, v in libelles(nom).items() if k in d.columns and v}
    d.to_stata(sortie / f"{nom}.dta", write_index=False, version=114,
               data_label=f"Wooldridge 7e ed., {nom} (paquet wooldridge)"[:80],
               variable_labels=lib)
    print(f"{nom:10s} {d.shape[0]:5d} obs  {d.shape[1]:3d} variables  {len(lib):3d} libelles")
