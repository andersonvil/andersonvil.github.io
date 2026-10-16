# Repère les nombres décimaux des documents (support, polycopié, série, corrigé)
# qui ne figurent pas, à l'arrondi près, dans les sorties des blocs Python.
# Ceux qui restent sont vérifiés un à un par verifier_valeurs.py.
# Prérequis : sorties de tester_blocs.py dans out_src_py et out_td_py.
import re, pathlib, sys
R = pathlib.Path(__file__).resolve().parents[2]
docs = [R / "seance02.tex", R / "annexes_explications.tex"] + sorted((R / "polycopie").glob("sec*.tex")) \
     + [R / "polycopie" / "retenir.tex", R / "td" / "serie02.tex", R / "td" / "corrige02.tex"]
sorties = " ".join(p.read_text(errors="ignore") for d in ("out_src_py", "out_td_py")
                   for p in (pathlib.Path(__file__).parent / d).glob("*.out"))
vals = [float(v) for v in re.findall(r"-?\d+\.\d+(?:e-?\d+)?|-?\d+", sorties)]
def trouve(x, dec):
    tol = 0.5 * 10 ** -dec + 1e-12
    return any(abs(abs(v) - x) <= tol for v in vals)
manquants = {}
for d in docs:
    t = d.read_text()
    t = re.sub(r"(?<!\\)%.*", "", t)
    for _ in range(3):
        t = re.sub(r"(\d)\\,(\d{3})", r"\1\2", t)          # séparateur de milliers
    for m in re.finditer(r"(\d+)(?:\{,\}|,)(\d+)", t):
        ent = re.sub(r"\D", "", m.group(1)); dec = m.group(2)
        x = float(f"{ent}.{dec}")
        if not trouve(x, len(dec)):
            manquants.setdefault(f"{ent},{dec}", set()).add(d.name)
for k, v in sorted(manquants.items(), key=lambda kv: float(kv[0].replace(",", "."))):
    print(f"{k:>12s}  {', '.join(sorted(v))}")
print(len(manquants), "valeurs à vérifier à part")
