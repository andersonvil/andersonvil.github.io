# Compare, bloc par bloc, les nombres imprimés par Python et par R.
# Usage : python3 comparer.py sorties_py sorties_r
import re, sys, pathlib
NB = re.compile(r"-?\d+(?:[.,]\d+)?(?:e[+-]?\d+)?")
a, b = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
ok = 0
for f in sorted(a.glob("*.py.out")):
    ident = f.name[:-7]
    g = b / f"{ident}.r.out"
    na = [float(x.replace(",", ".")) for x in NB.findall(f.read_text())]
    nb = [float(x.replace(",", ".")) for x in NB.findall(g.read_text())]
    same = len(na) == len(nb) and all(abs(x - y) <= 1e-9 * max(1, abs(x)) for x, y in zip(na, nb))
    ok += same
    print(f"{ident:22s} {len(na):3d} nombres  {'identiques' if same else 'DIFFÉRENTS'}")
    if not same:
        print("   py :", na); print("   R  :", nb)
print(ok, "blocs identiques sur", len(list(a.glob('*.py.out'))))
