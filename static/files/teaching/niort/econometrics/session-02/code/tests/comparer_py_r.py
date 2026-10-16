# Compare les nombres affichés par Python et par R, bloc par bloc.
# Un nombre Python est retrouvé s'il figure dans la sortie R à l'arrondi de son
# dernier chiffre près. Les blocs de simulation diffèrent par construction.
import re, sys, pathlib
d_py, d_r = map(pathlib.Path, sys.argv[1:3])
def nombres(t):
    t = t.split("\n--- stderr ---\n")[0]
    return re.findall(r"-?\d+\.\d+|-?\d+", t)
for f in sorted(d_py.glob("*.py.out")):
    k = f.name[:-7]
    r = (d_r / f"{k}.r.out")
    if not r.exists(): continue
    vr = [float(v) for v in nombres(r.read_text())]
    absents = []
    for v in nombres(f.read_text()):
        dec = len(v.split(".")[1]) if "." in v else 0
        x = float(v)
        if not any(abs(x - w) <= 0.5 * 10**-dec + 1e-9 or abs(round(w, dec) - x) < 1e-9 for w in vr):
            absents.append(v)
    print(f"{k:16s} {'tous retrouvés' if not absents else 'non retrouvés : ' + ' '.join(absents[:12])}")
