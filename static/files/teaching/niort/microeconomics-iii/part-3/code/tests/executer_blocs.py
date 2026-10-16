# Exécute chaque bloc d'un script séparément, dans un processus neuf et un
# dossier vide, comme le ferait un étudiant qui copie un seul bloc.
# Usage : python3 executer_blocs.py script.{py,R} dossier_sortie
import json, re, shutil, subprocess, sys, tempfile, pathlib, os

MARQUE = re.compile(r"^# -{10} \[([a-z0-9-]+)\]")

def blocs(chemin):
    out, cle = {}, None
    for l in pathlib.Path(chemin).read_text(encoding="utf-8").splitlines():
        m = MARQUE.match(l)
        if m:
            cle = m.group(1); out[cle] = []
        elif cle:
            out[cle].append(l)
    return {k: "\n".join(v).strip() + "\n" for k, v in out.items()}

if __name__ == "__main__":
    src, dossier = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
    lang = "py" if src.suffix == ".py" else "r"
    dossier.mkdir(parents=True, exist_ok=True)
    bilan = {}
    for ident, code in blocs(src).items():
        travail = pathlib.Path(tempfile.mkdtemp())
        f = travail / ("bloc.py" if lang == "py" else "bloc.R")
        f.write_text(code, encoding="utf-8")
        cmd = [sys.executable, "-I", str(f)] if lang == "py" else ["Rscript", str(f)]
        env = dict(os.environ, MPLBACKEND="Agg")
        r = subprocess.run(cmd, cwd=travail, env=env, capture_output=True, text=True, timeout=600)
        (dossier / f"{ident}.{lang}.out").write_text(r.stdout + ("\n--- stderr ---\n" + r.stderr if r.returncode else ""))
        for fig in travail.glob("*.pdf"):
            shutil.copy(fig, dossier / (fig.name if lang == "py" else f"{ident}.{fig.name}"))
        shutil.rmtree(travail)
        bilan[ident] = r.returncode
        print(f"{ident:22s} {'OK' if r.returncode == 0 else 'ÉCHEC'}", flush=True)
    (dossier / "bilan.json").write_text(json.dumps(bilan, indent=1))
