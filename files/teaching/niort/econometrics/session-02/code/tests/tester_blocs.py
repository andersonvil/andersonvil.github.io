# Teste chaque bloc de code séparément, dans un processus neuf.
# Python : environnement de type Colab (numpy, pandas, matplotlib, statsmodels),
#          wooldridge désinstallé avant chaque bloc pour tester l'installation.
# R      : bibliothèque vierge (sans wooldridge) avant chaque bloc.
# Usage  : python3 tester_blocs.py source {py|r} dossier_sortie [fichier_blocs]
import json, os, re, shutil, subprocess, sys, tempfile, pathlib

SCRATCH = pathlib.Path(os.environ["SCRATCH"])
PY = SCRATCH / "colabenv" / "bin" / "python"
REPO = "file://" + str(SCRATCH / os.environ.get("DEPOT_R", "crepo"))
MARQUE = re.compile(r"^[#*] -{10} \[([a-z0-9-]+)\]")

def blocs_depuis_source(chemin):
    blocs, cle = {}, None
    for l in pathlib.Path(chemin).read_text(encoding="utf-8").splitlines():
        m = MARQUE.match(l)
        if m:
            cle = m.group(1); blocs[cle] = []
        elif cle:
            blocs[cle].append(l)
    return {k: "\n".join(v).strip() + "\n" for k, v in blocs.items()}

def lancer(lang, ident, code, sortie):
    travail = pathlib.Path(tempfile.mkdtemp(dir=SCRATCH))
    if lang == "py":
        subprocess.run([str(PY), "-m", "pip", "uninstall", "-y", "-q", "wooldridge"],
                       capture_output=True)
        f = travail / "bloc.py"; f.write_text(code, encoding="utf-8")
        cmd = [str(PY), str(f)]
        env = dict(os.environ, MPLBACKEND="Agg")
    else:
        lib = travail / "lib"; lib.mkdir()
        prof = travail / "Rprofile"
        prof.write_text(f'options(repos = c(CRAN = "{REPO}"))\n')
        f = travail / "bloc.R"; f.write_text(code, encoding="utf-8")
        cmd = ["Rscript", str(f)]
        env = dict(os.environ, R_LIBS_USER=str(lib), R_LIBS_SITE=str(lib),
                   R_PROFILE_USER=str(prof))
        # la bibliothèque système ne doit pas contenir wooldridge
    r = subprocess.run(cmd, cwd=travail, env=env, capture_output=True, text=True, timeout=600)
    (sortie / f"{ident}.{lang}.out").write_text(r.stdout + "\n--- stderr ---\n" + r.stderr)
    for fig in travail.glob("*.pdf"):
        shutil.copy(fig, sortie / f"{ident}.{lang}.{fig.name}")
    shutil.rmtree(travail)
    return r.returncode

if __name__ == "__main__":
    src, lang, dossier = sys.argv[1], sys.argv[2], pathlib.Path(sys.argv[3])
    dossier.mkdir(parents=True, exist_ok=True)
    if len(sys.argv) > 4:                       # blocs extraits du PDF (json)
        blocs = json.loads(pathlib.Path(sys.argv[4]).read_text(encoding="utf-8"))
    else:
        blocs = blocs_depuis_source(src)
    bilan = {}
    for ident, code in blocs.items():
        bilan[ident] = lancer(lang, ident, code, dossier)
        print(f"{ident:18s} {'OK' if bilan[ident] == 0 else 'ÉCHEC'}", flush=True)
    (dossier / "bilan.json").write_text(json.dumps(bilan, indent=1))
