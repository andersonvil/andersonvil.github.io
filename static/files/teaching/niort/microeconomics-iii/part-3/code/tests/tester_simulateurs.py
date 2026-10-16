# Ouvre chaque simulateur dans Chromium, clique sur chaque préréglage, attend la fin
# de la simulation, relève les valeurs affichées et les erreurs JavaScript,
# puis enregistre une capture d'écran.
# Usage : python3 tester_simulateurs.py dossier_simulateurs dossier_sortie
import sys, pathlib, json
from playwright.sync_api import sync_playwright

src, out = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
out.mkdir(parents=True, exist_ok=True)
CHAMPS = {
    "diversification-correlation": ["rVal", "wVal", "empSd", "theoSd", "empMean"],
    "mutualisation": ["nVal", "piVal", "lVal", "empMean", "theoMean", "empSd", "theoSd", "empTail", "theoTail"],
    "choix-portefeuille": ["aVal", "xStar", "prixRisque", "empMean", "theoMean", "empSd", "theoSd", "worst"],
    "cascade-informationnelle": ["qVal", "nVal", "empWrong", "theoWrong", "empNone", "theoNone"],
}
rapport = {}
with sync_playwright() as p:
    nav = p.chromium.launch()
    for nom, champs in CHAMPS.items():
        page = nav.new_page(viewport={"width": 900, "height": 1400})
        erreurs = []
        page.on("pageerror", lambda e: erreurs.append(str(e)))
        page.on("console", lambda m: erreurs.append(m.text) if m.type == "error" and "fonts.googleapis" not in m.text else None)
        page.goto((src / f"{nom}.html").resolve().as_uri())
        resultats = []
        presets = page.locator(".presets button")
        for k in range(presets.count()):
            presets.nth(k).click()
            page.wait_for_function("document.getElementById('runBtn').disabled === false", timeout=120000)
            page.wait_for_timeout(300)
            valeurs = {c: page.locator(f"#{c}").inner_text() for c in champs}
            valeurs["verdict"] = page.locator("#verdict").inner_text()
            resultats.append({"préréglage": presets.nth(k).inner_text(), **valeurs})
        page.screenshot(path=str(out / f"{nom}.png"), full_page=True)
        rapport[nom] = {"erreurs": erreurs, "résultats": resultats}
        page.close()
    nav.close()
(out / "rapport_simulateurs.json").write_text(json.dumps(rapport, ensure_ascii=False, indent=1), encoding="utf-8")
for nom, r in rapport.items():
    print(f"== {nom} : {len(r['erreurs'])} erreur(s) JavaScript")
    for e in r["erreurs"]:
        print("   ", e)
    for res in r["résultats"]:
        print("   ", {k: v for k, v in res.items() if k != "verdict"})
        print("      verdict :", res["verdict"][:160])
