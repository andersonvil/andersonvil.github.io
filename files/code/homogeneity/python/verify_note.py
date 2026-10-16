"""
verify_note.py -- Second, independent verification before delivery.

Re-derives the key results without statsmodels or sympy and checks that the note
(PDF and Markdown) reports exactly what the scripts produced.

  1. OLS by hand (numpy) on data/sim_data.csv vs output/python_estimates.csv.
  2. Restricted-least-squares formula (eq. 5 of the note) vs OLS on the deflated model.
  3. Theory by finite differences: Stone-Geary elasticities and Euler sum; 2^0.1 - 1.
  4. Table cells vs output/results.json.
  5. Every number printed in the PDF body vs the Markdown version (same content).
  6. Layout: main text ends on page 4, appendix on page 5.

Run from the project root after everything else:  python python/verify_note.py
"""
import json
import re
import subprocess
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "output"
ok = []


def check(name, cond, detail=""):
    ok.append(bool(cond))
    print(f"[{'PASS' if cond else 'FAIL'}] {name} {detail}")


# 1. ----------------------------------------------------------------- OLS by hand
d = pd.read_csv(ROOT / "data" / "sim_data.csv", float_precision="round_trip")
py = pd.read_csv(OUT / "python_estimates.csv", index_col=0)["value"]
n, K = len(d), 5
XU = np.column_stack([np.ones(n), d.logp1, d.logp2, d.logp3, d.logY])
lp3 = d.logp3.to_numpy()
XR = np.column_stack([np.ones(n), d.logp1 - lp3, d.logp2 - lp3, d.logY - lp3])
c = np.array([0, 1, 1, 1, 1.0])
for h in ("H0", "H1"):
    y = d[f"logq_{h}"].to_numpy()
    b = np.linalg.solve(XU.T @ XU, XU.T @ y)
    e = y - XU @ b; s2 = e @ e / (n - K)
    V = s2 * np.linalg.inv(XU.T @ XU)
    th, se = c @ b, np.sqrt(c @ V @ c)
    bR = np.linalg.solve(XR.T @ XR, XR.T @ y)
    eR = y - XR @ bR
    F = (eR @ eR - e @ e) / (e @ e / (n - K))
    diff = max(abs(b[1] - py[f"{h}_U_b1"]), abs(th - py[f"{h}_theta"]),
               abs(se - py[f"{h}_se_theta"]), abs(F - py[f"{h}_F"]),
               abs(th / se - py[f"{h}_t"]), abs(-(bR[1] + bR[2] + bR[3]) - py[f"{h}_R_b3"]))
    check(f"hand OLS = scripts ({h})", diff < 1e-10, f"max diff {diff:.1e}")
    # 2. restricted LS formula
    A = np.linalg.inv(XU.T @ XU)
    bRf = b - A @ c * (c @ b) / (c @ A @ c)
    bR_full = np.array([bR[0], bR[1], bR[2], -(bR[1] + bR[2] + bR[3]), bR[3]])
    check(f"restricted-LS formula (20) = OLS on (18) ({h})", np.max(abs(bRf - bR_full)) < 1e-10,
          f"max diff {np.max(abs(bRf - bR_full)):.1e}")
    rho = (A @ c)[1] / np.sqrt(A[1, 1] * (c @ A @ c))
    vU, vR = s2 * A[1, 1], s2 * (A - np.outer(A @ c, A @ c) / (c @ A @ c))[1, 1]
    check(f"Var_R = (1 - rho^2) Var_U ({h})", np.isclose(vR, (1 - rho ** 2) * vU, rtol=1e-12))
th0, th1 = py["H0_theta"], py["H1_theta"]
check("theta_H1 - theta_H0 = 0.1 (same u)", abs(th1 - th0 - 0.1) < 1e-10, f"{th1 - th0:.12f}")

# 3. ----------------------------------------------------------------- theory, numerically
a1, g, p, Y = 0.3, np.array([2, 3, 1.0]), np.array([2, 1, 1.5]), 20.0


def x1(p, Y):
    return g[0] + a1 / p[0] * (Y - p @ g)


h_ = 1e-6
eps = [(x1(p * np.where(np.arange(3) == j, 1 + h_, 1), Y) - x1(p * np.where(np.arange(3) == j, 1 - h_, 1), Y))
       / (2 * h_ * x1(p, Y)) for j in range(3)]
eta = (x1(p, Y * (1 + h_)) - x1(p, Y * (1 - h_))) / (2 * h_ * x1(p, Y))
der = json.loads((OUT / "derivations.json").read_text())["stone_geary"]
check("Stone-Geary elasticities (finite differences)",
      np.allclose(eps + [eta], der["eps_at_point"] + [der["eta_at_point"]], atol=1e-6),
      f"sum = {sum(eps) + eta:.1e}")
check("x1(2p, 2Y) = x1(p, Y)", abs(x1(2 * p, 2 * Y) - x1(p, Y)) < 1e-12)
check("2^0.1 - 1 = 7.2%", round(100 * (2 ** 0.1 - 1), 1) == 7.2)

# 4. ----------------------------------------------------------------- tables vs results.json
res = json.loads((OUT / "results.json").read_text())
t1 = (OUT / "table1.md").read_text(); t2 = (OUT / "table2.md").read_text()
need1 = [f"{v:.3f}" for v in res["H0"]["bU"] + res["H0"]["bR"] + res["H0"]["seU"] + res["H0"]["seR"]]
need2 = [f"{res[h][k]:.4f}" for h in ("H0", "H1") for k in ("theta", "se_theta", "t", "F", "t2")]
check("Table 1 cells match results.json", all(s.lstrip("-") in t1 for s in need1))
check("Table 2 cells match results.json", all(s.lstrip("-") in t2 for s in need2))

# 5. ----------------------------------------------------------------- PDF vs Markdown
pdf_txt = subprocess.run(["pdftotext", "-layout", str(ROOT / "note" / "homogeneity.pdf"), "-"],
                         capture_output=True, text=True).stdout
md_txt = (ROOT / "note" / "homogeneity.md").read_text()
num = re.compile(r"(?<![\w.])\d+\.\d+")
norm = lambda s: s.replace("−", "-")
md_nums = set(num.findall(md_txt))
# (a) every number of the Markdown is printed in the PDF (PDF also holds figure tick labels)
check("every Markdown number appears in the PDF", md_nums <= set(num.findall(norm(pdf_txt))),
      f"missing: {sorted(md_nums - set(num.findall(norm(pdf_txt))))[:8]}")
# (b) the LaTeX source, macros expanded and tables inlined, holds the same numbers as the Markdown
mac = {**json.loads((OUT / "macros_theory.json").read_text()),
       **json.loads((OUT / "macros_sim.json").read_text())}
tex = (ROOT / "note" / "homogeneity.tex").read_text()
tex = tex[tex.index("\\begin{document}"):]
for t in ("table1", "table2"):
    tex = tex.replace(f"\\input{{../output/{t}.tex}}", (OUT / f"{t}.tex").read_text())
for k in sorted(mac, key=len, reverse=True):
    tex = re.sub(r"\\" + k + r"(?![A-Za-z])", lambda m, v=mac[k]: v, tex)
tex = tex.replace("$-$", "-")
tex = re.sub(r"[\d.]+\\textwidth", "", tex)          # layout widths are not content
tex_nums = set(num.findall(tex))
check("same decimal numbers in .tex and .md", tex_nums == md_nums,
      f"only in tex: {sorted(tex_nums - md_nums)[:8]}, only in md: {sorted(md_nums - tex_nums)[:8]}")
macros = {**json.loads((OUT / "macros_theory.json").read_text()),
          **json.loads((OUT / "macros_sim.json").read_text())}
key = ["illusionPct", "sgEta", "thetaHzero", "pHone", "ciLoHone", "ciHiHone", "sizeRate",
       "powSmall", "powLarge", "sdUNoInf", "sdRNoInf", "ratioStrong", "sdThetaNoInf", "sdThetaStrong"]
absent = [k for k in key if macros[k].lstrip("-") not in pdf_txt]
check("key macro values printed in the PDF", not absent, f"absent: {absent}")

# 6. ----------------------------------------------------------------- layout
pages = pdf_txt.split("\f")
p_ref = next(i for i, pg in enumerate(pages, 1) if "References" in pg)
p_app = next(i for i, pg in enumerate(pages, 1) if "Appendix: power" in pg)
check("references end the main text", p_ref < len([p for p in pages if p.strip()]), f"references on page {p_ref} of {len([p for p in pages if p.strip()])}")
check("appendix (Figure A.1) after the main text", p_app == p_ref + 1, f"appendix on page {p_app}")
check("no unresolved references in the PDF", "??" not in pdf_txt)

print(f"\n{sum(ok)}/{len(ok)} checks passed")
(OUT / "verification.txt").write_text(f"{sum(ok)}/{len(ok)} checks passed\n")
if not all(ok):
    raise SystemExit(1)
