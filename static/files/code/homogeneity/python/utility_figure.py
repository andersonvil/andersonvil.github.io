"""
utility_figure.py -- Figure 1 of the note (standalone).

Left panel : Cobb-Douglas indifference curves, budget line at (p, Y), budget line at
             (2p, 2Y) -- identical -- and the optimal basket; dashed: income doubles alone.
Right panel: surface of U(x1, x2) with the budget constraint traced on it and the
             constrained maximum marked.

Outputs note/figures/fig1_utility.pdf (LaTeX) and .png (website).
Run from the project root:  python python/utility_figure.py
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

ROOT = Path(__file__).resolve().parents[1]
FIG = ROOT / "note" / "figures"
FIG.mkdir(parents=True, exist_ok=True)

# ---------------------------------------------------------------- parameters
ALPHA = 0.4                 # U(x1, x2) = ALPHA log x1 + (1 - ALPHA) log x2
P1, P2, Y = 40.0, 25.0, 2000.0
LAM = 2.0                   # scaling factor applied to prices and income

INK, INK2, GRID = "#0b0b0b", "#52514e", "#d9d8d3"
BLUE, ORANGE, AQUA = "#2a78d6", "#eb6834", "#1baf7a"

plt.rcParams.update({"font.family": "serif", "font.size": 9, "axes.edgecolor": INK2,
                     "axes.labelcolor": INK, "xtick.color": INK2, "ytick.color": INK2,
                     "mathtext.fontset": "cm"})


def U(x1, x2):
    return ALPHA * np.log(x1) + (1 - ALPHA) * np.log(x2)


def demand(p1, p2, y):
    """Cobb-Douglas Marshallian demands x_j = alpha_j * Y / p_j."""
    return ALPHA * y / p1, (1 - ALPHA) * y / p2


x1s, x2s = demand(P1, P2, Y)
x1d, x2d = demand(LAM * P1, LAM * P2, LAM * Y)
assert np.isclose(x1s, x1d) and np.isclose(x2s, x2d)   # homogeneity of degree zero

fig = plt.figure(figsize=(7.0, 2.9))
ax = fig.add_subplot(1, 2, 1)

# indifference curves: x2 = exp((u - ALPHA log x1) / (1 - ALPHA))
grid1 = np.linspace(1, 110, 400)
u_star = U(x1s, x2s)
for k, du in enumerate([-0.35, 0.0, 0.35]):
    x2 = np.exp((u_star + du - ALPHA * np.log(grid1)) / (1 - ALPHA))
    ax.plot(grid1, x2, color=INK2 if du else INK, lw=0.9 if du else 1.3,
            label=r"indifference curves" if k == 0 else None)

# budget lines
b = np.linspace(0, Y / P1, 50)
ax.plot(b, (Y - P1 * b) / P2, color=BLUE, lw=3.2, solid_capstyle="round",
        label=r"budget at $(p, Y)$")
ax.plot(b, (LAM * Y - LAM * P1 * b) / (LAM * P2), color=ORANGE, lw=1.2, ls=(0, (6, 3)),
        label=r"budget at $(2p, 2Y)$ (identical)")
b2 = np.linspace(0, LAM * Y / P1, 50)
ax.plot(b2, (LAM * Y - P1 * b2) / P2, color=INK2, lw=1.0, ls=":",
        label=r"budget at $(p, 2Y)$")
ax.plot(x1s, x2s, "o", ms=6, mfc=AQUA, mec="white", mew=1.0, zorder=5,
        label=rf"$x^*=({x1s:.0f},{x2s:.0f})$ under both budgets")
ax.set_xlim(0, 105); ax.set_ylim(0, 170)
ax.set_xlabel(r"$x_1$"); ax.set_ylabel(r"$x_2$")
ax.set_title(r"(a) Budget sets and optimal basket", fontsize=9, color=INK)
ax.legend(frameon=False, fontsize=7, loc="upper right")
for s in ("top", "right"):
    ax.spines[s].set_visible(False)

# right panel: utility surface with the budget constraint traced on it
ax3 = fig.add_subplot(1, 2, 2, projection="3d")
g1, g2 = np.meshgrid(np.linspace(2, 60, 60), np.linspace(2, 90, 60))
ax3.plot_surface(g1, g2, U(g1, g2), cmap="Blues", alpha=0.55, linewidth=0,
                 antialiased=True, rstride=2, cstride=2)
bb = np.linspace(2, Y / P1 - 0.6, 200)
bx2 = (Y - P1 * bb) / P2
ax3.plot(bb, bx2, U(bb, bx2), color=ORANGE, lw=2)
ax3.scatter([x1s], [x2s], [u_star], color=AQUA, s=30, depthshade=False,
            edgecolor="white", zorder=10)
ax3.set_xlabel(r"$x_1$", labelpad=-4); ax3.set_ylabel(r"$x_2$", labelpad=-4)
ax3.zaxis.set_rotate_label(False)
ax3.set_zlabel(r"$U$", labelpad=-6, rotation=0)
ax3.tick_params(labelsize=7, pad=-2)
ax3.view_init(elev=24, azim=-128)
ax3.set_title(r"(b) Utility surface and budget path",
              fontsize=9, color=INK)

fig.tight_layout(w_pad=1.0)
fig.savefig(FIG / "fig1_utility.pdf", bbox_inches="tight")
fig.savefig(FIG / "fig1_utility.png", dpi=200, bbox_inches="tight")
print(f"Optimum at (p,Y): ({x1s:.4f}, {x2s:.4f}); at (2p,2Y): ({x1d:.4f}, {x2d:.4f})")
