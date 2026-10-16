"""
simulate_and_test.py -- Sections 3-5 of the note: impose and test zero-degree homogeneity
in the log-linear demand equation (1), on simulated data.

    log q1 = b0 + b1 log p1 + b2 log p2 + b3 log p3 + b4 log Y + u           (1)
    H0: theta = b1 + b2 + b3 + b4 = 0

Structure (same order as the note)
    0. Parameters
    1. Data-generating process and the main simulated sample (-> data/sim_data.csv)
    2. Imposing the restriction: restricted OLS on (3), recovery of b3 and its s.e.,
       invariance to the numeraire                                         (Table 1)
    3. Testing: t test on theta, reparametrised regression, F from SSRs, F = t^2,
       Wald and LR                                                         (Table 2)
    4. Monte Carlo: size of the F test                                     (Figure 2)
    5. Monte Carlo: power curves for two sample sizes                      (Figure 3)
    6. Monte Carlo: restricted vs unrestricted b1 under H0 and H1          (Figure 4)
    7. Write every number used by the note to output/

Run from the project root:  python python/simulate_and_test.py
"""
import json
import platform
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import scipy
import statsmodels
import statsmodels.api as sm
from scipy import stats

ROOT = Path(__file__).resolve().parents[1]
DATA, OUT, FIG = ROOT / "data", ROOT / "output", ROOT / "note" / "figures"
for d in (DATA, OUT, FIG):
    d.mkdir(parents=True, exist_ok=True)

# =============================================================================
# 0. Parameters
# =============================================================================
SEED = 20261015
R, T = 50, 10                      # regions x periods -> n = 500
J = 3                              # number of goods; K = J + 2 regressors incl. constant
K = J + 2
BETA0 = 1.0
BETA_H0 = np.array([-0.6, 0.2, 0.1, 0.3])     # b1, b2, b3 (prices), b4 (income): theta = 0
THETA_H1 = 0.1                     # under H1 only the income elasticity changes: b4 = 0.3 + theta
SIG_PI = 0.20                      # s.d. of common inflation pi_t (baseline design)
SIG_MU = 0.15                      # s.d. of region-good price levels mu_jr
SIG_EPS = 0.05                     # s.d. of idiosyncratic price shocks eps_jrt
SIG_OM = 0.30                      # s.d. of the region-period component of log income
MEAN_LOGY = np.log(2000.0)         # average log income
SIG_U = 0.20                       # s.d. of the demand error u
ALPHA = 0.05                       # test level

N_MC = 2000                        # replications, size experiment (Figure 2)
THETA_GRID = np.round(np.linspace(-0.15, 0.15, 31), 2)
N_POWER = 1000                     # replications per sample size, power experiment (Figure 3)
R_POWER = (50, 200)                # n = 500 and n = 2000
SIG_PI_FIG4 = {"no_inflation": 0.0, "strong_inflation": 1.0}   # Figure 4 designs
N_FIG4 = 2000

C_THETA = np.array([0.0, 1, 1, 1, 1])          # theta = c'beta in (1); beta = (b0, b1..b4)

INK, INK2 = "#0b0b0b", "#52514e"
BLUE, ORANGE, AQUA = "#2a78d6", "#eb6834", "#1baf7a"
plt.rcParams.update({"font.family": "serif", "font.size": 8.5, "axes.edgecolor": INK2,
                     "axes.labelcolor": INK, "xtick.color": INK2, "ytick.color": INK2,
                     "mathtext.fontset": "cm", "axes.spines.top": False,
                     "axes.spines.right": False})


def beta_true(theta):
    """True (b1, b2, b3, b4) for a given degree of money illusion theta."""
    b = BETA_H0.copy()
    b[3] += theta
    return b


# =============================================================================
# 1. Data-generating process
# =============================================================================
def simulate_regressors(rng, R, T, sig_pi=SIG_PI):
    """log p_jrt = pi_t + mu_jr + eps_jrt ;  log Y_rt = mean + pi_t + omega_rt."""
    pi = rng.normal(0.0, sig_pi, T)
    mu = rng.normal(0.0, SIG_MU, (J, R))
    eps = rng.normal(0.0, SIG_EPS, (J, R, T))
    logp = pi[None, None, :] + mu[:, :, None] + eps                # J x R x T
    logY = MEAN_LOGY + pi[None, :] + rng.normal(0.0, SIG_OM, (R, T))
    X = np.column_stack([logp[j].ravel() for j in range(J)] + [logY.ravel()])
    return X                                                       # n x (J+1), row = (r, t)


def design_matrices(X):
    """Unrestricted design (1) and restricted design (3) with numeraire p_J."""
    n = X.shape[0]
    one = np.ones(n)
    XU = np.column_stack([one, X])
    lpJ = X[:, J - 1]
    XR = np.column_stack([one] + [X[:, j] - lpJ for j in range(J - 1)] + [X[:, J] - lpJ])
    return XU, XR


rng = np.random.default_rng(SEED)
Xraw = simulate_regressors(rng, R, T)
u = rng.normal(0.0, SIG_U, R * T)
n = R * T
logq = {h: BETA0 + Xraw @ beta_true(th) + u for h, th in (("H0", 0.0), ("H1", THETA_H1))}

df = pd.DataFrame({"region": np.repeat(np.arange(1, R + 1), T),
                   "period": np.tile(np.arange(1, T + 1), R),
                   "logp1": Xraw[:, 0], "logp2": Xraw[:, 1], "logp3": Xraw[:, 2],
                   "logY": Xraw[:, 3], "u": u,
                   "logq_H0": logq["H0"], "logq_H1": logq["H1"]})
df.to_csv(DATA / "sim_data.csv", index=False, float_format="%.17g")
df = pd.read_csv(DATA / "sim_data.csv", float_precision="round_trip")  # exactly what R reads
assert np.allclose(df[["logp1", "logp2", "logp3", "logY"]].to_numpy(), Xraw, rtol=0, atol=0)

# regressors, named
df["r1"] = df.logp1 - df.logp3            # log(p1/p3)
df["r2"] = df.logp2 - df.logp3            # log(p2/p3)
df["yr"] = df.logY - df.logp3             # log(Y/p3)
UNRES = ["logp1", "logp2", "logp3", "logY"]
RES = ["r1", "r2", "yr"]
REPAR = ["r1", "r2", "yr", "logp3"]

results, flat = {}, {}         # results: structured; flat: name -> value (cross-check with R)
log_lines = []


def log(s=""):
    print(s)
    log_lines.append(s)


log(f"Python {platform.python_version()}, numpy {np.__version__}, scipy {scipy.__version__}, "
    f"statsmodels {statsmodels.__version__}, pandas {pd.__version__}, "
    f"matplotlib {matplotlib.__version__}")
log(f"n = {n}, K = {K}, seed = {SEED}")
corr = np.corrcoef(Xraw.T)
log("Correlation of (log p1, log p2, log p3, log Y):\n" + np.array2string(corr, precision=3))


# =============================================================================
# 2-3. Imposing and testing, on the main sample, under H0 and under H1
# =============================================================================
def ols(y, cols):
    return sm.OLS(df[y], sm.add_constant(df[cols])).fit()


for h in ("H0", "H1"):
    y = f"logq_{h}"
    mU, mR, mP = ols(y, UNRES), ols(y, RES), ols(y, REPAR)
    bU, VU = mU.params.to_numpy(), mU.cov_params().to_numpy()
    bR, VR = mR.params.to_numpy(), mR.cov_params().to_numpy()
    bP, VP = mP.params.to_numpy(), mP.cov_params().to_numpy()

    # (i) restricted model: recover b3 = -(b1 + b2 + b4) and Var = a' V a
    a = np.array([0.0, -1, -1, -1])
    b3R, se_b3R = a @ bR, np.sqrt(a @ VR @ a)
    bR_full = np.array([bR[0], bR[1], bR[2], b3R, bR[3]])           # (b0, b1, b2, b3, b4)
    seR_full = np.r_[np.sqrt(np.diag(VR))[:3], se_b3R, np.sqrt(VR[3, 3])]

    # (ii) t test on theta = c'beta in the unrestricted model
    theta, se_theta = C_THETA @ bU, np.sqrt(C_THETA @ VU @ C_THETA)
    t_stat = theta / se_theta
    tt = mU.t_test("logp1 + logp2 + logp3 + logY = 0")
    assert np.isclose(float(np.squeeze(tt.effect)), theta) and np.isclose(float(np.squeeze(tt.tvalue)), t_stat)

    # (iii) reparametrised regression: coefficient on log p3 is theta, exactly
    assert np.isclose(bP[4], theta, rtol=0, atol=1e-12)
    assert np.isclose(np.sqrt(VP[4, 4]), se_theta, rtol=0, atol=1e-12)
    assert np.allclose(bP[:4], bU[[0, 1, 2, 4]], rtol=0, atol=1e-12)  # same b0, b1, b2, b4
    b3P = bP[4] - bP[1] - bP[2] - bP[3]                             # b3 = theta - b1 - b2 - b4
    assert np.isclose(b3P, bU[3], rtol=0, atol=1e-12)

    # (iv) F test from the sums of squared residuals; F = t^2
    ssrU, ssrR = mU.ssr, mR.ssr
    F = (ssrR - ssrU) / (ssrU / (n - K))
    pF = stats.f.sf(F, 1, n - K)
    pt = 2 * stats.t.sf(abs(t_stat), n - K)
    ft = mU.f_test("logp1 + logp2 + logp3 + logY = 0")
    assert np.isclose(float(np.squeeze(ft.fvalue)), F)
    assert abs(F - t_stat ** 2) < 1e-9 and abs(pF - pt) < 1e-12
    tcrit = stats.t.ppf(1 - ALPHA / 2, n - K)
    ci = (theta - tcrit * se_theta, theta + tcrit * se_theta)

    # (v) asymptotic versions
    W = n * (ssrR - ssrU) / ssrU
    LR = n * np.log(ssrR / ssrU)
    LM = n * (ssrR - ssrU) / ssrR

    # (vi) invariance to the numeraire: restricted fits with p1, p2 and p3 as numeraire
    full = {}
    for k in range(J):
        lpk = df[f"logp{k+1}"]
        Z = pd.DataFrame({f"rel{j+1}": df[f"logp{j+1}"] - lpk for j in range(J) if j != k})
        Z["yr"] = df.logY - lpk
        m = sm.OLS(df[y], sm.add_constant(Z)).fit()
        b = np.empty(J + 2); b[0] = m.params["const"]; b[J + 1] = m.params["yr"]
        for j in range(J):
            if j != k:
                b[j + 1] = m.params[f"rel{j+1}"]
        b[k + 1] = -(sum(b[i + 1] for i in range(J) if i != k) + b[J + 1])
        full[k + 1] = (b, m.ssr)
    inv_coef = max(np.max(np.abs(full[k][0] - bR_full)) for k in full)
    inv_ssr = max(abs(full[k][1] - ssrR) for k in full)

    # (vii) the trade-off formula (conditional on X):
    #       Var(b1_R) = Var(b1_U)(1 - rho^2),  bias(b1_R) = delta * theta
    Vc = VU @ C_THETA
    rho = Vc[1] / np.sqrt(VU[1, 1] * (C_THETA @ Vc))
    delta = -Vc[1] / (C_THETA @ Vc)

    results[h] = dict(bU=bU.tolist(), seU=np.sqrt(np.diag(VU)).tolist(),
                      bR=bR_full.tolist(), seR=seR_full.tolist(),
                      bP=bP.tolist(), seP=np.sqrt(np.diag(VP)).tolist(), b3P=b3P,
                      se_b3P=float(np.sqrt(np.array([0, -1, -1, -1, 1]) @ VP
                                           @ np.array([0, -1, -1, -1, 1]))),
                      theta=theta, se_theta=se_theta, t=t_stat, p_t=pt, F=F, p_F=pF,
                      t2=t_stat ** 2, F_minus_t2=F - t_stat ** 2, ci=ci,
                      ssrU=ssrU, ssrR=ssrR, W=W, LR=LR, LM=LM,
                      p_W=stats.chi2.sf(W, 1), p_LR=stats.chi2.sf(LR, 1),
                      invariance_max_coef_diff=inv_coef, invariance_max_ssr_diff=inv_ssr,
                      rho_b1_theta=rho, delta_b1=delta)
    for nm, v in zip(["b0", "b1", "b2", "b3", "b4"], bU):
        flat[f"{h}_U_{nm}"] = v
    for nm, v in zip(["b0", "b1", "b2", "b3", "b4"], np.sqrt(np.diag(VU))):
        flat[f"{h}_U_se_{nm}"] = v
    for nm, v in zip(["b0", "b1", "b2", "b3", "b4"], bR_full):
        flat[f"{h}_R_{nm}"] = v
    for nm, v in zip(["b0", "b1", "b2", "b3", "b4"], seR_full):
        flat[f"{h}_R_se_{nm}"] = v
    flat.update({f"{h}_theta": theta, f"{h}_se_theta": se_theta, f"{h}_t": t_stat,
                 f"{h}_F": F, f"{h}_p": pF, f"{h}_ssrU": ssrU, f"{h}_ssrR": ssrR,
                 f"{h}_W": W, f"{h}_LR": LR})
    log(f"\n[{h}] theta = {theta:.6f} (se {se_theta:.6f}), t = {t_stat:.6f}, F = {F:.6f}, "
        f"t^2 = {t_stat**2:.6f}, |F - t^2| = {abs(F - t_stat**2):.2e}, p = {pF:.6f}")
    log(f"[{h}] 95% CI for theta: [{ci[0]:.6f}, {ci[1]:.6f}];  W = {W:.4f}, LR = {LR:.4f}, "
        f"LM = {LM:.4f}")
    log(f"[{h}] numeraire invariance: max |coef diff| = {inv_coef:.2e}, "
        f"max |SSR diff| = {inv_ssr:.2e}")
    log(f"[{h}] rho(b1_U, theta_hat) = {rho:.4f}, delta = {delta:.4f}")

pd.Series(flat).to_csv(OUT / "python_estimates.csv", header=["value"], index_label="name",
                       float_format="%.17g")


# =============================================================================
# Fast OLS helpers for the Monte Carlo experiments
# =============================================================================
def resid_maker(Xd):
    Q, _ = np.linalg.qr(Xd)
    return lambda Y: Y - Q @ (Q.T @ Y)          # works for Y of shape (n,) or (n, m)


def F_stats(XU, XR, Y):
    """F statistics for H0: theta = 0, for each column of Y."""
    eU, eR = resid_maker(XU)(Y), resid_maker(XR)(Y)
    ssrU, ssrR = (eU ** 2).sum(0), (eR ** 2).sum(0)
    return (ssrR - ssrU) / (ssrU / (XU.shape[0] - K))


# =============================================================================
# 4. Size of the F test under H0 (Figure 2)
# =============================================================================
rng = np.random.default_rng(SEED + 1)
F_mc = np.empty(N_MC)
for s in range(N_MC):
    X = simulate_regressors(rng, R, T)
    XU, XR = design_matrices(X)
    y = BETA0 + X @ beta_true(0.0) + rng.normal(0.0, SIG_U, n)
    F_mc[s] = F_stats(XU, XR, y)
Fcrit = stats.f.ppf(1 - ALPHA, 1, n - K)
size = np.mean(F_mc > Fcrit)
size_mcse = np.sqrt(ALPHA * (1 - ALPHA) / N_MC)
size_W = np.mean(n * F_mc / (n - K) > stats.chi2.ppf(1 - ALPHA, 1))   # W = n F/(n-K) here
ks = stats.kstest(F_mc, stats.f(1, n - K).cdf)
log(f"\n[size] {N_MC} reps: rejection rate at 5% = {size:.4f} (MC s.e. {size_mcse:.4f}); "
    f"Wald(chi2) rejection = {size_W:.4f}; KS p-value vs F(1,{n-K}) = {ks.pvalue:.3f}")

fig, ax = plt.subplots(figsize=(3.3, 2.35))
ax.hist(F_mc, bins=np.arange(0, 12.01, 0.25), density=True, color=BLUE, alpha=0.55,
        edgecolor="white", linewidth=0.5, label=f"simulated $F$ ({N_MC:,} reps)")
grid = np.linspace(0.02, 12, 600)
ax.plot(grid, stats.f.pdf(grid, 1, n - K), color=INK, lw=1.4, label=f"$F(1,{n-K})$ density")
ax.axvline(Fcrit, color=ORANGE, lw=1.2, ls="--")
ax.text(Fcrit + 0.3, 0.75, f"5% critical value {Fcrit:.2f}\nrejection rate {100*size:.1f}%",
        color=INK, fontsize=7.5)
ax.set_xlim(0, 12); ax.set_ylim(0, 1.2)
ax.set_xlabel("$F$ statistic"); ax.set_ylabel("density")
ax.legend(frameon=False, fontsize=7, loc="upper right", bbox_to_anchor=(1.0, 1.02))
fig.tight_layout()
fig.savefig(FIG / "fig2_size.pdf"); fig.savefig(FIG / "fig2_size.png", dpi=200)
plt.close(fig)

# =============================================================================
# 5. Power (Figure 3): common random numbers across the theta grid
# =============================================================================
rng = np.random.default_rng(SEED + 2)
power = {}
for Rp in R_POWER:
    npow = Rp * T
    rej = np.zeros(len(THETA_GRID))
    crit = stats.f.ppf(1 - ALPHA, 1, npow - K)
    for s in range(N_POWER):
        X = simulate_regressors(rng, Rp, T)
        XU, XR = design_matrices(X)
        uu = rng.normal(0.0, SIG_U, npow)
        Y = np.column_stack([BETA0 + X @ beta_true(th) + uu for th in THETA_GRID])
        rej += F_stats(XU, XR, Y) > crit
    power[npow] = rej / N_POWER
    log(f"[power] n = {npow}: power at theta = 0.10 -> "
        f"{power[npow][THETA_GRID == 0.10][0]:.3f}; at theta = 0 -> "
        f"{power[npow][THETA_GRID == 0.0][0]:.3f}")

fig, ax = plt.subplots(figsize=(3.3, 2.35))
for (npow, pw), col in zip(power.items(), (BLUE, ORANGE)):
    ax.plot(THETA_GRID, pw, color=col, lw=2, marker="o", ms=3, label=f"$n={npow}$")
ax.axhline(ALPHA, color=INK2, lw=0.8, ls=":")
ax.text(-0.15, ALPHA + 0.03, "nominal size 5%", color=INK2, fontsize=7)
ax.set_xlabel(r"money illusion $\theta$")
ax.set_ylabel("rejection rate at 5%")
ax.set_ylim(0, 1.03)
ax.legend(frameon=False, fontsize=7.5, loc="lower right")
fig.tight_layout()
fig.savefig(FIG / "fig3_power.pdf"); fig.savefig(FIG / "fig3_power.png", dpi=200)
plt.close(fig)

# =============================================================================
# 6. Restricted vs unrestricted b1 under H0 and H1, two designs (Figure 4)
# =============================================================================
rng = np.random.default_rng(SEED + 3)
fig4 = {}
for name, sp_ in SIG_PI_FIG4.items():
    b1U = np.empty((N_FIG4, 2)); b1R = np.empty((N_FIG4, 2))
    rho2, dlt, c12, sdth = (np.empty(N_FIG4) for _ in range(4))
    for s in range(N_FIG4):
        X = simulate_regressors(rng, R, T, sig_pi=sp_)
        XU, XR = design_matrices(X)
        uu = rng.normal(0.0, SIG_U, n)
        Y = np.column_stack([BETA0 + X @ beta_true(th) + uu for th in (0.0, THETA_H1)])
        b1U[s] = np.linalg.lstsq(XU, Y, rcond=None)[0][1]
        b1R[s] = np.linalg.lstsq(XR, Y, rcond=None)[0][1]
        V = np.linalg.inv(XU.T @ XU); Vc = V @ C_THETA
        rho2[s] = Vc[1] ** 2 / (V[1, 1] * (C_THETA @ Vc)); dlt[s] = -Vc[1] / (C_THETA @ Vc)
        c12[s] = np.corrcoef(X[:, 0], X[:, 1])[0, 1]
        sdth[s] = SIG_U * np.sqrt(C_THETA @ Vc)
    fig4[name] = dict(sig_pi=sp_,
                      corr_logp1_logp2=float(c12.mean()),
                      sdU_H0=b1U[:, 0].std(ddof=1), sdR_H0=b1R[:, 0].std(ddof=1),
                      meanU_H0=b1U[:, 0].mean(), meanR_H0=b1R[:, 0].mean(),
                      sdU_H1=b1U[:, 1].std(ddof=1), sdR_H1=b1R[:, 1].std(ddof=1),
                      meanU_H1=b1U[:, 1].mean(), meanR_H1=b1R[:, 1].mean(),
                      sd_ratio_H0=b1R[:, 0].std(ddof=1) / b1U[:, 0].std(ddof=1),
                      sd_ratio_formula=float(np.sqrt(1 - rho2.mean())),
                      biasR_H1=b1R[:, 1].mean() - BETA_H0[0],
                      shiftR_minus_U_H1=float((b1R[:, 1] - b1U[:, 1]).mean()),
                      sd_theta=float(np.sqrt((sdth ** 2).mean())),
                      biasR_H1_formula=float(dlt.mean() * THETA_H1),
                      rho_mean=float(np.sqrt(rho2.mean())))
    fig4[name]["_draws"] = (b1U, b1R)
    f = fig4[name]
    log(f"[fig4:{name}] sigma_pi={sp_}: corr(log p1, log p2) = {f['corr_logp1_logp2']:.3f}; "
        f"H0 sd U = {f['sdU_H0']:.4f}, sd R = {f['sdR_H0']:.4f}, ratio = {f['sd_ratio_H0']:.3f} "
        f"(formula {f['sd_ratio_formula']:.3f}); H1 bias R = {f['biasR_H1']:.4f} "
        f"(formula {f['biasR_H1_formula']:.4f}), mean U = {f['meanU_H1']:.4f}, "
        f"mean(R - U) = {f['shiftR_minus_U_H1']:.4f}, sd(theta_hat) = {f['sd_theta']:.4f}")

fig, axes = plt.subplots(2, 2, figsize=(6.6, 3.0), sharex=True)
xg = np.linspace(-0.85, -0.35, 400)
titles = {"no_inflation": r"$\sigma_\pi=0$ (no common inflation)",
          "strong_inflation": rf"$\sigma_\pi={SIG_PI_FIG4['strong_inflation']:.0f}$ (strong common inflation)"}
for i, name in enumerate(SIG_PI_FIG4):
    b1U, b1R = fig4[name]["_draws"]
    for k, hyp in enumerate(("H0", "H1")):
        ax = axes[i, k]
        dU, dR = stats.gaussian_kde(b1U[:, k])(xg), stats.gaussian_kde(b1R[:, k])(xg)
        ax.fill_between(xg, dU, color=BLUE, alpha=0.25, lw=0)
        ax.plot(xg, dU, color=BLUE, lw=1.4, label="unrestricted (1)")
        ax.plot(xg, dR, color=ORANGE, lw=1.8, label="restricted (18)")
        ax.axvline(BETA_H0[0], color=INK, lw=0.8, ls="--")
        ax.set_yticks([]); ax.set_ylim(0, 9.5)
        hlab = r"$H_0$: $\theta=0$" if hyp == "H0" else rf"$H_1$: $\theta={THETA_H1}$"
        ax.set_title(f"{titles[name]}, {hlab}", fontsize=8, color=INK)
        stat = "sd" if hyp == "H0" else "mean"
        fn = (lambda v: v.std(ddof=1)) if hyp == "H0" else (lambda v: v.mean())
        ax.text(0.02, 0.95, f"unrestricted {stat} {fn(b1U[:, k]):.3f}\n"
                            f"restricted    {stat} {fn(b1R[:, k]):.3f}",
                transform=ax.transAxes, va="top", fontsize=6.5, color=INK)
axes[1, 0].set_xlabel(r"$\hat\beta_1$ (true $\beta_1=-0.6$)")
axes[1, 1].set_xlabel(r"$\hat\beta_1$ (true $\beta_1=-0.6$)")
axes[0, 1].legend(frameon=False, fontsize=7, loc="upper right")
fig.tight_layout(h_pad=0.6, w_pad=0.8)
fig.savefig(FIG / "fig4_restricted_vs_unrestricted.pdf")
fig.savefig(FIG / "fig4_restricted_vs_unrestricted.png", dpi=200)
plt.close(fig)
for name in fig4:
    del fig4[name]["_draws"]

# =============================================================================
# 7. Tables and macros (every number in the note comes from here)
# =============================================================================
h0 = results["H0"]
names = [r"$\beta_0$ (constant)", r"$\beta_1$ ($\log p_1$)", r"$\beta_2$ ($\log p_2$)",
         r"$\beta_3$ ($\log p_3$)", r"$\beta_4$ ($\log Y$)"]
true = np.r_[BETA0, BETA_H0]
fmt = lambda b, s: (f"{b:.3f}", f"({s:.3f})")


def table1_rows():
    rows = []
    for i, nm in enumerate(names):
        rec = "\\textsuperscript{a}" if i == 3 else ""
        reP = "\\textsuperscript{b}" if i == 3 else ""
        # reparametrised coefficients are ordered (b0, b1, b2, b4, theta); b3 is recovered
        jP = {0: 0, 1: 1, 2: 2, 4: 3}.get(i)
        bP = h0["b3P"] if i == 3 else h0["bP"][jP]
        sP = h0["se_b3P"] if i == 3 else h0["seP"][jP]
        rows.append((nm, f"{true[i]:.3f}", fmt(h0["bU"][i], h0["seU"][i]),
                     fmt(h0["bR"][i], h0["seR"][i]), fmt(bP, sP), rec, reP))
    return rows


rows = table1_rows()
th_row = (r"$\theta=\sum_j\beta_j+\beta_4$", "0.000",
          fmt(h0["theta"], h0["se_theta"]), ("0", "(imposed)"),
          fmt(h0["bP"][4], h0["seP"][4]), "", "\\textsuperscript{c}")

tex = [r"\begin{tabular}{lcccc}", r"\toprule",
       r" & True & Unrestricted (1) & Restricted (18) & Reparametrised (25) \\", r"\midrule"]
for nm, tv, U, Rr, P, ra, rb in rows + [th_row]:
    tex.append(f"{nm} & {tv} & {U[0]} {U[1]} & {Rr[0]}{ra} {Rr[1]} & {P[0]}{rb} {P[1]} \\\\")
tex += [r"\midrule",
        f"SSR & & {h0['ssrU']:.4f} & {h0['ssrR']:.4f} & {h0['ssrU']:.4f} \\\\",
        f"Parameters & & {K} & {K-1} & {K} \\\\", r"\bottomrule", r"\end{tabular}"]
import re
minus = lambda line: re.sub(r"(?<![\w$^{])-(?=\d)", "$-$", line)   # proper minus signs
(OUT / "table1.tex").write_text("\n".join(minus(l) for l in tex) + "\n")

md = ["| | True | Unrestricted (1) | Restricted (18) | Reparametrised (25) |",
      "|---|:-:|:-:|:-:|:-:|"]
for nm, tv, U, Rr, P, ra, rb in rows + [th_row]:
    ra_m = "<sup>a</sup>" if ra else ""
    rb_m = "<sup>b</sup>" if "b" in rb else ("<sup>c</sup>" if rb else "")
    md.append(f"| {nm} | {tv} | {U[0]} {U[1]} | {Rr[0]}{ra_m} {Rr[1]} | {P[0]}{rb_m} {P[1]} |")
md += [f"| SSR | | {h0['ssrU']:.4f} | {h0['ssrR']:.4f} | {h0['ssrU']:.4f} |",
       f"| Parameters | | {K} | {K-1} | {K} |"]
(OUT / "table1.md").write_text("\n".join(md) + "\n")

h1 = results["H1"]


def pfmt(p):
    return "$<0.001$" if p < 1e-3 else f"{p:.3f}"


t2rows = [(r"$\hat\theta$", f"{h0['theta']:.4f}", f"{h1['theta']:.4f}"),
          (r"$\mathrm{se}(\hat\theta)$", f"{h0['se_theta']:.4f}",
           f"{h1['se_theta']:.4f}"),
          (r"$t$", f"{h0['t']:.4f}", f"{h1['t']:.4f}"),
          (r"$t^2$", f"{h0['t2']:.4f}", f"{h1['t2']:.4f}"),
          (r"$F$ from SSRs", f"{h0['F']:.4f}", f"{h1['F']:.4f}"),
          (r"$p$-value ($t$ and $F$)", pfmt(h0['p_F']), pfmt(h1['p_F'])),
          (r"95\% CI for $\theta$", f"[{h0['ci'][0]:.3f}, {h0['ci'][1]:.3f}]",
           f"[{h1['ci'][0]:.3f}, {h1['ci'][1]:.3f}]"),
          (r"Wald / LR ($\chi^2_1$)", f"{h0['W']:.3f} / {h0['LR']:.3f}",
           f"{h1['W']:.3f} / {h1['LR']:.3f}")]
tex2 = [r"\begin{tabular}{lcc}", r"\toprule",
        rf" & $H_0$: $\theta=0$ & $H_1$: $\theta={THETA_H1}$ \\",
        r"\midrule"] + [f"{a} & {b} & {c} \\\\" for a, b, c in t2rows] + \
       [r"\bottomrule", r"\end{tabular}"]
(OUT / "table2.tex").write_text("\n".join(minus(l) for l in tex2) + "\n")


def tex2md(s):
    return (s.replace("\\%", "%").replace("$<0.001$", "< 0.001"))


md2 = [f"| | $H_0$: $\\theta=0$ | $H_1$: $\\theta={THETA_H1}$ |",
       "|---|:-:|:-:|"] + [f"| {tex2md(a)} | {tex2md(b)} | {tex2md(c)} |" for a, b, c in t2rows]
(OUT / "table2.md").write_text("\n".join(md2) + "\n")

fi, fs = fig4["no_inflation"], fig4["strong_inflation"]
macros = {
    "nObs": f"{n}", "nR": f"{R}", "nT": f"{T}", "Kpar": f"{K}", "dfres": f"{n-K}",
    "sigPi": f"{SIG_PI}", "sigMu": f"{SIG_MU}", "sigEps": f"{SIG_EPS}", "sigOm": f"{SIG_OM}",
    "sigU": f"{SIG_U}", "thetaOne": f"{THETA_H1}", "seed": f"{SEED}",
    "nMC": f"{N_MC:,}".replace(",", "{,}"), "nPow": f"{N_POWER:,}".replace(",", "{,}"),
    "nFigFour": f"{N_FIG4:,}".replace(",", "{,}"),
    "corrPOneY": f"{corr[0, 3]:.2f}", "corrPOneTwo": f"{corr[0, 1]:.2f}",
    "thetaHzero": f"{h0['theta']:.3f}", "seThetaHzero": f"{h0['se_theta']:.3f}",
    "tHzero": f"{h0['t']:.2f}", "FHzero": f"{h0['F']:.2f}", "pHzero": f"{h0['p_F']:.2f}",
    "ciLoHzero": f"{h0['ci'][0]:.3f}", "ciHiHzero": f"{h0['ci'][1]:.3f}",
    "thetaHone": f"{h1['theta']:.3f}", "tHone": f"{h1['t']:.2f}", "FHone": f"{h1['F']:.2f}",
    "pHone": f"{h1['p_F']:.2f}",
    "ciLoHone": f"{h1['ci'][0]:.3f}", "ciHiHone": f"{h1['ci'][1]:.3f}",
    "maxFtTwo": f"{max(abs(h0['F_minus_t2']), abs(h1['F_minus_t2'])):.0e}".replace("e-", r"\times 10^{-") + "}",
    "invCoef": f"{max(h0['invariance_max_coef_diff'], h1['invariance_max_coef_diff']):.0e}".replace("e-", r"\times 10^{-") + "}",
    "seBoneU": f"{h0['seU'][1]:.3f}", "seBoneR": f"{h0['seR'][1]:.3f}",
    "seBthreeR": f"{h0['seR'][3]:.3f}", "seBthreeU": f"{h0['seU'][3]:.3f}",
    "rhoMain": f"{h0['rho_b1_theta']:.2f}", "deltaMain": f"{h0['delta_b1']:.2f}",
    "sizeRate": f"{100*size:.1f}", "sizeMCSE": f"{100*size_mcse:.1f}",
    "sizeWald": f"{100*size_W:.1f}", "Fcrit": f"{Fcrit:.2f}", "ksP": f"{ks.pvalue:.2f}",
    "powSmall": f"{power[R_POWER[0]*T][THETA_GRID == 0.10][0]:.2f}",
    "powLarge": f"{power[R_POWER[1]*T][THETA_GRID == 0.10][0]:.2f}",
    "nPowSmall": f"{R_POWER[0]*T}", "nPowLarge": f"{R_POWER[1]*T:,}".replace(",", "{,}"),
    "sigPiStrong": f"{fs['sig_pi']:.0f}",
    "corrPPNoInf": f"{fi['corr_logp1_logp2']:.2f}", "corrPPStrong": f"{fs['corr_logp1_logp2']:.2f}",
    "sdUNoInf": f"{fi['sdU_H0']:.3f}", "sdRNoInf": f"{fi['sdR_H0']:.3f}",
    "ratioNoInf": f"{fi['sd_ratio_H0']:.2f}", "ratioFormNoInf": f"{fi['sd_ratio_formula']:.2f}",
    "sdUStrong": f"{fs['sdU_H0']:.3f}", "sdRStrong": f"{fs['sdR_H0']:.3f}",
    "ratioStrong": f"{fs['sd_ratio_H0']:.2f}", "ratioFormStrong": f"{fs['sd_ratio_formula']:.2f}",
    "biasNoInf": f"{fi['biasR_H1']:.3f}", "biasFormNoInf": f"{fi['biasR_H1_formula']:.3f}",
    "biasStrong": f"{fs['biasR_H1']:.3f}", "biasFormStrong": f"{fs['biasR_H1_formula']:.3f}",
    "meanUHoneNoInf": f"{fi['meanU_H1']:.3f}", "meanUHoneStrong": f"{fs['meanU_H1']:.3f}",
    "shiftNoInf": f"{fi['shiftR_minus_U_H1']:.3f}", "shiftStrong": f"{fs['shiftR_minus_U_H1']:.3f}",
    "sdThetaNoInf": f"{fi['sd_theta']:.3f}", "sdThetaStrong": f"{fs['sd_theta']:.3f}",
}
(OUT / "macros_sim.tex").write_text(
    "% generated by python/simulate_and_test.py -- do not edit\n"
    + "".join(f"\\newcommand{{\\{k}}}{{{v}}}\n" for k, v in macros.items()))
(OUT / "macros_sim.json").write_text(json.dumps(macros, indent=2))

results.update(size=dict(rate=size, mcse=size_mcse, wald_rate=size_W, Fcrit=Fcrit,
                         ks_p=ks.pvalue),
               power={str(k): v.tolist() for k, v in power.items()},
               theta_grid=THETA_GRID.tolist(), fig4=fig4)
(OUT / "results.json").write_text(json.dumps(results, indent=2, default=float))
mc_flat = {"size": size, **{f"power_{k}_0.10": v[THETA_GRID == 0.10][0] for k, v in power.items()},
           **{f"fig4_{k}_sd_ratio_H0": v["sd_ratio_H0"] for k, v in fig4.items()},
           **{f"fig4_{k}_shift_H1": v["shiftR_minus_U_H1"] for k, v in fig4.items()}}
pd.Series(mc_flat).to_csv(OUT / "python_mc.csv", header=["value"], index_label="name",
                          float_format="%.17g")
(OUT / "log_python.txt").write_text("\n".join(log_lines) + "\n")
log("\nDone. Outputs in data/, output/ and note/figures/.")
