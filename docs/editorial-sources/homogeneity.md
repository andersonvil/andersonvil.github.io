---
title: "Imposing and Testing Zero-Degree Homogeneity in Demand"
subtitle: "A teaching note"
---

## 1. Intuition and a numerical example

A household with a monthly income of €2,000 chooses a basket. Suppose every price and its income double. A basket $x$ was affordable before if $p\cdot x\le 2000$; it is affordable now if $2p\cdot x\le 4000$, which is the same inequality. The budget set has not moved, so a household that cares about quantities, not about nominal values, has no reason to change its basket. Demand is *homogeneous of degree zero* in prices and income: this is the formal definition of "no money illusion".

The econometrician observes $J$ prices and income $Y$ and writes the demand for good 1 as

$$
\log q_1=\beta_0+\sum_{j=1}^{J}\beta_j\log p_j+\beta_{J+1}\log Y+u . \qquad (1)
$$

Section 3 shows that multiplying all prices and income by $\lambda>0$ multiplies $q_1$ by $\lambda^{\theta}$, where $\theta\equiv\sum_{j=1}^J\beta_j+\beta_{J+1}$. With $\theta=0.1$, doubling prices and income raises demand by $2^{0.1}-1=7.2\%$, although nothing real has changed: that is money illusion. Homogeneity is the restriction $\theta=0$.

## 2. From the utility function to the demand function

**The general result.** The Marshallian demand $x(p,Y)$ maximises $U(x)$ over the budget set $B(p,Y)=\{x\ge0:\ p\cdot x\le Y\}$. (I write $U$ for utility and keep $u$ for the error term.) For any $\lambda>0$:

$$
\begin{aligned}
B(\lambda p,\lambda Y)&=\{x\ge0:\ (\lambda p)\cdot x\le \lambda Y\} \\
&=\{x\ge0:\ \lambda\,(p\cdot x)\le \lambda Y\} \\
&=\{x\ge0:\ p\cdot x\le Y\}\qquad\text{(divide by }\lambda>0) \\
&=B(p,Y), \\
x(\lambda p,\lambda Y)&=\arg\max_{x\in B(\lambda p,\lambda Y)}U(x)=\arg\max_{x\in B(p,Y)}U(x)=x(p,Y). && (2)
\end{aligned}
$$

Homogeneity is therefore a consequence of the budget constraint, not an assumption about the shape of preferences. It requires only that $U$ depends on quantities and not on nominal magnitudes, and that the maximiser is unique (otherwise the demand *set* is homogeneous).

**Cobb–Douglas.** Let $U(x)=\sum_j\alpha_j\log x_j$ with $\alpha_j>0$ and $\sum_j\alpha_j=1$. The budget constraint binds because $U$ is increasing.

$$
\begin{aligned}
\mathcal L&=\sum_{j=1}^J\alpha_j\log x_j-\mu\Big(\sum_{j=1}^Jp_jx_j-Y\Big) && (3) \\
\frac{\partial\mathcal L}{\partial x_j}&=\frac{\alpha_j}{x_j}-\mu p_j=0,\qquad j=1,\dots,J && (4) \\
\frac{\partial\mathcal L}{\partial \mu}&=0\ \Longleftrightarrow\ \sum_{j=1}^Jp_jx_j=Y && (5) \\
\text{from (4):}\quad p_jx_j&=\frac{\alpha_j}{\mu},\qquad j=1,\dots,J && (6) \\
\text{sum (6) over } j:\quad \sum_{j=1}^Jp_jx_j&=\frac1\mu\sum_{j=1}^J\alpha_j=\frac1\mu \\
\text{use (5):}\quad Y&=\frac1\mu\quad\Longrightarrow\quad \mu=\frac1Y \\
\text{put }\mu=1/Y\text{ in (6), }j=1:\quad p_1x_1&=\alpha_1Y \\
x_1&=\frac{\alpha_1Y}{p_1} && (7) \\
\log x_1&=\log\alpha_1-\log p_1+\log Y. && (8)
\end{aligned}
$$

Comparing (8) with (1): $\beta_0=\log\alpha_1$, $\beta_1=-1$, $\beta_j=0$ for $j=2,\dots,J$, $\beta_{J+1}=1$, so $\theta=-1+0+1=0$. Directly on (7):

$$
x_1(\lambda p,\lambda Y)=\frac{\alpha_1\,\lambda Y}{\lambda p_1}=\frac{\alpha_1Y}{p_1}=x_1(p,Y).
$$

**Stone–Geary (linear expenditure system).** Cobb–Douglas forces zero cross-price elasticities and a unit income elasticity. Let instead $U(x)=\sum_j\alpha_j\log(x_j-\gamma_j)$, with $\alpha_j>0$, $\sum_j\alpha_j=1$, $x_j>\gamma_j$ and $Y>\sum_jp_j\gamma_j$ (Stone, 1954). The Lagrangian is the same as (3) with $\log(x_j-\gamma_j)$ in place of $\log x_j$; the condition $\partial\mathcal L/\partial\mu=0$ is again (5).

$$
\begin{aligned}
\frac{\partial\mathcal L}{\partial x_j}&=\frac{\alpha_j}{x_j-\gamma_j}-\mu p_j=0,\qquad j=1,\dots,J && (9) \\
\text{from (9):}\quad p_jx_j&=p_j\gamma_j+\frac{\alpha_j}{\mu},\qquad j=1,\dots,J && (10) \\
\text{sum (10) over } j\text{, use (5) and }\textstyle\sum_j\alpha_j=1:\quad Y&=\sum_{j=1}^Jp_j\gamma_j+\frac1\mu \\
\frac1\mu&=Y-\sum_{j=1}^Jp_j\gamma_j && (11) \\
\text{put (11) in (10), }j=1:\quad p_1x_1&=p_1\gamma_1+\alpha_1\Big(Y-\sum_{j=1}^Jp_j\gamma_j\Big) \\
x_1&=\gamma_1+\frac{\alpha_1}{p_1}\Big(Y-\sum_{j=1}^Jp_j\gamma_j\Big). && (12)
\end{aligned}
$$

The multiplier is the inverse of *supernumerary* income (11); the household buys the committed quantity $\gamma_1$ and spends a share $\alpha_1$ of what is left (12). Homogeneity, directly on (12):

$$
\begin{aligned}
x_1(\lambda p,\lambda Y)&=\gamma_1+\frac{\alpha_1}{\lambda p_1}\Big(\lambda Y-\sum_{j=1}^J\lambda p_j\gamma_j\Big)
=\gamma_1+\frac{\lambda\,\alpha_1}{\lambda\,p_1}\Big(Y-\sum_{j=1}^Jp_j\gamma_j\Big)=x_1(p,Y).
\end{aligned}
$$

*Elasticities.* Differentiate (12); for the own price, use (12) in the form $(\alpha_1/p_1)(Y-\sum_jp_j\gamma_j)=x_1-\gamma_1$:

$$
\begin{aligned}
\frac{\partial x_1}{\partial Y}=\frac{\alpha_1}{p_1}
\quad&\Longrightarrow\quad \eta_1=\frac{Y}{x_1}\frac{\partial x_1}{\partial Y}=\frac{\alpha_1Y}{p_1x_1} \\
\frac{\partial x_1}{\partial p_j}=-\frac{\alpha_1\gamma_j}{p_1}\ (j\ne1)
\quad&\Longrightarrow\quad \varepsilon_{1j}=\frac{p_j}{x_1}\frac{\partial x_1}{\partial p_j}=-\frac{\alpha_1\gamma_jp_j}{p_1x_1} \\
\frac{\partial x_1}{\partial p_1}=-\frac{\alpha_1}{p_1^2}\Big(Y-\sum_{j}p_j\gamma_j\Big)-\frac{\alpha_1\gamma_1}{p_1}=-\frac{x_1-\gamma_1+\alpha_1\gamma_1}{p_1}
\quad&\Longrightarrow\quad \varepsilon_{11}=-1+\frac{(1-\alpha_1)\gamma_1}{x_1}. && (13)
\end{aligned}
$$

Cross-price elasticities are non-zero as soon as $\gamma_j\ne0$ (negative when $\gamma_j>0$), and $\eta_1\ne1$ unless $\gamma=0$. Their sum, over the common denominator $p_1x_1$:

$$
\begin{aligned}
p_1x_1\Big(\sum_{j=1}^J\varepsilon_{1j}+\eta_1\Big)
&=\big[-p_1x_1+(1-\alpha_1)p_1\gamma_1\big]-\alpha_1\sum_{j\ne1}p_j\gamma_j+\alpha_1Y \\
&=-p_1x_1+p_1\gamma_1+\alpha_1\Big(Y-\sum_{j=1}^Jp_j\gamma_j\Big) \\
&=-p_1x_1+p_1x_1=0\qquad\text{by (12).}
\end{aligned}
$$

Numerically, with $J=3$, $\alpha=(0.3,0.3,0.4)$, $\gamma=(2,3,1)$, $p=(2,1,1.5)$, $Y=20$: $x_1=3.725$, $\varepsilon_{11}=-0.624$, $\varepsilon_{12}=-0.121$, $\varepsilon_{13}=-0.060$, $\eta_1=0.805$; the sum is exactly zero.

**Back to model (1): Euler's identity.** The zero sum is not a property of Stone–Geary; it holds for every demand satisfying (2). Fix $(p,Y)$ and let $g(\lambda)=x_1(\lambda p,\lambda Y)$.

$$
\begin{aligned}
g(\lambda)&=x_1(p,Y)\ \text{ for all }\lambda>0\ \text{ by (2)}\quad\Longrightarrow\quad g'(\lambda)=0 \\
g'(\lambda)&=\sum_{j=1}^Jp_j\,\frac{\partial x_1}{\partial p_j}(\lambda p,\lambda Y)+Y\,\frac{\partial x_1}{\partial Y}(\lambda p,\lambda Y)\qquad\text{(chain rule)} \\
\text{at }\lambda=1:\quad 0&=\sum_{j=1}^Jp_j\,\frac{\partial x_1}{\partial p_j}+Y\,\frac{\partial x_1}{\partial Y} \\
\text{divide by }x_1:\quad 0&=\sum_{j=1}^J\varepsilon_{1j}+\eta_1. && (14)
\end{aligned}
$$

In (1), $\varepsilon_{1j}=\partial\log q_1/\partial\log p_j=\beta_j$ and $\eta_1=\beta_{J+1}$ are constant, so (14) reads $\theta=0$. Model (1) is a constant-elasticity approximation to a utility-generated demand, and testing $\theta=0$ tests Euler's identity. The approximation has limits: a double-log demand with unrestricted elasticities is not, in general, exactly and globally derivable from a utility function, since integrability imposes further restrictions (LaFrance, 1986, for incomplete systems of constant-elasticity demands). Homogeneity is the part of the theory that a single equation can carry.

**Maintained assumptions.** (i) Scaling by $\lambda$ changes neither preferences nor the distribution of $u$: homogeneity concerns $E(\log q_1\mid p,Y)$. (ii) Rejecting $\theta=0$ rejects *jointly* the absence of money illusion and the maintained specification (constant elasticities, exogenous prices and income).

![Figure 1](figures/fig1_utility.png)

*Figure 1: Cobb–Douglas, $U=0.4\log x_1+0.6\log x_2$, $p=(40,25)$, $Y=2000$. (a) The budget lines at $(p,Y)$ and $(2p,2Y)$ coincide, so the optimum $x^*$ is unchanged; doubling income alone (dotted) shifts the line out. (b) The utility surface, the budget path on it, and its highest point $x^*$.*

## 3. Imposing the restriction

**The restriction.** Replace every $p_j$ by $\lambda p_j$ and $Y$ by $\lambda Y$ in (1):

$$
\begin{aligned}
\log q_1(\lambda p,\lambda Y)&=\beta_0+\sum_{j=1}^J\beta_j\log(\lambda p_j)+\beta_{J+1}\log(\lambda Y)+u \\
&=\beta_0+\sum_{j=1}^J\beta_j(\log\lambda+\log p_j)+\beta_{J+1}(\log\lambda+\log Y)+u \\
&=\underbrace{\beta_0+\sum_{j=1}^J\beta_j\log p_j+\beta_{J+1}\log Y+u}_{\log q_1(p,Y)}+\Big(\sum_{j=1}^J\beta_j+\beta_{J+1}\Big)\log\lambda \\
&=\log q_1(p,Y)+\theta\log\lambda. && (15)
\end{aligned}
$$

Homogeneity requires $\theta\log\lambda=0$ for all $\lambda>0$. A product of two numbers is zero only if one of them is. Taking $\lambda=2$, so that $\log\lambda\ne0$, shows that $\theta=0$ is necessary; conversely $\theta=0$ makes (15) hold for every $\lambda$. Hence

$$
\text{homogeneity of degree zero}\quad\Longleftrightarrow\quad \sum_{j=1}^J\beta_j+\beta_{J+1}=0. \qquad (16)
$$

**Solving the restriction.** Any of the $J+1$ slope coefficients can be eliminated; take the last price elasticity $\beta_J$:

$$
\begin{aligned}
\sum_{j=1}^{J-1}\beta_j+\beta_J+\beta_{J+1}&=0 \\
\beta_J&=-\Big(\sum_{j=1}^{J-1}\beta_j+\beta_{J+1}\Big). && (17)
\end{aligned}
$$

Split the price sum in (1), substitute (17), expand and regroup:

$$
\begin{aligned}
\log q_1&=\beta_0+\sum_{j=1}^{J-1}\beta_j\log p_j+\beta_J\log p_J+\beta_{J+1}\log Y+u \\
&=\beta_0+\sum_{j=1}^{J-1}\beta_j\log p_j-\Big(\sum_{j=1}^{J-1}\beta_j+\beta_{J+1}\Big)\log p_J+\beta_{J+1}\log Y+u \\
&=\beta_0+\sum_{j=1}^{J-1}\beta_j\log p_j-\sum_{j=1}^{J-1}\beta_j\log p_J-\beta_{J+1}\log p_J+\beta_{J+1}\log Y+u \\
&=\beta_0+\sum_{j=1}^{J-1}\beta_j\,(\log p_j-\log p_J)+\beta_{J+1}\,(\log Y-\log p_J)+u \\
\log q_1&=\beta_0+\sum_{j=1}^{J-1}\beta_j\log\frac{p_j}{p_J}+\beta_{J+1}\log\frac{Y}{p_J}+u. && (18)
\end{aligned}
$$

Imposing homogeneity means deflating every price and income by one of the prices: the regressors become $J-1$ relative prices and one real income. Model (1) has $K=J+2$ parameters; model (18) has $1+(J-1)+1=J+1=K-1$, one fewer, which is what one restriction should cost. The coefficients keep their meaning: $\beta_j$ is still the elasticity with respect to $p_j$, other nominal prices and $Y$ held fixed.

**Recovering $\hat\beta_J$ and its standard error.** After OLS on (18), the missing elasticity comes from the restriction itself. Write $\hat b_R=(\hat\beta_0,\hat\beta_1,\dots,\hat\beta_{J-1},\hat\beta_{J+1})'$, $\hat V_R$ its estimated covariance matrix, $a=(0,-1,\dots,-1)'$ and $S=\{1,\dots,J-1,J+1\}$:

$$
\begin{aligned}
\hat\beta_J&=-\Big(\sum_{j=1}^{J-1}\hat\beta_j+\hat\beta_{J+1}\Big)=a'\hat b_R \\
\widehat{\operatorname{Var}}(\hat\beta_J)&=a'\hat V_Ra=\sum_{j\in S}\widehat{\operatorname{Var}}(\hat\beta_j)+2\sum_{j,k\in S,\ j<k}\widehat{\operatorname{Cov}}(\hat\beta_j,\hat\beta_k). && (19)
\end{aligned}
$$

The covariances matter: summing the variances alone would be wrong.

**Invariance to the numeraire.** OLS on (18) minimises the sum of squared residuals of (1) subject to (16). That problem does not depend on which coefficient is eliminated, so the estimates are identical whichever price deflates. In the simulation below, the three numeraires give coefficients that differ by at most $6\times 10^{-15}$.

**The trade-off, step by step.** Stack (1) as $y=X\beta+u$ with $\beta=(\beta_0,\dots,\beta_{J+1})'$, and write the restriction as $c'\beta=0$ with $c=(0,1,\dots,1)'$. Let $\hat\beta_U=(X'X)^{-1}X'y$, $\hat\theta=c'\hat\beta_U$, $h=(X'X)^{-1}c$ and $m=c'(X'X)^{-1}c$. Restricted least squares minimises $(y-X\beta)'(y-X\beta)$ subject to $c'\beta=0$:

$$
\begin{aligned}
\text{FOC of } (y-X\beta)'(y-X\beta)+2\nu\,c'\beta:\quad -2X'(y-X\hat\beta_R)+2\nu c&=0 \\
X'X\hat\beta_R&=X'y-\nu c \\
\hat\beta_R&=\hat\beta_U-\nu h \\
\text{impose } c'\hat\beta_R=0:\quad 0=\hat\theta-\nu m\quad&\Longrightarrow\quad \nu=\hat\theta/m \\
\hat\beta_R&=\hat\beta_U-\frac{\hat\theta}{m}\,h. && (20)
\end{aligned}
$$

This is the OLS estimator of (18), written in terms of (1). With $\mathrm{Var}(\hat\beta_U)=\sigma^2(X'X)^{-1}$, $\mathrm{Cov}(\hat\beta_U,\hat\theta)=\mathrm{Var}(\hat\beta_U)\,c=\sigma^2h$ and $\mathrm{Var}(\hat\theta)=\sigma^2m$. Take the coefficient $\beta_1$ (first element of $h$: $h_1$), conditional on $X$:

$$
\begin{aligned}
\hat\beta_{1,R}&=\hat\beta_{1,U}+\delta\,\hat\theta,\qquad
\delta\equiv-\frac{h_1}{m}=-\frac{{\operatorname{Cov}}(\hat\beta_{1,U},\hat\theta)}{{\operatorname{Var}}(\hat\theta)} && (21) \\
E(\hat\beta_{1,R})&=\beta_1+\delta\,\theta && (22) \\
{\operatorname{Var}}(\hat\beta_{1,R})&={\operatorname{Var}}(\hat\beta_{1,U})+\delta^2{\operatorname{Var}}(\hat\theta)+2\delta\,{\operatorname{Cov}}(\hat\beta_{1,U},\hat\theta) \\
&={\operatorname{Var}}(\hat\beta_{1,U})+\frac{{\operatorname{Cov}}(\hat\beta_{1,U},\hat\theta)^2}{{\operatorname{Var}}(\hat\theta)}-2\,\frac{{\operatorname{Cov}}(\hat\beta_{1,U},\hat\theta)^2}{{\operatorname{Var}}(\hat\theta)} \\
&=(1-\rho^2)\,{\operatorname{Var}}(\hat\beta_{1,U}),\qquad \rho\equiv\operatorname{corr}(\hat\beta_{1,U},\hat\theta). && (23)
\end{aligned}
$$

If $H_0$ is true ($\theta=0$), imposing the restriction removes the share $\rho^2$ of the variance (23) at no cost. If it is false, the estimator is shifted by $\delta\theta$ (22). Both effects come from the same source: how strongly $\hat\beta_1$ co-moves with $\hat\theta$.

A common intuition says that the gain is largest when prices move together because of common inflation. This intuition is wrong. A common inflation factor raises the pairwise correlations among nominal prices, but it adds variation only along the direction $(1,\dots,1)$: exactly the direction that identifies $\theta$, which leaves relative prices unchanged. It therefore *lowers* $\rho$, and imposing the restriction then buys almost no precision. Yet $\delta$, and hence the bias when $H_0$ is false, is unchanged in our design (Figure 3). The gain is large when the price level varies little independently of relative prices and real income: a single cross-section, a period of stable inflation, or a price level that trends together with real income.

**Identification.** The restriction creates no identifying variation. In (18), $\beta_j$ ($j<J$) is identified only by variation in relative prices across regions or over time. If prices move only through common inflation, $\log(p_j/p_J)$ is constant and (18) cannot be estimated; in (1) only $\theta$ is then identified.

## 4. Testing the restriction

The hypotheses are $H_0:\ \sum_{j=1}^J\beta_j+\beta_{J+1}=0$ against $H_1:\ \sum_{j=1}^J\beta_j+\beta_{J+1}\ne0$, in the unrestricted model (1). Let $s^2=\mathrm{SSR}_U/(n-K)$, so that $\hat V_U=s^2(X'X)^{-1}$.

**1. Student $t$ on a linear combination.**

$$
\begin{aligned}
\hat\theta&=c'\hat\beta_U=\sum_{j=1}^J\hat\beta_j+\hat\beta_{J+1} \\
{\operatorname{se}}(\hat\theta)^2&=c'\hat V_Uc=s^2m=\sum_{j=1}^{J+1}\widehat{\operatorname{Var}}(\hat\beta_j)+2\sum_{1\le j<k\le J+1}\widehat{\operatorname{Cov}}(\hat\beta_j,\hat\beta_k) \\
t&=\frac{\hat\theta}{{\operatorname{se}}(\hat\theta)}\ \sim\ t_{n-K}\quad\text{under }H_0\text{ (normal errors).} && (24)
\end{aligned}
$$

**2. Reparametrisation.** Rewrite (1) so that $\theta$ becomes a coefficient. Start from (1) with the price sum split, write $\log p_j=\log(p_j/p_J)+\log p_J$ and $\log Y=\log(Y/p_J)+\log p_J$, and collect the terms in $\log p_J$:

$$
\begin{aligned}
\log q_1&=\beta_0+\sum_{j=1}^{J-1}\beta_j\log p_j+\beta_J\log p_J+\beta_{J+1}\log Y+u \\
&=\beta_0+\sum_{j=1}^{J-1}\beta_j\Big(\log\frac{p_j}{p_J}+\log p_J\Big)+\beta_J\log p_J+\beta_{J+1}\Big(\log\frac{Y}{p_J}+\log p_J\Big)+u \\
&=\beta_0+\sum_{j=1}^{J-1}\beta_j\log\frac{p_j}{p_J}+\beta_{J+1}\log\frac{Y}{p_J}+\Big(\sum_{j=1}^{J-1}\beta_j+\beta_J+\beta_{J+1}\Big)\log p_J+u \\
\log q_1&=\beta_0+\sum_{j=1}^{J-1}\beta_j\log\frac{p_j}{p_J}+\beta_{J+1}\log\frac{Y}{p_J}+\theta\log p_J+u. && (25)
\end{aligned}
$$

Model (25) is (1) with the same fit and the same coefficients $\beta_0,\dots,\beta_{J-1},\beta_{J+1}$; it reduces to (18) when $\theta=0$. Regressing $\log q_1$ on the regressors of (18) *plus* $\log p_J$, the coefficient on $\log p_J$ is $\hat\theta$, its standard error is $\mathrm{se}(\hat\theta)$, and its usual $t$ statistic is (24). Any regression software then reports the test and the confidence interval for $\theta$ directly.

**3. Fisher test, and why $F=t^2$.** Let $e_U=y-X\hat\beta_U$, with $X'e_U=0$. By (20), $y-X\hat\beta_R=e_U+(\hat\theta/m)Xh$, and $h'X'Xh=c'(X'X)^{-1}c=m$:

$$
\begin{aligned}
\mathrm{SSR}_R&=e_U'e_U+2\frac{\hat\theta}{m}h'X'e_U+\frac{\hat\theta^2}{m^2}h'X'Xh
=\mathrm{SSR}_U+\frac{\hat\theta^2}{m} && (26) \\
F&=\frac{(\mathrm{SSR}_R-\mathrm{SSR}_U)/1}{\mathrm{SSR}_U/(n-K)}=\frac{\hat\theta^2/m}{s^2}=\frac{\hat\theta^2}{{\operatorname{se}}(\hat\theta)^2}=t^2\ \sim\ F(1,n-K)\ \text{ under }H_0. && (27)
\end{aligned}
$$

The large-sample versions are $W=n(\mathrm{SSR}_R-\mathrm{SSR}_U)/\mathrm{SSR}_U$ (Wald) and $LR=n\log(\mathrm{SSR}_R/\mathrm{SSR}_U)$ (likelihood ratio), both $\chi^2_1$ under $H_0$, with $W\ge LR$.

**Reading the result.** Not rejecting $H_0$ does not prove homogeneity: it means only that the data cannot distinguish $\theta$ from zero at the chosen level. The confidence interval $\hat\theta\pm t_{n-K,0.975}\,\mathrm{se}(\hat\theta)$ is the informative object: it says which degrees of money illusion the data rule out. Applied work on complete demand systems has rejected homogeneity strikingly often (Deaton and Muellbauer, 1980). Laitinen (1978) showed that much of this comes from asymptotic tests, which over-reject badly when the number of equations is large relative to the sample, and derived the exact finite-sample distribution of the test statistic.

## 5. Simulation

**Design.** $J=3$ goods, $50$ regions $\times$ $10$ periods ($n=500$). Log prices are $\log p_{jrt}=\pi_t+\mu_{jr}+\epsilon_{jrt}$: common inflation $\pi_t\sim N(0,0.2^2)$, regional price levels $\mu_{jr}\sim N(0,0.15^2)$, shocks $\epsilon_{jrt}\sim N(0,0.05^2)$. Log income is $\log Y_{rt}=\log 2000+\pi_t+\omega_{rt}$ with $\omega_{rt}\sim N(0,0.3^2)$, and $u\sim N(0,0.2^2)$ (seed 20261015). Under $H_0$: $\beta=(-0.6,\,0.2,\,0.1,\,0.3)$, so $\theta=0$. Under $H_1$, only the income elasticity changes, to $0.4$, so $\theta=0.1$; the same $u$ is used for both, which makes the $H_0$ and $H_1$ columns directly comparable.

*Table 1: Estimates on the data generated under $H_0$ (standard errors in parentheses).*

| | True | Unrestricted (1) | Restricted (18) | Reparametrised (25) |
|---|:-:|:-:|:-:|:-:|
| $\beta_0$ (constant) | 1.000 | 0.872 (0.218) | 0.855 (0.217) | 0.872 (0.218) |
| $\beta_1$ ($\log p_1$) | -0.600 | -0.594 (0.050) | -0.586 (0.048) | -0.594 (0.050) |
| $\beta_2$ ($\log p_2$) | 0.200 | 0.195 (0.049) | 0.205 (0.046) | 0.195 (0.049) |
| $\beta_3$ ($\log p_3$) | 0.100 | 0.053 (0.048) | 0.064<sup>a</sup> (0.045) | 0.053<sup>b</sup> (0.048) |
| $\beta_4$ ($\log Y$) | 0.300 | 0.315 (0.029) | 0.317 (0.028) | 0.315 (0.029) |
| $\theta=\sum_j\beta_j+\beta_4$ | 0.000 | -0.032 (0.050) | 0 (imposed) | -0.032<sup>c</sup> (0.050) |
| SSR | | 18.6994 | 18.7154 | 18.6994 |
| Parameters | | 5 | 4 | 5 |

<sup>a</sup> Recovered by (19). <sup>b</sup> Recovered as $\hat\theta-\hat\beta_1-\hat\beta_2-\hat\beta_4$. <sup>c</sup> Coefficient of $\log p_3$ in (25).

**Estimates and tests.** In Table 1, the reparametrised regression (25) reproduces the unrestricted coefficients and delivers $\hat\theta$ as an ordinary coefficient. Imposing the restriction shrinks the standard error of $\hat\beta_1$ from $0.050$ to $0.048$ and that of $\hat\beta_3$ from $0.048$ to $0.045$. The gains are modest, as (23) predicts with $\hat\rho=0.25$ in this sample. In Table 2, $F$ and $t^2$ agree to $2\times 10^{-13}$, as (27) requires. Under $H_0$ the test does not reject ($p=0.51$). Under $H_1$ it does not reject either ($p=0.17$), yet the interval $[-0.030,0.165]$ contains the true $\theta=0.1$. This is the warning of Section 4 in numbers: non-rejection is not evidence of homogeneity.

**Size and power.** Over $2{,}000$ samples drawn under $H_0$, the $F$ statistic follows its $F(1,495)$ distribution (Figure 2; Kolmogorov–Smirnov $p=0.53$). The test rejects in $5.0\%$ of samples, with a Monte Carlo standard error of $0.5$ points. At $\theta=0.1$, power is $0.60$ with $n=500$ and $0.98$ with $n=2{,}000$ (Figure A.1, appendix).

*Table 2: Tests of $H_0:\theta=0$ on the data generated under $H_0$ and under $H_1$ (same regressors and errors).*

| | $H_0$: $\theta=0$ | $H_1$: $\theta=0.1$ |
|---|:-:|:-:|
| $\hat\theta$ | -0.0323 | 0.0677 |
| $\mathrm{se}(\hat\theta)$ | 0.0495 | 0.0495 |
| $t$ | -0.6517 | 1.3683 |
| $t^2$ | 0.4247 | 1.8723 |
| $F$ from SSRs | 0.4247 | 1.8723 |
| $p$-value ($t$ and $F$) | 0.515 | 0.172 |
| 95% CI for $\theta$ | [-0.130, 0.065] | [-0.030, 0.165] |
| Wald / LR ($\chi^2_1$) | 0.429 / 0.429 | 1.891 / 1.888 |

![Figure 2](figures/fig2_size.png)

*Figure 2: Size: $F$ over $2{,}000$ samples under $H_0$, and the $F(1,495)$ density.*

![Figure 3](figures/fig4_restricted_vs_unrestricted.png)

*Figure 3: Sampling distribution of $\hat\beta_1$, unrestricted (1) and restricted (18), $n=500$, $2{,}000$ samples. Top row: no common inflation. Bottom row: strong common inflation, where $\mathrm{corr}(\log p_1,\log p_2)=0.97$.*

**The cost of imposing.** Figure 3 illustrates (22) and (23). Without common inflation, nominal prices are uncorrelated ($\mathrm{corr}=0.00$), yet imposing a true restriction cuts the standard deviation of $\hat\beta_1$ from $0.059$ to $0.049$ (ratio $0.83$; (23) gives $0.83$). With strong common inflation, prices are highly correlated and the gain vanishes (ratio $1.00$). If the restriction is false, the restricted estimator is shifted by $-0.029$ and $-0.031$ relative to the unrestricted one ((22) gives $-0.031$ and $-0.031$), while the unrestricted estimator stays centred on $-0.6$. The design that makes imposition worthwhile is also the one in which the test is weakest: $\mathrm{se}(\hat\theta)$ is $0.106$ without common inflation against $0.011$ with it. Precisely when imposing homogeneity buys precision, the data say least about whether it holds.

**Conclusion.** Zero-degree homogeneity is not an assumption on preferences but a consequence of the budget constraint, and in a log-linear demand it reduces to one linear restriction, $\theta=0$. Test it as the coefficient on $\log p_J$ in the reparametrised regression (25) and report its confidence interval. Impose it knowing that the gain (23) and the risk (22) are governed by the same correlation between $\hat\beta_j$ and $\hat\theta$.

### References

Deaton, A. and J. Muellbauer (1980). *Economics and Consumer Behavior*. Cambridge: Cambridge University Press.

LaFrance, J. T. (1986). The structure of constant elasticity demand models. *American Journal of Agricultural Economics* 68(3), 543–552.

Laitinen, K. (1978). Why is demand homogeneity so often rejected? *Economics Letters* 1(3), 187–191.

Stone, R. (1954). Linear expenditure systems and demand analysis: an application to the pattern of British demand. *Economic Journal* 64(255), 511–527.

### Appendix: power of the test

![Figure A.1](figures/fig3_power.png)

*Figure A.1: Power: rejection rate of the 5% $F$ test as a function of $\theta$ (1{,}000 samples per sample size; common random numbers across $\theta$).*
