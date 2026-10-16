
## 1. Research question

### What do children cost when parental childcare time is partly productive and partly experienced as leisure?

Children cost their parents money and time, and neither cost is observed. Children's consumption is pooled with parents' consumption in household expenditure data. Childcare hours are observed but cannot all be counted as a cost, because parents may draw leisure-like utility from them (*process benefits*).

The paper asks how both components, and their sum (the full cost of children), can be recovered from observed behavior without assuming that an hour of childcare is worth the parent's wage.

## 2. Contribution

### An endogenous price of parental time, carried through to the full cost of children

- **Time cost of children.** Existing measures value parental time at the market wage or at the price of equivalent services (Gustafsson & Kjulin, 1994; Apps & Rees, 2001; Colombino, 2000; Bradbury, 2008). Here the price of parental childcare time is a shadow price recovered from behavior; wage valuation is the special case $\theta_I = 1$.
- **Cost-of-children methods.** The Rothbarth logic is applied to time rather than goods. The adult good is the parent's *full leisure*, and resources are measured by *full expenditure*. Unlike Bargain & Donni (2012) and Bargain, Donni & Hentati (2022), no data on singles are needed. Unlike Dunbar, Lewbel & Pendakur (2013), no strong parametric restriction is placed on preferences across individuals.
- **Process benefits.** Graham & Green (1984), Kerkhofs & Kooreman (2003) and Gørtz (2011) study process benefits in home production. Cosaert & Hennebel (2023) show that childcare process benefits are only set-identified by revealed preference. This paper obtains point identification from the substitution between parental and purchased care (Reid's 1934 third-party criterion) and maps it into a cost of children.

## 3. Non-technical overview

### Valuing parental time to measure the full cost of children

Pricing a childcare hour at the wage presumes two things: the parent can freely trade that hour against paid work, and the hour is pure work, neither more nor less pleasant than a job.

The paper drops the second presumption. Following Reid's criterion, the productive part of childcare is the part a nanny or a daycare could supply instead. Couples’ choices reveal what that part is worth. When wages change, couples shift between their own care hours and purchased care, and the rate at which one parental hour replaces paid care is its productive value. Whatever remains of the wage is the value of the hour as leisure.

The paper then infers how much of the family’s spending goes to children by comparing parents’ leisure—including the part of childcare they experience as leisure—with that of comparable childless couples. This comparison assumes that parenthood changes neither adults’ preferences for leisure nor how they share the resources left for themselves. Differences in leisure can then reveal resources devoted to children, rather than changes in tastes or redistribution between spouses. After accounting for the time cost of childcare, the model recovers the monetary cost of children.

**Example.** A mother in the sample earns $29.48 an hour on average (Table 2). The benchmark estimate $\hat{\theta}_W = 0.44$ prices the productive part of her childcare hour at about **$13**. The remaining **$16.50** is leisure-like value (Table 4, col. A). Across couples with one child, the median weekly time cost is **$230**. Pricing every hour at the wage would give **$350**, about 50% more (Table 6, rows A and A\*).

## Abstract

A mettre tel quel

## 4. Economic model

### A collective model in which the wage splits into two shadow prices

**Couple.** Each spouse $I \in \{W, H\}$ has preferences $U_I(l_I, t_I, c_I, c_K, n)$ over:
- own leisure $l_I$,
- childcare time $t_I$,
- own consumption $c_I$,
- children's consumption $c_K$.

Decisions are Pareto-efficient.

**Childcare technology.** The function $g(t_W, t_H, n)$ is the minimum purchased care needed, given parental hours, to meet the childcare requirement, so that $m \ge g(t_W, t_H, n)$ and $m \ge 0$. It does not describe the production of child quality. It describes only how far parental time substitutes for market care.

**Decentralization (Proposition 1).** Each childcare hour is split into a productive component and a process-benefit component. Their prices sum to the wage, like Lindahl prices:

$$
w_I \;=\; \underbrace{\theta_I\, w_I}_{\text{productive: substitutes for paid care}} \;+\; \underbrace{(1-\theta_I)\, w_I}_{\text{process benefit}} .
$$

The couple first minimizes the cost of meeting the childcare requirement:

$$
e_T \;=\; \min_{t_W,\,t_H}\; \theta_W w_W\, t_W + \theta_H w_H\, t_H + g(t_W, t_H, n),
$$

and then shares the remaining resources between the spouses and the children.

**Cost of children.** The *time cost* $e_T$ is shadow-valued parental time plus purchased care. The *monetary cost* is $e_M = c_K$. The *full cost* is $e_T + e_M$.

With $\varphi_I$ linear (Assumption A.1), full leisure is $L_I = l_I + (1-\theta_I)\,t_I$. The leisure-like share of childcare is counted as leisure, and only $\theta_I t_I$ is counted as work.

## 5. Identification

### Prices from substitution with paid care and children's consumption from full-leisure Engel curves

**Step 1: the price of parental time (Propositions 2–3, Corollary 4).**
- *Interior regime ($m^* > 0$).* The wage responses of $(m^*, t_W^*, t_H^*)$ identify the gradient of $g$. At the optimum, the shadow price equals the marginal rate of technical substitution between a parental hour and purchased care, $\theta_I w_I = -\partial g/\partial t_I$.
- *Corner regime ($m^* = 0$).* Only the ratio $\theta_W w_W / \theta_H w_H$ is identified.
- *Resolving the corner.* Under A.1, $\theta_I$ depends only on own childcare time, so it extends to the corner by continuity and the corner multiplier is identified.
- *Testable implication.* Childcare choices do not depend on non-labor income.

**Step 2: the monetary cost (Proposition 6).** 
Two assumptions make the full-leisure demand $L_I = F_I(w_H, w_W, Y_R)$ the same for parents and non-parents:
- A.2: children leave each spouse's trade-off between full leisure and own consumption unchanged.
- A.3: children affect the sharing rule only through remaining full income $Y_R$.

$F_I$ is therefore estimated on childless couples and inverted for parents:

$$
e_M \;=\; Y - e_T - F_I^{-1}\!\left(w_H, w_W, L_I\right).
$$

This requires only local invertibility of one spouse's Engel curve. It is robust to unmodelled interactions such as altruism, public goods or home production, provided children affect behavior only through $Y_R$. Under the weaker Similarity Across Types condition (A.2′), identification rests instead on the curvature of $\partial F_I / \partial Y_R$.

## 6. Econometric model & methods

### Two recursive stages that mirror the two identification steps

**Stage 1: couples with children, maximum likelihood.**
- The time cost is Generalized Leontief in the shadow prices $w_I^* = \theta_I w_I$:

$$
e_T = \alpha^* + \textstyle\sum_I \beta_I^* w_I^* + 2\sum_I \gamma_I \sqrt{w_I^*} + 2\gamma_{WH}\sqrt{w_W^* w_H^*}.
$$

- Shephard's lemma gives two linear childcare-time equations and a Tobit equation for purchased care.
- The errors are jointly normal, and $\theta_W, \theta_H$ are estimated as free parameters.

**Stage 2: all couples, three-stage least squares.**
- Full leisure is built as $L_I = l_I + (1-\hat{\theta}_I)t_I$.
- Each spouse's full leisure is quadratic in remaining full expenditure $Y_R = Y - e_M - e_T$, with or without demographic shifts (SAT).
- The monetary cost is specified as $e_M = \log\!\big(1+\exp(\kappa_0 + \kappa_H w_H + \kappa_W w_W + \kappa_Y Y)\big)\sqrt{n}$.
- Instruments are the stage-1 predicted time cost and, for full expenditure, non-labor income and its square.

## 7. Data & sources

### PSID 2019: hours of care and dollars of paid care for the same couples

- **Survey.** Panel Study of Income Dynamics, 2019 wave (9,569 households). It is one of the few surveys that records parental childcare time, childcare expenditure and wages for the same households.
- **Sample.** Dual-earner, non-student, heterosexual couples aged 22–50. Couples with children have at most two children and a youngest child aged six or under. The sample has **825 couples: 463 with children and 362 without** (Table 2).
- **Childcare time.** The PSID question includes passive supervision. Reported hours are rescaled to match ATUS primary-childcare averages, with a transformation that preserves zeros:

$$
t_I^{adj} = t_I^{psid} - k_I \tanh\!\left(t_I^{psid}/k_I\right).
$$

Mean childcare falls from 35.1 to 9.7 h/week for fathers and from 53.7 to 19.0 h/week for mothers (Table 1).
- **Leisure.** Leisure is non-market time, $l_I = 168 - h_I - t_I$.
- **Paid care.** Purchased childcare is 2018 annual spending divided by 52: $100/week on average, with 35% zeros (Table 1).
- **Robustness.** The analysis is replicated on the 2017 wave (Tables A.1–A.3).

## 8. Results & limits

### Time valuation and cost estimates

| PSID 2019 | Benchmark | Range across specifications |
|---|---|---|
| Labor-like share of mothers' childcare, $\hat{\theta}_W$ (Table 4) | 0.44 (0.26) | 0.30–0.44 |
| Labor-like share of fathers' childcare, $\hat{\theta}_H$ (Table 4) | 0.35 (0.21) | 0.20–0.35 |
| Time cost, one child, $/week: median / mean (Table 6) | 230 / 430 | 170–570 / 290–810 |
| … same, with time priced at the wage (Table 6, A\*) | 350 / 930 | — |
| Monetary cost, one child, $/week: median / mean (Table 6) | 480 / 770 | 430–890 / 770–1,210 |

Standard errors are reported in parentheses.

**Main findings.**
- Between 56% and 70% of mothers' childcare time, and between 65% and 80% of fathers', is leisure-like (Table 4).
- The 2017 wave also yields $\hat{\theta}_I < 1$, with $\hat{\theta}_W = 0.66$ and $\hat{\theta}_H = 0.18$ (Table A.3).
- Parental time inputs are complements ($\hat{\gamma}_{WH} = -3.26$).
- Full leisure rises by 3.2 hours (men) and 4.4 hours (women) per extra $1,000 of weekly full expenditure (Table 5).
- The model's exclusion restrictions are not rejected: non-labor income can be excluded from the full-leisure equations (p = 0.98), and the monetary-cost parameters are equal across spouses (p = 0.98).
- Parental childcare is valued at about 30–50% of the wage.

**Limits acknowledged by the authors.**
- Results apply to dual-earner couples with young children and capture short-run costs only; career costs are excluded.
- The PSID childcare measure includes passive care. The ATUS rescaling reshapes its distribution, and the time cost is sensitive to it (row E).
- Rationing of formal care or unobserved fixed costs may bias $\hat{\theta}_I$ downward.
- Full leisure responds weakly to resources, so the monetary cost is imprecise. At median wages it is $566 (s.e. 286), against $344 (s.e. 120) for the time cost.
- The Generalized Leontief cost function is not globally consistent, and the zero-paid-care regime is not modelled in estimation.