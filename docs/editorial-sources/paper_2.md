## Research question

### How can children enter labor supply models as an economic mechanism rather than as a demographic control?

In empirical labor supply equations, the number of children usually enters as one more regressor next to age or education. Demand analysis has long done better. Equivalence scales say how household composition transforms behavior (Browning, 1992; Lewbel, 1999). The paper asks what the labor supply counterpart should be. It looks for a structure in which children act through what they cost the parent, in money and in time, and which can be estimated on a standard labor force survey without observing childcare time or spending on children. The cost of children should then come out of the estimation rather than be assumed.

*Illustration caption.* **Two channels into labor supply.** Children cost the mother money and time. Both costs lower the resources she keeps, which raises her hours if leisure is normal; the hours of care displace market work, which lowers them. Her preferences are those of a childless woman. Explanatory diagram, not an empirical result.

---

## Contribution

### An equivalence scale for labor supply

The paper joins three literatures.

- **Equivalence scales.** It borrows the idea that children transform a reference behavior through a few interpretable parameters. In its tractable form, children translate the childless labor supply function twice, in resources and in available time, as Independent-of-the-Base scales translate demand functions (Lewbel, 1989; Blundell & Lewbel, 1991; Blackorby & Donaldson, 1993). Hurd & Pencavel (1981), with Barten scales, is one of the few earlier attempts in labor supply.
- **Collective models with children.** Parent and child have distinct preferences and share resources, as in Bargain & Donni (2012), Dunbar, Lewbel & Pendakur (2013), Lechene, Pendakur & Wolff (2022) and Penglase (2021). Their proportionality assumption on children's expenditure is generalized to linearity in non-labor income.
- **Childcare and mothers' labor supply.** Structural studies treat childcare as a claim on mothers' time (Blau & Robins, 1988; Ribar, 1995; Kornstad & Thoresen, 2007) but require childcare-time data and leave out the income effect of children. The paper adds that income effect and needs neither childcare-time data nor child-specific spending.

The novelty is to estimate the total, composite cost of children from labor supply alone. This economy of data is bought with structure, that is fewer variables are observed, so more is assumed.

---

## Non-technical overview

### Children act as two fixed costs and a time requirement; how labor supply responds reveals their size

Picture a single mother as a childless woman with the same tastes and two extra burdens: hours of care she cannot cut, and spending on the child. The hours cost her the wage. Together with the spending, they lower the resources she keeps for herself, which pushes her to work more if leisure is normal. The hours of care also leave fewer hours to sell, which pushes her to work less. The effect of children on hours therefore has no sign a priori, and a coefficient on the number of children cannot tell the two channels apart.

What does tell them apart is how her responses differ from a childless woman's. Care hours are valued at the wage, so a raise also raises the cost of care and makes her poorer, and she increases her hours by more. Part of each extra dollar of non-labor income goes to the child, so it reduces her hours by less. These slope differences, more than level differences, carry the cost of children.

In the 2019 PSID, at the median, a $1 raise adds 27 hours of work a year for a childless working woman and 40 hours for a working mother; $1,000 of extra resources removes 28 and 25 hours respectively (Table 4).

---

## Abstract

A mettre tel quel

---

## Economic model

### Two fixed costs and a time shift on the labor supply of a childless woman

A single mother chooses market hours $h$, childcare time $t$, purchased care $k$, her consumption $c$ and her children's consumption $m$. Her preferences are $u_f(h+t,\,c) + \lambda(n)\,u_c(m)$: she is altruistic toward a representative child, and market work and childcare enter her utility as perfect substitutes. This last assumption is the keystone of the model. Every hour of care is priced at the wage, and the problem becomes recursive. She first meets the childcare requirement $f(t,n) + k \ge T_c(n)$ at minimum cost, which defines the time cost $e_t(w,n) = \min_{t,k}\,(tw + k)$. She then sets spending on the children, $e_m(y,w,n)$. She finally supplies labor as a childless woman would, with fewer resources.

**Equation block**

$$
h(y,w,n) \;=\; F\big(w,\; y - e_t(w,n) - e_m(y,w,n)\big) \;-\; t(w,n), \qquad h(y,w,0) \;=\; F(w,y)
$$

**Card 1. Income effect of two fixed costs.** The time cost $e_t$ and the money cost $e_m$ reduce the resources the mother keeps. With normal leisure ($F'_2 < 0$), this raises market hours, as fixed costs of work do in Cogan (1981).

**Card 2. Time shift.** At given resources, each hour of care displaces one hour of market work, which lowers market hours. The two effects have opposite signs.

---

## Identification

### Preference stability and a money cost linear in income: the curvature of labor supply reveals the total cost of children

Identification rests on two assumptions.
- **Preference stability (Rothbarth–Gronau type).** Mothers and childless women share the same trade-off between work and consumption, hence the same function $F$. The paper uses this throughout but does not state it as a formal assumption. It is what makes childless women a valid reference.
- **Assumption L.** Spending on children is linear in non-labor income: $e_m = a_m(w,n) + b_m(w,n)\,y$, with $0 < b_m < 1$.

$F$ is estimated on childless women. For a mother who works, $\partial h / \partial y = F'_2 \cdot (1 - b_m)$.
- If $F$ is nonlinear in $y$, the first and second income derivatives identify $b_m$. Inverting $F'_2$ then yields the total cost $e = e_t + e_m$ and childcare time $t$ (Proposition).
- If $F$ is linear in $y$, only $b_m$ is identified. The total cost is known only up to an arbitrary function of $(w,n)$.

The total cost is identified, and so are its two economically meaningful parts: the value of the mother's time, $w\,t$, and the value of goods and services, $e - w\,t$. The primitive pair $(e_t, e_m)$ is not identified. Among non-working women $e_m$ is not identified and the price of time is a reservation wage, so their costs rely on a parametric extension.

**Tractable model (box).** Assume that childcare time does not respond to the wage ($t = b_t$, so $e_t = a_t + b_t w$), and that spending on children does not depend on the wage ($e_m = a_m + b_m y$). Children then reduce to three constants:

$$
h(y,w,n) \;=\; F\big(w,\; y - a(n) - b_t(n)\,w - b_m(n)\,y\big) \;-\; b_t(n), \qquad a = a_t + a_m .
$$

Even with a linear $F$, they are read off three shifts relative to childless women: the intercept, the wage slope $\alpha_1 - \alpha_2 b_t$ and the income slope $\alpha_2(1-b_m)$. This does not contradict the general result. The arbitrary function that defeats identification would make either childcare time or spending on children depend on the wage, and the tractable model rules out both. The model also yields a testable restriction: if $F'_1 > 0$ and $F'_2 < 0$, children make hours more responsive to wages and less responsive to non-labor income.

---

## Econometric model & methods

### A Brown–Levin–Ulph labor supply with participation, translated by children

Annual hours are quadratic in the wage and in net total expenditures (Brown, Levin & Ulph, 1976), a form that nests Hausman's (1980) linear specification. Child costs scale with $\delta(n) = \sqrt{n}$. Net total expenditures (household expenditures minus labor income) stand in for non-labor income. They are corrected for endogeneity with a control function built on asset, welfare, transfer and other incomes. A wage equation, with education as the excluded variable, handles endogenous wages and the unobserved wages of non-participants. The likelihood is of the Tobit type: the density of hours for workers, and for non-workers the probability of not participating, integrated over the latent wage.

Working mothers' time is priced at the wage, non-working mothers' at the reservation wage solving $h = 0$. That equation has two roots; the reservation wage is the one consistent with Slutsky positivity. For some draws no real root exists, so the price of time and the costs are reported as quartiles over 200 Halton draws per woman.

**Equation block**

$$
\begin{aligned}
&(16)\quad h = \alpha_0 z^h + \alpha_1 w + \alpha_2 y + \alpha_3 w^2 + \alpha_4 y^2 + \alpha_5 y w + u\\
&(17)\quad e_t(w,n) = (a_t + b_t w)\,\delta(n)\\
&(18)\quad e_m(y,n) = (a_m + b_m y)\,\delta(n)\\
&(19)\quad h = \alpha_0 z^h + \alpha_1 w + \alpha_2\big[y - a\,\delta(n) - b_t\,\delta(n)\,w - b_m\,\delta(n)\,y\big] - b_t\,\delta(n) + u \quad \text{(linear baseline)}\\
&(21)\quad \alpha_1^*(n) = \alpha_1 - \alpha_2 b_t\,\delta(n), \qquad \alpha_2^*(n) = \alpha_2\big(1 - b_m\,\delta(n)\big)\\
&(22)\quad w = \beta_w z^w + \sigma_\nu \nu \\
&(23)\quad y = \beta_y z^y + \sigma_\mu \mu
\end{aligned}
$$

**Symbols, units and indices.** $h$: annual market hours (thousands). $w$: hourly wage ($). $y$: net total expenditures ($ thousands). $n$: number of children; $\delta(n) = \sqrt{n}$, $\delta(1) = 1$. $a$: fixed cost, time and money combined. $b_t$: childcare time per child (thousands of hours). $b_m$: share of non-labor income spent on the child. $z^h$: age, age², Black, Hispanic. $z^w$: $z^h$ and years of education. $z^y$: $z^w$, asset, welfare, transfer and other incomes and their squares.

---

## Data & sources

### PSID 2019: labor supply, wages and expenditures for single women

The 2019 wave is the first in which the PSID records detailed household expenditures, which makes net total expenditures computable. The sample covers single women aged 20–60. Mothers have at most two children, the youngest aged six or under, the age range in which childcare matters most. The sample has 1,270 women: 987 childless women and 283 mothers. Single women fit the model's single decision-maker, and despite their exposure to poverty they are rarely the subject of structural labor supply analysis.

Table 1 shows why a control variable will not do. Mothers participate more than childless women (0.90 against 0.83) and earn less ($13.22 against $18.78), yet work slightly fewer hours (1,620 against 1,639). Net total expenditures are positive for mothers ($9,470) and negative for childless women (−$3,010): mothers' earnings fall short of household spending. Among women in couples, by contrast, participation falls with children.

*[Table 1 and the Provider / Access / Exact data used blocks as on the current page.]*

---

## Results & limits

### Children steepen the wage response and flatten the income response; one child costs a working mother about $21,000 a year

The estimates satisfy the testable restriction. At the median, a $1 raise adds 27 hours of work a year for a childless working woman and 40 hours for a working mother. $1,000 of extra resources removes 28 and 25 hours respectively (Table 4).

The child-cost equation gives $\hat b_t = 0.497$ (s.e. 0.178), about 500 hours of care per child per year. The 2019 ATUS reports 389 hours for comparable single mothers. The income share $\hat b_m = 0.089$ (s.e. 0.084) is imprecise (Table 3).

The median annual cost of one child is $21,000 for working mothers and $31,000 for non-working mothers (Table 5). The gap comes from the price of time: for non-working mothers the median reservation wage is $36.5, about twice their market wage of $18.4 (Table 4). At the median across all mothers, the mother's time is worth about $12,200 a year and goods and services about $14,200. The second figure is close to the USDA estimate for a single parent (Lino et al., 2017).

These costs answer Browning's (1992) positive question, how children change behavior. They are costs as perceived by parents, not amounts that ought to be compensated.

**Limits documented by the manuscript (callout)**
- **Identification in practice.** The general result needs precise second income derivatives. Income effects are only mildly nonlinear in the data, so the estimates rest on the tractable parametric model.
- **Keystone assumptions.** Work and childcare are perfect substitutes in preferences, so care is priced at the wage (the companion paper relaxes this). Childcare time does not respond to the wage. Preferences are stable across parental status.
- **Non-participants.** Their costs rely on parametric extrapolation and on a reservation wage whose normative meaning is open.
- **Scope.** The model is static and the sample covers single women with young children. Couples are only sketched, and identification fails if the husband's hours are constrained. Childcare time is sensitive to the sample: 190 hours in the trimmed sample.