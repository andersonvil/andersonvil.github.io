## 1. Research question

### Do child-cost estimates built on couples carry over to households with a single adult?

Collective-model estimates of what children cost are almost all obtained on two-adult households. They are nonetheless used, explicitly or implicitly, as benchmarks for single-parent families when living standards are compared or transfers are designed. A one-adult household differs on the margins that drive those estimates: there is no second decision-maker, no second earner, and fewer people to share public goods.

The paper asks whether couple-based estimates of the child's resource share remain valid when there is a single adult, and how that share varies with family size and household resources. The second question bears directly on transfer rules that condition support on family size, such as the UK two-child limit.

## 2. Contribution

### Resource shares in one-adult households 

- **Model.** Extends collective consumption models with assignable goods to one-adult households, with separate sharing rules and shadow prices for mothers and fathers.
- **Evidence.** Provides the first estimates of children's resource shares in single-parent households of a high-income country. Existing evidence concerns Côte d'Ivoire (Bargain, Donni & Kwenda, 2014), Argentina (Echeverria et al., 2019) and Jamaica (Bose-Duker et al., 2021).
- **Test of external validity.** The model, data and assignable good are those of Bargain, Donni & Hentati (2022) for couples, so the two sets of estimates are directly comparable.
- **Policy.** The literature on the two-child limit documents its effect on income and poverty (González & Trommlerová, 2023; Chzhen & Bradshaw, 2025; Stewart et al., 2025). This paper asks whether intra-household allocation itself justifies conditioning support on family size.

## 3. Non-technical overview

### A parent's own wardrobe reveals how much of the budget goes to the child

Surveys record spending by household, not by person, so what a single parent spends on a child is never observed. Adult clothing is the exception as it is recorded for the person who wears it.

If having a child changes a parent's budget but not his or her taste for clothing, childless single adults with the same characteristics show how clothing spending varies with the resources one has for oneself. A single parent's clothing then reveals the resources the parent keeps. The rest is the child's share.

**Example.** Childless single women spend £9.46 a week on their own clothing; single mothers with one child spend £7.53, although their total budget is larger, £136 against £117 (Table 2, 1987 pounds). The model attributes 14.7% of the mother's resources to the child (Table 4), about £20 a week at that budget.

## Abstract

A mettre tel quel

## 4. Economic model

### One altruistic decision-maker who shares resources with the child and gains from joint consumption

A single parent cares about his or her own consumption and about the child's. As sole decision-maker, the parent's choices are efficient by construction, and children have no bargaining power. The household differs from a childless adult in two ways: a **sharing rule** $\varphi$, the share of total expenditure $y$ the parent keeps, and a **Barten price** $\pi$ on non-clothing consumption, which captures the gains from sharing goods such as housing and heating.

The parent therefore behaves like a childless adult with budget $\varphi y$ and prices adjusted for joint consumption:

$$
\frac{\omega}{\varphi} \;=\; g\!\left(\frac{p}{\pi},\; \frac{\varphi\, y}{\pi}\right),
$$

where $\omega$ is the clothing budget share, $p$ the price of clothing and $g$ the clothing demand of a childless adult. The child's share $1-\varphi$ measures the cost of children.

## 5. Identification

### Information that recovers latent allocation

Three ingredients recover $\varphi$ and $\pi$ from household-level data:
- clothing preferences are the same for childless adults and parents with the same characteristics (preference stability);
- adult clothing expenditure is separately recorded, providing the single assignable good needed;
- Engel curves are nonlinear in expenditure.

One difficulty remains because $\varphi$ and $\pi$ enter the same demand equation. Proposition 1 gives sufficient conditions to distinguish them. The benchmark uses decade indicators to shift adult clothing demand through fashion, norms, and relative prices, while excluding these indicators from the sharing rule and the scale-economies equation. These indicators are relevant in the data, with joint tests yielding $p=0.034$ for women and $p=0.011$ for men (Table 1). Alternative specifications impose independence from expenditure, independence from prices, or a multiplicative effect of family size.

## 6. Econometric model & methods

### Quadratic Engel curves, a logistic sharing rule, and nonlinear iterated 2SLS

The clothing budget share is quadratic in log expenditure, estimated separately for men and women and jointly on childless adults and single parents:

**Single adult demand (equation 14).**

$$
\omega = \alpha z^{\omega} + \beta \ln p + \gamma \ln y + \eta (\ln y)^2 + \epsilon .
$$

For parents, $\omega$, $p$ and $y$ are replaced by $\omega/\varphi$, $p/\pi$ and $\varphi y/\pi$. The sharing rule is logistic in parental characteristics, children's characteristics (number, age, sex composition), log prices and log expenditure. The Barten price depends on expenditure and $\sqrt{n}$.

Estimation is by nonlinear iterated two-stage least squares, with total income instrumenting total expenditure. Resource shares are evaluated at sample means (delta method) and as averages of individual predictions (Monte Carlo).

## 7. Data & sources

### Four decades of UK expenditure surveys, with clothing recorded by wearer

- **Surveys.** Harmonized Family Expenditure Survey, Expenditure and Food Survey and Living Costs and Food Survey, 1978–2022.
- **Sample.** Single adults aged 18–55, childless or living with biological children aged 0–15: 24,748 women (10,924 childless, 13,824 mothers) and 15,634 men (14,059 childless, 1,575 fathers) (Tables 3–4).
- **Goods and prices.** Men's and women's clothing as assignable goods; all other non-durables as a composite; national annual price indices, with relative clothing prices falling sharply until about 2009 (Figure 8).
- **Descriptive pattern.** Parents' own clothing falls with each child, from £7.53 to £6.29 and £5.15 for mothers and from £4.34 to £3.56 and £2.02 for fathers, while total expenditure does not (Table 2).

## 8. Results & limits

### Single mothers match the couple benchmark; per-child shares do not dilute once uncertainty is accounted for

| Children's resource share (Table 4, col. I) | One child | Two children | Three children |
|---|---|---|---|
| Single mothers | 0.147 (0.061) | 0.256 (0.076) | 0.357 (0.083) |
| Single fathers | 0.222 (0.085) | 0.383 (0.079) | 0.533 (0.097) |
| Couples (Bargain, Donni & Hentati, 2022) | 0.142 | | |

Standard errors are reported in parentheses.

**Main findings.**
- **External validity.** For single mothers, the child's share (14.7%) is close to the couple benchmark (14.2%).
- **Gender.** The share is higher for single fathers (22.2%), but the difference with mothers is not statistically significant, whether evaluated at the mean or averaged over households (Table 12).
- **Family size.** The total share for children rises with each child, less than proportionally. Across the expenditure distribution, per-child shares do not differ significantly by family size, except among low-expenditure single fathers (Figure 1).
- **Equivalence scales.** The OECD-modified scale implies a 23% share for one child, above the estimate for single mothers.
- **Robustness.** Results hold when identification relies on regional rather than temporal variation, without economies of scale, and with childless comparison groups matched to parents (Tables 5 and 13).

**Limits acknowledged by the author.**
- Preference stability may fail if single parents and childless singles differ in unobserved ways, for instance through fertility choices. Controls and matched samples mitigate this without eliminating it.
- Clothing purchases are infrequent, and the fathers' sample is small.
- The model is static and covers consumption only; labor supply, home production and time costs are left out.