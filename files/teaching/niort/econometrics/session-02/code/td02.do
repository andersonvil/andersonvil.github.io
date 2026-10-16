* =============================================================
* Économétrie L3, séance 2. Série d'exercices, code Stata du corrigé
* Chaque bloc est autonome. On le copie seul dans l'éditeur de do-files
* puis on l'exécute. Les fichiers .dta du cours doivent se trouver dans
* le dossier de travail (File > Change Working Directory).
* Les simulations fixent la graine (2026). Stata, Python et R utilisent
* des générateurs différents. Les dernières décimales diffèrent.
* NON TESTÉ : Stata n'était pas disponible lors de la préparation.
* =============================================================

* ---------- [ex1] Exercice 1, une loi jointe à la main ----------
clear
input x u p
0 -2 0.10
0  0 0.15
0  2 0.05
2 -2 0.10
2  0 0.20
2  2 0.10
4 -2 0.05
4  0 0.15
4  2 0.10
end
tabstat p, by(x) statistics(sum)          // loi de x
tabstat p, by(u) statistics(sum)          // loi de u
generate px = p*x
generate px2 = p*x^2
generate pu = p*u
generate pxu = p*x*u
tabstat p px px2 pu pxu, statistics(sum)  // 1, E(x), E(x^2), E(u), E(xu)
egen sp = total(p), by(x)                 // P(x) dans chaque ligne
generate q = p/sp                         // P(u | x)
generate qu = q*u
generate qu2 = q*u^2
tabstat qu qu2, by(x) statistics(sum)     // E(u | x) et E(u^2 | x)
display "E(y | x) = 8 + x + E(u | x) : " 8 + 0 - 1/3 " " 8 + 2 " " 8 + 4 + 1/3
display "pente de E(y | x) : " (12 + 1/3 - (8 - 1/3))/4 " | 1 + Cov/Var : " 1 + 0.4/2.4
egen pu_marg = total(p), by(u)            // P(u)
generate pind = sp*pu_marg                // loi jointe si x et u sont indépendants
generate qind = pind/sp*u
tabstat pind qind, by(x) statistics(sum)  // E(u | x) = 0 pour tout x

* ---------- [ex1-simul] Exercice 1, la pente des MCO sur des échantillons tirés ----------
clear
set seed 2026
set obs 100
generate x = .
generate u = .
generate y = .
postfile sim b1 using ex1simul, replace
forvalues r = 1/5000 {
    quietly replace x = cond(runiform() < 0.3, 0, cond(runiform() < 4/7, 2, 4))   // loi de x
    quietly replace u = runiform()            // puis u selon sa loi conditionnelle à x
    quietly replace u = cond(x == 0, cond(u < 1/3, -2, cond(u < 5/6, 0, 2)), ///
        cond(x == 2, cond(u < 1/4, -2, cond(u < 3/4, 0, 2)), cond(u < 1/6, -2, cond(u < 2/3, 0, 2))))
    quietly replace y = 8 + 1*x + u           // vraie pente 1
    quietly regress y x
    post sim (_b[x])
}
postclose sim
use ex1simul, clear
summarize b1                              // moyenne proche de 7/6 = 1,1667

* ---------- [ex2] Exercice 2, la pente des extrêmes ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
display "pente des extrêmes : " (13 - 7)/(10 - 2) " | pente des MCO : " 32/40
display "Var / sigma^2 : extrêmes " 2/(10 - 2)^2 " | MCO " 1/40 " | SCT_x 40 >= " (10 - 2)^2/2
set seed 2026
postfile sim ext mco using ex2simul, replace
forvalues r = 1/10000 {
    quietly generate e = rnormal(0, 1)
    quietly generate yy = 6 + x + e           // vraie droite 6 + x, sigma = 1
    quietly regress yy x
    post sim ((yy[5] - yy[1])/8) (_b[x])
    drop e yy
}
postclose sim
use ex2simul, clear
summarize ext mco                         // variances : carrés des écarts-types, 1/32 et 1/40

* ---------- [ex3] Exercice 3, table de régression, NBASAL ----------
use nbasal.dta, clear                     // 269 joueurs de NBA
regress wage points
display "sigma chapeau : " e(rmse)
predict res, residuals
regress points exper
regress wage exper
xtile quart = points, nquantiles(4)
tabstat res, by(quart) statistics(sd) format(%9.0f)   // écart-type des résidus par quart
quietly summarize points
display "SCT_points : " r(Var)*(r(N) - 1) " | s_points : " r(sd)
display "prévisions pour 10 et 20 points : " 287.101 + 111.330*10 " " 287.101 + 111.330*20

* ---------- [ex5-depart] Exercice 5, charger les données HTV ----------
use htv.dta, clear                        // 1 230 hommes de 26 à 34 ans, en 1991
summarize wage educ abil

* ---------- [ex5-mco] Exercice 5, MCO, sigma chapeau et erreur-type ----------
use htv.dta, clear
regress wage educ
local s = sqrt(e(rss)/(e(N) - 2))         // sigma chapeau
quietly summarize educ
local sct = r(Var)*(r(N) - 1)
display "SCR : " e(rss) " | sigma chapeau : " `s' " | SCT_educ : " `sct'
display "se(b1) = sigma chapeau / racine(SCT) : " `s'/sqrt(`sct')

* ---------- [ex5-residus] Exercice 5, résidus et dispersion ----------
use htv.dta, clear
quietly regress wage educ
predict res, residuals
generate groupe = 1 + (educ >= 13) + (educ >= 15)   // 12 ou moins, 13 ou 14, 15 ou plus
tabstat res, by(groupe) statistics(sd n) format(%9.2f)
twoway scatter res educ, mcolor("31 78 121%35") msize(vsmall) yline(0, lcolor("192 80 43")) ///
    xtitle("Années d'études (educ)") ytitle("Résidu")
graph export "fig_td_htv_stata.pdf", replace

* ---------- [ex5-simul] Exercice 5, la population des 1 230 hommes ----------
use htv.dta, clear
set seed 2026
postfile sim b1 se using ex5simul, replace
forvalues r = 1/2000 {
    preserve
    quietly bsample 100                   // 100 hommes tirés avec remise
    quietly regress wage educ
    post sim (_b[educ]) (_se[educ])
    restore
}
postclose sim
use ex5simul, clear
summarize b1 se                           // écart-type des pentes et moyenne des erreurs-types

* ---------- [ex5-abil] Exercice 5, l'aptitude, un facteur de u observé ici ----------
use htv.dta, clear
correlate educ abil wage
generate etudes15 = (educ >= 15)
tabstat abil, by(etudes15) statistics(mean) format(%9.2f)   // aptitude moyenne selon educ
