* =============================================================
* Économétrie L3, séance 2. Code Stata des figures, tables et calculs
* Chaque bloc est autonome. On le copie seul dans l'éditeur de do-files
* (Do-file Editor) puis on l'exécute. La fenêtre Command n'accepte pas
* les commentaires // ni les continuations ///.
* Données. Les fichiers .dta du cours (dossier « donnees » sur Moodle)
* sont à placer dans un même dossier, désigné une fois par session comme
* dossier de travail (menu File > Change Working Directory, ou cd "chemin").
* Les simulations fixent la graine (2026). Stata, Python et R utilisent
* des générateurs différents. Les dernières décimales diffèrent.
* Stata 14 ou plus (runiformint, rnormal). Le bloc [mc-variance] ne trace pas
* les histogrammes, pour rester lisible sur une slide (tableau seulement).
* NON TESTÉ : Stata n'était pas disponible lors de la préparation.
* =============================================================

* ---------- [ech-quatre] Quatre échantillons, quatre droites ----------
use econmath.dta, clear                   // les 856 étudiants, vus comme la population
quietly regress score colgpa
local a = _b[_cons]
local b = _b[colgpa]
display "pente sur les 856 : " %6.2f `b'
set seed 2026                             // graine : mêmes tirages à chaque exécution
forvalues k = 1/4 {
    preserve
    bsample 50                            // 50 étudiants tirés avec remise
    quietly regress score colgpa
    display "échantillon `k' : pente = " %6.2f _b[colgpa]
    twoway (scatter score colgpa, mcolor("31 78 121") msize(small)) ///
        (function y = `a' + `b'*x, range(0.8 4.1) lcolor(gs9) lpattern(dash)) ///
        (lfit score colgpa, lcolor("192 80 43")), legend(off) ///
        title("pente = `: display %4.1f _b[colgpa]'") name(g`k', replace)
    restore
}
graph combine g1 g2 g3 g4, rows(1) ycommon
graph export "fig_echantillons_stata.pdf", replace

* ---------- [ech-distrib] Mille échantillons, la distribution des pentes ----------
use econmath.dta, clear
quietly regress score colgpa
local b = _b[colgpa]                      // pente calculée sur les 856
set seed 2026
tempname sim
postfile `sim' b1 using pentes_ech, replace
forvalues r = 1/1000 {
    preserve
    quietly bsample 50                    // un échantillon de 50, avec remise
    quietly regress score colgpa
    post `sim' (_b[colgpa])
    restore
}
postclose `sim'
use pentes_ech, clear
summarize b1                              // moyenne, écart-type, minimum, maximum
histogram b1, bin(30) frequency fcolor("31 78 121") lcolor(white) ///
    xline(`b', lcolor("192 80 43") lwidth(thick)) ///
    xtitle("Pente estimée sur un échantillon de 50 étudiants") ytitle("Nombre d'échantillons")
graph export "fig_distrib_echant_stata.pdf", replace

* ---------- [va] Un étudiant tiré au hasard ----------
use econmath.dta, clear                   // population de 856 étudiants
generate X = (work >= 15)                 // X = 1 si 15 heures ou plus de travail salarié
tabulate X                                // loi de X, effectifs et proportions
display "probabilité de chaque note : " %7.5f 1/856

* ---------- [esp-prop] Calculer avec l'espérance ----------
use econmath.dta, clear
quietly summarize score                   // chaque note a la probabilité 1/856
local mu = r(mean)                        // E(Y), moyenne de la population
display "E(Y) : " %6.2f `mu'
generate Y20 = 0.2*score                  // note sur 20
generate ecart = score - `mu'
generate Y2 = score^2
tabstat Y20 ecart Y2, statistics(mean) format(%9.2f)   // E(0,2 Y), E(Y - mu), E(Y^2)
display "0,2 E(Y) : " %6.2f 0.2*`mu' " | [E(Y)]^2 : " %7.1f `mu'^2

* ---------- [variance] Variance et écart-type ----------
use econmath.dta, clear
generate X = (work >= 15)
quietly summarize X
display "Var(X) = p(1 - p) : " %6.4f r(mean)*(1 - r(mean))
quietly summarize score
local v = r(Var)*(r(N) - 1)/r(N)          // division par N = 856
display "Var(Y) : " %7.2f `v' " | écart-type : " %6.2f sqrt(`v')
display "Var(0,2 Y) = 0,04 Var(Y) : " %6.3f 0.04*`v'

* ---------- [lgn] La moyenne empirique se rapproche de l'espérance ----------
use econmath.dta, clear
keep score
quietly summarize score
local EY = r(mean)
expand 3                                  // 2 568 lignes, les 856 premières sont les données
set seed 2026
generate i = runiformint(1, 856) in 1/2000    // 2 000 étudiants tirés au hasard, avec remise
generate tirage = score[i] in 1/2000
keep in 1/2000
generate n = _n
generate moy = sum(tirage) / n            // moyenne des n premiers tirages
list n moy if inlist(n, 10, 100, 2000), clean
display "E(Y) : " %6.2f `EY'
twoway line moy n, xscale(log) xlabel(1 10 100 1000) lcolor("31 78 121") ///
    yline(`EY', lcolor("192 80 43") lpattern(dash)) yscale(range(60 85)) ///
    xtitle("Nombre de tirages n (échelle logarithmique)") ytitle("Moyenne des n notes")
graph export "fig_lgn_stata.pdf", replace

* ---------- [cov-pop] Covariance et corrélation dans la population ----------
use econmath.dta, clear
generate X = (work >= 15)
generate XY = X*score
quietly summarize X
local EX = r(mean)
local sdX = sqrt(r(Var)*(r(N) - 1)/r(N))   // écart-type de la population (division par N)
quietly summarize score
local EY = r(mean)
local sdY = sqrt(r(Var)*(r(N) - 1)/r(N))
quietly summarize XY
display "Cov(X, Y) = E(XY) - E(X)E(Y) : " %7.3f r(mean) - `EX'*`EY'
display "Corr(X, Y) : " %6.3f (r(mean) - `EX'*`EY')/(`sdX'*`sdY')

* ---------- [independance] Covariance nulle sans indépendance ----------
clear
input x prob
-1 0.3333333333
 0 0.3333333333
 1 0.3333333333
end
generate y = x^2                          // Y = X^2, entièrement déterminée par X
generate px = prob*x
generate py = prob*y
generate pxy = prob*x*y
tabstat px py pxy, statistics(sum)        // E(X), E(Y), E(XY) : Cov = E(XY) - E(X)E(Y) = 0
display "P(X = 0 et Y = 1) = 0 | P(X = 0) P(Y = 1) = " %6.4f (1/3)*(2/3)

* ---------- [cond-cov] Covariance nulle, espérance conditionnelle non nulle ----------
clear
input x prob
-1 0.3333333333
 0 0.3333333333
 1 0.3333333333
end
generate u = x^2 - 2/3                    // u dépend de x
generate pu = prob*u
generate pxu = prob*x*u
tabstat pu pxu, statistics(sum)           // E(u) = 0 et E(xu) = Cov(x, u) = 0
list x u, clean                           // E(u | x) pour chaque valeur de x

* ---------- [var-moyenne] La variance d'une moyenne ----------
use econmath.dta, clear
quietly summarize score
display "écart-type théorique de la moyenne : " %6.3f sqrt(r(Var)*(r(N) - 1)/r(N))/sqrt(50)
set seed 2026
tempname sim
postfile `sim' moy using moyennes, replace
forvalues r = 1/5000 {
    preserve
    quietly bsample 50                    // 50 notes tirées avec remise
    quietly summarize score
    post `sim' (r(mean))
    restore
}
postclose `sim'
use moyennes, clear
summarize moy                             // moyenne et écart-type des 5 000 moyennes

* ---------- [esp-cond] Espérance conditionnelle ----------
use econmath.dta, clear
generate X = (work >= 15)
tabstat score, by(X) statistics(mean n)   // E(Y | X = 0) et E(Y | X = 1)
generate classe = floor(colgpa/0.25)*0.25 + 0.125    // classes de colgpa de largeur 0,25
egen moy = mean(score), by(classe)
egen nb = count(score), by(classe)
tabstat score if nb >= 10, by(classe) statistics(mean n)
twoway (scatter score colgpa, mcolor("31 78 121%15") msize(vsmall)) ///
    (scatter moy classe if nb >= 10, mcolor("192 80 43")), legend(off) ///
    xtitle("Moyenne universitaire (colgpa, sur 4)") ytitle("Note finale (score, %)")
graph export "fig_esp_cond_stata.pdf", replace

* ---------- [iterees] La loi des espérances itérées ----------
use econmath.dta, clear
generate X = (work >= 15)
quietly summarize X
local p = r(mean)
quietly summarize score if X == 0
local m0 = r(mean)
quietly summarize score if X == 1
local m1 = r(mean)
display "(1 - p) E(Y|X=0) + p E(Y|X=1) : " %6.2f (1 - `p')*`m0' + `p'*`m1'
display "moyenne simple des deux : " %6.2f (`m0' + `m1')/2
summarize score
display "Cov(X, Y) = p(1 - p)[E(Y|X=1) - E(Y|X=0)] : " %7.3f `p'*(1 - `p')*(`m1' - `m0')

* ---------- [var-cond] Variance conditionnelle ----------
use econmath.dta, clear
xtile quart = colgpa, nquantiles(4)       // quarts de colgpa
egen m = mean(score), by(quart)
generate d2 = (score - m)^2
egen v = mean(d2), by(quart)              // variance de la population, division par N
generate et = sqrt(v)
tabstat score et, by(quart) statistics(mean n)   // moyenne, écart-type et effectif par quart
_pctile colgpa, nquantiles(4)
display "bornes des quarts : " r(r1) " " r(r2) " " r(r3)

* ---------- [rls1] Moyennes par classe et droite des MCO ----------
use econmath.dta, clear
quietly regress score colgpa              // droite des MCO
generate classe = floor(colgpa/0.25)*0.25 + 0.125
egen moy = mean(score), by(classe)
egen nb = count(score), by(classe)
generate droite = _b[_cons] + _b[colgpa]*classe
tabstat moy droite if nb >= 10, by(classe) statistics(mean) format(%9.2f)
twoway (scatter score colgpa, mcolor("31 78 121%15") msize(vsmall)) ///
    (scatter moy classe if nb >= 10, mcolor("192 80 43")) ///
    (lfit score colgpa, lcolor(gs9) lpattern(dash)), legend(off) ///
    xtitle("Moyenne universitaire (colgpa, sur 4)") ytitle("Note finale (score, %)")
graph export "fig_rls1_stata.pdf", replace

* ---------- [rls3] Variation de x dans l'échantillon ----------
use econmath.dta, clear
summarize colgpa, detail                  // écart-type positif : RLS.3 est satisfaite
quietly tabulate colgpa
display "nombre de valeurs différentes : " r(r)

* ---------- [equilibre] Comparer les groupes sur ce qui est observé ----------
use econmath.dta, clear
generate w15 = (work >= 15)
tabstat colgpa mothcoll actmth, by(w15) statistics(mean) format(%9.2f)
use jtrain2.dta, clear
tabstat educ age re75, by(train) statistics(mean) format(%9.2f)   // re75 : revenu de 1975

* ---------- [rls5] Résidus et dispersion selon colgpa ----------
use econmath.dta, clear
quietly regress score colgpa
predict res, residuals                    // résidus des MCO
xtile quart = colgpa, nquantiles(4)
tabstat res, by(quart) statistics(sd) format(%9.2f)   // écart-type des résidus par quart
twoway scatter res colgpa, mcolor("31 78 121%35") msize(vsmall) ///
    yline(0, lcolor("192 80 43")) xtitle("Moyenne universitaire (colgpa, sur 4)") ytitle("Résidu")
graph export "fig_rls5_stata.pdf", replace

* ---------- [jouet-bruit] Exemple à la main, l'estimation est la vérité plus du bruit ----------
clear
input x y u2
 2  7  1
 4 10 -1
 6 11  2
 8 14  0
10 13  1
end
generate u = y - (6 + 1*x)                // erreurs, si la vraie droite est 6 + x
generate w = (x - 6)/40                   // poids w_i : moyenne de x 6, SCT_x 40
generate wu = w*u
generate wu2 = w*u2                       // u2 : d'autres erreurs
list x y u w, clean
tabstat wu wu2, statistics(sum)           // b1 = 1 + somme : 0,8 puis 1,05
generate t = 6 + x                        // vraie droite
twoway (scatter y x, mcolor("31 78 121")) (function y = 6 + x, range(0 12) lcolor(gs9) lpattern(dash)) ///
    (rspike y t x, lcolor(gs9)), legend(off) title("Erreurs u") name(A, replace)
generate f = 6.2 + 0.8*x                  // droite des MCO
twoway (scatter y x, mcolor("31 78 121")) (function y = 6.2 + 0.8*x, range(0 12) lcolor("192 80 43")) ///
    (rspike y f x, lcolor("192 80 43")), legend(off) title("Résidus") name(B, replace)
graph combine A B, rows(1)
graph export "fig_jouet_vraie_stata.pdf", replace

* ---------- [jouet-b0] Exemple à la main, la constante ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
generate u = y - (6 + 1*x)                // erreurs, si la vraie droite est 6 + x
generate wu = (x - 6)/40*u
quietly summarize wu
local b1 = 1 + r(sum)
quietly summarize u
display "moyenne des erreurs : " r(mean) " | b1 - 1 : " `b1' - 1
display "b0 = 6 + moyenne(u) - (b1 - 1) x moyen : " 6 + r(mean) - (`b1' - 1)*6

* ---------- [mc-sansbiais] Simulation, l'absence de biais ----------
use econmath.dta, clear
keep in 1/50                              // x fixés : 50 premiers étudiants
keep colgpa
quietly summarize colgpa
display "écart-type théorique 11 / racine(SCT_x) : " %6.3f 11/sqrt(r(Var)*(r(N) - 1))
set seed 2026
generate y = .
tempname sim
postfile `sim' b1 using mc1, replace
forvalues r = 1/5000 {
    quietly replace y = 32 + 14*colgpa + rnormal(0, 11)   // vraie pente 14
    quietly regress y colgpa
    post `sim' (_b[colgpa])
}
postclose `sim'
use mc1, clear
summarize b1                              // moyenne et écart-type des 5 000 pentes
histogram b1, bin(40) frequency fcolor("31 78 121") lcolor(white) ///
    xline(14, lcolor("192 80 43") lwidth(thick)) xtitle("Pente estimée") ytitle("Nombre d'échantillons")
graph export "fig_mc_sansbiais_stata.pdf", replace

* ---------- [mc-biais] Simulation, un biais quand RLS.4 échoue ----------
use econmath.dta, clear
keep in 1/50
keep colgpa
expand 10                                 // 500 observations, même dispersion de x
generate premiers = (_n <= 50)            // les 50 premières lignes : les x d'origine
set seed 2026
generate y = .
tempname sim
postfile `sim' b50 b500 using mc2, replace
forvalues r = 1/5000 {
    quietly replace y = 32 + 14*colgpa + 4*(colgpa - 2.8) + rnormal(0, 11)   // E(u | x) = 4 (x - 2,8)
    quietly regress y colgpa if premiers
    local a = _b[colgpa]
    quietly regress y colgpa
    post `sim' (`a') (_b[colgpa])
}
postclose `sim'
use mc2, clear
summarize b50 b500
twoway (histogram b50, bin(40) color(gs12)) (histogram b500, bin(40) color("31 78 121%70")), ///
    xline(14, lcolor("192 80 43") lwidth(thick)) legend(order(1 "n = 50" 2 "n = 500")) xtitle("Pente estimée")
graph export "fig_mc_biais_stata.pdf", replace

* ---------- [mc-variance] Simulation, ce qui rend la pente précise ----------
use econmath.dta, clear
keep in 1/50
quietly summarize colgpa
generate xr = r(mean) + 0.5*(colgpa - r(mean))   // x deux fois moins dispersé
expand 4                                  // 200 observations : chaque x répété 4 fois
generate premiers = (_n <= 50)            // les 50 premières lignes : les x d'origine
set seed 2026
generate y = .
postfile sim ref n200 resserre sigma55 using mc3, replace
forvalues r = 1/5000 {
    quietly replace y = 32 + 14*colgpa + rnormal(0, 11)
    quietly regress y colgpa if premiers  // référence, n = 50
    local a = _b[colgpa]
    quietly regress y colgpa              // n = 200
    local b = _b[colgpa]
    quietly replace y = 32 + 14*xr + rnormal(0, 11)
    quietly regress y xr if premiers      // x deux fois moins dispersé
    local c = _b[xr]
    quietly replace y = 32 + 14*colgpa + rnormal(0, 5.5)
    quietly regress y colgpa if premiers  // sigma = 5,5
    post sim (`a') (`b') (`c') (_b[colgpa])
}
postclose sim
use mc3, clear
tabstat ref n200 resserre sigma55, statistics(mean sd) format(%9.3f)   // théoriques 2,606 1,303 5,213 1,303

* ---------- [eventail] Pourquoi la dispersion de x aide ----------
clear
set obs 20
generate xl = 1 + 4*(_n - 1)/19           // x dispersés entre 1 et 5
generate xe = 3 + 0.25*(xl - 3)           // x resserrés entre 2,5 et 3,5
set seed 2026
generate e = .
generate y = .
local gl ""
local ge ""
forvalues r = 1/20 {
    quietly replace e = rnormal(0, 1)     // mêmes erreurs à gauche et à droite
    quietly replace y = 2 + xl + e
    quietly regress y xl
    local gl `gl' (function y = `=_b[_cons]' + `=_b[xl]'*x, range(0 6) lcolor(navy%35))
    quietly replace y = 2 + xe + e
    quietly regress y xe
    local ge `ge' (function y = `=_b[_cons]' + `=_b[xe]'*x, range(0 6) lcolor(navy%35))
}
twoway `gl' (function y = 2 + x, range(0 6) lcolor(orange) lwidth(thick)), ///
    legend(off) yscale(range(-4 12)) title("x entre 1 et 5") name(L, replace)
twoway `ge' (function y = 2 + x, range(0 6) lcolor(orange) lwidth(thick)), ///
    legend(off) yscale(range(-4 12)) title("x entre 2,5 et 3,5") name(E, replace)
graph combine L E, rows(1) ycommon
graph export "fig_eventail_stata.pdf", replace

* ---------- [mc-hetero] Simulation, quand RLS.5 échoue ----------
use econmath.dta, clear
keep in 1/50
keep colgpa
generate s = 3 + 14*abs(colgpa - 2.8)     // écart-type de u, variable selon x
quietly summarize colgpa
local sct = r(Var)*(r(N) - 1)
generate w = (colgpa - r(mean))/`sct'     // poids w_i
generate s2 = s^2
generate w2s2 = w^2*s2
quietly summarize s2
local ms2 = r(mean)
display "moyenne des variances de u : " %6.1f `ms2'
quietly summarize w2s2
display "vrai écart-type : " %6.3f sqrt(r(sum)) " | formule usuelle : " %6.3f sqrt(`ms2'/`sct')
set seed 2026
generate y = .
postfile sim b1 using mc4, replace
forvalues r = 1/5000 {
    quietly replace y = 32 + 14*colgpa + s*rnormal(0, 1)
    quietly regress y colgpa
    post sim (_b[colgpa])
}
postclose sim
use mc4, clear
summarize b1                              // moyenne et écart-type simulé

* ---------- [mc-sigma] Simulation, estimer la variance de l'erreur ----------
use econmath.dta, clear
keep in 1/50
keep colgpa
set seed 2026
generate y = .
tempname sim
postfile `sim' scr using mc5, replace
forvalues r = 1/5000 {
    quietly replace y = 32 + 14*colgpa + rnormal(0, 11)
    quietly regress y colgpa
    post `sim' (e(rss))                   // SCR de l'échantillon
}
postclose `sim'
use mc5, clear
generate s2_n2 = scr/48                   // SCR / (n - 2)
generate s2_n = scr/50                    // SCR / n
tabstat s2_n2 s2_n, statistics(mean) format(%9.2f)
display "vraie variance 121 | 121 x 48 / 50 = " 121*48/50

* ---------- [mc-se] Simulation, l'erreur-type ----------
use econmath.dta, clear
keep in 1/50
keep colgpa
quietly summarize colgpa
display "écart-type théorique 11 / racine(SCT_x) : " %6.3f 11/sqrt(r(Var)*(r(N) - 1))
set seed 2026
generate y = .
tempname sim
postfile `sim' b1 se using mc6, replace
forvalues r = 1/5000 {
    quietly replace y = 32 + 14*colgpa + rnormal(0, 11)
    quietly regress y colgpa
    post `sim' (_b[colgpa]) (_se[colgpa])   // pente et erreur-type
}
postclose `sim'
use mc6, clear
tabstat se b1, statistics(mean sd) format(%9.3f)   // moyenne des se, écart-type des pentes

* ---------- [erreur-type] D'où viennent les erreurs-types de la table ----------
use econmath.dta, clear
regress score colgpa
local s2 = e(rss)/(e(N) - 2)              // sigma chapeau au carré = SCR / (n - 2)
quietly summarize colgpa
local sct = r(Var)*(r(N) - 1)
generate x2 = colgpa^2
quietly summarize x2
display "moyenne des x au carré : " %6.3f r(mean)
display "SCR : " %9.1f e(rss) " | sigma chapeau : " %6.3f sqrt(`s2') " | SCT_x : " %7.2f `sct'
display "se(b1) : " %6.4f sqrt(`s2'/`sct') " | se(b0) : " %6.4f sqrt(`s2'*r(sum)/(e(N)*`sct'))
display "Root MSE de Stata (sigma chapeau) : " %6.3f e(rmse)
