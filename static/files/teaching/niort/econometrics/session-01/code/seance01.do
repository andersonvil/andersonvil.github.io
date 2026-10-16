* =============================================================
* Économétrie L3, séance 1. Code Stata des figures, tables et calculs
* Chaque bloc est autonome. On le copie seul dans l'éditeur de do-files
* (Do-file Editor) puis on l'exécute. La fenêtre Command n'accepte pas
* les commentaires // ni les continuations ///.
* Données. Les fichiers .dta du cours (dossier « donnees » sur Moodle)
* sont à placer dans un même dossier, désigné une fois par session comme
* dossier de travail (menu File > Change Working Directory, ou cd "chemin").
* NON TESTÉ : Stata n'était pas disponible lors de la préparation.
* =============================================================

* ---------- [etude] Heures d'étude et note ----------
use econmath.dta, clear                   // 856 étudiants
correlate study score

* ---------- [outils] Charger des données de Wooldridge ----------
use econmath.dta, clear                   // charge ECONMATH
describe                                  // variables et libellés
list in 1/5                               // premières lignes

* ---------- [ext-ct] Coupe transversale, ECONMATH ----------
use econmath.dta, clear                   // fichier du cours, dossier de travail
list score colgpa work study econhs in 1/5
count

* ---------- [ext-st] Série temporelle, PHILLIPS ----------
use phillips.dta, clear                   // fichier du cours, dossier de travail
list year unem inf in 1/3
list year unem inf in -2/l
twoway (line unem year, lcolor("31 78 121")) (line inf year, lcolor(gs9) lpattern(dash)), ///
    yline(0, lwidth(vthin)) xtitle("Année") ytitle("") ///
    legend(order(1 "Taux de chômage (%)" 2 "Inflation (%)") position(11) ring(0) cols(1))
graph export "fig_phillips_stata.pdf", replace

* ---------- [ext-cte] Coupes transversales empilées, CPS78_85 ----------
use cps78_85.dta, clear                   // fichier du cours, dossier de travail
list year educ exper female lwage in 1/2          // enquête de 1978
list year educ exper female lwage in 551/552      // enquête de 1985
tabulate year

* ---------- [ext-panel] Données de panel, WAGEPAN ----------
use wagepan.dta, clear                    // fichier du cours, dossier de travail
list nr year educ exper hours lwage if inlist(nr, 13, 17) & year <= 1981
xtset nr year                             // déclare la structure de panel

* ---------- [jtrain] Données expérimentales, JTRAIN2 ----------
use jtrain2.dta, clear                    // fichier du cours, dossier de travail
tabstat re78 educ age, by(train) statistics(mean) format(%9.2f)
tabulate train                            // effectifs

* ---------- [travail] Travailler 15 h ou plus, comparaison de groupes ----------
use econmath.dta, clear                   // fichier du cours, dossier de travail
generate w15 = (work >= 15)               // 1 si 15 h ou plus
tabstat score colgpa mothcoll, by(w15) statistics(mean) format(%9.2f)
tabulate w15                              // effectifs

* ---------- [variance] Construire la variance ----------
clear
input a b
 2 4
 4 5
 6 6
 8 7
10 8
end
generate ea = a - 6                       // écarts à la moyenne de A
generate ca = (a - 6)^2                   // carrés des écarts de A
generate cb = (b - 6)^2                   // carrés des écarts de B
tabstat ea ca cb, statistics(sum)         // somme des écarts nulle
tabstat a b, statistics(variance)         // variances 10 et 2,5
generate ha = 2.7 + 0.3*_n
generate hb = 0.3 + 0.3*_n
generate m = 6
twoway (pcspike ha m ha a, lcolor(gs9)) (scatter ha a, mcolor("31 78 121")) ///
    (pcspike hb m hb b, lcolor(gs9)) (scatter hb b, mcolor("31 78 121")), ///
    xline(6, lpattern(dash) lcolor("192 80 43")) text(3.6 0.5 "A" 1.2 0.5 "B") ///
    ylabel(none) ytitle("") xtitle("") legend(off) title("Écarts à la moyenne") name(G, replace)
graph bar (asis) ca cb, over(a) legend(order(1 "A" 2 "B")) ///
    title("Carrés des écarts") name(D, replace)
graph combine G D, rows(1)
graph export "fig_variance_stata.pdf", replace

* ---------- [quadrants] Signe de la covariance, ECONMATH ----------
use econmath.dta, clear                   // fichier du cours, dossier de travail
quietly summarize colgpa
local mx = r(mean)
quietly summarize score
local my = r(mean)
twoway scatter score colgpa, msize(vsmall) mcolor("31 78 121%35") ///
    xline(`mx', lpattern(dash) lcolor(gs9)) yline(`my', lpattern(dash) lcolor(gs9)) ///
    text(97 3.8 "+" 25 1.1 "+" 25 3.8 "-" 97 1.1 "-", color("192 80 43") size(large)) ///
    xtitle("Moyenne universitaire (colgpa, sur 4)") ytitle("Note finale (score, %)")
graph export "fig_quadrants_stata.pdf", replace
correlate colgpa score, covariance
correlate colgpa score

* ---------- [deplacer] Déplacer des points change le signe ----------
clear
input x yA yB yC
 1 2 7 7
 2 4 4 8
 3 3 3 6
 4 5 5 5
 5 4 4 4
 6 6 6 6
 7 5 5 5
 8 7 7 4
 9 6 6 2
10 8 3 3
end
correlate x yA yB yC                      // 1re colonne, r entre x et chaque y
correlate x yA yB yC, covariance          // 1re colonne, covariances
twoway (scatter yA x, mcolor("31 78 121")), xline(5.5, lpattern(dash)) yline(5, lpattern(dash)) ///
    title("Départ") legend(off) name(A, replace)
twoway (scatter yA x if inlist(x, 1, 10), mcolor(gs12)) (scatter yB x, mcolor("31 78 121")), ///
    xline(5.5, lpattern(dash)) yline(5, lpattern(dash)) title("2 points déplacés") legend(off) name(B, replace)
twoway (scatter yB x if inlist(x, 2, 3, 8, 9), mcolor(gs12)) (scatter yC x, mcolor("31 78 121")), ///
    xline(5.5, lpattern(dash)) yline(5, lpattern(dash)) title("4 autres déplacés") legend(off) name(C, replace)
graph combine A B C, rows(1) ycommon
graph export "fig_deplacer_stata.pdf", replace

* ---------- [jouet-stats] Exemple jouet, statistiques descriptives ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
summarize x y                             // écarts-types calculés avec n - 1
correlate x y, covariance
correlate x y

* ---------- [nuage] Nuage de points du fil rouge ----------
use econmath.dta, clear                   // fichier du cours, dossier de travail
twoway scatter score colgpa, msize(vsmall) mcolor("31 78 121%35") ///
    xtitle("Moyenne universitaire (colgpa, sur 4)") ytitle("Note finale (score, %)")
graph export "fig_nuage_stata.pdf", replace

* ---------- [mco-illus] Quelle droite choisir ? ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
generate f1 = 4 + 1.2*x                   // droite 1
generate f2 = 6.2 + 0.8*x                 // droite 2
generate c1 = (y - f1)^2
generate c2 = (y - f2)^2
tabstat c1 c2, statistics(sum)            // SCR 11 et 4,4
twoway (rspike y f1 x, lcolor(gs9)) (function y = 4 + 1.2*x, range(0 12) lcolor(gs9)) ///
    (scatter y x, mcolor("31 78 121")), title("y = 4 + 1,2 x   SCR = 11,0") legend(off) ///
    xtitle("x (heures)") ytitle("y (note sur 20)") name(A, replace)
twoway (rspike y f2 x, lcolor(gs9)) (function y = 6.2 + 0.8*x, range(0 12) lcolor("192 80 43")) ///
    (scatter y x, mcolor("31 78 121")), title("y = 6,2 + 0,8 x   SCR = 4,4") legend(off) ///
    xtitle("x (heures)") ytitle("") name(B, replace)
graph combine A B, rows(1) ycommon
graph export "fig_mco_illus_stata.pdf", replace

* ---------- [carres] Trois critères pour choisir la droite ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
generate ua = y - (17 - x)                // a) droite fausse, pente -1
generate ab1 = abs(y - (5 + x))           // b) deux droites
generate ab2 = abs(y - (8 + 0.5*x))
generate uc = y - (6.2 + 0.8*x)           // c) droite des MCO
generate uc2 = uc^2
tabstat ua uc ab1 ab2 uc2, statistics(sum)
generate fa = 17 - x
generate fc = 6.2 + 0.8*x
twoway (rspike y fa x, lcolor(gs9)) (function y = 17 - x, range(0 12) lcolor(gs9)) ///
    (scatter y x, mcolor("31 78 121")), title("a) somme des résidus = 0") legend(off) name(A, replace)
twoway (function y = 5 + x, range(0 12) lcolor(gs9)) (function y = 8 + 0.5*x, range(0 12) lpattern(dash)) ///
    (scatter y x, mcolor("31 78 121")), title("b) somme des |résidus| = 4 et 4") legend(off) name(B, replace)
twoway (rspike y fc x, lcolor(gs9)) (function y = 6.2 + 0.8*x, range(0 12) lcolor("192 80 43")) ///
    (scatter y x, mcolor("31 78 121")), title("c) somme des carrés = 4,4") legend(off) name(C, replace)
graph combine A B C, rows(1) ycommon
graph export "fig_carres_stata.pdf", replace

* ---------- [jouet-mco] Exemple jouet, MCO à la main et par le code ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
quietly summarize x
scalar xbar = r(mean)
quietly summarize y
scalar ybar = r(mean)
generate num = (x - xbar)*(y - ybar)
generate den = (x - xbar)^2
quietly summarize num
scalar snum = r(sum)
quietly summarize den
scalar b1 = snum/r(sum)
scalar b0 = ybar - b1*xbar
display "à la main : b0 = " b0 "   b1 = " b1
regress y x                               // par le code

* ---------- [jouet-proprietes] Valeurs prédites, résidus, propriétés ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
quietly regress y x
predict yhat, xb                          // valeurs prédites
predict uhat, residuals                   // résidus
generate xu = x*uhat
list x y yhat uhat xu, clean
tabstat uhat xu, statistics(sum)          // sommes nulles
quietly summarize x
display "droite en x moyen : " _b[_cons] + _b[x]*r(mean)

* ---------- [decomp] Décomposition pour une observation ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
quietly regress y x
predict yhat, xb
list x y yhat in 4                        // 4e étudiant, x = 8
twoway (scatter y x, mcolor("31 78 121")) (function y = 6.2 + 0.8*x, range(0 12) lcolor("192 80 43")) ///
    (pcarrowi 11 8.25 12.6 8.25, lcolor("192 80 43") mcolor("192 80 43")) ///
    (pcarrowi 12.6 7.75 14 7.75, lcolor(gs9) mcolor(gs9)), ///
    yline(11, lpattern(dash) lcolor(gs9)) legend(off) xtitle("x (heures)") ytitle("y (note sur 20)")
graph export "fig_decomp_stata.pdf", replace

* ---------- [jouet-r2] SCT, SCE, SCR et R2 ----------
clear
input x y
 2  7
 4 10
 6 11
 8 14
10 13
end
quietly regress y x
display "SCT = " e(mss) + e(rss) "   SCE = " e(mss) "   SCR = " e(rss) "   R2 = " e(r2)
quietly correlate x y
display "r2 = " r(rho)^2

* ---------- [jtrain-r2] Un R2 faible dans une expérience, JTRAIN2 ----------
use jtrain2.dta, clear                    // fichier du cours, dossier de travail
regress re78 train

* ---------- [reg-table] Première estimation sur ECONMATH ----------
use econmath.dta, clear                   // fichier du cours, dossier de travail
regress score colgpa
display "prédiction pour colgpa = 3 : " _b[_cons] + 3*_b[colgpa]
regress score actmth                      // 42 valeurs manquantes écartées
summarize colgpa

* ---------- [droite] Droite des MCO sur le nuage ----------
use econmath.dta, clear                   // fichier du cours, dossier de travail
quietly summarize colgpa
local mx = r(mean)
quietly summarize score
local my = r(mean)
display "point moyen : " `mx' "  " `my'
twoway (scatter score colgpa, msize(vsmall) mcolor("31 78 121%35")) ///
    (lfit score colgpa, lcolor("192 80 43") lwidth(medthick)) ///
    (scatteri `my' `mx', msymbol(S) mcolor("192 80 43")), legend(off) ///
    xtitle("Moyenne universitaire (colgpa, sur 4)") ytitle("Note finale (score, %)")
graph export "fig_droite_stata.pdf", replace

* ---------- [residus] Valeurs prédites et résidus sur ECONMATH ----------
use econmath.dta, clear                   // fichier du cours, dossier de travail
quietly regress score colgpa
predict score_chap, xb                    // valeurs prédites
predict u_chap, residuals                 // résidus
list colgpa score score_chap u_chap in 1/5, clean
tabstat u_chap, statistics(sum)
display "SCT = " e(mss) + e(rss) "   SCE = " e(mss) "   SCR = " e(rss)
