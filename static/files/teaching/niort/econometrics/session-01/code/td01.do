* =============================================================
* Économétrie L3, séance 1. Série d'exercices, code Stata du corrigé
* Chaque bloc est autonome. On le copie seul dans l'éditeur de do-files
* (Do-file Editor) puis on l'exécute. La fenêtre Command n'accepte pas
* les commentaires // ni les continuations ///.
* Données. Les fichiers .dta du cours (dossier « donnees » sur Moodle)
* sont à placer dans un même dossier, désigné une fois par session comme
* dossier de travail (menu File > Change Working Directory, ou cd "chemin").
* NON TESTÉ : Stata n'était pas disponible lors de la préparation.
* =============================================================

* ---------- [ex1] Exercice 1, calculs à la main vérifiés ----------
clear
input x y
2 14
3 12
4 13
6 9
7 8
8 4
end
summarize x y                                   // moyennes et écarts-types
correlate x y, covariance                       // s2_x, s2_y et s_xy
correlate x y                                   // r
regress y x                                     // b0, b1, SCE, SCR, R2
predict yhat, xb
predict uhat, residuals
generate xu = x * uhat
list x y yhat uhat
tabstat uhat xu, statistics(sum)                // propriétés 1 et 2
display _b[_cons] + _b[x] * 5 "   " _b[_cons] + _b[x] * 15
generate xc = 100 * x                           // prix en centimes
regress y xc

* ---------- [ex2] Exercice 2, les deux droites de régression ----------
clear
input x y
2 14
3 12
4 13
6 9
7 8
8 4
end
quietly regress y x
scalar b1 = _b[x]                               // régression de y sur x
quietly regress x y
scalar g1 = _b[y]                               // régression de x sur y
quietly correlate x y
display b1 "   " g1 "   " b1 * g1 "   " r(rho)^2 "   " 1 / g1

* ---------- [ex3] Exercice 3, table de régression, ATTEND ----------
use attend.dta, clear                           // 680 étudiants
regress final attend
regress final priGPA
regress attend priGPA
summarize attend
quietly regress final attend
display _b[_cons] + _b[attend] * 32 "   " _b[_cons] + _b[attend] * 16
quietly regress attend priGPA
display _b[_cons] + _b[priGPA] * 3
regress final atndrte                           // présence en %

* ---------- [ex4] Exercice 4, résultats potentiels et tirage au sort ----------
clear
input str1 etudiant y0 y1 d t
A 8 11 1 1
B 9 11 1 0
C 7 10 1 1
D 10 12 1 0
E 14 15 0 1
F 13 15 0 0
G 15 16 0 1
H 12 14 0 0
end
generate effet = y1 - y0
summarize effet                                 // effet moyen
summarize effet if d == 1                       // chez les participants
generate yobs = cond(d == 1, y1, y0)            // note observée
ttest yobs, by(d)                               // écart observé (0 moins 1)
summarize y0 if d == 1
summarize y0 if d == 0
generate ytir = cond(t == 1, y1, y0)            // note observée avec le tirage
ttest ytir, by(t)

* ---------- [ex4-tirages] Exercice 4, les 70 tirages possibles ----------
clear
matrix Y0 = (8, 9, 7, 10, 14, 13, 15, 12)       // notes sans tutorat, A à H
matrix Y1 = (11, 11, 10, 12, 15, 15, 16, 14)    // notes avec tutorat
scalar s0 = 88                                  // somme des y0
scalar total = 0
scalar nombre = 0
scalar mini = 100
scalar maxi = -100
forvalues a = 1/5 {
forvalues b = `=`a'+1'/6 {
forvalues c = `=`b'+1'/7 {
forvalues e = `=`c'+1'/8 {
scalar m1 = (Y1[1,`a'] + Y1[1,`b'] + Y1[1,`c'] + Y1[1,`e']) / 4
scalar m0 = (s0 - Y0[1,`a'] - Y0[1,`b'] - Y0[1,`c'] - Y0[1,`e']) / 4
scalar total = total + m1 - m0
scalar nombre = nombre + 1
scalar mini = min(mini, m1 - m0)
scalar maxi = max(maxi, m1 - m0)
}
}
}
}
display nombre "   " total / nombre "   " mini "   " maxi

* ---------- [ex5-depart] Exercice 5, charger les données WINE ----------
use wine.dta, clear                             // 21 pays
list

* ---------- [ex5-mco] Exercice 5, statistiques et MCO ----------
use wine.dta, clear
summarize alcohol heart
correlate alcohol heart, covariance
correlate alcohol heart
regress heart alcohol
predict prevu, xb
predict residu, residuals
list country alcohol heart prevu residu if inlist(country, "France", "Japan")
tabstat residu, statistics(sum)
regress liver alcohol
regress deaths alcohol

* ---------- [ex5-figure] Exercice 5, nuage de points et droite ----------
use wine.dta, clear
generate nom = ""
replace nom = "France" if country == "France"
replace nom = "Japon" if country == "Japan"
replace nom = "Italie" if country == "Italy"
twoway (scatter heart alcohol, mcolor("31 78 121") mlabel(nom) mlabsize(small)) ///
    (lfit heart alcohol, lcolor("192 80 43")), legend(off) ///
    xtitle("Alcool issu du vin (litres par habitant)") ///
    ytitle("Décès cardiaques pour 100 000")
graph export "fig_td_wine_stata.pdf", replace
