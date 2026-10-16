# Rapport de tests du code, séance 2

## Environnements

- **Python.** Environnement virtuel de type Colab (Python 3.13, numpy 2.5, pandas 3.0, matplotlib 3.11, statsmodels 0.15), sans le paquet `wooldridge`, désinstallé avant chaque bloc pour tester la ligne d'installation. Chaque bloc tourne seul dans un processus neuf et un dossier vide.
- **R.** R 4.3.3 avec une bibliothèque vide pour chaque bloc. `wooldridge` (1.4-7) s'installe par `install.packages` depuis un dépôt local construit à partir des sources publiques du paquet, le réseau de l'environnement bloquant CRAN. Pour tenir le temps de calcul, le dépôt sert une version précompilée du même paquet. La ligne d'installation est la même.
- **Stata.** Non disponible. **Aucun bloc Stata n'a été exécuté**, ni dans `seance02.do` ni dans `td02.do`. Les do-files suivent les mêmes étapes que Python et R, sans garantie. Ils chargent `econmath.dta`, `jtrain2.dta`, `htv.dta` et `nbasal.dta`, produits par `generer_dta.py`.

## Tests par bloc

1. **Source.** Chaque bloc de `seance02.py`, `seance02.R`, `td02.py` et `td02.R`, exécuté seul.
2. **PDF, mode disposition.** Texte extrait par `pdftotext -layout` puis exécuté seul (`extraire_pdf.py` pour le support, `extraire_article.py` pour le polycopié, la série et le corrigé, puis `tester_blocs.py`).
3. **PDF, mode brut.** Texte extrait par `pdftotext` sans option, qui suit l'ordre de lecture du PDF comme la plupart des copier-coller.
4. **Comparaison.** Texte extrait en mode disposition comparé caractère par caractère à la source.

Les tests 2 et 3 ont porté sur les quatre PDF qui contiennent du code, à savoir le support (27 blocs par langage), le polycopié (27), le corrigé (9) et la série (bloc de départ de l'exercice 5). Au total 328 exécutions, toutes réussies. Pour chaque bloc, les sorties des tests 2 et 3 sont identiques, caractère pour caractère, à celles du test 1, y compris pour les simulations, puisque la graine est fixée.

Un premier passage a fait échouer cinq blocs Python en mode brut. Des chaînes affichées contenaient plusieurs espaces de suite, que `pdftotext` coupe en deux lignes. Ces espaces ont été remplacés par un séparateur « | » dans les trois langages, et tous les tests ont été refaits.

Les sorties sont dans `sorties_python.txt`, `sorties_R.txt`, `sorties_td02_python.txt` et `sorties_td02_R.txt`.

## Python et R donnent-ils les mêmes résultats

`comparer_py_r.py` cherche chaque nombre affiché par Python dans la sortie R du même bloc. Pour les blocs sans tirage aléatoire, les seuls nombres non retrouvés sont des différences d'affichage (étiquettes, centres de classe 1,625 affichés 1,62 par R, bornes des quarts écrites sous forme d'intervalles par pandas). Les blocs de simulation diffèrent dans les dernières décimales, car Python (`numpy`, PCG64), R (Mersenne-Twister) et Stata utilisent des générateurs différents. Par exemple, pour `mc-sansbiais`, la moyenne et l'écart-type des 5 000 pentes valent 14,006 et 2,622 en Python, 13,995 et 2,561 en R, pour une vraie pente de 14 et un écart-type théorique de 2,606. Les documents citent les valeurs de Python. Le corrigé cite aussi celles de R pour l'exercice 5 (1,412, 0,510 et 0,357), vérifiées dans `sorties_td02_R.txt`.

## Notebooks

`seance02_python.ipynb` (27 cellules) et `td02_python.ipynb` (9 cellules) ont été exécutés en entier par un noyau Jupyter (nbclient), sans erreur. Les notebooks R n'ont pas été exécutés, faute de noyau R Jupyter dans l'environnement. Leurs cellules sont, caractère pour caractère, les blocs R testés ci-dessus.

## Valeurs des documents

`audit_valeurs.py` relève tous les nombres décimaux du support, des annexes, du polycopié, de la série et du corrigé, et les cherche dans les sorties Python. Les 22 qui n'y figurent pas tels quels (calculs intermédiaires, valeurs R, valeurs du quiz) sont recalculés un à un par `verifier_valeurs.py`. 29 contrôles, tous réussis (`verif_valeurs.txt`). Ce contrôle a corrigé une valeur du corrigé (exercice 3, SCT_x ≈ 9 323 et non 9 324).

## Liens du PDF du support

`verifier_liens.py` liste les 288 liens internes (`liens_verifies.txt`), et `controler_liens.py` les contrôle automatiquement. Tous ont une destination, aucune erreur.
- 8 renvois numérotés vers les annexes 1 à 8, et 8 boutons « Retour » d'annexe, chacun vers la slide du renvoi
- 27 boutons « Code », chacun vers la version Python du bon bloc
- 81 boutons « Retour » de code, chacun vers la slide qui porte le bouton « Code »
- 162 onglets de langage, chacun vers le même bloc dans l'autre langage
- le lien de « D'où vient le (0,70) » vers « RLS.5, homoscédasticité » et le bouton de retour correspondant

## Résultats par bloc

| Bloc | Contenu | Python source | Python PDF disposition | Python PDF brut | Python texte extrait | R source | R PDF disposition | R PDF brut | R texte extrait | Stata | Stata texte extrait |
|---|---|---|---|---|---|---|---|---|---|---|---|
| `ech-quatre` | quatre échantillons, quatre droites | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `ech-distrib` | mille échantillons, la distribution des pentes | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | identique |
| `va` | un étudiant tiré au hasard | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `esp-prop` | calculer avec l'espérance | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `variance` | variance et écart-type | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | identique |
| `lgn` | la moyenne empirique se rapproche de l'espérance | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | blancs internes |
| `cov-pop` | covariance et corrélation dans la population | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `independance` | covariance nulle sans indépendance | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `cond-cov` | covariance nulle, espérance conditionnelle non nulle | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `var-moyenne` | la variance d'une moyenne | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `esp-cond` | espérance conditionnelle | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `iterees` | la loi des espérances itérées | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `var-cond` | variance conditionnelle | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | identique |
| `rls1` | moyennes par classe et droite des MCO | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `rls3` | variation de x dans l'échantillon | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | identique |
| `equilibre` | comparer les groupes sur ce qui est observé | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | blancs internes | non testé | identique |
| `rls5` | résidus et dispersion selon colgpa | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | blancs internes |
| `jouet-bruit` | exemple à la main, l'estimation est la vérité plus du bruit | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `jouet-b0` | exemple à la main, la constante | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `mc-sansbiais` | simulation, l'absence de biais | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | identique |
| `mc-biais` | simulation, un biais quand RLS.4 échoue | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | blancs internes |
| `mc-variance` | simulation, ce qui rend la pente précise | réussi | réussi | réussi | identique | réussi | réussi | réussi | indentation d'une ligne de continuation | non testé | blancs internes |
| `eventail` | pourquoi la dispersion de x aide | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | blancs internes | non testé | identique |
| `mc-hetero` | simulation, quand RLS.5 échoue | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `mc-sigma` | simulation, estimer la variance de l'erreur | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | identique |
| `mc-se` | simulation, l'erreur-type | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | blancs internes |
| `erreur-type` | d'où viennent les erreurs-types de la table | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | blancs internes | non testé | identique |
| `ex1` | exercice 1, une loi jointe à la main | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `ex1-simul` | exercice 1, la pente des MCO sur des échantillons tirés | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `ex2` | exercice 2, la pente des extrêmes | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `ex3` | exercice 3, table de régression, NBASAL | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `ex5-depart` | exercice 5, charger les données HTV | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `ex5-mco` | exercice 5, MCO, sigma chapeau et erreur-type | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | identique |
| `ex5-residus` | exercice 5, résidus et dispersion | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `ex5-simul` | exercice 5, la population des 1 230 hommes | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `ex5-abil` | exercice 5, l'aptitude, un facteur de u observé ici | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |

Les blocs `ex1` à `ex5-abil` sont ceux de la série et du corrigé. Leur colonne « PDF » porte sur le corrigé. Le bloc `ex5-depart`, imprimé aussi dans la série, a été testé depuis les deux documents.

« Blancs internes » signifie que seul le nombre d'espaces entre le code et un commentaire de fin de ligne diffère, sans effet à l'exécution. Pour le bloc R `mc-variance`, une ligne de continuation est extraite avec un espace d'indentation de plus, sans effet. La comparaison donne les mêmes résultats pour le polycopié.
