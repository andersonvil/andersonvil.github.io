# Rapport de tests du code, séance 1 (v2)

## Environnements

- **Python.** Environnement virtuel de type Colab (Python 3.11, numpy, pandas, matplotlib, statsmodels), sans le paquet `wooldridge`, désinstallé avant chaque bloc pour tester la ligne d'installation. Chaque bloc tourne seul dans un processus neuf et un dossier vide.
- **R.** R 4.3.3 avec une bibliothèque vide pour chaque bloc. `wooldridge` s'installe par `install.packages` depuis un dépôt local construit à partir des sources publiques du paquet, le réseau de l'environnement bloquant CRAN.
- **Stata.** Non disponible. **Aucun bloc Stata n'a été exécuté.** Le do-file suit les mêmes étapes que Python et R, sans garantie.

## Quatre tests par bloc

1. **Source.** Chaque bloc des fichiers `seance01.py` et `seance01.R`, exécuté seul.
2. **PDF, mode disposition.** Texte extrait du PDF compilé par `pdftotext -layout`, puis exécuté seul (`tests/extraire_pdf.py`, puis `tests/tester_blocs.py`).
3. **PDF, mode brut.** Texte extrait par `pdftotext` sans option, qui suit l'ordre de lecture du PDF comme la plupart des copier-coller (les commentaires de fin de ligne passent à la ligne suivante).
4. **Comparaison.** Texte extrait en mode disposition comparé caractère par caractère à la source.

Les sorties des tests 2 et 3 sont identiques à celles du test 1, pour les 23 blocs Python et les 23 blocs R. Elles correspondent aux valeurs des slides (par exemple 32,306 + 14,316 colgpa, R² = 0,329, n = 856, SCT = 153 539,3, corrélation study-score 0,009).

Les sorties de chaque bloc sont dans `sorties_python.txt` et `sorties_R.txt`.

Le notebook `seance01_python.ipynb` a été exécuté en entier par un noyau Jupyter (nbclient), 23 cellules, aucune erreur. Le notebook `seance01_R.ipynb` n'a pas été exécuté, faute de noyau R Jupyter dans l'environnement. Ses cellules sont, caractère pour caractère, les blocs R testés ci-dessus.

## Résultats

| Bloc | Contenu | Python source | Python PDF disposition | Python PDF brut | Python texte extrait | R source | R PDF disposition | R PDF brut | R texte extrait | Stata | Stata texte extrait |
|---|---|---|---|---|---|---|---|---|---|---|---|
| `etude` | heures d'étude et note | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `outils` | charger des données de Wooldridge | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `ext-ct` | coupe transversale | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `ext-st` | série temporelle | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `ext-cte` | coupes transversales empilées | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `ext-panel` | données de panel | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `jtrain` | données expérimentales | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `travail` | travailler 15 heures ou plus | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | identique |
| `variance` | construire la variance | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | blancs internes | non testé | indentation des lignes /// |
| `quadrants` | signe de la covariance | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `deplacer` | déplacer des points | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `jouet-stats` | exemple jouet, statistiques descriptives | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `nuage` | nuage de points du fil rouge | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | identique |
| `mco-illus` | quelle droite choisir | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `carres` | trois critères | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | blancs internes |
| `jouet-mco` | exemple jouet, estimateurs des MCO | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `jouet-proprietes` | exemple jouet, propriétés algébriques | réussi | réussi | réussi | identique | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `decomp` | décomposition d'une observation | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | blancs internes | non testé | blancs internes |
| `jouet-r2` | exemple jouet, SCT, SCE, SCR et R2 | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `jtrain-r2` | un R2 faible dans une expérience | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `reg-table` | première estimation | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | identique |
| `droite` | droite des MCO sur le nuage | réussi | réussi | réussi | identique | réussi | réussi | réussi | identique | non testé | blancs internes |
| `residus` | valeurs prédites et résidus | réussi | réussi | réussi | blancs internes | réussi | réussi | réussi | identique | non testé | identique |

« Blancs internes » signifie que seul le nombre d'espaces entre le code et un commentaire de fin de ligne diffère, ce qui n'a aucun effet à l'exécution. Pour Stata, le bloc `variance` diffère aussi par un espace d'indentation au début des lignes de continuation, sans effet.

## Liens du PDF

`tests/verifier_liens.py` liste les 247 liens internes, leur page de départ et la page d'arrivée avec son titre (`tests/liens_verifies.txt`). Tous ont une destination. Contrôles effectués un à un
- 4 renvois numérotés vers les annexes 1 à 4, et 6 boutons « Retour » d'annexe vers la slide d'origine
- 25 boutons « Code » (23 blocs, dont la slide à trois étapes), chacun vers la version Python du bon bloc
- 69 boutons « Retour » de code, chacun vers la slide qui porte le bouton « Code »
- 138 onglets de langage, chacun vers le même bloc dans l'autre langage
- 2 boutons vers la partie 3, « Quelle droite choisir ? » vers S33 et le bouton de S33 vers « Retour à l'exemple à la main »
