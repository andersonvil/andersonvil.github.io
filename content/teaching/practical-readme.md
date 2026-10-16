+++
title = "Utiliser les supports, les données et les programmes"
path = "teaching/practical-readme"
template = "practical-readme.html"
[extra]
lang = "fr"
+++

Ce dossier accompagne les cours et les TD. Les supports présentent le raisonnement ; les exercices permettent de l'appliquer ; les données, scripts et notebooks permettent de retrouver les calculs et les graphiques. Les simulateurs HTML permettent de faire varier une hypothèse directement dans le navigateur.

## Trouver le bon ensemble

Commencez par la [page Teaching](/teaching/), choisissez le cours puis sa séance ou sa partie. La page [Code & Data](/code-data/) reprend les mêmes fichiers et le même classement. Téléchargez le dossier complet de la séance pour conserver les chemins relatifs ; les liens individuels permettent de ne récupérer qu'un support ou un programme.

| Élément | Rôle et correspondance |
|---|---|
| `support/` | Cours auquel se rapportent les figures et les calculs des programmes. |
| `td/` | Série d'exercices ; les scripts dont le nom commence par `td` correspondent au corrigé et aux applications. |
| `donnees/` | Fichiers Stata utilisés par les exemples ; à conserver ensemble. |
| `code/` | Scripts Python, R, Stata et notebooks du cours et des exercices. Les notebooks se reconnaissent à l'extension `.ipynb`. |
| `simulateur/` ou `simulateurs/` | Applications HTML autonomes, à ouvrir dans un navigateur. |
| `code/tests/` dans les dossiers complets | Contrôles et comptes rendus accompagnant les matériaux fournis. |

Le dossier s'enrichit progressivement : les nouvelles séances reprennent cette organisation, sans changer la correspondance entre support, exercices, données et programmes.

## Parcours conseillé

1. Lire le support et repérer le nom ou l'identifiant du calcul.
2. Essayer l'exercice avant d'ouvrir le script de TD associé.
3. Extraire le dossier complet, puis ouvrir un terminal à sa racine.
4. Installer les dépendances du langage retenu et exécuter les blocs dans leur ordre.
5. Comparer les statistiques, coefficients et graphiques au support. Vérifier l'échantillon et les variables avant de comparer les dernières décimales.

## Économétrie L3 — UCO Niort

La [page du cours](/teaching/econometrics-niort/) distingue les séances. La séance 1 porte sur la covariance, la corrélation et la régression simple ; la séance 2 développe l'échantillonnage, les probabilités et l'inférence. Chaque dossier contient le support, la série, les versions Python/R/Stata, leurs notebooks et un simulateur.

- [Dossier complet de la séance 1](/files/teaching/niort/econometrics/session-01.zip)
- [Dossier complet de la séance 2](/files/teaching/niort/econometrics/session-02.zip)

### Python

Utilisez Python 3.10 ou plus récent. Depuis un environnement de travail, installez :

```
python -m pip install numpy pandas matplotlib scipy statsmodels wooldridge jupyterlab
```

À la racine de la séance 1 :

```
python code/seance01.py
python code/td01.py
jupyter lab
```

À la racine de la séance 2 :

```
python code/seance02.py
python code/td02.py
```

Les blocs Python du cours sont autonomes et peuvent aussi être exécutés dans Spyder ou Colab. Les scripts chargent les jeux pédagogiques via `wooldridge`. Les fichiers `.dta` fournis permettent de travailler avec Stata et de consulter les mêmes bases. Les utilitaires `generer_dta.py` et `generer_annexes.py` servent respectivement à exporter les données et à produire les annexes.

### R et notebooks

Installez les paquets utilisés par les scripts :

```r
install.packages(c("wooldridge", "ggplot2", "IRkernel"))
IRkernel::installspec()
```

Puis, depuis la racine de la séance :

```
Rscript code/seance01.R
Rscript code/td01.R
```

Pour la séance 2, remplacez `01` par `02`. Ouvrez les notebooks `_python.ipynb` avec un noyau Python, et les notebooks `_R.ipynb` avec un noyau R. Exécutez les cellules dans l'ordre. Dans Colab, utilisez un environnement adapté au langage du notebook.

### Stata

Les scripts utilisent les noms simples des fichiers `.dta`. Définissez le dossier courant sur `donnees/`, puis appelez les scripts :

```
cd "chemin/vers/la/seance/donnees"
do "../code/seance01.do"
do "../code/td01.do"
```

Remplacez `01` par `02` pour la séance 2. Stata doit pouvoir écrire dans le dossier courant pour créer ses fichiers et graphiques. Les versions Stata utilisent les données locales et n'exigent pas le paquet Python.

### Résultats et écarts entre langages

Les scripts affichent des statistiques descriptives, covariances, corrélations, estimations et tests, puis produisent les figures indiquées dans les commentaires de chaque bloc. Certains blocs ouvrent des fenêtres graphiques : fermez-les pour poursuivre. Les simulations fixent une graine, mais Python, R et Stata utilisent des générateurs différents ; les tirages et les dernières décimales peuvent donc différer. Comparez les résultats déterministes à une tolérance numérique raisonnable.

## Microéconomie III — Niort {#microeconomics}

La [page du cours](/teaching/microeconomics-iii-niort/) classe les documents en trois parties. Les simulateurs de la partie II concernent les jeux répétés et le jeu de pile ou face. La partie III associe le cours des séances V à VIII aux séries 5 à 8, aux suppléments, aux programmes Python/R et aux simulateurs de risque, de diversification, de portefeuille et d'information.

Après extraction du [dossier de la partie III](/files/teaching/niort/microeconomics-iii/part-3.zip) :

```
python -m pip install numpy pandas scipy matplotlib
python code/partie3.py
Rscript code/partie3.R
```

Les commentaires identifient chaque bloc et le calcul correspondant au support. Les figures sont écrites dans le dossier courant. Les simulateurs s'ouvrent directement dans le navigateur.

## Autres cours

- [Économétrie L3 à Cergy](/teaching/econometrics-l3/) : exercices par chapitre, collection de données avec dictionnaires et formats d'origine, puis anciens CC classés par année. Les documents « Answer key » sont les corrigés fournis ; leurs dates historiques restent celles de l'épreuve.
- [Data Analysis with Python](/teaching/data-analysis-l3/) : ouvrir le dossier du TD concerné, lire l'énoncé, puis exécuter son notebook dans l'ordre. Les imports et cellules d'installation précisent les dépendances et les modalités d'accès aux données propres au TD. Conserver les CSV et dictionnaires à leur place.
- [Homogeneity](/code-data/#homogeneity) : utiliser les [instructions Python/R propres à la note](/code-data/homogeneity-readme/), ainsi que les données de simulation et sorties associées.

## Résoudre les erreurs habituelles

- **Fichier introuvable** : vérifier le dossier courant et l'arborescence extraite.
- **Module absent** : installer le paquet dans le même environnement que celui qui exécute le programme ou le notebook.
- **Notebook R dans un noyau Python** : sélectionner le noyau R enregistré avec `IRkernel`.
- **Graphique non enregistré** : vérifier les droits d'écriture et le chemin de sortie indiqué dans le script.
- **Résultat différent** : vérifier les valeurs manquantes, l'échantillon, les unités, la version de la base et le caractère simulé du calcul.

Les exemples peuvent être adaptés à une autre base en modifiant explicitement les noms de variables, les formules et les chemins. Conservez une copie du programme et des données d'origine avant de modifier un exercice.
