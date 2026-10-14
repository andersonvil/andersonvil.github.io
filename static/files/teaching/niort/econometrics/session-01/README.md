# Économétrie — séance 1 (Niort, 2026–2027)

Ce dossier accompagne la première séance et la série d’exercices de 2026/2027. Il rassemble les supports, les scripts parallèles en Python, R et Stata, quatre notebooks, un simulateur HTML et sept jeux de données. Il s’agit de matériel pédagogique, pas d’un package de réplication d’article. La séance 2 annoncée n’est pas dans les fichiers fournis pour cette révision.

## Inventaire exact

- `seance01.pdf` : cours (142 pages).
- `serie01.pdf` : série d’exercices (6 pages).
- `code/seance01.py`, `code/seance01.R`, `code/seance01.do` : exemples du cours.
- `code/td01.py`, `code/td01.R`, `code/td01.do` : solutions et applications de la série.
- `notebooks/seance01_python.ipynb`, `notebooks/td01_python.ipynb` : versions Jupyter/Colab Python.
- `notebooks/seance01_R.ipynb`, `notebooks/td01_R.ipynb` : versions Jupyter R.
- `code/covariance_interactive.html` : simulateur autonome de covariance.
- `code/generer_dta.py` : script documentant la génération des fichiers Stata depuis `wooldridge`.
- `data/attend.dta`, `cps78_85.dta`, `econmath.dta`, `jtrain2.dta`, `phillips.dta`, `wagepan.dta`, `wine.dta` : données utilisées.

## Origine et chargement des données

Les sept jeux de données proviennent du paquet pédagogique `wooldridge`, correspondant aux exemples de *Introductory Econometrics* de Jeffrey M. Wooldridge. Le script `generer_dta.py` charge chaque table depuis le paquet Python puis l’exporte au format Stata 114. Les exemples Python chargent les données avec `wooldridge.data`; les exemples R les chargent depuis le paquet R. Les scripts Stata utilisent les noms simples des `.dta` : leur dossier de travail doit donc être `data/`.

## Dépendances

Python 3.10+ : `numpy`, `pandas`, `matplotlib`, `statsmodels` et `wooldridge`. Installation typique :

```text
python -m pip install numpy pandas matplotlib statsmodels wooldridge
```

R : une version récente de R et le paquet `wooldridge` (`install.packages("wooldridge")`). Pour les notebooks, installez aussi un noyau Jupyter R tel que `IRkernel`.

Stata : les fichiers `.dta` sont enregistrés au format 114, lisible par Stata 10 ou version ultérieure. Aucun module utilisateur n’est requis par les scripts fournis.

## Exécution par langage

1. Décompressez ou conservez l’arborescence complète.
2. Ouvrez un terminal dans le dossier `session-01`.
3. Python : exécutez `python code/seance01.py`, puis `python code/td01.py`.
4. R : exécutez `Rscript code/seance01.R`, puis `Rscript code/td01.R`.
5. Stata : définissez le dossier courant sur `session-01/data`, puis lancez `do "../code/seance01.do"` et `do "../code/td01.do"`. Les graphiques seront écrits dans ce dossier de travail. Cette procédure a été contrôlée dans le code, mais n’a pas été exécutée faute de Stata.

Les trois versions suivent la même séquence pédagogique : chargement, statistiques descriptives, covariance/corrélation, visualisations et régressions simples. Le rapport fourni par l’auteur documente des tests Python et R, mais aucun test Stata et aucun lancement du notebook R faute de noyau. Ce sont des vérifications fournies avec les sources, distinctes de celles de la présente révision.

Le 14 octobre 2026, `seance01.py` et `td01.py` ont été exécutés sans modification dans un environnement temporaire Python 3.13, avec `wooldridge==0.4.5`, pandas 3.0.5 et le backend graphique `Agg`. Les deux processus ont terminé avec le code 0. Les journaux et graphiques ont été conservés dans le dossier de vérification. Exemples contrôlés : la régression de la note sur `colgpa` donne 32,306 + 14,316 × `colgpa`, R² = 0,329 et n = 856; la somme des résidus est 0,0 et SCT = 153 539,3. R, Stata et les notebooks UCO n’ont pas été exécutés lors de cette révision. Aucune équivalence numérique entre langages n’est revendiquée.

## Sorties attendues

Les scripts affichent des dimensions et statistiques descriptives, des coefficients de covariance/corrélation et des résumés de régression. Ils produisent également des graphiques PDF ou PNG selon le langage et le script, notamment autour de la courbe de Phillips, des salaires et de l’éducation. Les valeurs exactes doivent être comparées entre langages à tolérance numérique raisonnable, pas par égalité de mise en page.

## Notebooks et Colab

- Python : importez un notebook dans Google Colab, téléversez les fichiers `.dta` si nécessaire, puis exécutez les cellules dans l’ordre. Les cellules d’installation peuvent installer `wooldridge` et `statsmodels`.
- R : ouvrez le notebook dans un environnement Jupyter muni d’un noyau R. Dans Colab, choisissez un environnement R si l’interface le propose ; un notebook R ne doit pas être exécuté avec le noyau Python par défaut.
- Localement : lancez `jupyter lab` depuis `session-01` afin que les chemins relatifs restent valides.

## Erreurs fréquentes et réutilisation

- « fichier introuvable » : vérifiez le dossier courant et la présence de `data/`.
- « module/package introuvable » : installez la dépendance correspondant au langage.
- notebook R sans noyau : installez et enregistrez `IRkernel`, puis sélectionnez ce noyau.
- graphique non créé : vérifiez les droits d’écriture dans le dossier courant.
- résultats différents : contrôlez le jeu de données chargé, les valeurs manquantes et la définition de l’échantillon.

Pour réutiliser les exemples avec une autre base, gardez les noms de variables attendus ou modifiez explicitement les formules. Citez la source des nouvelles données, documentez les transformations et ne diffusez aucune donnée restreinte.
