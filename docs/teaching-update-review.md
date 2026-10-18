# Mise à jour à approuver — 18 octobre 2026

Prévisualisation locale : [Teaching](http://127.0.0.1:1122/teaching/), [Code & Data](http://127.0.0.1:1122/code-data/), [Home](http://127.0.0.1:1122/), [Research](http://127.0.0.1:1122/research/).

Branche dédiée : `codex/teaching-resources-cv-editorial`. Aucun push, fusion dans `main`, lancement du workflow ou déploiement. La version publique attend l’approbation et une autorisation explicite de publication. Le workflow publie sur push vers `main` et sur lancement manuel ; GitHub Pages sert `gh-pages`.

## Changements

- Les cinq cours concernés proposent des fiches par chapitre, TD, séance ou partie. Supports et exercices restent visibles ; un seul volet regroupe les compléments. Les cours M1 et M2 & PhD conservent leur présentation.
- Econometrics L3 Cergy suit l’ordre exercices, CC par année, données. La collection complète reste en tête des données, suivie de 15 jeux distincts, chacun avec formats et archive regroupant tous ses fichiers aux noms d’origine.
- Data Analysis conserve TD01 à TD05. Les présentations TD02, TD03 et TD05 et le CSV de TD05 sont ajoutés ; les 19 fichiers des dossiers approuvés et les cinq ZIP étudiants sont synchronisés. TD01 ne comporte aucun support de présentation fourni. Les archives de travail contenant des corrigés ne sont pas publiées.
- Sciences Po conserve le support et les lectures de séance 1 ensemble. Niort conserve ses séances, les deux PDF visibles et le README. Microeconomics III conserve ses parties ; Part III présente le cours avant les séries 5 à 8, puis les suppléments et ressources pratiques.
- Code & Data garde ses sections techniques par cours et séance/partie, avec formats des données et variantes de programmes regroupés, sans duplication des fichiers entre parcours.
- Le CV actualisé est recopié sans réécriture depuis la source `04-cv-teaching/cv-academic.pdf`, générée le 18 octobre 2026 à 15 h 33. Son URL historique est conservée. Le build vérifie son empreinte avant de construire ; les 76 accès au CV servent le même fichier.
- Les trois statuts confirmés sont centralisés dans `data/academic.toml` et affichés dans Home, Research et les pages d’articles. Job Market Paper, numéros de working papers, abstracts, résumés et Work in progress sont préservés.

## Vérifications

Build Zola et configuration : PASS. Audit de 64 documents HTML et 1 873 liens/ressources locales : PASS. L’audit éditorial existant confirme les 148 copies suivies, 64 fichiers de données extraits, 9 CC et 2 corrigés.

L’inventaire de 185 fichiers sources est documenté dans `teaching-update-source-inventory.json` : 180 copies identiques, deux simulateurs avec leur enveloppe HTML déjà approuvée et leur contenu intact, trois archives de travail conservées hors publication. Les 175 URL déjà cataloguées sont préservées. Les 20 ZIP générés et leurs 83 membres sont vérifiés par empreinte et noms exacts.

Chrome : 80 contrôles de pages, 193 téléchargements vérifiés par empreinte, 56 contrôles clavier, aucun échec JavaScript. Thèmes clair/sombre à 1 440, 768, 390 et 360 px ; volets ouverts, focus, survol et absence de débordement horizontal contrôlés. Un ZIP de jeu de données est aussi téléchargé via son bouton réel. Les 47 PDF sont lisibles, dont un document source constitué d’images. Le CV récupéré depuis la prévisualisation est identique à la source actualisée.

Résultats synthétiques : `teaching-update-verification.json`. Empreintes des copies et ZIP : `teaching-update-manifest.json`. Rapports détaillés locaux : `tmp/teaching-update-qa/`. Procédure de maintenance et de relance de la preview : `guide-site.md` à la racine du dépôt.

## Captures des cours

| Cours | Desktop clair | Mobile sombre |
|---|---|---|
| Econometrics L3 — Cergy | [Capture](screenshots/teaching-update/econometrics-l3-1440-light.png) | [Capture](screenshots/teaching-update/econometrics-l3-390-dark.png) |
| Data Analysis | [Capture](screenshots/teaching-update/data-analysis-l3-1440-light.png) | [Capture](screenshots/teaching-update/data-analysis-l3-390-dark.png) |
| Microeconomics L1 — Sciences Po | [Capture](screenshots/teaching-update/microeconomics-l1-1440-light.png) | [Capture](screenshots/teaching-update/microeconomics-l1-390-dark.png) |
| Econometrics L3 — Niort | [Capture](screenshots/teaching-update/econometrics-niort-1440-light.png) | [Capture](screenshots/teaching-update/econometrics-niort-390-dark.png) |
| Microeconomics III — Niort | [Capture](screenshots/teaching-update/microeconomics-iii-niort-1440-light.png) | [Capture](screenshots/teaching-update/microeconomics-iii-niort-390-dark.png) |

Les cinq cours disposent aussi des captures desktop sombre et mobile clair dans le même dossier. Les captures isolent la section Resources, avec un volet représentatif ouvert ; les en-têtes fixes sont masqués uniquement pendant la prise de vue.
