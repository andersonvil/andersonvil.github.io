# Livraison privée finale — v7

Accès local : **http://127.0.0.1:1117/**. Branche : **`codex/final-private-v7`**. Point de retour avant intervention : **`codex/backup-before-final-v7`**, commit **`79d0288`**. L'état initial du dépôt était propre. Aucune fusion, aucun push et aucun déploiement n'ont été effectués.

Le serveur sert `public-preview-final/`, uniquement sur `127.0.0.1`. Les anciennes prévisualisations peuvent encore utiliser d'autres ports : le port de cette livraison est **1117**. Pour relancer, suivre `guide-site.md`. Le verrou de production vérifie explicitement `site_mode = "preview"` ; les métadonnées et les réponses HTTP gardent `noindex, nofollow, noarchive`.

## Corrections livrées

- Home : Research, six blocs de même hauteur sur desktop avec liens alignés, hauteur naturelle sur mobile. Retours permanents Back to Home sur les six pages principales et fil d'Ariane Home sur les pages de détail.
- Trois articles : View/Hide Abstract au clavier ; 19 blocs d'équations avec détails locaux à l'intérieur, sans glossaire global ; alignement à gauche ; accents KaTeX correctement groupés ; titres à largeur naturelle, titres de Research question au-dessus de la grille, espacement et séparation de Main findings corrigés.
- Références aux assumptions, propositions, corollary, équations, tableaux et figures reliées au manuscrit. Les plages couvrant des pages différentes ont plusieurs cibles ; les plages sur la même page gardent leur libellé d'origine. Légendes partagées, Open manuscript, erreurs-types dans les cellules et notes sous les tableaux concernés. Les tableaux descriptifs conservent leur distinction entre erreurs-types et écarts-types.
- Tous les remplacements scientifiques demandés, y compris A/B/C, sont intégrés. Photo `p7` dans Presentation du JMP. Nouveau PDF de l'offre de travail, daté du 16 octobre 2026, téléchargements et métadonnées actualisés. Notes internes retirées. Phrase de disponibilité du code de recherche corrigée.
- Enseignement : titres harmonisés, année 2026/2027, rôle Lecture & Tutorial conservé à UCO, retrait des notes de fabrication. Catalogue partagé entre Teaching et Code & Data, organisé par cours puis séance ou partie. Practical README HTML en français, avec environnement, commandes, dossiers complets, correspondance supports/exercices/code et consignes par séance.
- Insights : dernière version intégrale de Qu'est-ce qu'un modèle ?, sa note de bas de page et son retour ; note Homogeneity intégrale, October 2021, 289 formules, 27 numéros d'équations, deux tableaux et quatre figures, renvois internes et note éditoriale. Sources documentaires Homogeneity non proposées comme code. Ancienne Teaching Note archivée hors site.
- Beyond Economics : citation fournie, crédits vérifiés dans AFI/Cinémathèque, introduction demandée, restrictions des œuvres conservées. Suggested Books, If I Had 24 Hours Left et Echoes of Childhood dans cet ordre, enregistrements français, lecteurs natifs sans autoplay. Deux pistes AAC extraites sans réencodage des MP4 fournis, avec leurs images de présentation. Aucune nouvelle transcription ou date inventée.
- Dimensions réelles réservées pour les figures et la photo : les ancres ne se décalent plus après le chargement différé des images.

## Matériaux intégrés

- CY Cergy, économétrie L3 : sept sujets de CC (2017–18, 2018–19, 2019–20, 2021–22, 2022–23, 2023–24, 2024–25) et deux corrigés fournis (2022–23 et 2024–25), en conservant les dates des documents. Collection Dataset.zip et ses **64 fichiers** de données et dictionnaires dans leurs formats d'origine.
- UCO Niort, économétrie L3 : dossiers des séances 1 et 2, supports, TD, Python/R/Stata, notebooks, simulateurs, données et contrôles accompagnant les sources. Dossiers complets téléchargeables.
- Microéconomie III : partie III (séances V à VIII), séries 5–8, suppléments, programmes Python/R, quatre nouveaux simulateurs et dossier complet, avec conservation des parties I/II et de leurs simulateurs.
- Data Analysis L3 : compléments et dossiers complets TD01–TD05. Microéconomie L1 Saint-Germain : lectures accompagnant la séance.
- Homogeneity : quatre scripts Python, deux scripts R, requirements, données de simulation, sorties, figures et README HTML. Le ZIP de calcul exclut les sources de la note. Le README distingue la procédure de reproduction numérique des commandes de compilation documentaire figurant dans le README original.

Les **148 copies directes** de l'intégration et les **64 fichiers extraits** de Dataset.zip sont vérifiés octet pour octet. L'inventaire source → URL → empreinte est dans `docs/final-v7-resource-manifest.json`. Le catalogue compte les fichiers actuellement exposés, sans reprendre les anciens résumés de séance 1.

## Vérifications réellement effectuées

| Contrôle | Résultat et preuve |
|---|---|
| Build Zola et KaTeX | Réussi ; 623 rendus de formules, dépendance locale épinglée. `scripts/run-tests.ps1`. |
| Liens, médias, ancres et routes | 64 fichiers HTML inspectés, 1 861 liens/ressources locales valides, zéro cible ou ancre manquante ; `docs/final-v7-static-audit.json`. |
| Fidélité des ressources | 148 copies directes et 64 fichiers extraits vérifiés ; abstracts des trois articles et PDF comparés aux originaux. |
| Références scientifiques | 62 cibles déclarées de références/équations trouvées dans le texte de leur page physique ; `docs/final-v7-pdf-reference-audit.json`. Ce contrôle porte sur la fidélité des cibles, pas sur la validité scientifique des matériaux. |
| Python et notebooks | Syntaxe de 35 fichiers Python et structure de 17 notebooks vérifiées. Les programmes pédagogiques n'ont pas été réexécutés dans tous leurs environnements ; aucune exécution R ou Stata n'est revendiquée. |
| Navigateur réel | 192 combinaisons page × largeur × thème : 28 pages principales/détaillées et trois lecteurs de sources + 404, en 1440/768/390 px, clair et sombre. Aucun débordement global ni erreur KaTeX. `docs/final-v7-browser-audit.json`. |
| Interactions | Abstracts ouverts/fermés au clavier sur les trois articles ; un seul détail ouvert localement ; menu mobile/Escape, copie d'email, lecture/pause d'un nouvel audio, lecteur immersif/Escape et aller-retour de note de bas de page. Trois audios chargés, initialement en pause et sans autoplay. |
| Ancres après images | JMP Results & limits, Homogeneity Section 5 et ligne d'équation 23 contrôlés après réservation des dimensions, sous l'en-tête fixe. |
| HTTP | Home et téléchargements échantillonnés en 200, route inconnue en 404 personnalisée ; en-têtes de protection présents sur les deux types de réponse. |
| Protection de publication | Prévisualisation refusée par le contrôle de production ; fixture propre acceptée. La protection reste active. |
| État des modifications | `git diff --check` sans erreur d'espacement. Captures conservées dans `docs/screenshots/final-v7/`. |

Le lecteur PDF natif du navigateur intégré affiche une page vide dans l'essai effectué. L'ouverture directe à la page demandée n'est donc pas annoncée comme vérifiée dans ce lecteur. Les trois articles portent un libellé explicite et un lien vers le **lecteur HTML de secours**, dont les 45 pages physiques et les ancres sont vérifiées. Les fichiers PDF répondent bien en HTTP 200.

## Maintenance et archives

- Guide en français : **`guide-site.md`**, avec chemins, champs, composants, trois exemples concrets et commandes locales.
- Ancienne Teaching Note : **`docs/archive/pre-final-v7/insights/what-household-averages-can-hide.md`**.
- Anciens téléchargements synthétiques : **`docs/archive/pre-final-v7/demo-files/`**, hors arborescence servie.
- Les anciennes pages de démonstration et de laboratoire sont désactivées ; les redirections utiles sont conservées.

Aucun fichier nécessaire à ces intégrations n'est manquant. Aucune question éditoriale A/B/C ne reste en attente : les trois propositions étaient déjà approuvées. La publication attendra une nouvelle autorisation explicite.
