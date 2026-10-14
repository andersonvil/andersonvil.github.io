# Révision privée finale v5 — dossier de remise

Préparé le 14 octobre 2026. Cette livraison permet l’examen privé ; aucune publication publique, aucun push ni aucune fusion dans `main` n’ont été effectués. Les parties dépendant de fichiers absents sont indiquées ci-dessous.

## Accès, branche et sauvegarde

- Preview locale : **http://127.0.0.1:1114/**, servie uniquement sur la boucle locale. Les fichiers sont dans `public-preview`.
- Branche de travail : **`codex/final-private-v5`**.
- Référence récupérable avant intervention : **`codex/backup-before-final-v5-20261014`**, commit **`1603ce9268cf8f5fa5b6ee9804d3629f7d118052`**.
- Le dépôt était propre au départ. Aucun changement préexistant n’a été écrasé.
- Sauvegarde de la preview précédente : `C:\dev\andersonvil.github.io\tmp\final-v5\before-preview.zip`.
- Sauvegarde Git complète initiale : `C:\dev\andersonvil.github.io\tmp\final-v5\before-v5.bundle`.
- Sauvegarde des nouveaux inputs nécessaires, y compris notes internes TD04 : `C:\dev\andersonvil.github.io\tmp\final-v5\new-inputs.zip`. Aucun secret de configuration n’y est inclus.

Le serveur doit rester lancé pour l’accès par URL. Pour le relancer après fermeture, utiliser les commandes du README. La preview ne repose pas sur `noindex` comme contrôle d’accès : elle n’est pas hébergée publiquement.

Pour revenir à l’état sauvegardé, après avoir conservé tout travail ultérieur :

```powershell
git switch codex/backup-before-final-v5-20261014
```

Pour consulter uniquement les fichiers de l’ancienne preview sans modifier la branche, décompresser `before-preview.zip` dans un **nouveau** dossier local puis y lancer un serveur de boucle locale sur un autre port. Le bundle permet aussi de récupérer le commit initial si les références locales étaient perdues. Aucun reset forcé n’est nécessaire.

## Inventaire et plan exécuté

L’inventaire porte sur 107 fichiers sources disponibles : rôle, version retenue, chemin, taille, empreinte SHA-256 et, pour les PDF, page de titre. Il est livré dans `final-v5-source-inventory.json`. Les Markdown ont été lus intégralement. Les PDF ont été extraits et les pages scientifiques concernées rendues. Les dates imprimées et le contenu, et non la date de modification informatique, ont guidé la sélection.

Le travail a suivi quatre ensembles : sauvegarde et sélection des sources ; intégration éditoriale et mathématique ; présentation/interactions et ressources ; build, liens, responsive, contrôles visuels et préparation à la décision de publication. La stack Zola, les routes et les composants existants sont conservés.

## Résultat principal et premier écran

La Home conserve ses deux paragraphes exacts de bio, à 16,64 px CSS, justifiés et sans coupe. Le portrait a une ligne compacte d’icônes ; la mention opérationnelle de droits reste dans le registre interne. Le titre entier du JMP est lié. Son résumé exact demandé apparaît sur Home et Research. Les six accès sont en deux colonnes et trois rangées, dans l’ordre demandé, et s’empilent dans ce même ordre sur mobile.

Les captures `v1.png` et `v2.png` font 1908 × 908 et 1901 × 827 pixels. Elles ne contiennent pas les métadonnées du zoom ni la hauteur CSS effective du navigateur ; `v2` représente un écran déjà défilé. Aucune équivalence physique écran/pixels CSS n’est supposée. Le protocole de comparaison utilise **1908 × 908 CSS px, DPR 1, zoom/visualViewport.scale 1**, avant et après dans Chrome. Le badge privé reste visible.

| Mesure au même viewport | Avant | Après | Gain |
|---|---:|---:|---:|
| Hauteur du hero | 583,94 px | 466,23 px | 117,70 px |
| Début du JMP | 680,94 px | 563,23 px | 117,70 px |
| Bas des actions JMP | 940,50 px | 817,73 px | **122,77 px** |

Les actions sont donc visibles dans la hauteur de 908 px, avec environ 90 px restants. La variante de composition sans le bandeau privé place leur bas à 785,73 px, soit 32 px supplémentaires ; elle a été testée par masquage du bandeau pour la capture, sans changer le mode privé, les garde-fous ni l’état publié.

À 1908 × 768 CSS px, zoom 100 %, le bas des actions se trouve à 775,47 px : cette fenêtre reste trop basse de 7,47 px. Sur mobile, la bio et le portrait s’empilent et le JMP vient plus bas. Le texte, ses zones cliquables et ses titres n’ont pas été réduits pour promettre artificiellement une visibilité complète dans ces fenêtres.

Preuves : `screenshots/final-v5/before-home-1908-light.png`, `final-home-1908-light.png`, `after-home-1908-dark.png`, `future-public-layout-1908-light.png`, captures 1440/768/390/360 et `verification.json`.

## Matrice de couverture

« Réalisé / vérifié » indique une modification et un contrôle effectif. « Partiel » ou « absent » ne signifie pas que cette partie est terminée.

| Demande | Réalisation | Vérification / limite exacte |
|---|---|---|
| 1. Instructions, Git, stack, preview, déclencheurs | Réalisé | Aucun AGENTS.md trouvé dans les dépôts concernés ni leurs parents inspectés ; dépôt Zola propre ; workflow limité à `main`. Inventaire 107 sources. |
| 2. Branche, préservation et preview récupérable | Réalisé / vérifié | Branche dédiée, référence, bundle et deux archives ; pas de push ni publication. |
| Menu anglais exact, CV PDF, routes, secondaires et 404 | Réalisé / vérifié | Menu conservé ; 58 HTML contrôlés par le validateur, 59 avec l’audit des assets ; CV direct, Talks hors menu et 404 accessibles. |
| Thème mémorisé et sombre initial | Réalisé / vérifié | Test navigateur sans préférence puis persistance après rechargement. |
| Couleurs au survol et focus | Réalisé / vérifié | 116 contrôles de styles interactifs dans deux thèmes, tous positifs après corrections ; focus du lien d’évitement visible avec contour 3 px. Contrôles natifs préservés. |
| Section active, accordéons, copies, menus | Réalisé / vérifié | Scrollspy actif sur les trois articles ; symboles ouverts au clavier ; copies de citations/email ; menu mobile ; abstract Home. |
| Prose justifiée, langue et césure | Réalisé / vérifié | Prose ciblée, dernière ligne à gauche, hyphens auto, note/langue française explicite ; contrôle visuel desktop et mobile. Titres, métadonnées, équations et tableaux exclus. |
| Compacité Home et portrait | Réalisé / vérifié | Gain mesuré de 122,77 px jusqu’aux actions, pas de zoom réduit ni bio tronquée. |
| Six blocs Home, titre et résumé JMP | Réalisé / vérifié | Deux colonnes / trois rangées ; six sous-titres et Explore exacts ; titre entier cliquable ; ordre mobile testé. |
| Monogramme Editorial et favicon | Réalisé / vérifié | Contours tracés depuis la variante 4, courbes vectorielles, cercle et barre faible ; comparaison grande taille et header ; aucun remplacement par une police. |
| Research : résumé dédié, priorité, métadonnées | Réalisé / vérifié | Bio non dupliquée ; JMP du 4 octobre ; autres versions identifiées par titre. Statuts éditoriaux restent à confirmer avant publication. |
| Trois nouveaux textes et abstracts conservés | Réalisé / vérifié | Nouveaux Markdown complets ; blancs/instructions éditoriales non inventés ; abstracts comparés et préservés. |
| Sections orange, sous-titres actuels, TOC | Réalisé / vérifié | Style et tailles de sous-titres conservés ; libellés majuscules par CSS ; nom Econometric model & methods ; ancres et section active. |
| Trois fonctions de blocs et LIMITS | Réalisé / vérifié | Équation blanche/bandeau vert, explication/hypothèse existante, détails dépliables ; limites exactes des nouveaux textes, titre LIMITS. Bloc1/2/3 absents : langage des composants actuels réutilisé. |
| Moteur mathématique et parsing | Réalisé / vérifié | KaTeX 0.16.22 figé, fonts/styles locaux, HTML+MathML, mode strict ; minification Zola désactivée après défaut observé. 19 blocs d’articles capturés, note française rendue, symboles et overflow local contrôlés. |
| JMP : quatre objets et relations | Réalisé / vérifié | Notation canonique en indices, conditions linéaires et unités explicites ; aucune ancienne formule Unicode rendue. |
| JMP : Step 1 → figure → Step 2 → figure | Réalisé / vérifié | Slides 10/11 EEA originales, associations retrouvées dans le texte/LaTeX ; marges seules retirées du viewBox ; SVG agrandissables au clavier. |
| JMP : trois équations Stage 1 | Réalisé / vérifié | Après les erreurs normales, système complet avec censure et symboles, comparé p. 29 ; régime théorique distingué de l’estimation p. 32. |
| JMP : Stage 2 avec/sans SAT | Réalisé / vérifié | Deux systèmes séparés et définis ; SAT = Similarity Across Types ; indicatrice de parents distincte du facteur racine de n. |
| JMP : Tables 1/2 et provenance | Réalisé / vérifié | Valeurs du manuscrit d’octobre confrontées ; quatre valeurs de loisir corrigées ; Provider/Access/Exact data used en trois colonnes, sans hauteur forcée. |
| JMP : résultats et limites | Réalisé / vérifié | Texte/chiffres actuels, incertitude conservée ; source de chaque tableau et note de traçabilité. |
| Article britannique | Réalisé / vérifié | Nouveau texte ; demandes, partage, prix d’échelle et équation estimée complets ; tableaux, résultats, limites et cibles PDF vérifiés. |
| Offre de travail | Réalisé / vérifié | `paper_2.md` identifié par titre ; spécification quadratique finale, contrôles et participation explicites ; aucun transfert d’équation du JMP. |
| Insights : vraie note française | Réalisé / vérifié | Texte/langue/références conservés, sans fausse date ni étiquette synthétique ; ancien URL Methods redirigé ; Teaching Note reste signalée comme démonstration. |
| Teaching : introduction, niveaux, année, ordre | Réalisé / vérifié | Introduction exacte ; 2026/2027 partout ; Graduate M1/M2 & PhD et Undergraduate L1/L3 selon sources ; UCO Lecture & Tutorial ; 06 sans multilingual ; photo/légende Stata 2021 conservées. |
| Data Analysis TD01–TD05 | Réalisé / vérifié | Énoncés et notebooks réels ; slides TD04 ; CSV/dictionnaire TD03 ; mêmes liens depuis Teaching et Code & Data, fichiers uniques. |
| Slides TD01/02/03/05 | Absents / traités honnêtement | Presentation slides forthcoming, sans faux lien. Seul TD04 dispose d’un support de présentation PDF fourni. |
| Corrigés / notes internes TD04 | Partiel | `td04-notes-td.pdf` porte « document interne » ; sauvegardé hors téléchargements, diffusion à confirmer. Notebooks d’exercice à compléter, pas présentés comme corrigés. Aucun corrigé étudiant séparé fourni pour TD04/05. |
| HTML fourni dans TD05 | Partiel / source conservée | Il s’agit du programme général L3 et d’un guide pédagogique, pas de slides TD05. Ses liens configurés visent des fichiers non fournis ; original conservé en documentation interne, pas exposé comme ressource fonctionnelle. |
| UCO séance 1 et README | Réalisé / vérifié | Supports/code/data/notebooks/simulateur préservés ; README corrigé pour les chemins Stata et les dépendances ; Python cours et TD exécutés avec code 0, journaux livrés. |
| UCO séance 2, TD, codes/programmes/données | **Absent / bloqué par source** | Aucun `seance02`, `serie02`, `td02` ni code/donnée de séance 2 trouvé dans `Niort/2026-27/Econométrie` ou ailleurs dans les sources inventoriées. Attendu : support de séance 2, TD correspondant et leur dossier code/notebooks/données. |
| Code & Data : fichiers et usages | Réalisé / vérifié | Descriptions pédagogiques concrètes et cours liés ; cinq TD ; aucune réplication d’article revendiquée ; fixture synthétique séparée et bloquée. |
| Contact : introduction, Email me, email/copie | Réalisé / vérifié | Texte exact ; adresse à 18 px desktop, sans overflow mobile ; mailto et copie testés. |
| Rendez-vous | Réalisé / vérifié selon configuration disponible | Aucune URL de réservation ou intégration autorisée trouvée ; simulation retirée ; Request a meeting ouvre un vrai mailto prérempli. Aucune réservation ni invitation envoyée. |
| Beyond Economics : droits, œuvres et audio | Réalisé / vérifié | Confirmation enregistrée, avertissements de droits retirés, crédits gardés ; choix intégral/extraits et seul accompagnement Calligraphie préservés ; audio testé, sans autoplay. |
| Podcast et médias | Réalisé / partiel | Fichiers/lecteur/proportions contrôlés ; droits confirmés ; transcription machine et métadonnées de l’enregistrement restent à revoir, sans blocage de droits inventé. |
| Responsive deux thèmes | Réalisé / vérifié | 1908, 1440, 768, 390 et 360 px ; 170 cas, pas d’overflow horizontal global ; images et tableaux/formules consultables. |
| Liens scientifiques | Réalisé / vérifié | 22 intitulés sur les pages physiques exactes ; 36 renders de source disponibles ; alternative locale testée. Le comportement de tous les lecteurs PDF externes n’est pas garanti. |
| Build, tests, téléchargements, redirections | Réalisé / vérifié | Build Zola, validateur, audit de liens/assets, copies sources, fixture/notebook démo ; redirect Methods, secondaires et 404. |
| `last_version.mp4` | Absent | Non retrouvé dans les sources ni dans la recherche de noms de fichiers. Les deux captures fournies et les snapshots avant/après servent à la comparaison. |
| Préparation du futur public | Réalisé / toujours désactivé | Bandeau conditionné au mode preview ; variante visuelle testée ; garde de production conservée et ajouté avant déploiement GitHub. Démos à exclure, liste ci-dessous. |

## Tests exécutés et limites

- Build Zola final via `scripts/build-preview.ps1`, incluant rendu strict KaTeX et assets locaux.
- `validate-site.ps1` : 58 HTML contrôlés, PASS. Le validateur traite désormais les entités HTML et l’ordre des attributs sans dépendre de la minification.
- `audit-final-v5.py` : 1409 références locales de liens/assets, 59 pages HTML, zéro cible ou ancre cassée ; trois abstracts et empreintes PDF conservés ; syntaxe des scripts/notebooks Python et structure des notebooks contrôlées.
- `validate-source-copies.ps1` : 29 copies existantes byte-identiques aux sources, PDFs complets des poèmes restreints absents.
- `test-reading-times.ps1`, fixture de code synthétique et son notebook : PASS. La note réelle française affiche huit minutes sans date inventée.
- Chrome/Playwright : 170 cas de responsive, 19 blocs d’articles capturés, contrôles des disclosures/clavier/scrollspy/copies/menu/audio/redirect, focus d’évitement et 116 contrôles de survol. En complément, 10 contrôles de la dernière version Code & Data et 24 contrôles des encadrés d’hypothèses dans les deux thèmes ont passé. Les défauts constatés ont été corrigés puis les contrôles concernés relancés.
- Python UCO : `seance01.py` et `td01.py`, inchangés, exécutés avec Python 3.13 et backend Agg, codes 0 ; dix graphiques PDF générés dans le dossier de travail temporaire. Journaux dans `final-v5-teaching-execution`.
- Garde de production : rejet attendu du dépôt preview ; acceptation de la fixture propre. Aucun build/deploiement public effectué. Le nouveau step GitHub n’a pas été exécuté sur un runner distant.

R et Stata n’ont pas été exécutés dans cette révision. Les tests antérieurs fournis par l’auteur sont identifiés comme tels. Les notebooks étudiants Data Analysis comportent des placeholders intentionnels ; leur exécution complète sans réponses n’est pas revendiquée. Aucun appareil physique ni lecteur d’écran externe n’a été testé ; les dimensions CSS, les interactions au clavier et le MathML ont été contrôlés. Les API/data URLs des notebooks et les sites de profils/fournisseurs n’ont pas tous fait l’objet d’une nouvelle vérification réseau : leur stabilité externe n’est pas promise.

## Fichiers et preuves

- `final-v5-source-inventory.json` : inventaire, versions, rôles et SHA-256.
- `final-v5-scientific-trace.md` : équations, statistiques corrigées, conventions et passages scientifiques à revoir.
- `final-v5-scientific-link-audit.json` : 22 cibles scientifiques vérifiées.
- `final-v5-math-manifest.json` : inventaire des formules rendues.
- `final-v5-static-audit.json`, `final-v5-notebook-audit.json` : audits locaux et notebooks.
- `screenshots/final-v5/verification.json` : mesures, responsive et interactions.
- `screenshots/final-v5/source-viewer-verification.json` : tests de la visionneuse, PDF et overflow local.
- `screenshots/final-v5/math-sheet-*.png` : revue visuelle des 19 blocs.
- `screenshots/final-v5/monogram-reference-result.png` : référence à gauche, tracé vectoriel à droite.
- `docs/editorial-sources` : originaux éditoriaux et guide général Data Analysis non exposé.

## Décision de publication : éléments restants

Les contenus réels prêts à être examinés comprennent la bio, les trois nouvelles pages d’articles, les manuscrits retenus, la note française, les ressources pédagogiques présentes et Beyond Economics avec ses droits confirmés. Leur présence en preview ne vaut pas autorisation générale de publication.

Avant votre décision :

1. Fournir la séance UCO 2 avec son TD et ses fichiers de reproduction ; fournir ultérieurement les slides manquantes des TD et décider de l’accès aux notes/corrigés.
2. Relire les passages scientifiques signalés dans la traçabilité, les statuts de revue et les identifiants exacts des extractions de données ; confirmer le CV final et les crédits/droits des portraits et de la photo de cours, indépendamment des droits désormais confirmés de Beyond Economics.
3. Revoir la transcription machine du podcast et ses métadonnées non fournies.
4. Exclure de la future production la Teaching Note synthétique, les Talks fictifs, les laboratoires de design/preview, le cours synthétique et le projet de données fictives. La vraie Methods Note ne fait pas partie de ces exclusions.
5. Si une réservation directe est souhaitée : fournir une URL prête à l’emploi, ou le service/compte autorisé, la durée, les plages de disponibilité, le délai de prévenance et l’agenda de référence. Sans cela, la demande par email reste fonctionnelle.

La prochaine étape est votre examen de cette preview. Une publication ne pourra intervenir que dans une tâche distincte explicitement autorisée.
