# Guide pratique de maintenance du site

Le dépôt du site est **`C:\dev\andersonvil.github.io`**. Tous les chemins ci-dessous sont relatifs à cette racine, sauf mention contraire. Les documents originaux se trouvent dans **`C:\dev\andersonvil-academic-os-sources`**. Le site utilise **Zola**, des templates **Tera**, des données **TOML/JSON**, du Markdown et un rendu mathématique **KaTeX à la construction**. Il n'y a pas de serveur applicatif ni de base de données.

La version éditoriale approuvée est `codex/final-private-v7`, commit `ad690757b16cf724453cc072c40989b8e4b2cb4d`. Sa mise en production est préparée sur `codex/publish-final-v7`, puis intégrée dans `main`. Les contenus approuvés et les restrictions par extrait des poèmes sont conservés.

## Publication et futures mises à jour

Le site public est **https://andersonvil.github.io/**. GitHub Pages utilise toujours **`gh-pages`, dossier `/`**, en mode de construction historique (`legacy`), avec HTTPS obligatoire. Le dépôt reste public ; aucun changement d'hébergement ou de visibilité n'est nécessaire.

La source est `main`. À chaque push sur `main` (ou lancement manuel de **Build and deploy GH Pages**), `.github/workflows/main.yml` effectue `npm ci`, `npm run editorial`, contrôle la configuration, construit avec **Zola 0.19.2** et vérifie les fichiers construits. Node.js 22 et Ubuntu sont utilisés dans GitHub Actions. L'action Zola est épinglée à `960304368c31a93d6991d64ae962df0786b905f1` et s'exécute uniquement en mode build.

Seul le contenu de `public/` est copié vers `gh-pages`, avec `.nojekyll` et `deployment-commit.txt` qui identifie le commit source réellement servi. Le workflow crée un commit normal, pousse sans force et demande explicitement un build Pages. Les guides, archives de travail, sources éditoriales, scripts et configurations ne sont pas copiés. Les archives ZIP pédagogiques et les programmes proposés au téléchargement restent publics.

Les routes de test `content/preview-synthetic/**` sont exclues par `ignored_content`. La redirection utile `reading-model-based-estimates` a été déplacée dans `content/redirects`. Les pages courantes portent `index, follow`, les redirections et la page 404 conservent un `noindex` technique ; `robots.txt` autorise l'indexation et annonce le sitemap. Les URL canoniques et Open Graph utilisent l'adresse HTTPS définitive.

Pour une mise à jour : modifier la source indiquée dans ce guide, exécuter `npm ci` si nécessaire, puis `powershell -ExecutionPolicy Bypass -File scripts/build-production.ps1` et `python scripts/audit-final-v7.py --build-dir public-production`. Le second audit compare aussi les fichiers aux originaux présents sur cet ordinateur ; il n'est pas requis sur le runner GitHub. Le contrôle portable `scripts/audit-production.py` est exécuté localement et en CI. Faire un commit, intégrer dans `main` et pousser avec `git push origin main`. Suivre **les deux étapes** : workflow de construction, puis build/déploiement Pages. Enfin comparer `https://andersonvil.github.io/deployment-commit.txt` avec `git rev-parse main` et ouvrir le site public.

Contrôles Chrome reproductibles : définir `CODEX_NODE_MODULES` sur le répertoire de paquets du runtime local, servir `public-production` avec `scripts/serve-preview.py --directory public-production --port 1119`, puis exécuter `node scripts/verify-production.js http://127.0.0.1:1119`. Pour contrôler la version publique : `node scripts/verify-production.js https://andersonvil.github.io COMMIT_SOURCE_COMPLET`. Les rapports et captures sont écrits dans `tmp/publication-qa/`, hors publication.

## Prévisualisation de la mise à jour pédagogique du 18 octobre 2026

La branche dédiée est `codex/teaching-resources-cv-editorial`. **Cette mise à jour attend approbation et autorisation explicite de publication.** Ne pas fusionner/pousser sur `main`, pousser sur `gh-pages` ou lancer le workflow manuellement. Le workflow `.github/workflows/main.yml` publie lors d’un push sur `main` ou de `workflow_dispatch`. Aucun push ni déploiement n’est nécessaire à la prévisualisation locale.

```powershell
python scripts/sync-teaching-update.py
powershell -ExecutionPolicy Bypass -File scripts/build-preview.ps1 -OutputDirectory public-preview-update
python scripts/audit-production.py --build-dir public-preview-update
python scripts/audit-teaching-update.py --build-dir public-preview-update
python scripts/serve-preview.py --directory public-preview-update --port 1122
```

Ouvrir `http://127.0.0.1:1122/teaching/`. Le serveur écoute uniquement en local et ajoute `X-Robots-Tag: noindex, nofollow, noarchive`. La sortie `public-preview-update/` est ignorée par Git. Les builds conservent la configuration publique pour contrôler fidèlement les routes finales ; construire localement ne publie rien.

`data/teaching_resources.toml` reste le catalogue commun des fichiers. `scripts/sync-teaching-update.py` copie les sources approuvées, actualise le catalogue et produit `data/course-resource-views.json`, les regroupements destinés aux deux vues. Exécuter ce script après une modification de classement ou de source, puis reconstruire. `templates/macros/learning-resources.html` présente les fiches pédagogiques dans Teaching et les catégories techniques dans Code & Data. Les cours M1 et M2 & PhD utilisent désormais les mêmes fiches `learning-resources.html` que les autres cours.

- Teaching : support, exercices, puis un seul volet de lectures/ressources pratiques. Séances de Niort et parties de Microeconomics III conservées ; support de Part III avant les séries 5 à 8.
- Econometrics L3 Cergy : chapitres, CC regroupés par année, collection de données, puis 15 fiches de jeux. Les ZIP sous `static/files/teaching/cy-cergy/l3-econometrics/data/bundles/` sont construits par **dossier exact** de `Dataset/`, noms et octets originaux préservés. La collection `Dataset.zip` reste intacte. Les formats individuels restent téléchargeables dans Code & Data.
- Data Analysis : les dossiers `CY-Cergy-Paris-Université/2026-27/L3-Data Analysis/TD #1` à `TD #5` alimentent les mêmes fichiers dans les deux parcours, sans double copie. Les archives de travail `TD #1.zip` à `TD #3.zip`, qui contiennent des corrigés et des sources LaTeX, ne sont pas publiées. Les ZIP étudiants sont reconstruits à partir des dossiers approuvés, sans notebooks corrigés. Présentations TD02/03/05 et données TD05 ajoutées ; supports existants actualisés à la même URL.
- Sciences Po : support de séance 1 suivi des lectures associées dans le même volet ; archive de lectures conservée.
- Code & Data : cours et séances/parties gardés, avec catégories supports/exercices/lectures/notebooks/codes/données/simulateurs et variantes d’un programme ou d’un jeu regroupées. README et téléchargements individuels conservés.

`docs/teaching-update-manifest.json` conserve les empreintes des copies et de tous les membres des ZIP générés. `scripts/audit-teaching-update.py` contrôle l’inventaire par rapport à `main`, les sources, les ZIP, les deux parcours, les exceptions M1/M2, le CV et les statuts. `scripts/verify-teaching-update.js` contrôle le navigateur et produit les captures dans `docs/screenshots/teaching-update/` et le rapport dans `tmp/teaching-update-qa/`.

Les statuts confirmés des trois articles sont centralisés dans `data/academic.toml`, clés `papers[].status_label`. Home, Research et les pages individuelles affichent ces mêmes valeurs ; modifier uniquement ces clés. Résumés, abstracts, numéros de working papers et Work in progress restent leurs contenus approuvés.

## Sauvegarde de l'ancien site et retour arrière

La sauvegarde privée locale complète est **`C:\dev\andersonvil-academic-os-sources\site-backups\2026-10-16-before-publication`**. Elle contient `old-site-gh-pages.zip` (62 fichiers, ressources comprises), `old-source-main.zip`, `recovery.bundle` et `site/`. La copie consultable utilise des chemins locaux pour les liens et ressources du même domaine ; le ZIP conserve les octets originaux. Elle n'est pas servie par le site public.

Pour la consulter :

```powershell
python -m http.server 1120 --bind 127.0.0.1 --directory 'C:\dev\andersonvil-academic-os-sources\site-backups\2026-10-16-before-publication\site'
```

Ouvrir `http://127.0.0.1:1120/`, puis Ctrl+C pour arrêter. Références récupérables : ancien site **`a55c31ce72bb389ed8746593b0899634a6dde9ef`** (`archive/public-site-before-20261016`), ancienne source **`1e8f05a78127e1441927f1652876c28b3d4049a1`** (`archive/main-before-publication-20261016`). Le bundle conserve ces références et la version approuvée ; `git bundle verify CHEMIN\recovery.bundle` permet de vérifier son intégrité.

Pour rétablir exactement l'ancien site, sans force-push ni effacement d'historique, utiliser un nouveau clone propre **hors de l'arborescence publiée** :

```powershell
git clone --single-branch --branch gh-pages https://github.com/andersonvil/andersonvil.github.io.git C:\dev\andersonvil-academic-os-sources\site-backups\rollback-gh-pages
Set-Location C:\dev\andersonvil-academic-os-sources\site-backups\rollback-gh-pages
git restore --source=a55c31ce72bb389ed8746593b0899634a6dde9ef --staged --worktree -- .
git commit -m 'Restore public site before 2026-10-16 publication'
git push origin HEAD:gh-pages
gh api --method POST repos/andersonvil/andersonvil.github.io/pages/builds
gh api repos/andersonvil/andersonvil.github.io/pages/builds/latest
```

Attendre `status: built`, puis vérifier le site public. Ce retour restaure les fichiers de publication par un nouveau commit et conserve la source moderne sur `main` pour correction. Éviter tout nouveau push sur `main` tant que la correction n'est pas prête, puisqu'il republierait cette source. Ne pas réactiver l'ancien workflow de la sauvegarde : son action utilisait un force-push.

## Trouver le texte à modifier

Un fichier `content/...md` peut définir seulement la route, le titre et le template. Les textes visibles peuvent provenir d'un fichier `data/` ou d'un template : le tableau indique la source réelle.

| Route et titre | Fichiers sources à éditer | Contenu éditable et précautions |
|---|---|---|
| `/` — Anderson Vil | `content/_index.md`, `templates/home.html`, `data/academic.toml` | `identity.name`, `identity.job_market`, `identity.fields`, `identity.bio` : identité et biographie. Dans `home.html` : libellé Research, six accès, boutons et intertitres. Les cartes de recherche utilisent `papers` dans le TOML. |
| `/research/` — Research | `content/research/_index.md`, `templates/research.html`, `data/academic.toml` | Introduction dans le template ; `papers[].title`, `description`, `author_line`, `series`, `version`, `abstract`, `pdf`, `url` pour les cartes. L'ordre du tableau `papers` est l'ordre d'affichage. |
| `/research/household-demand-leisure/` — JMP | `content/research/household-demand-leisure.md`, `docs/editorial-sources/paper_jmp.md`, `data/editorial-supplements.json`, `data/equation-details.json`, `data/academic.toml` | Paragraphes, sous-titres et tableau des résultats dans le Markdown, neuf sections `##` à conserver. Métadonnées et abstract dans l'entrée `papers` d'id `household-demand-leisure`. Ajouts d'équations, slides et explications dans `replacements` du supplément ; définitions locales dans le tableau du même id d'`equation-details.json`, dans l'ordre des blocs. |
| `/research/children-costs-one-adult-household/` — Children Costs in a One-Adult Household | `content/research/children-costs-one-adult-household.md`, `docs/editorial-sources/paper_solo.md`, `data/editorial-supplements.json`, `data/equation-details.json`, `data/academic.toml` | Même fonctionnement ; id `children-costs-one-adult-household`. La source de l'article britannique est bien `paper_solo.md`. Ne pas intervertir avec `paper_2.md`. |
| `/research/children-labor-supply-equations/` — How to Incorporate Children into Labor Supply Equations? | `content/research/children-labor-supply-equations.md`, `docs/editorial-sources/paper_2.md`, `data/editorial-supplements.json`, `data/equation-details.json`, `data/academic.toml` | Même fonctionnement ; id `children-labor-supply-equations`. La légende du diagramme demandé est dans `templates/paper-final.html` (condition sur cet id), pour éviter sa répétition dans le texte. |
| `/research/household-demand-leisure/sources/` — Manuscript source pages | `content/research/household-demand-leisure-sources.md`, `data/editorial-supplements.json`, `scripts/prepare-source-pages.py`, `templates/source-pages.html` | `references` associe le nom de chaque référence à la page **physique** du PDF ; `equation_pages` associe les numéros d'équation aux pages. Les légendes génériques sont dans le template. Ne pas retoucher les images générées. Leurs dimensions sont réservées dès le chargement pour stabiliser les ancres. |
| `/research/children-costs-one-adult-household/sources/` — Manuscript source pages | `content/research/children-costs-one-adult-household-sources.md` et mêmes fichiers partagés ci-dessus | Même procédure pour le manuscrit britannique. |
| `/research/children-labor-supply-equations/sources/` — Manuscript source pages | `content/research/children-labor-supply-equations-sources.md` et mêmes fichiers partagés ci-dessus | Même procédure pour l'offre de travail. |
| `/teaching/` — Courses and teaching materials | `content/teaching/_index.md`, `templates/teaching-v7.html`, `data/teaching_resources.toml`, `data/teaching_photo.toml` | Introduction et principes dans le template ; titres, descriptions et ressources dans `groups`. `course_order` dans le template détermine l'ordre des cours. Photo et légende dans `teaching_photo.toml`. |
| `/teaching/econometrics-l3/` — Econometrics — L3, Cergy | `content/teaching/econometrics-l3.md`, `data/teaching_resources.toml`, groupe `econometrics-l3` | `title`, `summary`, `topics`, `institution`, `role`, `level`, `academic_year`, `language`, `parts`. Chaque `groups.resources` contient `label`, `file`, `format`, `part`, éventuellement `pages` et `bytes`. CC dans `static/files/teaching/cy-cergy/l3-econometrics/previous-cc/`, données dans `.../data/`. Conserver les années historiques des CC et distinguer sujet/corrigé. |
| `/teaching/econometrics-m1/` — Econometrics — M1 | `content/teaching/econometrics-m1.md`, `data/teaching_resources.toml`, groupe `econometrics-m1` | Mêmes clés ; fichiers dans `static/files/teaching/cy-cergy/m1-econometrics/`. |
| `/teaching/econometrics-m2/` — Econometrics — M2 & PhD | `content/teaching/econometrics-m2.md`, `data/teaching_resources.toml`, groupe `econometrics-m2` | Mêmes clés ; fichiers dans `static/files/teaching/cy-cergy/m2-econometrics/`. |
| `/teaching/data-analysis-l3/` — Data Analysis with Python — L3 | `content/teaching/data-analysis-l3.md`, `data/teaching_resources.toml`, groupe `data-analysis-l3` | Mêmes clés ; `part` vaut TD01 à TD05. Fichiers dans `static/files/teaching/cy-cergy/l3-data-analysis/td-01/` à `td-05/`. Les présentations fournies de TD02 à TD05 sont accessibles avant les énoncés ; TD01 ne comporte pas de présentation fournie. |
| `/teaching/microeconomics-l1/` — Microeconomics — L1 | `content/teaching/microeconomics-l1.md`, `data/teaching_resources.toml`, groupe `microeconomics-l1` | Mêmes clés ; supports et lectures dans `static/files/teaching/sciences-po-saint-germain/microeconomics-l1/`. Garder le niveau détaillé dans `level`. |
| `/teaching/econometrics-niort/` — Econometrics — L3, UCO | `content/teaching/econometrics-niort.md`, `data/teaching_resources.toml`, groupe `econometrics-niort` | Mêmes clés ; les séances restent dans ce cours. `parts` et `resources[].part` classent les séances 1 et 2. Fichiers dans `static/files/teaching/niort/econometrics/session-01/` et `session-02/`, archives `.zip` voisines. Garder `Lecture & Tutorial` et l'année utile. |
| `/teaching/microeconomics-iii-niort/` — Microeconomics III — L3 | `content/teaching/microeconomics-iii-niort.md`, `data/teaching_resources.toml`, groupe `microeconomics-iii-niort` | Mêmes clés ; trois parties. Partie III dans `static/files/teaching/niort/microeconomics-iii/part-3/`, anciens supports et simulateurs dans le dossier parent. |
| `/teaching/practical-readme/` — Utiliser les supports, les données et les programmes | `content/teaching/practical-readme.md`, `templates/practical-readme.html` | Le corps du Markdown est la source du README HTML, en français. Conserver l'ancre `microeconomics`. Les commandes doivent suivre les vrais noms des dossiers téléchargés. Les anciens téléchargements Markdown sous `static/files/teaching/niort/econometrics/` sont secondaires. |
| `/code-data/` — Teaching code, notebooks, and data | `content/code-data/_index.md`, `templates/code-data-v7.html`, `data/teaching_resources.toml` | Introduction et section `homogeneity` dans le template ; les cours utilisent **le même catalogue** que Teaching. Modifier le catalogue une seule fois met à jour les deux parcours. La section Homogeneity pointe vers `static/files/code/homogeneity/python/`, `R/`, `data/`, `output/`. |
| `/code-data/homogeneity-readme/` — Homogeneity — Practical README | `content/code-data/homogeneity-readme.md`, `docs/editorial-sources/homogeneity-readme.md`, `templates/practical-readme.html` | La page Markdown contient la procédure du téléchargement et le README fourni. La copie dans `docs/` conserve l'original. Le téléchargement exclut `note/` : les commandes de construction documentaire et les vérifications documentaires du script complet nécessitent les sources originales. |
| `/insights/` — Notes on economic methods and teaching | `content/insights/_index.md`, `templates/insights.html` | Introduction, deux cartes, résumés et libellés dans le template. Les métadonnées des pages individuelles restent dans leurs sources respectives. |
| `/insights/quest-ce-quun-modele/` — Qu'est-ce qu'un modèle ? | `content/insights/quest-ce-quun-modele.md`, `docs/editorial-sources/note_modele.md`, `templates/model-note.html`, `scripts/build-editorial.js` | Texte intégral et références dans le Markdown de `docs/`. Titre et durée dans le fichier `content`. Le titre porte l'appel `model-ref-1` ; la définition `[^1]:` est déplacée en bas de page par le générateur, avec id `model-note-1` et retour. Garder la syntaxe de la note. |
| `/insights/homogeneity/` — Imposing and Testing Zero-Degree Homogeneity in Demand | `content/insights/homogeneity.md`, `docs/editorial-sources/homogeneity.md`, `templates/homogeneity.html`, `scripts/build-editorial.js` | Texte intégral dans `docs/`, date **October 2021** et note éditoriale dans le template. Figures dans `static/img/insights/homogeneity/`. Les dimensions des PNG sont lues automatiquement par le générateur. Les numéros de section, figure, tableau et équation produisent les ancres `section-N`, `figure-N`, `table-N`, `equation-N`. Ne pas modifier une numérotation sans actualiser ses renvois. |
| `/beyond-economics/` — Poetry and conversations | `content/beyond-economics/_index.md`, `templates/beyond-economics.html`, `data/beyond_economics.toml` | Citation, crédits et introduction dans le template. Cartes des œuvres dans `poems`, audios dans `audios` : leur ordre dans ce tableau est leur ordre d'affichage. |
| `/beyond-economics/calligraphie-de-bonheur/` — Calligraphie de bonheur | `content/beyond-economics/calligraphie-de-bonheur.md`, `data/beyond_economics.toml` (`poems`, id correspondant), `data/poetry_audio.toml` | Le texte complet est dans le corps Markdown. Métadonnées, `signature_line`, fond, police et `access` dans le TOML. L'audio et son attribution sont dans `poetry_audio.toml`. |
| `/beyond-economics/ma-cherie/` — Ma chérie | `content/beyond-economics/ma-cherie.md`, mêmes données partagées | Le Markdown contient seulement l'extrait autorisé. Ne pas y recopier l'œuvre entière ; conserver `access = "first-stanza"`. |
| `/beyond-economics/mes-nuits/` — Mes nuits | `content/beyond-economics/mes-nuits.md`, mêmes données partagées | Même précaution pour l'extrait ; date et signature dans le TOML. |
| `/beyond-economics/on-me-l-a-interdit/` — On me l'a interdit | `content/beyond-economics/on-me-l-a-interdit.md`, mêmes données partagées | Même précaution pour l'extrait. Les PDF complets des œuvres restreintes ne sont pas servis. |
| `/beyond-economics/on-reading-preferences/` — Suggested Books | `content/beyond-economics/on-reading-preferences.md`, `data/beyond_economics.toml`, entrée `audios` id `on-reading-preferences` | Modifier `title`, `description`, `audio`, `poster`, `language`, `url` dans `audios` ; le template actif est `templates/media-work.html`. L'ancienne table `[media]` est conservée pour historique et n'alimente plus ces pages. |
| `/beyond-economics/if-i-had-24-hours-left/` — If I Had 24 Hours Left | `content/beyond-economics/if-i-had-24-hours-left.md`, `data/beyond_economics.toml`, entrée `audios` correspondante | Fichier `static/files/media/if-i-had-24-hours-left.m4a`, image `static/img/if-i-had-24-hours-left-poster.jpg`. Pas de transcription ni de date ajoutée. |
| `/beyond-economics/echoes-of-childhood/` — Echoes of Childhood | `content/beyond-economics/echoes-of-childhood.md`, `data/beyond_economics.toml`, entrée `audios` correspondante | Fichier `static/files/media/echoes-of-childhood.m4a`, image `static/img/echoes-of-childhood-poster.jpg`. Même lecteur partagé et absence d'autoplay. |
| `/contact/` — Start a conversation | `content/contact.md`, `templates/contact-v7.html`, `data/academic.toml` | Introduction, intitulés des rôles et demande de rendez-vous dans le template. Email dans `identity.email`, profils dans `profiles`. Le bouton rendez-vous a aussi un lien `mailto:` dans le template à actualiser si l'adresse change. |
| `/404.html` — Page Not Found | `templates/404.html` | Titre, message et liens de secours. Le serveur `scripts/serve-preview.py` utilise ce fichier pour les URL inconnues et renvoie HTTP 404. |
| Menu, en-tête, pied de page, thème et retours | `config.toml`, `templates/base.html`, `templates/macros/contextual-return.html`, `templates/macros/context-back.html`, `static/styles/site.css`, `static/js/site.js` | Menu dans `extra.menu`, titre/description dans la configuration ; bandeau et footer dans `base.html`. CSS : couleurs, dimensions, survol, focus et responsive. JS : menus, thème enregistré, abstract, détails locaux, copie, lecteur immersif et audio. Les retours Home sont permanents sur les pages principales. |

## Composants partagés et ressources scientifiques

- `templates/paper-final.html` rend les **trois articles** : navigation latérale, abstract, titres, diagramme, documents et phrase de disponibilité du code. La section photo `presentation` est conditionnelle au JMP ; son image est `static/img/presentation/michigan-jmp.jpg` et sa légende est dans ce template.
- `templates/paper-tables.html` porte les légendes des tableaux descriptifs et la première table de childcare du JMP. Les chiffres et en-têtes des autres tableaux descriptifs sont dans `data/academic.toml`, clés `data_table_caption`, `data_table_headers`, `data_table_rows`. Les tableaux de résultats et leur note d'erreurs-types sont dans les trois Markdown scientifiques.
- `data/research_resources.toml` porte fournisseur, conditions d'accès, échantillon et liens des données ; `data/research_visuals.toml` choisit le diagramme et ses légendes. Les SVG des diagrammes sont dans `templates/macros/research-visual.html`. Les slides d'identification conservées sont dans `static/img/research/identification/`.
- `data/editorial-supplements.json` ajoute des équations et explications au texte, à des points identifiés par `before` et `after`. Si vous modifiez un passage servant d'ancre `before`, adaptez aussi cette ancre : le build s'arrête volontairement si elle ne correspond plus. `references` et `equation_pages` portent les cibles scientifiques.
- `data/equation-details.json` définit un texte local par bloc d'équations. Conserver l'ordre et le nombre de blocs ; chaque détail doit définir les symboles de son seul bloc. Les formules restent en LaTeX, par exemple `\hat{\theta}_W`.
- `templates/teaching-detail.html` et `templates/macros/course-resources.html` rendent tous les cours. Un changement de libellé de téléchargement dans la macro se répercute aussi sur Code & Data.
- `templates/poem.html` porte le lecteur, les retours et les restrictions partagés des quatre poèmes. Le corps des œuvres reste dans leurs fichiers Markdown ; `templates/media-work.html` rend les trois enregistrements.

## Remplacer le CV ou un manuscrit

Le CV servi est **`static/files/cv/anderson-vil-cv-provisional.pdf`** ; son nom historique ne modifie pas son affichage « CV — PDF ». Remplacer ce fichier à l'identique préserve tous les liens. La source d'origine est `C:\dev\andersonvil-academic-os-sources\04-cv-teaching\cv-academic.pdf`. Si vous renommez le fichier, mettre à jour **`config.toml`** (`extra.cv_url` et l'entrée CV du menu) et **`data/academic.toml`** (`documents.cv.url`). La version synchronisée le 18 octobre 2026 est celle générée à 15 h 33 dans ce dossier, et non la copie à la racine des sources. Son SHA-256 est `765d5d593f00e23c4b2c2ac951436584013ae3ff173dd5fab66281847c0c9c16`. `scripts/build-editorial.js` vérifie cette empreinte à chaque build à partir de `docs/teaching-update-manifest.json` ; il ne recopie aucun ancien CV.

Les manuscrits sont `static/files/papers/household-demand-leisure.pdf`, `children-costs-one-adult-household.pdf` et `children-labor-supply-equations.pdf`. Après remplacement : vérifier la date dans `data/academic.toml`, les pages physiques dans le supplément puis régénérer les images avec :

```powershell
& 'C:\Users\vlade\.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe' scripts\prepare-source-pages.py
```

Ce script requiert `pypdf` et le `pdftoppm` du runtime local indiqué dans le fichier. Il compare les empreintes et actualise les rendus des PDF modifiés. Les liens PDF gardent `#page=N` ; le lecteur HTML fournit un secours lorsque le navigateur ne présente pas le PDF ou n'honore pas ce fragment.

## Trois modifications courantes

**Corriger une phrase.** Pour corriger une virgule dans le Non-technical overview du JMP, ouvrir `docs/editorial-sources/paper_jmp.md`, repérer `## 3. Non-technical overview`, modifier le paragraphe et enregistrer en UTF-8. Reconstruire avec la commande de preview ci-dessous. Ne pas éditer la copie rendue dans `data/paper-editorial.json`.

**Changer un titre.** Pour renommer un cours UCO, ouvrir `data/teaching_resources.toml`, repérer `id = "econometrics-niort"`, changer la ligne `title`. Modifier aussi `title` dans `content/teaching/econometrics-niort.md` pour le titre de l'onglet. Garder `id`, `url`, `group_id`, `level` et `role` si seul le titre change.

**Corriger un lien.** Pour remplacer le corrigé CC 2024–25, placer le nouveau PDF sous `static/files/teaching/cy-cergy/l3-econometrics/previous-cc/`. Dans le groupe `econometrics-l3` du catalogue, repérer la ressource `CC 2024-25 — Answer key`, puis corriger `file` si son nom a changé. Un lien `/files/...` correspond à `static/files/...` sur disque. Ne pas placer un chemin Windows dans une URL du site. Les deux parcours Teaching/Code & Data sont mis à jour ensemble.

## Prévisualiser sans publier

Ouvrir PowerShell à la racine :

```powershell
Set-Location -LiteralPath 'C:\dev\andersonvil.github.io'
git status --short
node --version
zola --version
```

Les dépendances Node sont épinglées dans `package-lock.json`. Si `node_modules` manque, exécuter `npm ci` une seule fois. Le build local normal est :

```powershell
powershell -ExecutionPolicy Bypass -File scripts\build-production.ps1
python scripts\serve-preview.py --directory public-production --port 1117
```

Ouvrir **`http://127.0.0.1:1117/`**. Garder le terminal du serveur ouvert ; Ctrl+C l'arrête. Si le serveur tourne déjà, ne pas en lancer un deuxième : reconstruire puis actualiser l'onglet. Choisir un autre port libre si nécessaire. `127.0.0.1` limite le serveur à cet ordinateur. Le HTML construit est celui de production, indexable et sans bandeau privé ; seul le serveur local ajoute un en-tête `X-Robots-Tag` de précaution. Ces commandes locales ne publient pas le site.

Le script efface **uniquement son dossier généré** avant reconstruction, avec contrôle de son emplacement. Ne pas y ranger des fichiers de travail ou des sauvegardes. Les textes et archives restent dans les dossiers sources.

Pour contrôler les liens, ancres, copies et intégrations après construction :

```powershell
python scripts\audit-final-v7.py --build-dir public-production
```

La suite locale (build et audits de production) se lance avec `powershell -ExecutionPolicy Bypass -File scripts\run-tests.ps1`.

Ce contrôle utilise `beautifulsoup4`, installé dans le Python utilisateur de cet ordinateur. Les rendus PDF utilisent le Python du runtime ci-dessus. Les sorties Homogeneity sont explicitement conservées par une exception dans `.gitignore` ; ne pas retirer cette exception. Les journaux de vérification sont dans `docs/final-v7-*.json` ; ils ne remplacent pas la lecture visuelle des pages sur desktop et mobile, en clair et sombre.

## Sources et fichiers générés

**À éditer** : `content/`, `templates/`, les TOML/JSON éditoriaux indiqués ci-dessus, `docs/editorial-sources/`, les fichiers réellement distribués dans `static/files/`, le CSS/JS partagé. Préserver les originaux dans l'autre dépôt lorsque vous remplacez un support. `.gitattributes` conserve les octets et les espaces d'origine des téléchargements et des archives ; ne pas normaliser les fichiers de données brutes.

**À ne pas corriger directement** : `public-preview-final/`, `public-preview/`, `public-production/`, `data/paper-editorial.json`, `data/homogeneity.json`, `data/scientific-pages.json`, les images `static/img/research/sources/*/page-N.png`, `docs/final-v7-math-manifest.json`. Les trois premiers sont des sorties Zola ; les deux JSON éditoriaux viennent de `scripts/build-editorial.js`, les pages scientifiques de `scripts/prepare-source-pages.py`. `static/vendor/katex/` vient de la dépendance épinglée.

L'ancienne Teaching Note est conservée dans **`docs/archive/pre-final-v7/insights/what-household-averages-can-hide.md`**, hors `content/` et `static/`. Les anciennes démonstrations et leurs téléchargements sont archivés dans le même dossier ; leurs anciennes pages ont `render = false`. Ne pas réactiver leur rendu. Les scripts `apply-final-v7.py` et `integrate-resources-v7.py` documentent l'intégration initiale ; ne pas les relancer pour corriger une virgule : ils appliquent des remplacements et inventaires de livraison.

Les templates historiques (`paper.html`, `paper-v7.html`, `code-data.html`, etc.) et `data/preview_synthetic.toml` ne sont plus les sources des pages principales. Vérifier le champ `template` du fichier `content` avant toute intervention. Le dossier `themes/kodama-theme/` est conservé ; la version actuelle utilise les templates racine et `static/styles/site.css`.

## Anciennes adresses

Les redirections gardent les anciens liens utilisables. Leur cible est dans `[extra].target` du Markdown correspondant, et le message partagé dans `templates/redirect.html`. Les pages sans équivalent utilisent `templates/gone.html`. La liste exacte des anciennes routes figure ci-dessous.

| Ancienne route | Source | Cible ou comportement |
|---|---|---|
| `/blog/` | `content/redirects/blog.md` | Page retirée, liens vers les rubriques actuelles |
| `/blog/children-don-t-matter/` | `content/redirects/children-dont-matter.md` | Page retirée, liens vers les rubriques actuelles |
| `/contacts_fr/` | `content/redirects/contacts-fr.md` | `/contact/` |
| `/contacts/` | `content/redirects/contacts.md` | `/contact/` |
| `/fr/` | `content/redirects/fr.md` | `/` |
| `/publications/paper1/index-copie/` | `content/redirects/paper1-copy.md` | `/research/children-costs-one-adult-household/` |
| `/publications/paper1/` | `content/redirects/paper1.md` | `/research/children-costs-one-adult-household/` |
| `/publications/paper2/` | `content/redirects/paper2.md` | `/research/children-labor-supply-equations/` |
| `/publications/paper3/index-copie/` | `content/redirects/paper3-copy.md` | `/research/household-demand-leisure/` |
| `/publications/paper3/` | `content/redirects/paper3.md` | `/research/household-demand-leisure/` |
| `/publications/paper4/` | `content/redirects/paper4.md` | Page retirée, liens vers les rubriques actuelles |
| `/publications/` | `content/redirects/publications.md` | `/research/` |
| `/blog/substitution-income-effects/` | `content/redirects/substitution-income.md` | Page retirée, liens vers les rubriques actuelles |
| `/teaching/econometrics-undergraduate/` | `content/redirects/teaching-econometrics.md` | `/teaching/` |
| `/teaching/econpub/` | `content/redirects/teaching-econpub.md` | `/teaching/` |
| `/teaching/hello-word/` | `content/redirects/teaching-hello.md` | `/teaching/` |
| `/teaching/macroeconomics-ii-undergraduate/` | `content/redirects/teaching-macro.md` | `/teaching/` |
| `/teaching/microeconomics-i-undergraduate/` | `content/redirects/teaching-micro.md` | `/teaching/` |
| `/teaching/statistics-i-undergraduate/` | `content/redirects/teaching-stats.md` | `/teaching/` |
| `/publications/thesis/` | `content/redirects/thesis.md` | Page retirée, liens vers les rubriques actuelles |
| `/insights/reading-model-based-estimates/` | `content/redirects/reading-model-based-estimates.md` | `/insights/quest-ce-quun-modele/` |

## Ajout Microeconomics III du 20 octobre 2026

Le PDF source `Niort/2026-27/Microéconomie III/partie2/td/serie01.pdf` est la série 1 de théorie des jeux (6 pages). Il est accessible dans Part II, immédiatement après le support, sous `/files/teaching/niort/microeconomics-iii/part-2/td/serie01.pdf`.

Le fichier autonome `Niort/Mock-Exams/Microéconomie L3/QCM/lab_microeconomie3.html` propose 60 questions, dont 30 sur les jeux et 30 sur le risque, avec corrections et résultats. Il figure après les trois parties dans **Self-assessment — Game theory and risk**, avec un lien direct vers `/files/teaching/niort/microeconomics-iii/qcm/lab_microeconomie3.html`. Il ne constitue pas une quatrième partie du cours.

Ces deux ressources sont utilisées dans Teaching et Code & Data. Le PDF est copié sans modification. Le QCM conserve intégralement ses questions, réponses et scripts ; seul un style `quiz-mobile-layout` est ajouté à la copie publique pour autoriser le retour à la ligne d’une longue phrase de la question `la10` sur mobile. La source originale reste intacte. `scripts/sync-teaching-update.py` assure leur copie et leur regroupement ; `kind = "self_assessment"` rend le QCM directement accessible dans sa fiche et le classe sous Self-assessment dans le catalogue technique. L’empreinte des copies figure dans `docs/teaching-update-manifest.json`.


## Mise à jour des examens blancs — 29 octobre 2026

Publication explicitement autorisée par le propriétaire. Les QCM proviennent désormais de `Niort/Mock-Exams/Econométrie/QCM` et `Niort/Mock-Exams/Microéconomie L3/QCM`. Le QCM de microéconomie conserve son URL publique. Chaque QCM dispose aussi d'un ZIP autonome construit avec les mêmes octets que le téléchargement individuel.

Les sujets et corrigés des examens blancs 2 et 3 de `CY-Cergy-Paris-Université/Mock-Exams/Econometrics-M1` sont classés dans Econometrics — M1, avec des libellés distincts « Question paper » et « Solutions » et un ZIP commun. Lors de la première intégration, le corrigé Mock 2 du dossier M2 était identique au fichier M1 et utilisait un lien partagé. La mise à jour suivante remplace ce lien par le nouveau corrigé propre au M2 ; les PDF M1 restent inchangés.

Le catalogue partagé et les vues sont générés par `python scripts/sync-teaching-update.py`. Après construction, `python scripts/audit-teaching-update.py --build-dir public-production` vérifie les copies, archives et liens. `node scripts/verify-mock-exams.js URL [COMMIT_SOURCE]` vérifie les nouveaux liens dans les deux parcours, les octets des téléchargements, ainsi que le déroulement complet, les corrections et le redémarrage des deux QCM aux largeurs 1440 et 390 pixels. Les rapports et captures sont conservés dans `tmp/mock-exams-qa/`.

Point de retour privé : `C:\dev\andersonvil-academic-os-sources\site-backups\2026-10-29-before-mock-exams`, contenant un bundle Git vérifié et `public-site.zip`. Références avant publication : source `69cfd185b4e236a98a1d041b4bda424942bd0fe1`, publication `1b773c4fb1a1619d980ea0a1c17d54e786c57068`. Pour annuler cette mise à jour, créer un commit avec `git revert COMMIT_DE_CETTE_MISE_A_JOUR`, puis pousser `main` et suivre les deux étapes de publication décrites plus haut. Le bundle permet de récupérer les références si nécessaire.


### Accès aux examens blancs depuis Teaching

Les fiches M1 et M2 & PhD présentent les examens blancs comme les fiches Self-assessment de Niort, avec sujets avant corrigés. La page `/teaching/` propose aussi des liens directs vers ces documents et les deux QCM, dans les blocs de cours correspondants. Chaque cours utilise désormais ses propres PDF, y compris le nouveau corrigé Mock exam 2 du M2. Code & Data utilise le même catalogue et les mêmes regroupements.

Point de retour avant cette correction : source `55d5de56d1ea9e4ef88a3e9b4851926dff97fb87`, récupérable par Git et conservée dans le bundle privé `site-backups/2026-10-29-before-mock-exams/after-first-publication.bundle`.

Le sujet `Econometrics-M2&PhD/Mock4_statement.pdf` (7 pages) est également intégré au cours M2 & PhD, sous Mock exam 4, avec accès direct depuis Teaching. L’archive M2 est reconstruite à partir de tous les fichiers actuels de ce dossier.


### Actualisation complète du dossier M2 & PhD

Le dossier `CY-Cergy-Paris-Université/Mock-Exams/Econometrics-M2&PhD` contient les sujets et corrigés actualisés des Mock exams 2 et 4. Les quatre PDF sont publiés dans `static/files/teaching/cy-cergy/m2-econometrics/mock-exams/`, sans modification de leurs octets. Les sujets ont 7 pages et les corrigés 4 pages. Le PDF Mock 4 existant est remplacé à la même URL ; le corrigé Mock 2 utilise désormais une URL M2 dédiée. Le fichier M1 anciennement partagé reste disponible uniquement dans le cours M1.

La synchronisation M2 parcourt tous les fichiers du dossier, classe les noms `MockN_statement.pdf` et `MockN_solutions_vN.pdf`, lit le nombre de pages avec `pypdf`, puis remplace la collection M2 dans le catalogue. Elle exige le module Python `pypdf` (disponible dans le runtime fourni avec Codex). Les ZIP dont les membres sont inchangés sont conservés. Teaching, la page M2 et Code & Data utilisent le même catalogue. `verify-mock-exams.js URL [COMMIT_SOURCE] --links-only` contrôle les liens, téléchargements et débordements sur ordinateur et mobile sans refaire les QCM inchangés.

Point de retour privé : `C:\dev\andersonvil-academic-os-sources\site-backups\2026-10-29-before-m2-refresh\recovery.bundle` (vérifié), source `7a33573ce0593c4776dadc989ce1203b2d903fe7`, publication `cf9d4c67276bf0b0227ac7dfe12ce6ab6efb2e6f`. Retour par un commit `git revert` de cette mise à jour puis publication normale sur `main`.
