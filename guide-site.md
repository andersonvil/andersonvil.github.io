# Guide pratique de maintenance du site

Le dépôt du site est **`C:\dev\andersonvil.github.io`**. Tous les chemins ci-dessous sont relatifs à cette racine, sauf mention contraire. Les documents originaux se trouvent dans **`C:\dev\andersonvil-academic-os-sources`**. Le site utilise **Zola**, des templates **Tera**, des données **TOML/JSON**, du Markdown et un rendu mathématique **KaTeX à la construction**. Il n'y a pas de serveur applicatif ni de base de données.

Cette livraison est sur `codex/final-private-v7`. Le point de retour antérieur est `codex/backup-before-final-v7`, commit `79d0288`. Les commandes ci-dessous construisent une version locale, sans fusion ni publication.

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
| `/teaching/data-analysis-l3/` — Data Analysis with Python — L3 | `content/teaching/data-analysis-l3.md`, `data/teaching_resources.toml`, groupe `data-analysis-l3` | Mêmes clés ; `part` vaut TD01 à TD05. Fichiers dans `static/files/teaching/cy-cergy/l3-data-analysis/td-01/` à `td-05/`. Les notes de présentation à venir sont dans `templates/teaching-detail.html`. |
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

Le CV servi est **`static/files/cv/anderson-vil-cv-provisional.pdf`** ; son nom historique ne modifie pas son affichage « CV — PDF ». Remplacer ce fichier à l'identique préserve tous les liens. La source d'origine est `C:\dev\andersonvil-academic-os-sources\04-cv-teaching\cv-academic.pdf`. Si vous renommez le fichier, mettre à jour **`config.toml`** (`extra.cv_url` et l'entrée CV du menu) et **`data/academic.toml`** (`documents.cv.url`).

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
powershell -ExecutionPolicy Bypass -File scripts\build-preview.ps1 -OutputDirectory public-preview-final
python scripts\serve-preview.py --directory public-preview-final --port 1117
```

Ouvrir **`http://127.0.0.1:1117/`**. Garder le terminal du serveur ouvert ; Ctrl+C l'arrête. Si le serveur tourne déjà, ne pas en lancer un deuxième : reconstruire puis actualiser l'onglet. Choisir un autre port libre si nécessaire. `127.0.0.1` limite la preview à cet ordinateur. Le build garde `noindex`, `nofollow`, `noarchive`, le bandeau Private preview et les en-têtes du serveur ; aucune de ces commandes ne publie le site.

Le script efface **uniquement son dossier généré** avant reconstruction, avec contrôle de son emplacement. Ne pas y ranger des fichiers de travail ou des sauvegardes. Les textes et archives restent dans les dossiers sources.

Pour contrôler les liens, ancres, copies et intégrations après construction :

```powershell
python scripts\audit-final-v7.py
```

La suite complète locale (build, audit et tests du verrou de publication) se lance avec `powershell -ExecutionPolicy Bypass -File scripts\run-tests.ps1`.

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
| `/insights/reading-model-based-estimates/` | `content/preview-synthetic/insights/reading-model-based-estimates.md` | `/insights/quest-ce-quun-modele/` |
