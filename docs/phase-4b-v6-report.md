# Phase 4B v6 — rapport d’exécution

Date de contrôle : 26 septembre 2026. Le site demeure une prévisualisation privée, `noindex`, avec publication de production bloquée.

## 1. Branche, hash et restauration

- Branche : `codex/phase-4b-provisional-content-v3`.
- Commit d’implémentation v6 : `e555806` (`feat: complete phase 4b interactions visuals and audio`).
- Checkpoint v5 immuable : branche `archive/phase-4b-v5-before-v6`, commit `30c16c46438262f108e51540dcb3d63169cfd1bd`.
- Restauration non destructive : `git switch -c restore/phase-4b-v5 archive/phase-4b-v5-before-v6`.

## 2. Fichiers modifiés

Le commit d’implémentation contient 54 fichiers :

```text
content/beyond-economics/on-me-l-a-interdit.md
data/academic.toml
data/beyond_economics.toml
data/poetry_audio.toml
data/preview_inventory.toml
data/research_visuals.toml
data/teaching_photo.toml
data/teaching_resources.toml
docs/phase-4b-provisional-content-v3-report.md
docs/screenshots/phase4b-v6/*
docs/source-conflicts.md
scripts/capture-phase4b-v6.js
scripts/check-production.ps1
scripts/test-phase4b-v6-ui.js
scripts/validate-site.ps1
scripts/validate-source-copies.ps1
static/files/licenses/poetry-audio-candidates.txt
static/files/media/poetry/calligraphie-de-bonheur--great-aunt-henrietta.mp3
static/files/media/poetry/ma-cherie--castle-in-the-cloud.mp3
static/files/media/poetry/mes-nuits--night-sun.mp3
static/files/media/poetry/on-me-l-a-interdit--lost-in-the-city.mp3
static/img/teaching/teaching-in-practice.avif
static/img/teaching/teaching-in-practice.jpg
static/img/teaching/teaching-in-practice.webp
static/js/site.js
static/styles/site.css
templates/beyond-economics.html
templates/code-data.html
templates/contact.html
templates/home.html
templates/insights.html
templates/macros/contextual-return.html
templates/macros/research-visual.html
templates/paper.html
templates/poem.html
templates/poetry-audio-lab.html
templates/preview-index.html
templates/research-visuals-lab.html
templates/research.html
templates/teaching.html
```

Le présent rapport est ajouté dans un commit documentaire séparé afin de pouvoir citer le hash stable de l’implémentation.

## 3. Photo Teaching

- Source en lecture seule : `C:\dev\andersonvil-academic-os-sources\05-media-links\photo4.jpg`.
- Emplacement : section **Teaching in practice**, après les trois principes et avant les ressources.
- Actifs : AVIF, WebP et JPEG de repli, 1600 × 1066, cadre 3:2 complet, chargement différé et dimensions réservées.
- Traitement : correction non générative de balance, exposition, contraste et saturation ; aucun recadrage narratif.
- Droits : crédit et permission non documentés ; statut `blocked` et production bloquée.
- Captures : `docs/screenshots/phase4b-v6/teaching-practice-desktop.png` et `teaching-practice-mobile.png`.

## 4. Beyond Economics sur Home

La nouvelle section compacte se trouve après Insights et avant Contact. Elle présente la poésie sans reproduire de texte restreint, un extrait de conversation avec lecteur natif sans autoplay et un CTA contextuel.

Captures : `docs/screenshots/phase4b-v6/home-beyond-desktop.png` et `home-beyond-mobile.png`.

## 5. Illustrations Research

| Papier | Sélection affichée | Alternatives conservées | Justification |
|---|---|---|---|
| Household Demand and Parental Childcare | Candidat 2, **One hour, three economic roles** | C1 **Verified PSID study groups** ; C3 **The triple identity of care time** | La scène humaine rend immédiatement lisible qu’une même heure de soin est production, temps vécu et substitut au soin acheté, sans présenter un résultat empirique. |
| The Cost of Children in a One-Adult Household | Candidat 3, **Observed total, latent shares** | C1 **UK expenditure survey sequence** ; C2 **Resources inside one household** | La décomposition visuelle exprime directement le problème de mesure : le total observé ne révèle pas les parts adulte, enfant et communes. |
| Children and Labor Supply Equations | Candidat 3, **Two channels into labor supply** | C1 **Verified 2019 PSID study groups** ; C2 **A workday with two constraints** | Les canaux monétaire et temporel restent distincts avant d’entrer dans l’offre de travail, sans coefficient inventé. |

Les neuf propositions sont des SVG originaux du site, réellement différents et disponibles en clair et sombre. Une seule sélection réversible apparaît sur chaque page normale, à droite de **Research question**.

## 6. Laboratoire des illustrations

URL locale : `http://127.0.0.1:1112/preview/design-lab/research-visuals/`.

Capture : `docs/screenshots/phase4b-v6/research-visuals-lab-desktop.png`.

## 7. Composants interactifs audités

Navigation principale et mobile, wordmark, bouton de thème, liens textuels, boutons primaires et secondaires, CTA transversaux, retour contextuel, cartes Research/Teaching/Insights/poésie/média, cartes de ressources, sommaires de papiers, accordéons `details/summary`, boutons de lecture, curseurs de progression et de volume, lecteurs audio natifs, CTA de demande d’accès et état courant des laboratoires.

Les états `hover`, `focus-visible`, `active`, `aria-pressed` et `aria-current` sont différenciés selon le composant. Les fonds décoratifs des poèmes ne sont pas cliquables.

## 8. Scrollspy

| Page | État initial | Test intermédiaire | Test final |
|---|---|---|---|
| `/research/household-demand-leisure/` | Question | Methods and data | Findings and limits |
| `/research/children-costs-one-adult-household/` | Question | Methods and data | Findings and limits |
| `/research/children-labor-supply-equations/` | Question | Methods and data | Findings and limits |

Les trois tests mettent à jour `aria-current="location"`. Le sommaire reste sticky sur desktop, horizontal et sticky sur mobile ; les marges de défilement empêchent les titres d’être masqués.

## 9. Matrice des retours contextuels

| CTA d’origine | Destination | Libellé de retour |
|---|---|---|
| Home · View/Explore Research | Research | Back to Home |
| Home · Explore Teaching | Teaching | Back to Home |
| Home · Explore Code & Data | Code & Data | Back to Home |
| Home · Explore Insights | Insights | Back to Home |
| Home · Explore Beyond Economics | Beyond Economics | Back to Home |
| Contact · Explore Research | Research | Back to Contact |
| Contact · Explore Teaching | Teaching | Back to Contact |
| Teaching · Explore Code & Data | Code & Data | Back to Teaching |

Une arrivée directe n’affiche aucun retour. Les origines sont limitées à une allowlist de slugs internes ; une valeur externe ou malformée est ignorée. Les liens de navigation principale repartent sans provenance.

## 10. Parcours Contact → Research → Contact

Test automatisé et inspection réelle : arrivée directe sur Contact sans retour ; activation de **Explore Research** ; URL `/research/?from=contact` et bouton **Back to Contact** ; ouverture d’un papier avec conservation de la provenance ; retour hiérarchique **Back to Research** ; réapparition de **Back to Contact** sur l’index ; activation du bouton et retour à Contact. Une entrée par le menu Research supprime correctement le contexte.

Capture : `docs/screenshots/phase4b-v6/context-return-contact-desktop.png`.

## 11. Musiques des poèmes

| Poème | Piste | Ambiance | Source | Licence | Droits |
|---|---|---|---|---|---|
| Calligraphie de bonheur | TRG Banks — **Great Aunt Henrietta** | Lumineuse, chaleureuse, contemplative | Wikimedia Commons / album *Dreamland* sur FMA | CC0 1.0 | Vérifiés le 2026-09-26 ; choix provisoire |
| Ma chérie | TRG Banks — **Castle in the cloud** | Intime, tendre, suspendue | Wikimedia Commons / album *Return to Dreamland* sur FMA | CC0 1.0 | Vérifiés le 2026-09-26 ; choix provisoire |
| Mes nuits | TRG Banks — **Night Sun** | Nocturne, lente, introspective | Wikimedia Commons / album *Return to Dreamland* sur FMA | CC0 1.0 | Vérifiés le 2026-09-26 ; choix provisoire |
| On me l’a interdit | TRG Banks — **Lost in the city** | Retenue, grave, légèrement tendue | Wikimedia Commons / album *Return to Dreamland* sur FMA | CC0 1.0 | Vérifiés le 2026-09-26 ; choix provisoire |

Les fichiers, URL exactes, SHA-256, créateur crédité, statut de composition, statut d’enregistrement, auto-hébergement, attribution et justification figurent dans `data/poetry_audio.toml` et `static/files/licenses/poetry-audio-candidates.txt`. Les quatre choix sont indépendants. Le site entier reste néanmoins bloqué pour production, notamment à cause de la photographie et d’autres contenus provisoires.

## 12. Laboratoire audio

URL locale : `http://127.0.0.1:1112/preview/design-lab/poetry-audio/`.

Le laboratoire conserve **About candidate recordings** et expose les quatre fichiers distincts avec leurs métadonnées. Capture : `docs/screenshots/phase4b-v6/poetry-audio-lab-desktop.png`.

## 13. Preuve fonctionnelle des quatre lecteurs

`scripts/test-phase4b-v6-ui.js` a chargé et joué les quatre MP3 dans Chrome :

| Poème | Piste réellement lue | Durée média détectée | Pause/reprise | Progression | Volume |
|---|---|---:|---|---|---|
| Calligraphie de bonheur | Great Aunt Henrietta | 269,79 s | OK | OK | 0,25 confirmé |
| Ma chérie | Castle in the cloud | 150,25 s | OK | OK | 0,25 confirmé |
| Mes nuits | Night Sun | 169,45 s | OK | OK | 0,25 confirmé |
| On me l’a interdit | Lost in the city | 188,50 s | OK | OK | 0,25 confirmé |

Le test vérifie un `currentTime` croissant après reprise, la progression affichée, le changement de volume, l’absence d’autoplay, puis l’arrêt. Dans le laboratoire, le lancement de la deuxième piste met la première en pause. Le gestionnaire global applique cette exclusivité à tous les éléments audio et `pagehide` arrête les pistes au changement de page. Résultats bruts : `docs/screenshots/phase4b-v6/ui-test-results.json`.

## 14. Capitalisation

Audit sensible à la casse sur les sources et validation des 49 HTML générés : zéro occurrence visible de `Université catholique de l’Ouest` et zéro apostrophe ASCII dans cette dénomination. La forme utilisée partout est `Université Catholique de l’Ouest`. La seule forme fautive restante dans le dépôt est volontairement présente dans la règle de test qui doit la détecter.

## 15. Tests

- Build : 42 pages, dont 3 orphelines, et 7 sections ; succès.
- Validation : 49 fichiers HTML, liens et ancres internes ; succès.
- Desktop : captures 1440 × 900 inspectées pour Teaching, Home, les trois papiers, les deux laboratoires, le lecteur et le retour contextuel.
- Mobile : captures 375 × 812 inspectées ; largeur document = largeur viewport sur Home, Teaching, un papier et un poème ; aucun débordement horizontal.
- Clavier : focus programmatique visible sur **Methods and data**, activation par Entrée et mise à jour du scrollspy ; succès.
- Lecteur d’écran : arbre d’accessibilité Chrome contrôlé ; région du lecteur, bouton **Play music**, curseur **Music playback position** et curseur **Music volume** ont chacun un rôle et un nom accessibles.
- Thèmes : bascule clair/sombre et libellé dynamique du bouton vérifiés ; une page Research mobile a aussi été capturée en sombre.
- JavaScript : zéro erreur console pendant les scénarios automatisés.
- Sources : 17 copies octet pour octet et 1 copie de CV figée au checkpoint ; succès. La divergence externe du CV est documentée dans `docs/source-conflicts.md` et n’a pas été écrasée silencieusement.
- Confidentialité : test négatif de production correctement bloqué ; fixture propre correctement acceptée.

Captures et métriques : `docs/screenshots/phase4b-v6/`.

## 16. Sources externes et publication

Aucune écriture n’a été effectuée dans `C:\dev\andersonvil-academic-os-sources`. Le CV canonique y a changé pendant l’exécution par un processus extérieur ; le site a conservé la copie v5 épinglée et documente explicitement ce conflit. Aucun push, aucune pull request, aucune fusion, aucun déploiement, aucune modification de `main` et aucune URL publique n’ont été produits. Les balises `noindex` et tous les blocages de production restent actifs.
