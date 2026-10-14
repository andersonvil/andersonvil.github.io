# Traçabilité scientifique — révision privée finale v5

Date de contrôle : 14 octobre 2026. Sources originales non modifiées. Leurs empreintes, rôles, versions et chemins complets sont dans `final-v5-source-inventory.json`.

## Versions retenues

| Article | Texte éditorial intégral | Manuscrit retenu, identifié sur sa page de titre | Décision |
|---|---|---|---|
| The Household Demand for Leisure… | `03-research/paper_jmp.md` | `03-research/paper-3.pdf`, 4 octobre 2026 | Remplace le PDF du 18 septembre de la preview sauvegardée. |
| Children Costs in a One-Adult Household… | `03-research/paper_solo.md` | `03-research/paper-1.pdf`, 12 janvier 2026 | PDF fourni conservé, copie byte-identique. |
| How to Incorporate Children into Labor Supply Equations?… | `03-research/paper_2.md` | `03-research/paper-2.pdf`, 4 septembre 2026 | Identification par titre, malgré la faute « Childen » du PDF. |

Les quatre Markdown de l’auteur, dont `note_modele.md`, sont archivés sans modification dans `docs/editorial-sources`. La page française ne reçoit aucune date de publication supposée. Le CV fourni est conservé byte-identique. Les slides du JMP et leur source LaTeX sont celles du 17 août 2026 ; elles ne sont pas présentées comme des slides d’octobre.

## Équations et notations

`data/editorial-supplements.json` contient les insertions et leurs ancres exactes dans les nouveaux textes. `scripts/build-editorial.js` vérifie ces ancres, rend les formules avec KaTeX en mode strict et échoue en cas d’erreur. `final-v5-math-manifest.json` recense chaque formule, son article et son type en ligne/bloc.

| Article / bloc | Source de vérification | Ce qui est rendu |
|---|---|---|
| JMP, choix / prix / coût | Nouveau Markdown, théorie et spécification du manuscrit | Les quatre objets, prix productif et process benefit, coût minimal, relations sur lignes séparées, `e_M=c_K`. |
| JMP, Stage 1 | PDF p. 29, équations 9–12 ; estimation p. 32 | Deux demandes de temps et dépense de garde censurée ; erreurs jointement normales ; distinction explicite entre le régime théorique à zéro et son omission dans les équations de temps estimées. |
| JMP, Stage 2 sans SAT | PDF p. 30, équations 14–16 | Les deux demandes de loisir complet et le coût monétaire softplus. |
| JMP, Stage 2 avec SAT | PDF p. 17 (A.2′), p. 31, équations 17–18 | Similarity Across Types, coefficients décalés et `delta(n)=1{n>0}`. Cette indicatrice est distincte du facteur `sqrt(n)` dans le coût monétaire. |
| JMP, correction du temps | Nouveau Markdown et PDF pp. 24–25 | Transformation tanh, zéro préservé, temps PSID ajusté ; ATUS est le benchmark, pas l’échantillon d’estimation. |
| Article britannique | PDF pp. 10–13, équations 13–19 | Demande de référence, demande parentale, partage logistique, variante multiplicative, prix de Barten et équation unifiée effectivement estimée. |
| Offre de travail | PDF pp. 15–18, équations 16–25 | Bloc original complet conservé ; complément avec la fonction quadratique traduite, contrôle des erreurs et participation. `q` est un raccourci explicitement défini, pas une variable observée supplémentaire. |

Les coûts du JMP sont écrits `e_T`, `e_M`, conformément au manuscrit actuel et au nouveau Markdown. Les anciennes notations en exposant et les formules Unicode du gabarit v7 ne sont plus rendues dans les pages d’articles. Les figures EEA gardent leurs labels originaux ; leurs légendes explicitent `omega_I=w_I`, `c_T=e_T`, `c_M=e_M` et les profits d’utilité de la figure générale, nuls sous la spécification linéaire estimée.

Le gabarit historique du commit `e555806` a été consulté : il présentait déjà les figures originales verticalement avec une transcription après chacune. Le gabarit `a65612d` les regroupait ensuite dans une galerie séparée. La révision rétablit leur présentation verticale et les associe directement aux textes Step 1 et Step 2 actuels. Les actifs SVG d’avant modification sont conservés dans `tmp/final-v5/slide-10-before.svg` et `slide-11-before.svg`.

Le moteur produit HTML vectoriel/net et MathML accessible. Les équations restent visibles ; les symboles se déplient juste après chaque bloc. Les styles et polices sont locaux. Les formules dépassant la largeur mobile se défilent à l’intérieur de leur bloc.

## Chiffres et liens

Les 22 cibles de tableaux/figures ont été recalculées sur les PDF retenus. Les intitulés ont été retrouvés sur chaque page physique cible : `final-v5-scientific-link-audit.json` contient 22 contrôles positifs. Une visionneuse locale de 36 pages originales rendues fournit une alternative explicite aux lecteurs ignorant `#page=`. Ce sont des sources supplémentaires ; les formules du site ne sont pas des captures.

| JMP | Page PDF actuelle |
|---|---:|
| Table 1 | 25 |
| Table 2 | 26 |
| Table 4 | 34 |
| Table 5 | 38 |
| Table 6 | 40 |
| Tables A.1 / A.2 / A.3 | 53 / 54 / 55 |

| Article britannique | Page PDF |
|---|---:|
| Tables 1 / 2 / 3 / 4 / 5 | 12 / 14 / 16 / 18 / 19 |
| Tables 12 / 13 | 36 / 37 |
| Figures 1 / 8 | 17 / 39 |

| Offre de travail | Page PDF |
|---|---:|
| Tables 1 / 2 / 3 / 4 / 5 | 14 / 20 / 21 / 23 / 24 |

La Table 1 du JMP conserve ses 5 lignes, unités, moyenne, SD, minimum, médiane, maximum et proportion de zéros. Les valeurs de la Table 2 ont été confrontées au PDF du 4 octobre. Quatre valeurs obsolètes ont été corrigées :

| Loisir, heures/semaine | Preview sauvegardée | PDF actuel et page révisée |
|---|---:|---:|
| Père, sans enfant | 77,03 | 125,33 |
| Père, avec enfants | 66,46 | 114,76 |
| Mère, sans enfant | 82,49 | 130,79 |
| Mère, avec enfants | 67,19 | 115,49 |

Les écarts-types, autres variables, groupes, unités et notes sont conservés et vérifiés. Les résultats et listes de limites proviennent des nouveaux Markdown ; les abstracts restent strictement ceux de la preview précédente.

## Passages à faire revoir par l’auteur

Ces observations ne donnent lieu à aucune réécriture du PDF original ni à une résolution scientifique inventée :

- JMP, `paper_jmp.md`, Economic model : « With varphi_I linear (Assumption A.1) » attribue la linéarité à A.1. Le PDF p. 14 définit A.1 avec une fonction différentiable et concave ; la restriction linéaire apparaît dans la spécification empirique p. 30. Le texte de l’auteur est conservé pour sa relecture : cette attribution doit être confirmée ou corrigée avant publication.
- JMP, PDF p. 29 : la phrase de dérivation utilise `m=e_T-w_H t_H-w_W t_W` avec des salaires sans étoile, alors que la décomposition du coût et les équations qui suivent utilisent les prix productifs. La page web suit le nouveau Markdown et les équations spécifiées, et conserve le manuscrit intact.
- JMP, PDF p. 25 : la dernière expression du loisir complet est écrite sous une forme qui semble incohérente avec `L_I=l_I+varphi_I(t_I)` et la spécification linéaire de la p. 30. La page web garde l’identité explicitement fournie dans le nouveau Markdown.
- Article britannique : la section descriptive du PDF p. 14 annonce 40 403 ménages avec une répartition différente de celle des échantillons d’estimation des Tables 3–4 et du nouveau Markdown (24 748 femmes, 15 634 hommes). Les tableaux et le nouveau texte restent associés à leurs sources ; la raison de la différence n’est pas supposée.
- Article britannique, PDF p. 15 : « augmenting effect on parental resources » coexiste avec une interprétation du coefficient négatif en termes de coût accru des enfants. Le nouveau texte éditorial est conservé ; cette formulation du PDF mérite une relecture.
- Offre de travail : « Childen » et « JEL Classication » demeurent dans le PDF fourni. Le titre corrigé du site et la note de divergence sont conservés.
- Offre de travail, nouveau Markdown : l’affirmation sur 2019 comme première vague de dépenses détaillées reprend une formulation du manuscrit p. 13. Cette chronologie n’a pas été vérifiée auprès du fournisseur ; elle reste à confirmer avant publication.

Les statuts de revue et les identifiants exacts des extractions de données restent à confirmer avant toute publication. Aucun programme de réplication de recherche n’a été fourni. Les ressources Code & Data présentées comme réelles sont pédagogiques.
