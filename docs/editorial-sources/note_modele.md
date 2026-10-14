# Qu'est-ce qu'un modèle ?

Borges imagine un empire où les cartographes, par souci d'exactitude, dressent une carte aussi grande que l'empire lui-même. Les générations suivantes la jugent inutile et l'abandonnent aux intempéries. La carte parfaite est inutilisable précisément parce qu'elle ressemble parfaitement au territoire. Un modèle, en économie comme ailleurs, ne cherche pas à photographier la réalité : il en *isole* quelques mécanismes et laisse délibérément le reste de côté (Mäki, 2009). C'est une approximation, et sa valeur se juge à ce qu'elle permet de faire, non à sa ressemblance avec le réel.

Encore faut-il dire « faire quoi ». On peut distinguer trois usages, par ordre d'exigence croissante. **Mesurer** : estimer une grandeur qu'on ne voit pas directement, par exemple l'élasticité de l'offre de travail au salaire. **Prédire** ce qui se passera dans des conditions semblables à celles qu'on a observées. **Prédire ce qui se passerait** dans des conditions jamais observées, par exemple une réforme qui n'a pas encore eu lieu. Un modèle se juge sur la question pour laquelle il a été construit, et le même modèle peut être excellent pour l'une et inutilisable pour l'autre.

## Des hypothèses, pas des descriptions

Un modèle n'est rien d'autre qu'un ensemble d'hypothèses articulées de façon à produire des conclusions. On ne juge donc pas une hypothèse isolément sur son réalisme : on la juge à ce qu'elle fait aux conclusions du modèle. Friedman (1953) en a tiré une thèse provocatrice : le réalisme des hypothèses serait sans importance, seule compterait la qualité des prédictions. Musgrave (1981) a montré que cette formule confond des hypothèses de statuts différents. Dire « tel facteur est négligeable pour la question posée », ce n'est pas énoncer une fiction commode. C'est affirmer quelque chose sur le monde, et cette affirmation peut être fausse.

L'économètre connaît ce point sous un nom précis. Un modèle empirique retient quelques facteurs et relègue les autres dans un terme d'erreur. Supposons que les heures travaillées dépendent du salaire (effet β) et d'un goût pour le travail que l'on n'observe pas (effet γ). Si l'on omet ce goût, l'estimateur du coefficient du salaire ne converge pas vers β mais vers

> **b → β + γ · δ**

où δ mesure à quel point le facteur omis varie avec le salaire (le coefficient de la régression du facteur omis sur le salaire). L'omission n'est gratuite que dans deux cas : le facteur ne compte pas (γ = 0) ou il n'est pas lié à ce que l'on garde (δ = 0). Sinon, elle déforme ce que l'on mesure. Chiffrons. Supposons que la vraie élasticité vaille 0,10 et que les personnes qui aiment travailler aient aussi davantage investi dans leur formation, donc gagnent plus (γ = 0,5 ; δ = 0,3). L'estimation converge alors vers 0,25, soit deux fois et demie la vraie valeur. Simplifier est légitime ; omettre ce qui est corrélé à ce qu'on étudie ne l'est pas. C'est la limite exacte du « moins, c'est mieux ».

## Montrer plus n'est pas informer plus

Deux peintres doivent représenter une partie d'un corps de sorte que le spectateur puisse deviner s'il s'agit d'un homme ou d'une femme. Le peintre A ne peint que le visage. Le peintre B peint une surface bien plus grande : les deux jambes, du genou à la cheville. Hommes et femmes diffèrent en moyenne sur presque tous les traits physiques, mais pas avec la même netteté partout. Sur le visage, les deux distributions se séparent nettement ; sur la forme des mollets, elles se recouvrent largement. Supposons qu'environ 95 % des spectateurs identifient correctement le sexe devant le tableau A, contre 60 % devant le tableau B, à peine mieux que le hasard. B montre plus ; A informe davantage.

Ce que l'exemple illustre porte un nom en statistique. L'information n'est pas proportionnelle à la quantité de détails : elle dépend du *pouvoir discriminant* de ce que l'on montre, c'est-à-dire de l'écart entre les deux groupes rapporté à la dispersion au sein de chacun. Pour la question du sexe, le visage est presque une *statistique exhaustive* : il contient l'essentiel de l'information utile. Les jambes sont presque une *statistique ancillaire* : leur aspect varie beaucoup d'une personne à l'autre, mais presque pas selon ce que l'on cherche à deviner.

Changeons maintenant la question : le personnage est-il footballeur ? Les mollets de B deviennent parlants, le visage de A devient muet. L'information n'est pas une propriété du tableau, mais du couple tableau–question. C'est pourquoi un modèle ne peut pas être bon « en général ».

Deux réserves s'imposent. D'abord, B ne montre pas *plus* que A, il montre *autre chose*. Un tableau qui contiendrait le visage et les jambes ne ferait pas moins bien que A pour un spectateur qui saurait ignorer les jambes. Le détail superflu ne coûte que lorsqu'il faut *apprendre* à l'ignorer à partir de données limitées ; c'est l'objet de l'exemple suivant. Ensuite, un tableau classe, il n'explique pas. Il ne contient aucun mécanisme et ne dit rien de ce qui arriverait si l'on modifiait quelque chose. L'analogie s'arrête là où commence le travail du modélisateur structurel.

## Ajouter des variables a un prix

Deux applications prévoient s'il pleuvra le lendemain dans une ville donnée. *Brise* utilise cinq ingrédients : la pression atmosphérique et sa variation sur trois heures, la température, le point de rosée, la direction et la force du vent. Ce sont, à peu de chose près, les informations du baromètre et de la girouette. *Panoptique* en utilise quatre cents : ces cinq-là, plus la phase de la lune, le jour de la semaine, le nombre de consultations de l'application, etc. Les deux sont estimées sur les mêmes trois années d'observations d'une seule station.

Sur ces trois années, Panoptique « prévoit » juste 88 % des jours, contre 76 % pour Brise. Sur l'année suivante, qu'aucune des deux n'a vue, Panoptique tombe à 77 % et Brise à 75 %. Avec quatre cents paramètres pour un millier de jours, Panoptique a appris une bonne part du bruit propre à son échantillon : c'est le *surapprentissage*. Chaque variable inutile réduit à peine le biais et ajoute de la variance d'estimation ; au-delà d'un certain point, la variance l'emporte. Le seul juge est la performance *hors échantillon*.

Peut-on conclure que Brise est aussi bonne parce que l'écart de 2 points n'est « pas significatif » ? Non. Sur 365 jours de test, avec des prévisions qui divergent un jour sur cinq, l'intervalle de confiance à 95 % de l'écart (Brise moins Panoptique) va d'environ −6,6 à +2,6 points. Une différence non significative signifie seulement que les données ne permettent pas de trancher ; elle ne prouve pas que les deux applications se valent. La démarche correcte fixe d'avance l'écart jugé négligeable, par exemple 3 points, puis vérifie que l'intervalle de confiance à 90 % de la différence tient tout entier dans [−3 ; +3]. C'est un *test d'équivalence*. Ici, l'intervalle va de −5,8 à +1,8 : on ne peut pas conclure, il faudrait davantage de jours de test. Si l'équivalence était établie, la *parcimonie* départagerait : à précision égale, le modèle le plus simple est moins coûteux, plus lisible et, en général, plus robuste quand les conditions changent. Quant à la satisfaction déclarée des utilisateurs, elle ne mesure pas la précision : elle mêle le prix payé, l'ergonomie et les attentes. Elle n'a pas sa place dans la comparaison.

Une précision s'impose. En prévision météorologique réelle, ce sont les grands modèles, nourris de physique et de quantités colossales d'observations, qui l'emportent. Cela ne contredit pas la thèse, cela la précise. La parcimonie n'est pas une vertu en soi : c'est la réponse raisonnable à des données rares au regard du nombre de paramètres à estimer. Quand les données sont surabondantes, le coût en variance de paramètres supplémentaires devient faible. Or l'économiste n'est presque jamais dans cette situation : ses échantillons sont limités, et la réforme qu'on lui demande d'évaluer n'a, par définition, jamais été observée.

## Le test le plus exigeant : ce qu'on n'a jamais vu

Le tableau et l'application prédisent dans des conditions semblables à celles d'où ils tirent leur information. Évaluer une politique demande davantage : prédire les comportements sous un régime qui n'a jamais existé. Marschak (1953), puis Lucas (1976), ont montré qu'une relation estimée sous une politique donnée peut se déformer quand la politique change, parce que les agents ajustent leurs décisions. Seuls des paramètres invariants au changement de politique (préférences, technologies, contraintes) autorisent ce saut. C'est la raison d'être des modèles structurels, et c'est aussi le critère auquel ils doivent se soumettre.

Todd et Wolpin (2006) en donnent une illustration rare. Ils estiment un modèle dynamique de scolarisation et de fécondité sur les seuls villages témoins de l'expérience mexicaine PROGRESA, où aucune subvention scolaire n'était versée. Ils prédisent ensuite l'effet de la subvention et comparent cette prédiction à l'effet mesuré dans les villages traités : les deux sont globalement proches. Le modèle a passé un test qu'aucune ressemblance avec le réel ne garantit, celui de dire juste sur une situation qu'il n'avait jamais vue. Il peut alors servir à évaluer des variantes du programme que personne n'a mises en œuvre.

On ne demande pas à un modèle de ressembler au monde, mais de dire juste là où l'on ne peut pas regarder.

---

**Références**

- Borges, J. L. (1960). « Del rigor en la ciencia ». In *El hacedor*. Buenos Aires : Emecé.
- Friedman, M. (1953). « The Methodology of Positive Economics ». In *Essays in Positive Economics*. Chicago : University of Chicago Press, 3–43.
- Lucas, R. E. (1976). « Econometric Policy Evaluation: A Critique ». *Carnegie-Rochester Conference Series on Public Policy*, 1, 19–46.
- Mäki, U. (2009). « MISSing the World. Models as Isolations and Credible Surrogate Systems ». *Erkenntnis*, 70(1), 29–43.
- Marschak, J. (1953). « Economic Measurements for Policy and Prediction ». In W. C. Hood et T. C. Koopmans (dir.), *Studies in Econometric Method*. Cowles Commission Monograph 14. New York : Wiley.
- Musgrave, A. (1981). « "Unreal Assumptions" in Economic Theory: The F-Twist Untwisted ». *Kyklos*, 34(3), 377–387.
- Todd, P. E. et Wolpin, K. I. (2006). « Assessing the Impact of a School Subsidy Program in Mexico: Using a Social Experiment to Validate a Dynamic Behavioral Model of Child Schooling and Fertility ». *American Economic Review*, 96(5), 1384–1417.
