
## CoutsQc

Le package `CoutsQc` permet d'automatiser des analyses de coûts utilisant les bases médico-administratives du Québec, tout en visant une standardisation méthodologique des procédures.

Les scripts contenus dans ce package aident à 
- Calcul le coût réel des services en considérant l'inflation annuelle selon l'indice des prix à la consommation; 
- Réalise la conversion des indicateurs de ressources utilisées en coûts à partir de tables de conversion de coûts unitaires;
- Effectuer les calculs pour des durées de période par année calendrier, par année fiscale, par mois, par trimestre ou encore par période de suivi définie en intervalle de jours à partir d'une date index.
- Produit des bases de données augmentées contenant les variables des bases de données originales auxquelles se rajoutent le coût réel et la période de chaque service,
 un indicateur si le coût est manquant («ajout_cout_0») et, optionnellement, des indicateurs pour savoir si l'individu est vivant ou s'il est admissible à l'assurance-médicaments publique du Québec (RAMQ);
- Produit une base de données de style cohorte avec une observation par individus par période et par type de base de données, avec la possibilité d'agréger les services par d'autres facteurs;
- Produit un sommaire comptabilisant les coûts réels cumulatifs, marginaux et croisés par période et par type de base de données.

Le package `CoutsQc` est la version `R` de la macro `%Couts-Qc`. 
Un guide technique sur l’automatisation des analyses de coûts basées sur les données médico-administratives du Québec, présentant plus en détail le type d'analyse pouvant être réalisé par la macro, est disponible à l'adresse suivant : https://doi.org/10.60918/30942

### Installation

Utiliser la librairie `devtools` pour une installation simple de la dernière version du package à partir de GitHub avec sa vignette.

```R
devtools::install_github("Equipe-Jason-Guertin/Package-CoutsQc", build_vignettes = TRUE)
```
-----------
# English version

## CoutsQc 

The `CoutsQc` package automates cost analyses using Quebec’s medical-administrative databases, while aiming to standardize the methodology of its procedures.

The scripts included in this package aims to
- Calculate the actual cost of services by taking into account annual inflation based on the Consumer Price Index;
- Convert resource usage indicators into costs using unit cost conversion tables;
- Perform calculations for periods by calendar year, fiscal year, month, quarter, or a follow period defined as day intervals starting from a reference date;
- Generate augmented databases containing the variables from the original databases, with the actual cost and period for each service,
 plus an indicator if the cost is missing (“ajout_cout_0”), and, optionally, indicators showing if the individual is alive or if the individual is eligible for Quebec public drug coverage (RAMQ);
- Generates a cohort-style database with one observation per individual per period and per database type, with the option to aggregate services by other factors;
- Generates a summary of cumulative, marginal, and cross-costs by period and by database type.

The `CoutsQc` package is the `R` version of the `%Couts-Qc` macro.
A technical guide on automating cost analyses based on Quebec’s medical-administrative data,
which provides more details on the types of analyses that can be performed using the macro,
is available at the following address : https://doi.org/10.60918/30942

### Installation

Use the `devtools` library for a straitforward installation of the latest version of the package from GitHub with it vignette.

```R
devtools::install_github("Equipe-Jason-Guertin/Package-CoutsQc", build_vignettes = TRUE)
```


