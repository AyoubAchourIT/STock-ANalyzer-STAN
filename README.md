# STAN (STock ANalyser)

STAN est une application d'analyse boursière qui permet de travailler sur des données locales, des historiques récupérés depuis Yahoo Finance, ou des données importées par l'utilisateur au format CSV.

## Contexte académique

Ce projet a été développé dans un cadre universitaire avec un objectif double :

- mettre en pratique la structuration d'un projet R propre et reproductible ;
- proposer une application Shiny simple, robuste et pédagogique pour l'analyse d'un stock.

## Description du projet

L'application permet d'explorer un ticker boursier à partir d'une base locale de fichiers CSV, de calculer plusieurs indicateurs financiers usuels, d'ajuster une régression log-linéaire sur le prix et de visualiser les résultats dans une interface Shiny claire et sobre.

## Fonctionnalités principales

- sélection d'un ticker disponible localement ;
- choix d'une année de début pour la période d'analyse ;
- affichage des indicateurs de base :
  - dernier prix ;
  - date de dernière mise à jour ;
  - volatilité des rendements journaliers ;
  - CAGR ;
- calcul des performances sur plusieurs horizons :
  - 1 mois ;
  - 6 mois ;
  - 1 an ;
  - 3 ans ;
  - 5 ans ;
- régression linéaire sur le logarithme du prix :
  - valeur théorique actuelle ;
  - beta ;
  - sigma des résidus ;
  - position actuelle en sigma ;
  - projection théorique à 1 an et 5 ans ;
- graphique en échelle logarithmique avec :
  - prix observé ;
  - droite de régression ;
  - bandes ±1 sigma ;
  - bandes ±2 sigma ;
- mise à jour d'un ticker via Yahoo Finance ;
- ajout d'un nouveau ticker depuis Yahoo Finance ;
- import d'un ticker à partir d'un fichier CSV utilisateur.

## Structure du projet

```text
stock-analyzer/
├── app.R
├── data/
├── images/
├── R/
│   ├── utils_data.R
│   └── utils_finance.R
├── README.Rmd
├── README.html
├── README.md
└── www/
    └── style.css
```

## Lancer l'application

Depuis RStudio, ouvrir le dossier du projet puis exécuter :

```r
shiny::runApp()
```

Le point d'entrée principal de l'application est le fichier `app.R`.

## Packages requis

Les principaux packages utilisés sont :

- `shiny`
- `quantmod`
- `tidyverse`
- `dplyr`
- `ggplot2`
- `readr`
- `lubridate`
- `DT`
- `scales`
- `zoo`

Commande d'installation :

```r
install.packages(c(
  "shiny",
  "quantmod",
  "tidyverse",
  "dplyr",
  "ggplot2",
  "readr",
  "lubridate",
  "DT",
  "scales",
  "zoo"
))
```

## Format CSV attendu

La base locale repose sur un fichier CSV par ticker dans le dossier `data/`.

Format minimal attendu :

- colonne `Date` au format `YYYY-MM-DD` ;
- colonne `Close` numérique strictement positive ;
- une ligne par date ;
- aucune date dupliquée.

Colonnes complémentaires acceptées si elles sont disponibles :

- `Open`
- `High`
- `Low`
- `Volume`
- `Adjusted`

## Exemple CSV

```csv
Date,Close
2025-01-02,184.52
2025-01-03,186.10
2025-01-06,185.74
2025-01-07,188.35
```

Comportement de l'import CSV :

- si un ticker est saisi dans l'interface, il est utilisé ;
- sinon, le nom du fichier CSV est utilisé automatiquement comme ticker.

## Captures de l'application

### Interface principale

![Interface principale de STAN](images/interface.png)

### Graphique d'analyse

![Graphique du stock avec régression log-linéaire et bandes de sigma](images/graphique.png)

### Base locale et gestion des données

![Gestion de la base locale et opérations d'import](images/base_locale.png)

## Remarques et limites

- les téléchargements Yahoo Finance dépendent de la disponibilité du service et de la connexion réseau ;
- certains horizons de performance peuvent être indisponibles si l'historique local est trop court ;
- les projections issues de la régression log-linéaire restent indicatives et ne constituent pas une prévision financière certaine ;
- l'application est volontairement simple afin de rester lisible, maintenable et adaptée à un projet universitaire.

## Fichiers de documentation

- `README.md` : présentation du projet pour GitHub ;
- `README.Rmd` : version détaillée au format R Markdown ;
- `README.html` : version HTML générée à partir du `README.Rmd`.
