# STAN (STock ANalyser)

STAN est une application d'analyse boursiere qui permet de travailler sur des donnees locales, des historiques recuperes depuis Yahoo Finance, ou des donnees importees par l'utilisateur au format CSV.

## Contexte academique

Ce projet a ete developpe dans un cadre universitaire avec un objectif double :

- mettre en pratique la structuration d'un projet R propre et reproductible ;
- proposer une application Shiny simple, robuste et pedagogique pour l'analyse d'un stock.

## Description du projet

L'application permet d'explorer un ticker boursier a partir d'une base locale de fichiers CSV, de calculer plusieurs indicateurs financiers usuels, d'ajuster une regression log-lineaire sur le prix et de visualiser les resultats dans une interface Shiny claire et sobre.

## Fonctionnalites principales

- selection d'un ticker disponible localement ;
- choix d'une annee de debut pour la periode d'analyse ;
- affichage des indicateurs de base :
  - dernier prix ;
  - date de derniere mise a jour ;
  - volatilite des rendements journaliers ;
  - CAGR ;
- calcul des performances sur plusieurs horizons :
  - 1 mois ;
  - 6 mois ;
  - 1 an ;
  - 3 ans ;
  - 5 ans ;
- regression lineaire sur le logarithme du prix :
  - valeur theorique actuelle ;
  - beta ;
  - sigma des residus ;
  - position actuelle en sigma ;
  - projection theorique a 1 an et 5 ans ;
- graphique en echelle logarithmique avec :
  - prix observe ;
  - droite de regression ;
  - bandes ±1 sigma ;
  - bandes ±2 sigma ;
- mise a jour d'un ticker via Yahoo Finance ;
- ajout d'un nouveau ticker depuis Yahoo Finance ;
- import d'un ticker a partir d'un fichier CSV utilisateur.

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

Depuis RStudio, ouvrir le dossier du projet puis executer :

```r
shiny::runApp()
```

Le point d'entree principal de l'application est le fichier `app.R`.

## Packages requis

Les principaux packages utilises sont :

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
- colonne `Close` numerique strictement positive ;
- une ligne par date ;
- aucune date dupliquee.

Colonnes complementaires acceptees si elles sont disponibles :

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

- si un ticker est saisi dans l'interface, il est utilise ;
- sinon, le nom du fichier CSV est utilise automatiquement comme ticker.

## Captures de l'application

### Interface principale

![Interface principale de STAN](images/interface.png)

### Graphique d'analyse

![Graphique du stock avec regression log-lineaire et bandes de sigma](images/graphique.png)

### Base locale et gestion des donnees

![Gestion de la base locale et operations d'import](images/base_locale.png)

## Remarques et limites

- les telechargements Yahoo Finance dependent de la disponibilite du service et de la connexion reseau ;
- certains horizons de performance peuvent etre indisponibles si l'historique local est trop court ;
- les projections issues de la regression log-lineaire restent indicatives et ne constituent pas une prevision financiere certaine ;
- l'application est volontairement simple afin de rester lisible, maintenable et adaptee a un projet universitaire.

## Fichiers de documentation

- `README.md` : presentation du projet pour GitHub ;
- `README.Rmd` : version detaillee au format R Markdown ;
- `README.html` : version HTML generee a partir du `README.Rmd`.
