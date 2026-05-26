# Qualitat de l'Aire als Centres Educatius de Catalunya

Aquest repositori presenta una aplicació interactiva desenvolupada amb R Shiny per explorar i analitzar les dades de qualitat de l'aire a l'entorn dels centres educatius de Catalunya. L'aplicació es pot consultar en línia a:

https://martafeco.shinyapps.io/qualitat_aire_shiny

---

## Descripció

Aquest projecte permet visualitzar de manera interactiva els nivells de contaminació atmosfèrica als entorns dels centres educatius catalans, combinant dades de qualitat de l'aire amb informació geogràfica dels centres.

L'objectiu és facilitar l'anàlisi de l'exposició a contaminants de la població escolar i proporcionar una eina de consulta accessible per a famílies i administració.

---

## Funcionalitats

- Mapa interactiu dels centres educatius amb indicadors de qualitat de l'aire  
- Filtres dinàmics per municipi, tipus de centre, nivell educatiu i contaminant  
- Visualitzacions estadístiques: sèries temporals, distribucions i comparatives entre centres  
- Taules de dades exportables per a un ús posterior  
- Indicadors de l'OMS i la UE com a referència per avaluar el nivell de risc  

---

## Fonts de les dades

Les dades utilitzades en aquest projecte provenen del Portal de Dades Obertes de la Generalitat de Catalunya:

- Qualitat de l’aire als punts de mesurament automàtics de la Xarxa de Vigilància i Previsió de la Contaminació Atmosfèrica:  
https://analisi.transparenciacatalunya.cat/Medi-Ambient/Qualitat-de-l-aire-als-punts-de-mesurament-autom-t/tasf-thgu/about_data  

- Equipaments de Catalunya:  
https://analisi.transparenciacatalunya.cat/Urbanisme-infraestructures/Equipaments-de-Catalunya/8gmd-gz7i/about_data  

Les dades són obertes i proporcionades per la Generalitat de Catalunya sota els principis de transparència i reutilització de dades públiques.

---

## Estructura del repositori
qualitat_aire_centres_educatius/

│

├── R/

│ ├── app.R # Codi de l'aplicació

├── data/ # Datasets del projecte

│ ├── centres_educatius/

│ └── qualitat_aire/

├── README.md

└── LICENSE


---

## Instal·lació i execució en local

### Requisits previs

- R (≥ 4.0)
- RStudio

### Passos


#### 1. Instal·la dependències
```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinydashboardPlus",
  "leaflet",
  "leaflet.extras",
  "dplyr",
  "tidyr",
  "plotly",
  "scales",
  "sf",
  "rlang",
  "shinyWidgets",
  "htmlwidgets",
  "htmltools"
))
```


##### 2. Executa l'aplicació
```r
shiny::runApp()
```

---

## Llicència

Aquest projecte es distribueix sota la llicència MIT.
