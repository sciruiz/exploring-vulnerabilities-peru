# Data Analysis Workflow

## Compiling Datasources

### Step 1: Obtaining the ENDES and MINSA standardised dataset from the pre-processed dataset. 
Script: `National Survey of Family Health Demographics (ENDES) variables.R`

🟢 input:
* **ENDES:** ENDES - regional level data (download from this [link](https://drive.google.com/drive/folders/1iyG6JRFPTgtwadds2-opJCipSYiaJMru?usp=sharing))
* **Labels:** `Labels- subindicators.xlsx`(download from this [link](https://docs.google.com/spreadsheets/d/1ssGscYMo6_0uPWvjKSqNOw5cUNKypsAf/edit?usp=drive_link&ouid=104420450089934000441&rtpof=true&sd=true))

Other Datasets
* `Standardized wasting and stunting 2012-2016.RDS` ([link](https://drive.google.com/file/d/1XunKTKdAluS6V3QJUoNd2gsNKpMjLTWx/view?usp=sharing))
* `Standardized adolescent mothers from 2012-2016.RDS` ([link](https://drive.google.com/file/d/1PoHaLwP-s3OVDpfwLnnwlCnONkVkkiIP/view?usp=drive_link))
  
🔴 output: `Standardized ENDES 2012-2016.RDS`

### Step 2: Obtaining the ENAHO standardised dataset from the pre-processed dataset. 
Script: `National Household Survey (ENAHO) variables.R`

🟢 input: 

* ENAHO district level `.RDS` files (download from this [link]())
* Distrcts and ubigeos: `inei_all.csv`

🔴 output: `standardized ENAHO 2012-2016.RDS`

### Step 3: Obtaining the PISCO standardised dataset. 
Script: `environmental variables (PISCO).R`

* Including the shapefiles 
🟢 input:

* Ditrict map: `DISTRITO_15.shp`
* Distrcts and ubigeos: `inei.csv`

*Precipitation*

🟢 input:`Precipitation.nc`
🔴 Intermediate output:`Monthly accumulated precipitation per district Peru (2012-2016).RDS`

*Maximum Temperature*

🟢 input:`MaxTemp.nc`
🔴 Intermediate output:`Monthly average Tmax per region Peru (2012-2016).RDS`

*Minimum Temperature*

🟢 input:`MinTemp.nc`
🔴 Intermediate output:`Monthly average Tmin per province Peru (2012-2016).RDS`

Compiled PISCO dataset (precipitation + MaxT + MinT):

🔴 output:`PISCO variables district level.RDS`

### Step 4: Obtaning the dengue and diarrhea datasets from the CDC. 
Script: `dengue and diarrhea cases (CDC).R`

🟢 input:

* Dengue: `dengue CDC-MINSA.xlsx`
* Distrcts and ubigeos: `inei_all.csv`
* Diarrhea: `diarrhea CDC-MINSA.xlsx`
* Population data for standardisation: `est. population 2012-2016 (district).RDS`

🔴 output:

* Dengue: `dengue diarrhea cases CDC-MINSA.RDS`
* Diarrhea: `standardized dengue diarrhea CDC-MINSA.RDS`

## Dataset Compilation
### Step 5: Compiling the datasets from Step 1-4 into one dataset. 
Script: `database compilation.R`

🟢 input:

* PISCO: `PISCO variables district level.RDS`
* ENAHO: `standardized ENAHO 2012-2016.RDS`
* ENDES: `Standardized ENDES 2012-2016.RDS`
* CDC: `dengue diarrhea cases CDC-MINSA.RDS`

🔴 output:

* Dataset 1: `07.25 - Aggregated DB (2012-2016) with NA districts.RDS`
* Dataset 2: `07.25 - Aggregated DB (2012-2016) without NAs.RDS`

*Information only for Caballococha:* `07.25 - Annual values Ramon Castilla.RDS`

### Step 6: Calculating Final Indicators + Obtaining Paper Figures and Tables
Script: `Final Indicator.R`

🟢 input:

* INEI Population: `est. population 2012-2016 (district).RDS`
* Dataset 2: `07.25 - Aggregated DB (2012-2016) without NAs.RDS`

🔴 output: Tables and Plots included in the article
