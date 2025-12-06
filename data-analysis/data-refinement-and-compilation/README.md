# Data Analysis Workflow

## Compiling Datasources

### Step 1: Obtaining the ENDES and MINSA standardised dataset from the pre-processed dataset. 
Script: `National Survey of Family Health Demographics (ENDES) variables.R`

🟢 input:
* **ENDES:** ENDES - regional level data (download the folder from from this [link](https://drive.google.com/drive/folders/1iyG6JRFPTgtwadds2-opJCipSYiaJMru?usp=sharing))
* **Labels:** `Labels- subindicators.xlsx`(download the file from this [link](https://docs.google.com/spreadsheets/d/1ssGscYMo6_0uPWvjKSqNOw5cUNKypsAf/edit?usp=drive_link&ouid=104420450089934000441&rtpof=true&sd=true))

Other Datasets
* `Standardized wasting and stunting 2012-2016.RDS` (download the file from this [link](https://drive.google.com/file/d/1XunKTKdAluS6V3QJUoNd2gsNKpMjLTWx/view?usp=sharing))
* `Standardized adolescent mothers from 2012-2016.RDS` (download the file from this [link](https://drive.google.com/file/d/1PoHaLwP-s3OVDpfwLnnwlCnONkVkkiIP/view?usp=drive_link))
  
🔴 output: `Standardized ENDES 2012-2016.RDS`

### Step 2: Obtaining the ENAHO standardised dataset from the pre-processed dataset. 
Script: `National Household Survey (ENAHO) variables.R` (download the file from this [link](https://drive.google.com/drive/folders/1iyG6JRFPTgtwadds2-opJCipSYiaJMru?usp=sharing))

🟢 input: 

* ENAHO district level `.RDS` files (download the folder from this [link](https://drive.google.com/drive/folders/1EfeueyDB_7Tn-86VckvicTjfQ49Prn8n?usp=sharing))
* Distrcts and ubigeos: `inei_all.csv` (download the file from this [link](https://drive.google.com/file/d/1AE_uxRlj7GZuHQveFcVmCGtDmV8Ac81T/view?usp=drive_link))

🔴 output: `standardized ENAHO 2012-2016.RDS`

### Step 3: Obtaining the PISCO standardised dataset. 
Script: `environmental variables (PISCO).R`

* Including the shapefiles 
🟢 input:

* Ditrict map: `DISTRITO_15.shp` (download the full folder here [link](https://drive.google.com/drive/folders/1CPx_yn1OxXNxjPYOoTNdiuzpLvA3ZprA?usp=drive_link))
* Distrcts and ubigeos: `inei.csv` (download the file from this [link](https://drive.google.com/file/d/184MktnLacwGZA22fhSVQzhsZCTKEDVLk/view?usp=drive_link))

*Precipitation*

🟢 input:`Precipitation.nc` (download the file from this [link](https://drive.google.com/file/d/1BLDNrkbkrMjF0VSwXU9H9_rdpqdV4YgF/view?usp=drive_link))

🔴 Intermediate output:`Monthly accumulated precipitation per district Peru (2012-2016).RDS`

*Maximum Temperature*

🟢 input:`MaxTemp.nc` (download the file from this [link](https://drive.google.com/file/d/1uBrUWL-A9L3P-n4OuKN4iH7DHSDOQcMs/view?usp=drive_link)

🔴 Intermediate output:`Monthly average Tmax per region Peru (2012-2016).RDS`

*Minimum Temperature*

🟢 input:`MinTemp.nc` (download the file from this [link](https://drive.google.com/file/d/1gnV94xaFPkKNM5ZSnGPp9UYYb2HmXFX0/view?usp=drive_link))

🔴 Intermediate output:`Monthly average Tmin per province Peru (2012-2016).RDS`

Compiled PISCO dataset (precipitation + MaxT + MinT):

🔴 output:`PISCO variables district level.RDS`

### Step 4: Obtaning the dengue and diarrhea datasets from the CDC. 
Script: `dengue and diarrhea cases (CDC).R`

🟢 input:

* Dengue: `dengue CDC-MINSA.xlsx` (download the file from this [link](https://docs.google.com/spreadsheets/d/1i34R6TeoWEd4hXz_VDtlYWjFdUKacb5z/edit?usp=drive_link&ouid=104420450089934000441&rtpof=true&sd=true))
* Diarrhea: `diarrhea CDC-MINSA.xlsx` (download the file from this [link](https://docs.google.com/spreadsheets/d/1TKUjZND7IJkYYhtkIEgceV-hneYEvu7g/edit?usp=drive_link&ouid=104420450089934000441&rtpof=true&sd=true))
* Distrcts and ubigeos: `inei_all.csv` (download the file from this [link](https://drive.google.com/file/d/1AE_uxRlj7GZuHQveFcVmCGtDmV8Ac81T/view?usp=drive_link))
* Population data for standardisation: `est. population 2012-2016 (district).RDS` (download the file from this [link](https://drive.google.com/file/d/14ri8uVmLikNrA-9Oly3-DE_heP460FfC/view?usp=drive_link))

🔴 output:

* Dengue: `dengue diarrhea cases CDC-MINSA.RDS`
* Diarrhea: `standardized dengue diarrhea CDC-MINSA.RDS`

## Dataset Compilation
### Step 5: Compiling the datasets from Step 1-4 into one dataset. 
Script: `database compilation.R`

🟢 input:

* PISCO: `PISCO variables district level.RDS` (a reference is available at this [link](https://drive.google.com/file/d/1q_tkBi0i6J1XN8OH16POzjS5yXOyjZ4I/view?usp=drive_link)]
* ENAHO: `standardized ENAHO 2012-2016.RDS` (a reference is available at this [link](https://drive.google.com/file/d/10cnKgPAKjVUBd3-mXvHoJlp0u1W8kLCK/view?usp=drive_link)]
* ENDES: `Standardized ENDES 2012-2016.RDS` (a reference is available at this [link](https://drive.google.com/file/d/1bLVAnC3PRE6qz1jT7VCPwLqsqEvt-oH8/view?usp=drive_link)]
* CDC: `dengue diarrhea cases CDC-MINSA.RDS` (a reference is available at this [link](https://drive.google.com/file/d/19rXf9RbhmdL1UQANM2uqvbYj-iT8Y1cy/view?usp=drive_link)]

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

## Citation
>Please reference this repository by citing the article as: Mezza et. al. (On Revision) "Exploring Vulnerabilities: A mixed-method Transdisciplinary Study of Dengue and Diarrhea in Peru". _The Journal of Climate Change and Health_.

