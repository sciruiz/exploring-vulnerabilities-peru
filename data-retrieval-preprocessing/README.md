# Framing Vulnerabilities: Dengue and Diarrhea in Peru’s Borderlands

This section outlines the source of the data used in this study. 

# Data description 

| Dataset | Type of Data | Granurality | Timespam | Source |  
|--------|---|-----|--|-|
| Demographic and Family Health Survey (ENDES) | Sociodemographic data | Regional | Annual | National Institute of Statistics and Informatics (INEI) |
| National Household Survey (ENAHO)|  Sociodemographic data | District | Annual | National Institute of Statistics and Informatics (INEI) |
| Dengue and diarrhea prevalence | Epidemiological data | District | Weekly | CDC Peru (Centro Nacional de Epidemiología) |
| PISCO | Climate data| District| Annnual |SENAMHI | https://www.senamhi.gob.pe |

Note: Peru’s political and administrative organization is structured in a hierarchical manner, with four main levels: region, departments, provinces, and districts. The first division consists of departments (departamentos), also referred to as regions (regiones). Peru has 25 of these: 24 departments plus the Constitutional Province of Callao, which holds the same status as a department. Each department has a regional government responsible for planning and executing policies in areas such as education, health, transport, and economic development within its boundaries. Examples of departments include Cusco, Piura, Loreto, Arequipa, and Lima.

Departments are further divided into provinces (provincias). There are 196 provinces across the country, each governed by a provincial municipality. Provincial governments oversee urban development plans, inter-district transport systems, and certain public works. For instance, the Department of Cusco contains provinces such as Cusco, Urubamba, La Convención, and Canchis.

Provinces are subdivided into districts (distritos), which form the most localized level of government. Peru has 1,874 districts, each managed by a district municipality responsible for community-level services, such as waste management, maintenance of local infrastructure, and regulation of neighborhood commerce. For instance, in the case of Cusco Province, its districts include Cusco, San Sebastián, San Jerónimo, Santiago, and Wanchaq.

# Data Retrieval

All datasets used in this study were accessed in 2021 from open-access repositories, unless otherwise specified. Sociodemographic data were obtained from the [INEI MicroDatos Repository](https://proyectos.inei.gob.pe/microdatos) via a custom Google Colab Notebook developed for this study, available [here](https://colab.research.google.com/drive/1X1AGu06S61092Y90uFJC4ArW3Mvn071i?usp=sharing). Climate data from the **PISCO** (*Peruvian Interpolation of the SENAMHI’s Climatological Observations*) dataset were retrieved from the SENAMHI open-data portal, which provides gridded temperature and precipitation records at daily and monthly resolutions, and accessed using institutional API endpoints and bulk download tools. Information on stunting and wasting in children under five years old was obtained from the Ministry of Health (**MINSA**) database, following a formal data request to the institution. 

# Data Preprocessing 

## Citation

Please cite the article as:
> Mezza et al. (On Revision). *Framing Vulnerabilities: A Transdisciplinary Study of Dengue and Diarrhea in Peru’s Borderlands*. _The Journal of Climate Change and Health_.

