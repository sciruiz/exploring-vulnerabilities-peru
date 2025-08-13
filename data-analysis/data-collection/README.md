# Framing Vulnerabilities: Dengue and Diarrhea in Peru’s Borderlands

This section outlines the source, processing steps, and scripts utilized for this purpose in this publication. The objective is to ensure full transparency and reproducibility of our findings, in accordance with best practices in open and replicable research.of the data used in this study. 

## Data description 

| Dataset                                                   | Type of Data          | Administrative and Political Unit Resolution | Frequency Resolution | Source                                                                 |
|-----------------------------------------------------------|-----------------------|-------------|----------|------------------------------------------------------------------------|
| Demographic and Family Health Survey (**ENDES**)          | Sociodemographic data | Regional    | Annual   | National Institute of Statistics and Informatics (**INEI**)           |
| National Household Survey (**ENAHO**)                     | Sociodemographic data | District    | Annual   | National Institute of Statistics and Informatics (**INEI**)           |
| Dengue and Diarrhea Prevalence                            | Epidemiological data  | District    | Weekly   | CDC Peru (*Centro Nacional de Epidemiología, Prevención y Control de Enfermedades*) |
| **PISCO** (Peruvian Interpolation of SENAMHI Climatological Observations) | Climate data         | District    | Annual   | SENAMHI (*Servicio Nacional de Meteorología e Hidrología del Perú*)   |

Note:
**Political and administrative organization of Peru**
Peru is structured in a hierarchical manner, with four main levels: region, departments, provinces, and districts. The first division consists of departments (departamentos), also referred to as regions (regiones). Peru has 25 of these: 24 departments plus the Constitutional Province of Callao, which holds the same status as a department. Each department has a regional government responsible for planning and executing policies in areas such as education, health, transport, and economic development within its boundaries. Examples of departments include Cusco, Piura, Loreto, Arequipa, and Lima.

Departments are further divided into provinces (provincias). There are 196 provinces across the country, each governed by a provincial municipality. Provincial governments oversee urban development plans, inter-district transport systems within the province, and certain public works relevant at province level. 

Provinces are further subdivided into districts (distritos), which form the most localized level of government. Peru has 1,874 districts, each managed by a district municipality responsible for community-level services, such as waste management, maintenance of local infrastructure, and regulation of neighborhood commerce. Districts can be divided, merged or created at the request of local communities. For instance, thirty one new districts were established between 2011 to 2016 [read more](https://elcomercio.pe/lima/cinco-anos-gobierno-creo-31-distritos-nuevos-peru-225670-noticia/?utm_source=chatgpt.com).

## Data Collection
All datasets used in this study were accessed in 2021 from open-access repositories, unless otherwise specified. Sociodemographic data were obtained from the [INEI MicroDatos Repository](https://proyectos.inei.gob.pe/microdatos) via a custom Google Colab Notebook developed for this study, available [here](https://colab.research.google.com/drive/1X1AGu06S61092Y90uFJC4ArW3Mvn071i?usp=sharing). Climate data from the **PISCO** (*Peruvian Interpolation of the SENAMHI’s Climatological Observations*) dataset were retrieved from the SENAMHI open-data portal, which provides gridded temperature and precipitation records at daily and monthly resolutions, and accessed using institutional API endpoints and bulk download tools. Information on stunting and wasting in children under five years old was obtained from the Ministry of Health (**MINSA**) database, following a formal data request to the institution. 

## Citation

Please cite the article as:
> Mezza et al. (On Revision). *Framing Vulnerabilities: A Transdisciplinary Study of Dengue and Diarrhea in Peru’s Borderlands*. _The Journal of Climate Change and Health_.

