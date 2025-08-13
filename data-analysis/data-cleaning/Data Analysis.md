# Framing Vulnerabilities: Dengue and Diarrhea in Peru’s Borderlands

This section outlines the data processing steps and analytical scripts utilized in the development of this publication. The objective is to ensure full transparency and reproducibility of our findings, in accordance with best practices in open and replicable research.

## Data analysis 
### Sociodemographical Data 
Data were retrieved from open-access governmental databases from Peru. Population- and infrastructure-related vulnerability factors, such as illiteracy rates and % population with access to clean water, were retrieved from the National Household Survey (ENAHO) and the Demographic and Family Health Survey (ENDES), both of which can be found in the Peruvian Institute of National Statistics and Computing interface (INEI, http://iinei.inei.gob.pe/microdatos/). Environmental variables (i.e., temperature and precipitation) were retrieved from the Peruvian Interpolated Data of SENAMHI’s Climatological and Hydrological Observations (PISCO; Aybar et al., 2017). Health-related information, such as population stunting rates, was requested from the Ministry of Health (MINSA). Dengue and diarrhea cases were retrieved from the National Center for Epidemiology, Prevention, and Disease Control (CDC), supervised by MINSA. 

For ENDES, data was only available at a department level, therefore we interpolated departmental values to all districts within. Given a high number of missing values for vulnerability factors at a district level, an average 2012-2016 value was calculated for each vulnerability indicator per district. Although this process jeopardizes the temporal resolution of the analysis, it strengthens district comparison given that it accounts for variations in geographic sampling between years and datasets. We standardized each vulnerability factor to account for differences in the surveyed populations. For datasets in which sample sizes were recorded, incidence rates were standardized using the sample number (e.g., stunting cases in Loreto 2012 was 150, the total surveyed population for that department and year was 665, the standardized value was 150/665). Variables without a specified sample population were standardized using the estimated annual district population (source: INEI). A normalization process was then applied to scale each vulnerability factor from 0 to 100. The scripts used are held in this repository: https://github.com/sciruiz/framing-vulnerabilities-peru. 

### Dialogue of Knowledge
<! include content here!>
## Citation

Please cite the article as:
> Mezza et al. (On Revision). *Framing Vulnerabilities: A Transdisciplinary Study of Dengue and Diarrhea in Peru’s Borderlands*. _The Journal of Climate Change and Health_.
