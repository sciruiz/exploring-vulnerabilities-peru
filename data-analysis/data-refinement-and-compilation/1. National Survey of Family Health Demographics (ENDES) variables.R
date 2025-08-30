##
### 1. National Survey of Family Health Demographics (ENDES) data
##

#Leonardo Doig-Alba
#Creation Date: 31/03/2022
#Last Modification: 07/08/2022

#Various possible co-founding variables are extracted from the Peruvian Survey of Family Health Demographics (ENDES for its Spanish acronym).
#This survey presents information at a regional level.
#Given that this study has a district level resolution, we requested information from the Ministry of Health (MINSA)
#at a district level for certain variables of the Survey. The used variables and their resolution are: 

# (i) wasting, (ii) stunting, (iii) adolescent mothers (source: MINSA, province resolution);

# (iv) violence towards woman, (v) diabetes, (Vi) never-married mothers with at least 1 son, and
# (vii) pregnant woman (source: ENDES, region resolution)

#ENDES variables are extracted from various data sources which can be accessed from the following platform:
#http://iinei.inei.gob.pe/microdatos/

#Preliminary extraction of variables and their aggregation process can be viewed in the following Scripts:
#   "Annex B - endes base aggregation.R",
#   "Annex C - woman violence variable generation.R".

#libraries
library(tidyverse)
library(readxl)

#file paths
path_endes <- "Bases de Datos/Database Compilation/ENDES 2012-2016/ENDES - regional level data"
path_MINSA <- "Bases de Datos/Database Compilation/ENDES 2012-2016/MINSA (variables at a district level)/"
path_export <- "Bases de Datos/Database Compilation/ENDES 2012-2016/"

#We import regional ENDES databases 
list_files <- list.files(path = path_endes,
                         pattern = ".rds")

db_endes <- readRDS(paste0(path_endes, "/", list_files[1])) %>% mutate(ENdiab.dk = NA)

for (i in 2:length(list_files)){
  db_endes_year <- readRDS(paste0(path_endes, "/", list_files[i])) %>% 
    mutate(ENdiab.dk = NA)

  # year <- as.numeric(unique(db_endes_year$year))
  # if(year %in% c(2014, 2015, 2016))
  #   db_endes_year <- db_endes_year %>% select(-c("ENdiab.dk"));
  # else
  #   db_endes_year <- db_endes_year
  #
  # db_endes_year <- ifelse((unique(db_endes_year$year) %in% c(2014, 2015, 2016)) == T, 
  #                          db_endes_year %>% select(-c("ENdiab.dk")), db_endes_year)
  db_endes <- rbind(db_endes, db_endes_year)
}
db_endes <- db_endes %>% select(-c("ENdiab.dk"))    #No entiendo por que no puedo eliminar la variable para ciertos years (2012,2013,2014) en el mismo loop. Intento esta arriba. Reduce script 

unique(db_endes$year) #4
n_distinct(db_endes$ENDREGION) #25

#We filter the variables that are of interest.
db_endes <- db_endes %>% 
  mutate(ENsm1.t = ENsm1.yes + ENsm1.no) %>% 
  select(c(ENDREGION, year,
           #ENstun.yes, ENwas.yes, ENteem.si,
           ENdiab.yes, ENdiab.t, ENvio.si, ENvio.t, ENsm1.yes, ENsm1.t, ENpreg.yes, ENpreg.TOTAL)) %>% 
  rename(department_num = ENDREGION,
         diabetes_cases = ENdiab.yes, diabetes_sample = ENdiab.t, 
         woman_violence_cases = ENvio.si, woman_violence_sample = ENvio.t, 
         single_mother_cases = ENsm1.yes, single_mother_sample = ENsm1.t,
         pregnant_woman_cases = ENpreg.yes, pregnant_woman_sample = ENpreg.TOTAL)

#We change the department numeric code into the department label (label-code association found in a supplementary data source: "Labels-subindicators.xlsx")
department_name_list <- read_xlsx(path = "Bases de Datos/Database Compilation/Labels- subindicators.xlsx", sheet = 3) %>% 
  rename(department_num = 1, department = Label)

db_endes <- left_join(db_endes, department_name_list) %>% 
  select(-c(department_num)) %>%
  mutate(department = toupper(department))

#We generate a database in which each variable is standardized given the sample number.
db_endes_standardized <- db_endes %>% 
  group_by(year, department) %>% 
  summarise(diabetes = diabetes_cases / diabetes_sample,
            woman_violence = woman_violence_cases / woman_violence_sample,
            single_mother = single_mother_cases / single_mother_sample,
            pregnant_woman = pregnant_woman_cases / pregnant_woman_sample)


#We then import databases generated for: (i) wasting & stunting, and (ii) adolescent mothers.
#These databases were generated in the following Scripts: 

#"Annex D - MINSA wasting and stunting database aggregation"
#"Annex E - MINSA adolescent mothers database aggregation"

wasting_stunting <- readRDS(paste0(path_export, "Standardized wasting and stunting 2012-2016.RDS"))
adolescent_mothers <- readRDS(paste0(path_export, "Standardized adolescent mothers from 2012-2016.RDS"))

# #We explore the number of provinces that each MINSA database has.
# n_distinct(wasting_stunting$province_ID)   #196
# n_distinct(adolescent_mothers$province_ID) #196
# #Difference in number of provinces between years is explored independently in each script (Annex D and Annex E).

#We join the MINSA databases with the ENDES database.
db_endes_standardized <- full_join (db_endes_standardized, wasting_stunting, by = c("year", "department")) 

db_endes_standardized <- full_join(db_endes_standardized, adolescent_mothers,
                                   by = c("year", "department","province", "district", "district_ID", "ubigeo"))

db_endes_standardized <- db_endes_standardized %>% 
  select(c("year", "department","province","district","district_ID","ubigeo",everything()))


#
### NA, NaN and Inf check
#

#Districts in which no adolescent mothers were registered were not included as rows in the dataframe, therefore
#they are now == NA. These are changed to 0
db_endes_standardized <- db_endes_standardized %>% 
  mutate(adolescent_mothers = ifelse((is.na(adolescent_mothers)) == T, 0, adolescent_mothers))

#We view NAs
table(is.na(db_endes_standardized))
ER <- db_endes_standardized[rowSums(is.na(db_endes_standardized)) > 0, 1:13] 
#NAs for ENDES/stunting & wasting databases. 
#However, no district has all 5 study years == NA. 
#Therefore, when estimated 5-year average, these NAs will not influence the number of data points.

#We transformt non-infinite ("Inf" and "NaN") values to NAs
db_endes_standardized_infint <- do.call(data.frame,                      # Replace Inf in data by NA
                                  lapply(db_endes_standardized,
                                         function(x) replace(x, !is.finite(x), NA)))

table(is.na(db_endes_standardized_infint[7:13])) 

#Same number as original NAs, no NaNs or Inf

#ER <- db_endes_standardized2[rowSums(is.na(db_endes_standardized2)) > 0, c(7:13),7:13]

write_rds(db_endes_standardized, file = paste0(path_export, "Standardized ENDES 2012-2016.RDS"))
#db_endes_standardized <- readRDS(paste0(path_export, "Standardized ENDES 2012-2016.RDS"))
 

  