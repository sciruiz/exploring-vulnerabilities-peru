##
### 2. National Household Survey (ENAHO) variables
##

#Leonardo Doig-Alba
#Creation Date: 31/03/2022
#Last Modification: 08/06/2022

#Various possible co-founding variables are extracted from the Peruvian National Household Survey (ENAHO, Spanish acronym).
#Information is used at a province level.

#Parameters are extracted from various data sources which can be accessed in the following link(s):
#http://iinei.inei.gob.pe/microdatos/

#Preliminary extraction and aggregation process in the following Scripts:
#   "Annex F - enaho base aggregation",
#   "Annex G - changename".

#libraries
library(tidyverse)
library(readxl)

#file path
path_enaho <- "Bases de Datos/Database Compilation/ENAHO 2012-2016/"

#We import annual ENAHO databases 
list_files <- list.files(path = paste0(path_enaho, "District Level Data (all)"),
                         pattern = ".rds")


db_enaho <- readRDS(paste0(path_enaho, "District Level Data (all)/", list_files[1]))

for (i in 2:length(list_files)){
  db_enaho_year <- readRDS(paste0(path_enaho, "District Level Data (all)/", list_files[i]))
  db_enaho <- rbind(db_enaho, db_enaho_year)
  }

rm(db_enaho_year)

#We change the names of some variables and create the "district_ID" variable
db_enaho <- db_enaho %>% 
  rename (ubigeo = HO2UBIG, department = departamento, province = provincia, district = distrito) %>% 
  mutate(district_ID = paste(department, province, district, sep = "-"))

#NAs seem to be characters "NA" not actual NAs
table(is.na(db_enaho))

#We replace these values with real NAs
db_enaho[db_enaho == "NA"] <- NA
table(is.na(db_enaho))

#We view the NAs
(db_enaho[rowSums(is.na(db_enaho)) > 0,1:11])
#NAs are geographical variables for the MAYNAS-PUTUMAYO district. This is probably due to this district being
#changed in 2016 to PUTUMAYO province (no spatial info of a no-longer existing province-district).

#We validate that all district_IDs only have one ubigeo.
ER <- db_enaho %>%
  select(c(ubigeo, district_ID)) %>%
  group_by(ubigeo) %>%
  mutate(n = n_distinct(district_ID))

unique(ER$n) #1

#We validate that all ubigeos only have one district_ID.
ER <- db_enaho %>% 
  select(c(ubigeo, district_ID)) %>% 
  group_by(district_ID) %>% 
  mutate(n = n_distinct(ubigeo))

unique(ER$n) #1
rm(ER)

#The list of district_IDs and ubigeos for this database is exported and compared to other used data sources in:
#"Annex0 - Revision of district_ID and ubigeo differences between data sources.R"
spatial_units_enaho <- db_enaho %>% 
  select(c(department, province, district, district_ID, ubigeo)) %>% 
  distinct()

write.csv(spatial_units_enaho, "Bases de Datos/Database Compilation/districts and ubigeos/enaho.csv")

n_distinct(db_enaho$district_ID)
#Department, province and district names present various spelling errors and nomenclature differences,
#both between years and data sources (i.e. other databases).
#We standardize this by using a "district_ID - ubigeo" list generated from the INEI database ("Annex A - INEI estimated population.R").
#To further explore the differences between data sources, see "Annex0 - Revision of district_ID and ubigeo differences between data sources.R"

#We import the district ID_list
district_ID_list_all <- read.csv("Bases de Datos/Database Compilation/districts and ubigeos/inei_all.csv") %>% 
  select(-X)

#We replace the names of all departments, provinces, districts and district_IDs in the dengue database with the 
#newly imported list (match is done by ubigeo)

#We only keep the ubigeo variable as a spatial ID.
db_enaho <- db_enaho %>% 
  mutate(ubigeo = as.numeric(ubigeo)) %>% 
  select(-c("department", "province", "district", "district_ID"))
# 
# #We change the UBIGEO of the Putumayo/Teniente Manuel Clavero  district:
# #from 2012-2015 in the MAYNAS province, 2016 in PUTUMAYO province; this district is standardized as it is in 2015 (province = Maynas).
db_enaho$ubigeo[which(db_enaho$ubigeo == 160801)] <- 160109 #Putumayo district
db_enaho$ubigeo[which(db_enaho$ubigeo == 160803)] <- 160114 #Teniente Manuel Clavero district

#We add the imported district_ID variables to the enaho database.
db_enaho <- left_join(db_enaho, district_ID_list_all, by = "ubigeo")

#We validate that there are no NA values 
table(is.na(db_enaho)) #No NAs

#Lastly, some districts were created in 2016, these are re-arranged into their original spatial distribution (2012-2015)
district_ID_list <- db_enaho %>% mutate(district_ID = paste(department, province, district, sep = "-")) %>% 
  select(district_ID, ubigeo) %>% distinct() 

#Distritcs in "Ayacucho - Tayacaja"
# map of 2007: http://www.munitayacaja.gob.pe/pdf/memoria2007.pdf
# map of 2016: http://www.munitayacaja.gob.pe/tayacaja/pdf/ppt2017.pdf
db_enaho$district[which(db_enaho$province == "TAYACAJA" &
                               db_enaho$district=="QUICHUAS")] <- "COLCABAMBA"

db_enaho$district[which(db_enaho$province == "TAYACAJA" &
                               db_enaho$district=="ANDAYMARCA")] <- "COLCABAMBA"

db_enaho$district[which(db_enaho$province == "TAYACAJA" &
                               db_enaho$district=="PICHOS")] <- "HUARIBAMBA"

db_enaho$district[which(db_enaho$province == "TAYACAJA" &
                               db_enaho$district=="ROBLE")] <- "TINTAY PUNCU"

#Distritcs in "Huanuco - Huanuco"
# map of 2016: https://es.wikipedia.org/wiki/Distrito_de_Yacus / https://es.wikipedia.org/wiki/Distrito_de_San_Pablo_de_Pillao
# map of 2010: http://www.proviasdes.gob.pe/planes/huanuco/pvdp/PVDP_Huanuco_2010_2019.pdf

db_enaho$district[which(db_enaho$province == "HUANUCO" &
                               db_enaho$district=="YACUS")] <- "MARGOS"

db_enaho$district[which(db_enaho$province == "HUANUCO" &
                               db_enaho$district=="SAN PABLO DE PILLAO")] <- "CHINCHAO"

#Distritcs in "Huanuco - Leoncio Prado" (creacion 2015)
# map of 2008: https://www.munitingomaria.gob.pe/mplp/sites/default/files/mplp/documentosdegestion/PDC2008-2015.pdf
# map of 2021: https://es.wikipedia.org/wiki/Provincia_de_Leoncio_Prado
db_enaho$district[which(db_enaho$province == "LEONCIO PRADO" &
                               db_enaho$district=="PUCAYACU")] <- "JOSE CRESPO Y CASTILLO"

db_enaho$district[which(db_enaho$province == "LEONCIO PRADO" &
                               db_enaho$district=="CASTILLO GRANDE")] <- "RUPA-RUPA"

#Districts in "Huanuco - Marañon"
#map of https://www.indeci.gob.pe/wp-content/uploads/2020/03/REPORTE-COMPLEMENTARIO-N%C2%BA-1466-30MAR2020-HUAICO-EN-EL-DISTRITO-DE-HUACRACHUCO-HUANUCO.pdf
#map of 2021: https://es.wikipedia.org/wiki/Distrito_de_La_Morada / https://es.wikipedia.org/wiki/Distrito_de_Santa_Rosa_de_Alto_Yanajanca
db_enaho$district[which(db_enaho$province == "MARAÑON" &
                               db_enaho$district=="LA MORADA")] <- "CHOLON"

db_enaho$district[which(db_enaho$province == "MARAÑON" &
                               db_enaho$district=="SANTA ROSA DE ALTO YANAJANCA")] <- "CHOLON"

#Districts of "Junin - satipo"
# map of 2008: http://terra.iiap.gob.pe/assets/files/meso/08_zee_satipo/14_Antropologia.pdf
db_enaho$district[which(db_enaho$province == "SATIPO" &
                               db_enaho$district=="VIZCATAN DEL ENE")] <- "PANGOA"

#Provinces: Maynas and Putumayo (Loreto region)
#2021 map: https://es.wikipedia.org/wiki/Provincia_de_Putumayo
#previous map of MAYNAS https://portal.mtc.gob.pe/transportes/acuatico/documentos/estudios/Informaci%C3%B3n%20Socioecon%C3%B3mica.pdf

db_enaho$province[which(db_enaho$province == "PUTUMAYO")] <- "MAYNAS"

db_enaho$district[which(db_enaho$province == "MAYNAS" &
                               db_enaho$district=="ROSA PANDURO")] <- "TENIENTE MANUEL CLAVERO"

db_enaho$district[which(db_enaho$province == "MAYNAS" &
                               db_enaho$district=="YAGUAS")] <- "PUTUMAYO"

#Districts of "Pasco - Oxapampa" 
#Map 2021: https://es.wikipedia.org/wiki/Distrito_de_Constituci%C3%B3n
#2008 map: https://www.peru.gob.pe/docs/PLANES/12163/PLAN_12163_Plan%20Desarrollo%20Concertado%20de%20la%20Provincia%20de%20Oxapampa%20-Parte%202_2013.pdf
db_enaho$district[which(db_enaho$province == "OXAPAMPA" &
                               db_enaho$district=="CONSTITUCION")] <- "PUERTO BERMUDEZ"


#Districts of "Piura - Piura"
#2021 map: https://es.wikipedia.org/wiki/Distrito_de_Veintis%C3%A9is_de_Octubre
#2006 map: https://www.regionpiura.gob.pe/documentos/edz_piura_2011.pdf 
db_enaho$district[which(db_enaho$province == "PIURA" &
                               db_enaho$district=="VEINTISEIS DE OCTUBRE")] <- "PIURA"

#Districts of "Tacna-Tacna"
#2015 map: http://www.dge.gob.pe/portal/Asis/indreg/asis_tacna.pdf
#2021 map: https://es.wikipedia.org/wiki/Distrito_de_La_Yarada-Los_Palos
db_enaho$district[which(db_enaho$province == "TACNA" &
                               db_enaho$district=="LA YARADA LOS PALOS")] <- "TACNA"

#Districts of "Ucayali - Padre Abad"
# 2012 map: http://munipadreabad.gob.pe/phocadownload/DOCUMENTOSDEGESTION/pdc-2016.pdf
db_enaho$district[which(db_enaho$province == "PADRE ABAD" &
                               db_enaho$district=="NESHUYA")] <- "IRAZOLA"
db_enaho$district[which(db_enaho$province == "PADRE ABAD" &
                               db_enaho$district=="ALEXANDER VON HUMBOLDT")] <- "IRAZOLA"

#Districts of "Cusco - La Convencion"
# Map: http://www.diresacusco.gob.pe/ASISprov/laconvencion.pdf
#2021 Map: https://www.muniecharati.gob.pe/municipalidad-echarati/mapa-provincial/
db_enaho$district[which(db_enaho$province == "LA CONVENCION" &
                               db_enaho$district=="VILLA VIRGEN")] <- "VILCABAMBA"

db_enaho$district[which(db_enaho$province == "LA CONVENCION" &
                               db_enaho$district=="INKAWASI")] <- "VILCABAMBA"

db_enaho$district[which(db_enaho$province == "LA CONVENCION" &
                               db_enaho$district=="VILLA KINTIARINA")] <- "KIMBIRI"

#Districts of "Callao - Callao"
#Map 2011: http://sitr.regioncallao.gob.pe/catalogoDocumento/CAPITULOII_2011.pdf
#Map 2021: https://es.wikipedia.org/wiki/Distrito_de_Mi_Per%C3%BA
db_enaho$district[which(db_enaho$province == "CALLAO" &
                               db_enaho$district=="MI PERU")] <- "VENTANILLA"


#Districts of "Ayacucho - La Mar" 
#Map 2007: http://www.proviasdes.gob.pe/planes/vrae/pvpm_vrae.pdf
db_enaho$district[which(db_enaho$province == "LA MAR" &
                               db_enaho$district=="SAMUGARI")] <- "SAN MIGUEL"

db_enaho$district[which(db_enaho$province == "LA MAR" &
                               db_enaho$district=="ANCHIHUAY")] <- "ANCO"

#Districts in "Ayacucho - Huanta"
#MAP 2014 https://www.mef.gob.pe/contenidos/conv/TDR_AYACUCHO_A003.pdf

db_enaho$district[which(db_enaho$province == "HUANTA" &
                               db_enaho$district=="CANAYRE")] <- "LLOCHEGUA"

db_enaho$district[which(db_enaho$province == "HUANTA" &
                               db_enaho$district=="UCHURACCAY")] <- "HUANTA"

db_enaho$district[which(db_enaho$province == "HUANTA" &
                               db_enaho$district=="UCHURACCAY")] <- "HUANTA"

db_enaho$district[which(db_enaho$province == "HUANTA" &
                               db_enaho$district=="PUCACOLPA")] <- "AYAHUANCO"

db_enaho$district[which(db_enaho$province == "HUANTA" &
                               db_enaho$district=="CHACA")] <- "SANTILLANA"

#Districts in "Apurimac - Chincheros"
#2021 map: https://www.scribd.com/document/407323057/Mapa-Politico-de-Chincheros
#Mapa 2013: http://www.perutoptours.com/index03ch_mapa_provincia_chincheros.html
db_enaho$district[which(db_enaho$province == "CHINCHEROS" &
                               db_enaho$district=="EL PORVENIR")] <- "ONGOY"

db_enaho$district[which(db_enaho$province == "CHINCHEROS" &
                               db_enaho$district=="ROCCHACC")] <- "ONGOY"

#Districts in "Apurimac -Andahuaylas"
#2015 map: https://www.peru.gob.pe/docs/PLANES/10398/PLAN_10398_2015_PLAN_AVANZADO-FINAL.PDF
db_enaho$district[which(db_enaho$province == "ANDAHUAYLAS" &
                               db_enaho$district=="JOSE MARIA ARGUEDAS")] <- "ANDAHUAYLAS"

#Districts in "Ayacucho - Huamanga"
#2009 MAP: https://munihuamanga.gob.pe/Documentos_mph/Munitransparencia/Doc_gestion/Informe_gestion_anual/MGM_2009_Act.190410.pdf
db_enaho$district[which(db_enaho$province == "HUAMANGA" &
                               db_enaho$district=="ANDRES AVELINO CACERES DORREGARAY")] <- "SAN JUAN BAUTISTA"

#Districts in "Huancavelica - Churcampa"
#2007 map: http://www.proviasdes.gob.pe/planes/vrae/pvpm_vrae.pdf
db_enaho$district[which(db_enaho$province == "CHURCAMPA" &
                               db_enaho$district=="COSME")] <- "ANCO"

db_enaho$district_ID <- paste(db_enaho$department, db_enaho$province, db_enaho$district, sep = "-")
ER <- anti_join(db_enaho %>% select(-ubigeo), district_ID_list %>% select(-ubigeo), by ="district_ID")

#Lastly, ubigeos are eliminated and joined back into the database given the new district_IDs
db_enaho <- left_join(db_enaho %>% select(-ubigeo), district_ID_list_all, by ="district_ID")

#Given the previous district name changes, we have some districts with various estimated population values. 
#These will be summed.
db_enaho <- db_enaho %>% 
  group_by(year, department, province, district, district_ID, ubigeo) %>% 
  gather(var, val, c(9:80)) %>% 
  ungroup() %>% 
  group_by(year, department, province, district, district_ID, ubigeo, var) %>% 
  summarise(val = sum(as.numeric(val), na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, department, province, district, district_ID, ubigeo) %>% 
  spread(var, val) %>% ungroup()

#
### Standardization
#

#We standardize all variables by the number of people that answered each question in the Survey.
db_enaho_standardized <- db_enaho %>% ungroup() %>% 
  group_by(year, department, province, district, district_ID, ubigeo) %>% 
 summarise(children_under5 = ifelse((HOMe5.no + HOMe5.si != 0), HOMe5.si / (HOMe5.no + HOMe5.si), 0),
           no_electricity_access = ifelse((HOelec.no + HOelec.si != 0), HOelec.no / (HOelec.no + HOelec.si), 0),
           adults_over65 = ifelse((HOMa65.no + HOMa65.si != 0), HOMa65.si / (HOMa65.no + HOMa65.si), 0),
           non_spanish_speaking_pop = ifelse((HOCast.no + HOCast.si != 0), HOCast.no / (HOCast.no + HOCast.si), 0),
           female_population = ifelse((HOMujer.no + HOMujer.si != 0), HOMujer.si / (HOMujer.no + HOMujer.si), 0),
#           no_internet_access = ifelse((HOInt.no + HOInt.si != 0), HOInt.no / (HOInt.no + HOInt.si), 0),
           no_water_access = ifelse((HOWATx.no + HOWATx.si != 0), HOWATx.no / (HOWATx.no + HOWATx.si), 0),
           no_basic_sanitation_access = ifelse((HOSSHHx.no + HOMSSHHx.si != 0), HOSSHHx.no / (HOSSHHx.no + HOMSSHHx.si), 0),
#           no_health_insurance = ifelse((HOSEGSA.no + HOSEGSA.si != 0), HOSEGSA.no / (HOSEGSA.no + HOSEGSA.si), 0), 
           no_healthcare_access = ifelse((HOAH.no + HOAH.si != 0), HOAH.no / (HOAH.no + HOAH.si), 0),
           age_dependecy = ifelse((HOWDEP.no + HOWDEP.si != 0), HOWDEP.no / (HOWDEP.no + HOWDEP.si), 0),
           no_adult15_literarcy = ifelse((HOLIT15.no + HOLIT15.si != 0), HOLIT15.no / (HOLIT15.no + HOLIT15.si), 0),
#           no_adult25_secundary_education = ifelse((HOedu26.no + HOedu26.si != 0), HOedu26.no / (HOedu26.no + HOedu26.si), 0),
#          no_secundary_education = ifelse((HOMe5.no + HOMe5.si != 0), HOedu.no / HOpob.edu.t, 0),
           woman_household_lead = ifelse((HOJFW.no + HOJFW.si != 0), HOJFW.si / (HOJFW.no + HOJFW.si), 0),
           deaf_mute_pop = ifelse((HOdisc1.no + HOdisc1.si != 0), HOdisc1.si / (HOdisc1.no + HOdisc1.si), 0),
           migrant_pop = ifelse((HOMigra.no + HOMigra.si != 0), HOMigra.si / (HOMigra.no + HOMigra.si), 0),
           aymara_quechua_amazonic_pop = ifelse((HOindg1.si + HOindg1.no != 0), HOindg1.si / (HOindg1.si + HOindg1.no), 0),
           amazonic_indigenous_pop = ifelse((HOindg2.no + HOindg2.si != 0), HOindg2.si / (HOindg2.no + HOindg2.si),0),
           afroamerican_descendent_pop = ifelse((HOindg5.no + HOindg5.si != 0), HOindg5.si / (HOindg5.no + HOindg5.si),0))

#We validate the number of NAs in our database
table(is.na(db_enaho_standardized))
(db_enaho_standardized[rowSums(is.na(db_enaho_standardized)) > 0,]) #0

#We transformt non-infinite ("Inf" and "NaN") values to NAs, and visualize how many there are
db_enaho_standardized_infint <- do.call(data.frame,                      
                                        lapply(db_enaho_standardized,
                                               function(x) replace(x, !is.finite(x), NA)))

table(is.na(db_enaho_standardized_infint[7:22]))   #0

#We export the generated databases
write_rds(db_enaho, file = paste0(path_enaho, "ENAHO province 2012-2016.RDS")) 
#db_enaho <- readRDS(db_enaho, file = paste0(path_enaho, "ENAHO province 2012-2016.RDS")) 

write_rds(db_enaho_standardized, file = paste0(path_enaho, "standardized ENAHO 2012-2016.RDS")) 
#db_enaho_standardized <- readRDS(paste0(path_enaho, "standardized ENAHO 2012-2016.RDS")) 


