##
### 4. Dengue and diarrhea (CDC)
##

#Leonardo Doig-Alba
#Creation Date: 08/12/2021
#Last Modification: 07/08/2021

#Information of dengue and diarrhea incidence in Peru per district, province and department. 
#This information was granted by the National Center of Epidemiology, Prevention and Disease Control (CDC)
#of the Peruvian Ministry of Health (MINSA), given the Law of Transparency and Access to Public information (N°27806).

#Libraries
library(tidyverse)               # Data tidying
library(lubridate)               # Date variables
library(sf)                      # Spatial data

#Paths are generated for data access and exportation
path_CDC <- "Bases de Datos/Database Compilation/MINSA-CDC - dengue and diarrhea/"
path_inei <- "Bases de Datos/Database Compilation/INEI (estimated population)/"


#
### Dengue
#

#Dengue data is imported and variables are renamed
dengue <- readxl::read_xlsx(paste0(path_CDC, "dengue CDC-MINSA.xlsx"), sheet = 2) %>% 
  rename(year = ANO, week = SEMANA, sex = SEXO, department = DEPARTAM, province = PROVINCIA,
         district = DISTRITO, cases = CASOS, age = EDAD_A, age_group_quantile = EDAD_Q,
         ubigeo = UBIGEO) #%>% 
  filter(year >= 2012, year <= 2016)

#A "date" and "month" variable is created from the given information (year and week). 
dengue <- dengue %>%
#  filter(week != max(week[which(year ==2014)]) | year != 2014) %>% 
  mutate(date = as.Date(paste(year, week, 1, sep = "-"), format = "%Y-%U-%u")) %>% 
  mutate(month = month(date))

(dengue[rowSums(is.na(dengue)) > 0,])
#Week 53 is nonexistent in 2014 (Monday of week 52 is 29 of December...) probably a digitalization error. 

#We discard this week from the database
dengue <- dengue %>%  filter(year != 2014 | week != 53) 

#We group information per year, month, department and province and estimate the overall reported cases.
dengue_monthly <- dengue %>% 
  ungroup() %>% 
  group_by(year, month, department, province, district, ubigeo) %>%
  summarise(dengue_cases = sum(cases, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(district_ID = paste(department, province, district, sep = "-")) %>% 
  ungroup()

#We validate that all district_IDs only have one ubigeo.
ER <- dengue_monthly %>% 
  ungroup() %>% 
  select(c(ubigeo, district_ID)) %>% 
  group_by(district_ID) %>% 
  mutate(n = n_distinct(ubigeo)) %>% 
  ungroup()

unique(ER$n) #1

#We validate that all ubigeos only have one district_ID.
ER <- dengue_monthly %>% 
  ungroup() %>% 
  select(c(ubigeo, district_ID)) %>% 
  group_by(ubigeo) %>% 
  mutate(n = n_distinct(district_ID)) %>% 
  ungroup()

unique(ER$n) #1
rm(ER)

#The list of district_IDs and ubigeos for this database is exported and compared to other used data sources in:
#"Annex0 - Revision of district_ID and ubigeo differences between data sources.R"
spatial_units_dengue <- dengue_monthly %>% 
  select(c(department, province, district, district_ID, ubigeo)) %>% 
  distinct() %>% 
  ungroup()
  
write.csv(spatial_units_dengue, "Bases de Datos/Database Compilation/districts and ubigeos/dengue.csv")

#Department, province and district names present various spelling errors and nomenclature differences,
#both between years and data sources (i.e. other databases).
#We standardize this by using a "district_ID - ubigeo" list generated from the INEI database ("Annex A - INEI estimated population.R").
#To further explore the differences between data sources, see "Annex0 - Revision of district_ID and ubigeo differences between data sources.R"

#We import the district ID_list
district_ID_list_all <- read.csv("Bases de Datos/Database Compilation/districts and ubigeos/inei_all.csv") %>% 
  select(-X)

#We replace the names of all departments, provinces, districts and district_IDs in the dengue database with the 
#newly imported list (match is done by ubigeo)

#We only keep the ubigeo variable as a spatial ID
dengue_monthly <- dengue_monthly %>% 
  mutate(ubigeo = as.numeric(ubigeo)) %>% 
  select(-c("department", "province", "district", "district_ID"))

#We add the imported district_ID variables to the dengue database.
dengue_monthly <- left_join(dengue_monthly, district_ID_list_all, by = "ubigeo")

#We change the UBIGEO of the Putumayo/Teniente Manuel Clavero  district:
#from 2012-2015 in the MAYNAS province, 2016 in PUTUMAYO province; this district is standardized as it is in 2016 (province = Putumayo).
dengue_monthly$province[which(dengue_monthly$province == "PUTUMAYO")] <- "MAYNAS"

dengue_monthly$ubigeo[which(dengue_monthly$ubigeo == 160801)] <- 160109 #Putumayo district
dengue_monthly$ubigeo[which(dengue_monthly$ubigeo == 160803)] <- 160114 #Teniente Manuel Clavero district

#We validate that there are no NA values 
table(is.na(dengue_monthly))

#Lastly, some districts were created in 2016, these are re-arranged into their original spatial distribution (2012-2015)

district_ID_list <- dengue_monthly %>% mutate(district_ID = paste(department, province, district, sep = "-")) %>% 
  select(district_ID, ubigeo) %>% distinct() 

#Distritcs in "Ayacucho - Tayacaja"
# map of 2007: http://www.munitayacaja.gob.pe/pdf/memoria2007.pdf
# map of 2016: http://www.munitayacaja.gob.pe/tayacaja/pdf/ppt2017.pdf
dengue_monthly$district[which(dengue_monthly$province == "TAYACAJA" &
                          dengue_monthly$district=="QUICHUAS")] <- "COLCABAMBA"

dengue_monthly$district[which(dengue_monthly$province == "TAYACAJA" &
                          dengue_monthly$district=="ANDAYMARCA")] <- "COLCABAMBA"

dengue_monthly$district[which(dengue_monthly$province == "TAYACAJA" &
                          dengue_monthly$district=="PICHOS")] <- "HUARIBAMBA"

dengue_monthly$district[which(dengue_monthly$province == "TAYACAJA" &
                          dengue_monthly$district=="ROBLE")] <- "TINTAY PUNCU"

#Distritcs in "Huanuco - Huanuco"
# map of 2016: https://es.wikipedia.org/wiki/Distrito_de_Yacus / https://es.wikipedia.org/wiki/Distrito_de_San_Pablo_de_Pillao
# map of 2010: http://www.proviasdes.gob.pe/planes/huanuco/pvdp/PVDP_Huanuco_2010_2019.pdf

dengue_monthly$district[which(dengue_monthly$province == "HUANUCO" &
                          dengue_monthly$district=="YACUS")] <- "MARGOS"

dengue_monthly$district[which(dengue_monthly$province == "HUANUCO" &
                          dengue_monthly$district=="SAN PABLO DE PILLAO")] <- "CHINCHAO"

#Distritcs in "Huanuco - Leoncio Prado" (creacion 2015)
# map of 2008: https://www.munitingomaria.gob.pe/mplp/sites/default/files/mplp/documentosdegestion/PDC2008-2015.pdf
# map of 2021: https://es.wikipedia.org/wiki/Provincia_de_Leoncio_Prado
dengue_monthly$district[which(dengue_monthly$province == "LEONCIO PRADO" &
                          dengue_monthly$district=="PUCAYACU")] <- "JOSE CRESPO Y CASTILLO"

dengue_monthly$district[which(dengue_monthly$province == "LEONCIO PRADO" &
                          dengue_monthly$district=="CASTILLO GRANDE")] <- "RUPA-RUPA"

#Districts in "Huanuco - Marañon"
#map of https://www.indeci.gob.pe/wp-content/uploads/2020/03/REPORTE-COMPLEMENTARIO-N%C2%BA-1466-30MAR2020-HUAICO-EN-EL-DISTRITO-DE-HUACRACHUCO-HUANUCO.pdf
#map of 2021: https://es.wikipedia.org/wiki/Distrito_de_La_Morada / https://es.wikipedia.org/wiki/Distrito_de_Santa_Rosa_de_Alto_Yanajanca
dengue_monthly$district[which(dengue_monthly$province == "MARAÑON" &
                          dengue_monthly$district=="LA MORADA")] <- "CHOLON"

dengue_monthly$district[which(dengue_monthly$province == "MARAÑON" &
                          dengue_monthly$district=="SANTA ROSA DE ALTO YANAJANCA")] <- "CHOLON"

#Districts of "Junin - satipo"
# map of 2008: http://terra.iiap.gob.pe/assets/files/meso/08_zee_satipo/14_Antropologia.pdf
dengue_monthly$district[which(dengue_monthly$province == "SATIPO" &
                          dengue_monthly$district=="VIZCATAN DEL ENE")] <- "PANGOA"

#Provinces: Maynas and Putumayo (Loreto region)
#2021 map: https://es.wikipedia.org/wiki/Provincia_de_Putumayo
#previous map of MAYNAS https://portal.mtc.gob.pe/transportes/acuatico/documentos/estudios/Informaci%C3%B3n%20Socioecon%C3%B3mica.pdf

dengue_monthly$district[which(dengue_monthly$province == "MAYNAS" &
                          dengue_monthly$district=="ROSA PANDURO")] <- "TENIENTE MANUEL CLAVERO"

dengue_monthly$district[which(dengue_monthly$province == "MAYNAS" &
                          dengue_monthly$district=="YAGUAS")] <- "PUTUMAYO"

#Districts of "Pasco - Oxapampa" 
#Map 2021: https://es.wikipedia.org/wiki/Distrito_de_Constituci%C3%B3n
#2008 map: https://www.peru.gob.pe/docs/PLANES/12163/PLAN_12163_Plan%20Desarrollo%20Concertado%20de%20la%20Provincia%20de%20Oxapampa%20-Parte%202_2013.pdf
dengue_monthly$district[which(dengue_monthly$province == "OXAPAMPA" &
                          dengue_monthly$district=="CONSTITUCION")] <- "PUERTO BERMUDEZ"


#Districts of "Piura - Piura"
#2021 map: https://es.wikipedia.org/wiki/Distrito_de_Veintis%C3%A9is_de_Octubre
#2006 map: https://www.regionpiura.gob.pe/documentos/edz_piura_2011.pdf 
dengue_monthly$district[which(dengue_monthly$province == "PIURA" &
                          dengue_monthly$district=="VEINTISEIS DE OCTUBRE")] <- "PIURA"

#Districts of "Tacna-Tacna"
#2015 map: http://www.dge.gob.pe/portal/Asis/indreg/asis_tacna.pdf
#2021 map: https://es.wikipedia.org/wiki/Distrito_de_La_Yarada-Los_Palos
dengue_monthly$district[which(dengue_monthly$province == "TACNA" &
                          dengue_monthly$district=="LA YARADA LOS PALOS")] <- "TACNA"

#Districts of "Ucayali - Padre Abad"
# 2012 map: http://munipadreabad.gob.pe/phocadownload/DOCUMENTOSDEGESTION/pdc-2016.pdf
dengue_monthly$district[which(dengue_monthly$province == "PADRE ABAD" &
                          dengue_monthly$district=="NESHUYA")] <- "IRAZOLA"
dengue_monthly$district[which(dengue_monthly$province == "PADRE ABAD" &
                          dengue_monthly$district=="ALEXANDER VON HUMBOLDT")] <- "IRAZOLA"

#Districts of "Cusco - La Convencion"
# Map: http://www.diresacusco.gob.pe/ASISprov/laconvencion.pdf
#2021 Map: https://www.muniecharati.gob.pe/municipalidad-echarati/mapa-provincial/
dengue_monthly$district[which(dengue_monthly$province == "LA CONVENCION" &
                          dengue_monthly$district=="VILLA VIRGEN")] <- "VILCABAMBA"

dengue_monthly$district[which(dengue_monthly$province == "LA CONVENCION" &
                          dengue_monthly$district=="INKAWASI")] <- "VILCABAMBA"

dengue_monthly$district[which(dengue_monthly$province == "LA CONVENCION" &
                          dengue_monthly$district=="VILLA KINTIARINA")] <- "KIMBIRI"

#Districts of "Callao - Callao"
#Map 2011: http://sitr.regioncallao.gob.pe/catalogoDocumento/CAPITULOII_2011.pdf
#Map 2021: https://es.wikipedia.org/wiki/Distrito_de_Mi_Per%C3%BA
dengue_monthly$district[which(dengue_monthly$province == "CALLAO" &
                          dengue_monthly$district=="MI PERU")] <- "VENTANILLA"


#Districts of "Ayacucho - La Mar" 
#Map 2007: http://www.proviasdes.gob.pe/planes/vrae/pvpm_vrae.pdf
dengue_monthly$district[which(dengue_monthly$province == "LA MAR" &
                          dengue_monthly$district == "SAMUGARI")] <- "SAN MIGUEL"

dengue_monthly$district[which(dengue_monthly$province == "LA MAR" &
                          dengue_monthly$district=="ANCHIHUAY")] <- "ANCO"

#Districts in "Ayacucho - Huanta"
#MAP 2014 https://www.mef.gob.pe/contenidos/conv/TDR_AYACUCHO_A003.pdf

dengue_monthly$district[which(dengue_monthly$province == "HUANTA" &
                          dengue_monthly$district=="CANAYRE")] <- "LLOCHEGUA"

dengue_monthly$district[which(dengue_monthly$province == "HUANTA" &
                          dengue_monthly$district=="UCHURACCAY")] <- "HUANTA"

dengue_monthly$district[which(dengue_monthly$province == "HUANTA" &
                          dengue_monthly$district=="UCHURACCAY")] <- "HUANTA"

dengue_monthly$district[which(dengue_monthly$province == "HUANTA" &
                          dengue_monthly$district=="PUCACOLPA")] <- "AYAHUANCO"

dengue_monthly$district[which(dengue_monthly$province == "HUANTA" &
                          dengue_monthly$district=="CHACA")] <- "SANTILLANA"

#Districts in "Apurimac - Chincheros"
#2021 map: https://www.scribd.com/document/407323057/Mapa-Politico-de-Chincheros
#Mapa 2013: http://www.perutoptours.com/index03ch_mapa_provincia_chincheros.html
dengue_monthly$district[which(dengue_monthly$province == "CHINCHEROS" &
                          dengue_monthly$district=="EL PORVENIR")] <- "ONGOY"

dengue_monthly$district[which(dengue_monthly$province == "CHINCHEROS" &
                          dengue_monthly$district=="ROCCHACC")] <- "ONGOY"

#Districts in "Apurimac -Andahuaylas"
#2015 map: https://www.peru.gob.pe/docs/PLANES/10398/PLAN_10398_2015_PLAN_AVANZADO-FINAL.PDF
dengue_monthly$district[which(dengue_monthly$province == "ANDAHUAYLAS" &
                          dengue_monthly$district=="JOSE MARIA ARGUEDAS")] <- "ANDAHUAYLAS"

#Districts in "Ayacucho - Huamanga"
#2009 MAP: https://munihuamanga.gob.pe/Documentos_mph/Munitransparencia/Doc_gestion/Informe_gestion_anual/MGM_2009_Act.190410.pdf
dengue_monthly$district[which(dengue_monthly$province == "HUAMANGA" &
                          dengue_monthly$district=="ANDRES AVELINO CACERES DORREGARAY")] <- "SAN JUAN BAUTISTA"

#Districts in "Huancavelica - Churcampa"
#2007 map: http://www.proviasdes.gob.pe/planes/vrae/pvpm_vrae.pdf
dengue_monthly$district[which(dengue_monthly$province == "CHURCAMPA" &
                          dengue_monthly$district=="COSME")] <- "ANCO"

dengue_monthly$district_ID <- paste(dengue_monthly$department, dengue_monthly$province, dengue_monthly$district, sep = "-")
ER <- anti_join(dengue_monthly %>% select(-ubigeo), district_ID_list %>% select(-ubigeo), by ="district_ID")

#Lastly, ubigeos are eliminated and joined back into the database given the new district_IDs
dengue_monthly <- left_join(dengue_monthly %>% select(-ubigeo), district_ID_list, by ="district_ID"); rm(district_ID_list)

#Given the previous district name changes, we have some districts with various estimated population values. 
#These will be summed.
dengue_monthly <- dengue_monthly %>% 
  group_by(year, month, department, province, district, district_ID, ubigeo) %>% 
  summarise(dengue_cases = sum(dengue_cases, na.rm = T))

#En error was found: AYACUCHO-LA MAR-SAN MIGUEL district in 2016 has no ubigeo. 
#The respective ubigeo is added
dengue_monthly$ubigeo[which(dengue_monthly$district_ID == "AYACUCHO-LA MAR-SAN MIGUEL" & dengue_monthly$year ==2016)] <- 50501

#
### Diarrhea
#

#Diarrhea data is imported and variables are renamed.
diarrhea <- readxl::read_xlsx(paste0(path_CDC, "diarrhea CDC-MINSA.xlsx"), sheet = 2) %>% 
  rename(year = ANO, week = SEMANA, department = DEPARTAM, province = PROVINCIA, district = DISTRITO, total_cases = EDA_total,
         cases_under_1y = `EDA< de 1 año`, cases_between_1_4y = `EDA_1-4 años`, cases_under_5y =`EDA_> 5 años`,
         ubigeo = UBIGEO) %>% 
  select(-c("MES_CALEND")) %>% 
  filter(year >= 2012, year <= 2016)

diarrhea <- diarrhea %>%
  mutate(date = as.Date(paste(year, week, 1, sep = "-"),format = "%Y-%U-%u")) %>% 
  mutate(month = month(date))

#We group information per year, month, department and province and estimate the overall reported cases.
diarrhea_monthly <- diarrhea %>% 
  group_by(year, month, department, province, district, ubigeo) %>% 
  summarise(diarrhea_cases = sum(total_cases, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(district_ID = paste(department, province, district, sep = "-"))

#We validate that all district_IDs only have one ubigeo.
ER <- diarrhea_monthly %>%
  select(c(ubigeo, district_ID)) %>%
  group_by(ubigeo) %>%
  mutate(n = n_distinct(district_ID))

unique(ER$n) #1

#We validate that all ubigeos only have one district_ID.
ER <- diarrhea_monthly %>% 
  select(c(ubigeo, district_ID)) %>% 
  group_by(ubigeo) %>% 
  mutate(n = n_distinct(district_ID))

unique(ER$n) #1
rm(ER)

#The list of district_IDs and ubigeos for this database is exported and compared to other used data sources in:
#"Annex0 - Revision of district_ID and ubigeo differences between data sources.R"
spatial_units_diarrhea <- diarrhea_monthly %>% 
  rename(ubigeo = ubigeo) %>% 
  select(c(department, province, district, district_ID, ubigeo)) %>% 
  distinct()

#write.csv(spatial_units_diarrhea, "Bases de Datos/Database Compilation/districts and ubigeos/diarrhea.csv")

#Department, province and district names present various spelling errors and nomenclature differences,
#both between years and data sources.
#We standardize this by using a "district_ID - ubigeo" list generated from the INEI database ("Annex A - INEI estimated population.R").
#To further explore the differences between data sources, see "Annex0 - Revision of district_ID and ubigeo differences between data sources.R"

#We replace the names of all departments, provinces, districts and district_IDs in the diarrhea database with the 
#newly imported list (match is done by ubigeo)

diarrhea_monthly <- diarrhea_monthly %>% 
  mutate(ubigeo = as.numeric(ubigeo)) %>% 
  select(-c("department", "province", "district", "district_ID"))

#We change the UBIGEO of the Putumayo/Teniente Manuel Clavero  district:
#from 2012-2015 in the MAYNAS province, 2016 in PUTUMAYO province; this district is standardized as in 2015.

district_ID_list_all <- read.csv("Bases de Datos/Database Compilation/districts and ubigeos/inei_all.csv") %>% 
  select(-X)

#We add the district_ID variables to the diarrhea database.
diarrhea_monthly <- left_join(diarrhea_monthly, district_ID_list_all, by = "ubigeo")

#We change the UBIGEO of the Putumayo/Teniente Manuel Clavero  district:
#from 2012-2015 in the MAYNAS province, 2016 in PUTUMAYO province; this district is standardized as it is in 2016 (province = Putumayo).
diarrhea_monthly$province[which(diarrhea_monthly$province == "PUTUMAYO")] <- "MAYNAS"

diarrhea_monthly$ubigeo[which(diarrhea_monthly$ubigeo == 160801)] <- 160109 #Putumayo district
diarrhea_monthly$ubigeo[which(diarrhea_monthly$ubigeo == 160803)] <- 160114 #Teniente Manuel Clavero district

#We validate that there are no NA values 
table(is.na(diarrhea_monthly))

#Lastly, some districts were created in 2016, these are re-arranged into their original spatial distribution (2012-2015)

district_ID_list <- diarrhea_monthly %>%mutate(district_ID = paste(department, province, district, sep = "-")) %>% 
  select(district_ID, ubigeo) %>% distinct() 

#Distritcs in "Ayacucho - Tayacaja"
# map of 2007: http://www.munitayacaja.gob.pe/pdf/memoria2007.pdf
# map of 2016: http://www.munitayacaja.gob.pe/tayacaja/pdf/ppt2017.pdf
diarrhea_monthly$district[which(diarrhea_monthly$province == "TAYACAJA" &
                          diarrhea_monthly$district=="QUICHUAS")] <- "COLCABAMBA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "TAYACAJA" &
                          diarrhea_monthly$district=="ANDAYMARCA")] <- "COLCABAMBA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "TAYACAJA" &
                          diarrhea_monthly$district=="PICHOS")] <- "HUARIBAMBA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "TAYACAJA" &
                          diarrhea_monthly$district=="ROBLE")] <- "TINTAY PUNCU"

#Distritcs in "Huanuco - Huanuco"
# map of 2016: https://es.wikipedia.org/wiki/Distrito_de_Yacus / https://es.wikipedia.org/wiki/Distrito_de_San_Pablo_de_Pillao
# map of 2010: http://www.proviasdes.gob.pe/planes/huanuco/pvdp/PVDP_Huanuco_2010_2019.pdf

diarrhea_monthly$district[which(diarrhea_monthly$province == "HUANUCO" &
                          diarrhea_monthly$district=="YACUS")] <- "MARGOS"

diarrhea_monthly$district[which(diarrhea_monthly$province == "HUANUCO" &
                          diarrhea_monthly$district=="SAN PABLO DE PILLAO")] <- "CHINCHAO"

#Distritcs in "Huanuco - Leoncio Prado" (creacion 2015)
# map of 2008: https://www.munitingomaria.gob.pe/mplp/sites/default/files/mplp/documentosdegestion/PDC2008-2015.pdf
# map of 2021: https://es.wikipedia.org/wiki/Provincia_de_Leoncio_Prado
diarrhea_monthly$district[which(diarrhea_monthly$province == "LEONCIO PRADO" &
                          diarrhea_monthly$district=="PUCAYACU")] <- "JOSE CRESPO Y CASTILLO"

diarrhea_monthly$district[which(diarrhea_monthly$province == "LEONCIO PRADO" &
                          diarrhea_monthly$district=="CASTILLO GRANDE")] <- "RUPA-RUPA"

#Districts in "Huanuco - Marañon"
#map of https://www.indeci.gob.pe/wp-content/uploads/2020/03/REPORTE-COMPLEMENTARIO-N%C2%BA-1466-30MAR2020-HUAICO-EN-EL-DISTRITO-DE-HUACRACHUCO-HUANUCO.pdf
#map of 2021: https://es.wikipedia.org/wiki/Distrito_de_La_Morada / https://es.wikipedia.org/wiki/Distrito_de_Santa_Rosa_de_Alto_Yanajanca
diarrhea_monthly$district[which(diarrhea_monthly$province == "MARAÑON" &
                          diarrhea_monthly$district=="LA MORADA")] <- "CHOLON"

diarrhea_monthly$district[which(diarrhea_monthly$province == "MARAÑON" &
                          diarrhea_monthly$district=="SANTA ROSA DE ALTO YANAJANCA")] <- "CHOLON"

#Districts of "Junin - satipo"
# map of 2008: http://terra.iiap.gob.pe/assets/files/meso/08_zee_satipo/14_Antropologia.pdf
diarrhea_monthly$district[which(diarrhea_monthly$province == "SATIPO" &
                          diarrhea_monthly$district=="VIZCATAN DEL ENE")] <- "PANGOA"

#Provinces: Maynas and Putumayo (Loreto region)
#2021 map: https://es.wikipedia.org/wiki/Provincia_de_Putumayo
#previous map of MAYNAS https://portal.mtc.gob.pe/transportes/acuatico/documentos/estudios/Informaci%C3%B3n%20Socioecon%C3%B3mica.pdf

diarrhea_monthly$province[which(diarrhea_monthly$province == "PUTUMAYO")] <- "MAYNAS"

diarrhea_monthly$district[which(diarrhea_monthly$province == "MAYNAS" &
                          diarrhea_monthly$district=="ROSA PANDURO")] <- "TENIENTE MANUEL CLAVERO"

diarrhea_monthly$district[which(diarrhea_monthly$province == "MAYNAS" &
                          diarrhea_monthly$district=="YAGUAS")] <- "PUTUMAYO"

#Districts of "Pasco - Oxapampa" 
#Map 2021: https://es.wikipedia.org/wiki/Distrito_de_Constituci%C3%B3n
#2008 map: https://www.peru.gob.pe/docs/PLANES/12163/PLAN_12163_Plan%20Desarrollo%20Concertado%20de%20la%20Provincia%20de%20Oxapampa%20-Parte%202_2013.pdf
diarrhea_monthly$district[which(diarrhea_monthly$province == "OXAPAMPA" &
                          diarrhea_monthly$district=="CONSTITUCION")] <- "PUERTO BERMUDEZ"


#Districts of "Piura - Piura"
#2021 map: https://es.wikipedia.org/wiki/Distrito_de_Veintis%C3%A9is_de_Octubre
#2006 map: https://www.regionpiura.gob.pe/documentos/edz_piura_2011.pdf 
diarrhea_monthly$district[which(diarrhea_monthly$province == "PIURA" &
                          diarrhea_monthly$district=="VEINTISEIS DE OCTUBRE")] <- "PIURA"

#Districts of "Tacna-Tacna"
#2015 map: http://www.dge.gob.pe/portal/Asis/indreg/asis_tacna.pdf
#2021 map: https://es.wikipedia.org/wiki/Distrito_de_La_Yarada-Los_Palos
diarrhea_monthly$district[which(diarrhea_monthly$province == "TACNA" &
                          diarrhea_monthly$district=="LA YARADA LOS PALOS")] <- "TACNA"

#Districts of "Ucayali - Padre Abad"
# 2012 map: http://munipadreabad.gob.pe/phocadownload/DOCUMENTOSDEGESTION/pdc-2016.pdf
diarrhea_monthly$district[which(diarrhea_monthly$province == "PADRE ABAD" &
                          diarrhea_monthly$district=="NESHUYA")] <- "IRAZOLA"
diarrhea_monthly$district[which(diarrhea_monthly$province == "PADRE ABAD" &
                          diarrhea_monthly$district=="ALEXANDER VON HUMBOLDT")] <- "IRAZOLA"

#Districts of "Cusco - La Convencion"
# Map: http://www.diresacusco.gob.pe/ASISprov/laconvencion.pdf
#2021 Map: https://www.muniecharati.gob.pe/municipalidad-echarati/mapa-provincial/
diarrhea_monthly$district[which(diarrhea_monthly$province == "LA CONVENCION" &
                          diarrhea_monthly$district=="VILLA VIRGEN")] <- "VILCABAMBA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "LA CONVENCION" &
                          diarrhea_monthly$district=="INKAWASI")] <- "VILCABAMBA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "LA CONVENCION" &
                          diarrhea_monthly$district=="VILLA KINTIARINA")] <- "KIMBIRI"

#Districts of "Callao - Callao"
#Map 2011: http://sitr.regioncallao.gob.pe/catalogoDocumento/CAPITULOII_2011.pdf
#Map 2021: https://es.wikipedia.org/wiki/Distrito_de_Mi_Per%C3%BA
diarrhea_monthly$district[which(diarrhea_monthly$province == "CALLAO" &
                          diarrhea_monthly$district=="MI PERU")] <- "VENTANILLA"


#Districts of "Ayacucho - La Mar" 
#Map 2007: http://www.proviasdes.gob.pe/planes/vrae/pvpm_vrae.pdf
diarrhea_monthly$district[which(diarrhea_monthly$province == "LA MAR" &
                          diarrhea_monthly$district == "SAMUGARI")] <- "SAN MIGUEL"

diarrhea_monthly$district[which(diarrhea_monthly$province == "LA MAR" &
                          diarrhea_monthly$district=="ANCHIHUAY")] <- "ANCO"

#Districts in "Ayacucho - Huanta"
#MAP 2014 https://www.mef.gob.pe/contenidos/conv/TDR_AYACUCHO_A003.pdf

diarrhea_monthly$district[which(diarrhea_monthly$province == "HUANTA" &
                          diarrhea_monthly$district=="CANAYRE")] <- "LLOCHEGUA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "HUANTA" &
                          diarrhea_monthly$district=="UCHURACCAY")] <- "HUANTA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "HUANTA" &
                          diarrhea_monthly$district=="UCHURACCAY")] <- "HUANTA"

diarrhea_monthly$district[which(diarrhea_monthly$province == "HUANTA" &
                          diarrhea_monthly$district=="PUCACOLPA")] <- "AYAHUANCO"

diarrhea_monthly$district[which(diarrhea_monthly$province == "HUANTA" &
                          diarrhea_monthly$district=="CHACA")] <- "SANTILLANA"

#Districts in "Apurimac - Chincheros"
#2021 map: https://www.scribd.com/document/407323057/Mapa-Politico-de-Chincheros
#Mapa 2013: http://www.perutoptours.com/index03ch_mapa_provincia_chincheros.html
diarrhea_monthly$district[which(diarrhea_monthly$province == "CHINCHEROS" &
                          diarrhea_monthly$district=="EL PORVENIR")] <- "ONGOY"

diarrhea_monthly$district[which(diarrhea_monthly$province == "CHINCHEROS" &
                          diarrhea_monthly$district=="ROCCHACC")] <- "ONGOY"

#Districts in "Apurimac -Andahuaylas"
#2015 map: https://www.peru.gob.pe/docs/PLANES/10398/PLAN_10398_2015_PLAN_AVANZADO-FINAL.PDF
diarrhea_monthly$district[which(diarrhea_monthly$province == "ANDAHUAYLAS" &
                          diarrhea_monthly$district=="JOSE MARIA ARGUEDAS")] <- "ANDAHUAYLAS"

#Districts in "Ayacucho - Huamanga"
#2009 MAP: https://munihuamanga.gob.pe/Documentos_mph/Munitransparencia/Doc_gestion/Informe_gestion_anual/MGM_2009_Act.190410.pdf
diarrhea_monthly$district[which(diarrhea_monthly$province == "HUAMANGA" &
                          diarrhea_monthly$district=="ANDRES AVELINO CACERES DORREGARAY")] <- "SAN JUAN BAUTISTA"

#Districts in "Huancavelica - Churcampa"
#2007 map: http://www.proviasdes.gob.pe/planes/vrae/pvpm_vrae.pdf
diarrhea_monthly$district[which(diarrhea_monthly$province == "CHURCAMPA" &
                          diarrhea_monthly$district=="COSME")] <- "ANCO"

diarrhea_monthly$district_ID <- paste(diarrhea_monthly$department, diarrhea_monthly$province, diarrhea_monthly$district, sep = "-")

#Lastly, ubigeos are eliminated and joined back into the database given the new district_IDs
diarrhea_monthly <- left_join(diarrhea_monthly %>% select(-ubigeo),
                              district_ID_list, by ="district_ID") #; rm(district_ID_list)

#Given the previous district name changes, we have some districts with various estimated population values. 
#These will be summed.
diarrhea_monthly <- diarrhea_monthly %>% 
  group_by(year, month, department, province, district, district_ID, ubigeo) %>% 
  summarise(diarrhea_cases = sum(diarrhea_cases, na.rm = T))


#
### Database Join
#

#We then join the dengue and diarrhea databases.
dengue_diarrhea <- full_join(diarrhea_monthly, dengue_monthly,
                             by = c("year", "month", "department", "province", "district", "district_ID", "ubigeo"))

rm(diarrhea_monthly); rm(diarrhea); rm(dengue_monthly); rm(dengue)

#We estimate one dengue/diarrhea values per year for each district. 
#Given that values might fluctuate strongly between seasons, an average value might sub-estimate anual values.
#Therefore, maximum number of diarrhea/dengue cases are used as annual proxy values.
dengue_diarrhea <- dengue_diarrhea %>% 
  group_by(year, department, province, district, district_ID, ubigeo) %>% 
  summarise(diarrhea_cases = max(diarrhea_cases, na.rm = T), dengue_cases = max(dengue_cases, na.rm = T))

#Various districts are NA for entire years, specifically for dengue (districts where dengue vector is not present)
#When we estimate maximum value removing NAs, -Inf is imputed since no values !=NA existe.
#We transform -Inf to NA.
dengue_diarrhea <- dengue_diarrhea %>% 
  mutate(diarrhea_cases = as.character(diarrhea_cases), dengue_cases = as.character(dengue_cases),
         diarrhea_cases = ifelse((diarrhea_cases == "-Inf") == T, NA, diarrhea_cases),
         dengue_cases = ifelse((dengue_cases == "-Inf") == T, NA, dengue_cases),
         diarrhea_cases = as.numeric(diarrhea_cases), dengue_cases = as.numeric(dengue_cases))


#
### Standardization
#

#The N° dengue and diarrhea cases are standardized given the estimated population per province.
#These population estimates were exported from the Script "Annex A - INEI estimated population".

inei_population <- readRDS(paste0(path_inei, "est. population 2012-2016 (district).RDS"))

#We aggregate the databases
names(dengue_diarrhea); names(inei_population)
dengue_diarrhea <- left_join(dengue_diarrhea, inei_population)

#We standardize the dengue and diarrhea cases given the province estimated population values.
dengue_diarrhea_standardized <- dengue_diarrhea %>% 
  mutate(dengue_cases = ifelse((is.na(dengue_cases)) == T, 0, dengue_cases),
         diarrhea_cases = ifelse((is.na(diarrhea_cases)) == T, 0, diarrhea_cases),
         dengue = dengue_cases / est_population,
         diarrhea = diarrhea_cases / est_population) %>% 
  select(-c("dengue_cases", "diarrhea_cases"))

  
#NOTE- NA´s are considered to be provinces without dengue/diarrhea cases (= 0). 
#NA´s are assumed to be 0s given that there are no rows are dengue/diarrhea cases are = 0 (only in APURIMAC for diarrhea, probably a formatting difference with the database).
#This indicates that provinces without dengue/diarrhea cases are simply not included in the database
#and are consequently NA´s. 

#Databases are exported.
saveRDS(dengue_diarrhea, paste0(path_CDC, "dengue diarrhea cases CDC-MINSA.RDS"))
#dengue_diarrhea <- readRDS(paste0(path_CDC, "dengue diarrhea cases CDC-MINSA.RDS"))

saveRDS(dengue_diarrhea_standardized, paste0(path_CDC,"standardized dengue diarrhea CDC-MINSA.RDS"))
#dengue_diarrhea_standardized <- readRDS(paste0(path_CDC, "standardized dengue diarrhea CDC-MINSA.RDS"))



#
## Graficas
#



names(dengue_diarrhea)

ggplot(dengue_diarrhea %>% gather(enf, cases, c(diarrhea_cases, dengue_cases)) %>%
         filter(department == "LORETO") %>%
         group_by(department, year, enf) %>% summarize(cases = mean(cases,na.rm = T)),
       aes(year, cases)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~enf, scales = "free")+
  theme_bw()






