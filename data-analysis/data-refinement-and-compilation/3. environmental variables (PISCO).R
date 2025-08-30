#Leonardo Doig-Alba
#Creation Date: 13/10/2021
#Last modification: 19/08/2022

#Libraries are imported.
library(tidyverse)               # Data tidying
library(lubridate)               # Date variables
library(sf)                      # Spatial data
library(tidync)                  # netCDF(.nc) data


#Environmental variables are extracted from the Peruvian Interpolated data of the SENAMHI’s
#Climatological and hydrological Observations (PISCO), elaborated by the 
#National Service of Meteorology and Hydrology of Peru (SENAMHI). This database interpolates 
#a nation-wide 5*5km grid for temperature and precipitation values at monthly intervals from 1960-2016.

# Data repository: https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/

# PISCO manual can be found as:
# Aybar, C.; Lavado-Casimiro, W.; Huerta, A.; Fernández, C.; Vega, F.; Sabino, E. & Felipe-Obando, O.(2017). 
# Uso del Producto Grillado “PISCO” de precipitación en Estudios, Investigaciones y Sistemas Operacionales de 
# Monitoreo y Pronóstico Hidrometeorológico. Nota Técnica 001 SENAMHI-DHI-2017, Lima-Perú.


#Objects with utilized projections are created (wgs84 and UTM).
projection_wgs84 <- 4326
projection_utm <- 32718

#Path for PISCO data access is created and a map of Peru´s departments/provinces/districts (.shp) is imported.
path_datos <- "Bases de Datos/Database Compilation/PISCO/"
peru_map_path <- "Bases de Datos/Maps and Shapes/"

district_map_wgs84 <- st_read(paste0(peru_map_path, "/2015 (INEI)/DISTRITO_15.shp")) %>% 
  select(-c(IDDPTO, IDPROV, NOM_CAP)) %>% 
  rename(department = NOMBDEP, province= NOMBPROV, district = NOMBDIST, ubigeo = IDDIST) 

#Map is transformed to a UTM projection for spatial estimations.
district_map_utm <- st_transform(district_map_wgs84, crs = projection_utm)

(mapa<- ggplot()+
  geom_sf(data = district_map_wgs84 %>% summarise(), color="tomato")+
  geom_sf(data = ER, color="tomato",  cex = 1.3)+
  geom_sf(data = db_prec_month, color = "blue", fill = NA, alpha=0.01, size= 0.01))

plotly::ggplotly (mapa, 
         dynamicTicks = TRUE, session="knitr")

#
### Monthly data download from SENAMHI
#

#Preliminary exploration of nc database.
#(db_prec_month <- nc_open(paste0(path_datos, "data_prec_monthly.nc"))); month_rain <- ncvar_get(db_prec_month, "T"); nc_close(db_prec_month)

#From the PISCO database we filter precipitation values from 2012 onward.
db_prec_month <- tidync(paste0(path_datos, "Precipitation.nc")) %>%     
  hyper_filter(T = lubridate::year(as.POSIXct("1960-01-01 00:00:00", tz = "UTC") %m+% months(floor(T))) >= 2012) %>% 
  hyper_tibble() %>%
  rename(time = T, prec = Prec) %>% 
  mutate(time = as.POSIXct("1960-01-01 00:00:00", tz = "UTC") %m+% months(floor(time)),
         year = year(time), month = month(time))

#We create an simple feature variable ("geometry") from the PISCO latitude and longitude values.
db_prec_month <- db_prec_month %>% 
  mutate(lat = Y,long = X) %>% 
  st_as_sf(coords = c("long","lat"), crs = projection_wgs84)

#We transform the sf variable to an UTM projection.
db_prec_month <- db_prec_month %>% st_transform(projection_utm)

#We filter PISCO points that are found inside the Peruvian border.
db_prec_month <- db_prec_month[               
  #peru_utm                                  #Region resolution
  #province_utm                              #Province resolution
  district_map_utm                           #District resolution
  %>% summarise,]

db_prec_month <- db_prec_month %>% 
  sf::st_intersection(
    #peru_utm                                #Region resolution
    #province_utm)                           #Province resolution
    district_map_utm)                        #District resolution

db_prec_month$geometry <- NULL

db_prec_month <- db_prec_month %>% 
  group_by(year, month, department                  #Region resolution
           , province                            #Province resolution
           , district, ubigeo                #District resolution
           ) %>% 
  summarise(accum_prec = sum(prec))

table(db_prec_month$month, db_prec_month$year) #No missing rows.
table(is.na(db_prec_month$accum_prec)) #No missing values.

db_prec_month <- left_join(db_prec_month,   
                           #peru_utm)        #Region resolution  
                           #province_utm)    #Province resolution
                           district_map_utm) #District resolution


db_prec_month <- db_prec_month %>% 
  group_by(year, month, department                #Region resolution
           , province                          #Province resolution
           , district, ubigeo              #District resolution
           ) %>% 
  mutate(area_m2 = as.numeric(st_area(geometry)),
         prec_by_m2 = accum_prec/area_m2)

db_prec_month$geometry <- NULL

#saveRDS(db_prec_month, file=paste0(path_datos,"Monthly accumulated precipitation per region Peru (1980-2016).RDS"))
saveRDS(db_prec_month, file=paste0(path_datos,"District/Monthly accumulated precipitation per district Peru (2012-2016).RDS"))


###
### Maximum Temperature 
### 

#https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Temp/.v1p1/.tmax/.stable/.monthly/.tmax/index.html

#(db_tmax_month<-nc_open(paste0(path_datos,"data_tmax_monthly.nc"))); nc_close(db_tmax_monthly)

db_tmax_month <- tidync(paste0(path_datos, "MaxTemp.nc")) %>%     
  hyper_filter(T = lubridate::year(as.POSIXct("1960-01-01 00:00:00", tz = "UTC") %m+% months(floor(T))) >= 2012) %>% 
  hyper_tibble() %>% 
  rename(time = T, tmax = tmax) %>% 
  mutate(time = as.POSIXct("1960-01-01 00:00:00", tz = "UTC") %m+% months(floor(time)),
         year= year(time), month =month(time))

#We create an sf object ("geometry") from the PISCO latitud and longitud values.
db_tmax_month <- db_tmax_month %>% 
  mutate(lat = Y,long = X) %>% 
  st_as_sf(coords = c("long","lat"), crs = projection_wgs84)

#We transform the sf variable to a UTM projection.
db_tmax_month <- db_tmax_month %>% st_transform(projection_utm)

db_tmax_month <- db_tmax_month[
  #peru_utm                                  #Region resolution
  #province_utm                               #Province resolution
  district_map_utm                             #District resolution
  %>% summarise,]

db_tmax_month <- db_tmax_month %>% 
  sf::st_intersection(
    #peru_utm)                                #Region resolution
    #province_utm)                            #Province resolution
    district_map_utm)                           #District resolution


db_tmax_month$geometry <- NULL

db_tmax_month <- db_tmax_month %>% 
  group_by(year, month, department                  #Region resolution
           , district, ubigeo                #District resolution
           , province) %>%                        #Province resolution 
  summarise(sd_tmax = sd(tmax, na.rm = T),
            mean_tmax = mean(tmax))

table(db_tmax_month$month, db_tmax_month$year) #No missing rows.
table(is.na(db_tmax_month$mean_tmax)) #No missing values.
table(is.na(db_tmax_month$sd_tmax)) #Only "Callao" has NA standard deviation. Smallest province, only one spatial point.

#saveRDS(db_tmax_month, file=paste0(path_datos,"Monthly average Tmax per region Peru (1980-2016).RDS"))
saveRDS(db_tmax_month, file = paste0(path_datos,"District/Monthly average Tmax per region Peru (2012-2016).RDS"))


#
### Minimum Temperature 
#

#https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/.Temp/.v1p1/.tmax/.stable/.monthly/.tmax/index.html

#(db_tmin_month <- nc_open(paste0(path_datos, "data_tmin_monthly.nc"))); month_rain<- ncvar_get(db_tmin_month, "T"); nc_close(db_tmin_month)

db_tmin_month <- tidync(paste0(path_datos,"MinTemp.nc")) %>%     
  hyper_filter(T = lubridate::year(as.POSIXct("1960-01-01 00:00:00", tz = "UTC") %m+% months(floor(T))) >= 2012) %>% 
  hyper_tibble() %>% 
  rename(time = T, tmin = tmin) %>% 
  mutate(time = as.POSIXct("1960-01-01 00:00:00", tz = "UTC") %m+% months(floor(time)),
         year= year(time), month =month(time))

#We create an sf object ("geometry") from the PISCO latitud and longitud values.
db_tmin_month <- db_tmin_month %>% 
  mutate(lat = Y,long = X) %>% 
  st_as_sf(coords = c("long","lat"), crs = projection_wgs84)

db_tmin_month <- db_tmin_month %>% st_transform(projection_utm)

db_tmin_month <- db_tmin_month[
  #peru_utm                                  #Region resolution
  district_map_utm                             #District resolution
#  province_utm                               #Province resolution
  %>% summarise,]

db_tmin_month <- db_tmin_month %>% 
  sf::st_intersection(
    #peru_utm)                                #Region resolution
    district_map_utm)                           #District resolution
#    province_utm)                            #Province resolution

db_tmin_month$geometry<-NULL

db_tmin_month <- db_tmin_month %>% 
  group_by(year, month, department                  #Region resolution
           , district, ubigeo                #District resolution
           , province) %>%                        #Province resolution 
  summarise(sd_tmin = sd(tmin, na.rm = T),
            mean_tmin = mean(tmin))

table(db_tmin_month$month, db_tmin_month$year) #No missing rows.
table(is.na(db_tmin_month$mean_tmin)) #No missing values.
table(is.na(db_tmin_month$sd_tmin)) #Only "Callao" has NA standard deviation. Smallest province, only one spatial point.

#saveRDS(db_tmin_month, file=paste0(path_datos,"Monthly average Tmin per region Peru (1980-2016).RDS"))
saveRDS(db_tmin_month, file=paste0(path_datos,"District/Monthly average Tmin per province Peru (2012-2016).RDS"))


#
### Database Compilation
#

#We join all databases.
# db_prec_month <- readRDS(paste0(path_datos,"District/Monthly accumulated precipitation per district Peru (2012-2016).RDS"))
# db_tmax_month <- readRDS(paste0(path_datos,"District/Monthly average Tmax per region Peru (2012-2016).RDS"))
# db_tmin_month <- readRDS(paste0(path_datos,"District/Monthly average Tmin per province Peru (2012-2016).RDS"))

PISCO_district <- left_join(db_prec_month, db_tmax_month) ; PISCO_district <- left_join(PISCO_district, db_tmin_month)

#We change some district/province names to adjust for inter-annual differences
#All these cases are explored and specified in Annex D - Minsa wasting and stunting"
PISCO_district$province[which(PISCO_district$province == "ANTONIO RAYMONDI")] <- "ANTONIO RAIMONDI"

PISCO_district$district[which(PISCO_district$district == "ANCO_HUALLO" | PISCO_district$district == "ANCCOHUALLO")] <- "ANCO-HUALLO"

PISCO_district$district[which(PISCO_district$province == "SATIPO" &
                                  (PISCO_district$district=="VIZCATAN DEL ENE" | PISCO_district$district == "MAZAMARI - PANGOA"))] <- "PANGOA"


PISCO_district$district[which(PISCO_district$district == "ALLAUCA")] <- "AYAUCA" 

PISCO_district$province[which(PISCO_district$province == "PUTUMAYO")] <- "MAYNAS"

PISCO_district$district[which(PISCO_district$province == "MAYNAS" &
                                  PISCO_district$district=="ROSA PANDURO")] <- "TENIENTE MANUEL CLAVERO"

PISCO_district$district[which(PISCO_district$province == "MAYNAS" &
                                  PISCO_district$district=="YAGUAS")] <- "PUTUMAYO"

PISCO_district$district[which(PISCO_district$province == "TACNA" &
                                  PISCO_district$district=="LA YARADA LOS PALOS")] <- "TACNA"

PISCO_district$district[which(PISCO_district$province == "PIURA" &
                                  PISCO_district$district=="VEINTISEIS DE OCTUBRE")] <- "PIURA"

PISCO_district$district[which(PISCO_district$province == "PADRE ABAD" &
                                  PISCO_district$district=="ALEXANDER VON HUMBOLDT")] <- "IRAZOLA"

PISCO_district$district[which(PISCO_district$province == "PADRE ABAD" &
                                  PISCO_district$district=="NESHUYA")] <- "IRAZOLA"

PISCO_district$district[which(PISCO_district$province == "OXAPAMPA" &
                                  PISCO_district$district=="CONSTITUCION")] <- "PUERTO BERMUDEZ"

PISCO_district$district[which(PISCO_district$district == "SAN FRANCISCO DE ASIS DE YARUSYACAN")] <- "SAN FCO.DE ASIS DE YARUSYACAN" 

PISCO_district$district[which(PISCO_district$district == "PAMPAS GRANDE")] <- "PAMPAS" 

PISCO_district$province[which(PISCO_district$province == "NASCA")] <- "NAZCA"
PISCO_district$district[which(PISCO_district$district == "NASCA")] <- "NAZCA"

PISCO_district$district[which(PISCO_district$province == "ANDAHUAYLAS" &
                                PISCO_district$district=="JOSE MARIA ARGUEDAS")] <- "ANDAHUAYLAS"

PISCO_district$district[which(PISCO_district$province == "CHINCHEROS" &
                                PISCO_district$district=="ROCCHACC")] <- "ONGOY"


PISCO_district$district[which(PISCO_district$province == "HUANTA" &
                                PISCO_district$district=="CANAYRE")] <- "LLOCHEGUA"

PISCO_district$district[which(PISCO_district$province == "HUANTA" &
                                PISCO_district$district=="UCHURACCAY")] <- "HUANTA"

PISCO_district$district[which(PISCO_district$province == "LA MAR" &
                                PISCO_district$district=="SAMUGARI")] <- "SAN MIGUEL"

PISCO_district$district[which(PISCO_district$province == "LA MAR" &
                                PISCO_district$district=="ANCHIHUAY")] <- "ANCO"

PISCO_district$district[which(PISCO_district$province == "LA CONVENCION" &
                                PISCO_district$district=="VILLA VIRGEN")] <- "VILCABAMBA"

PISCO_district$district[which(PISCO_district$province == "LA CONVENCION" &
                                PISCO_district$district=="INKAWASI")] <- "VILCABAMBA"

PISCO_district$district[which(PISCO_district$province == "LA CONVENCION" &
                                PISCO_district$district=="VILLA KINTIARINA")] <- "KIMBIRI"

PISCO_district$district[which(PISCO_district$province == "TAYACAJA" &
                                PISCO_district$district=="QUICHUAS")] <- "COLCABAMBA"

PISCO_district$district[which(PISCO_district$province == "CHURCAMPA" &
                                PISCO_district$district=="COSME")] <- "ANCO"

PISCO_district$district[which(PISCO_district$province == "TAYACAJA" &
                                PISCO_district$district=="ANDAYMARCA")] <- "COLCABAMBA"

PISCO_district$district[which(PISCO_district$province == "TAYACAJA" &
                                PISCO_district$district=="PICHOS")] <- "HUARIBAMBA"

PISCO_district$district[which(PISCO_district$province == "TAYACAJA" &
                                PISCO_district$district=="ROBLE")] <- "TINTAY PUNCU"

PISCO_district$district[which(PISCO_district$district == "QUISQUI (KICHKI)")] <- "QUISQUI" 

PISCO_district$district[which(PISCO_district$province == "HUANUCO" &
                                PISCO_district$district=="SAN PABLO DE PILLAO")] <- "CHINCHAO"

PISCO_district$district[which(PISCO_district$province == "HUANUCO" &
                                PISCO_district$district=="YACUS")] <- "MARGOS"

PISCO_district$district[which(PISCO_district$province == "LEONCIO PRADO" &
                                PISCO_district$district=="PUCAYACU")] <- "JOSE CRESPO Y CASTILLO"

PISCO_district$district[which(PISCO_district$province == "LEONCIO PRADO" &
                                PISCO_district$district=="CASTILLO GRANDE")] <- "RUPA-RUPA"

PISCO_district$district[which(PISCO_district$province == "MARAÑON" &
                                PISCO_district$district=="LA MORADA")] <- "CHOLON"

PISCO_district$district[which(PISCO_district$province == "MARAÑON" &
                                PISCO_district$district=="SANTA ROSA DE ALTO YANAJANCA")] <- "CHOLON"

PISCO_district$district[which(PISCO_district$province == "HUANTA" &
                                  PISCO_district$district=="PUCACOLPA")] <- "AYAHUANCO"

PISCO_district$district[which(PISCO_district$province == "AYMARAES" &
                               PISCO_district$district=="IHUAYLLO")] <- "HUAYLLO"

#A district_ID variables is created
PISCO_district <- PISCO_district %>% mutate(district_ID = paste(department, province, district, sep = "-")) %>% 
  mutate(ubigeo = as.numeric(ubigeo))

#We validate that all district_IDs match the INEI database
(anti_join(PISCO_district %>% ungroup() %>% select(district_ID) %>% distinct(), 
           read.csv("Bases de Datos/Database Compilation/districts and ubigeos/inei.csv") %>% select(-X),
           by = c('district_ID')))
#0 rows when comparing district_IDs to those in INEI.

#Ubigeos are changed accordingly
PISCO_district <- left_join(PISCO_district %>% ungroup() %>% select(-c(ubigeo)),
                              read.csv("Bases de Datos/Database Compilation/districts and ubigeos/inei.csv") %>% select(-X),
                              by = c("department", "province", "district", "district_ID"))

PISCO_district <- PISCO_district %>%   
  rename(accum_rain_per_m2 = prec_by_m2) %>% 
  select(-c(accum_prec)) %>% ungroup()

saveRDS(PISCO_district, file = paste0(path_datos, "District/PISCO variables district level.RDS"))
#PISCO_district <- readRDS(paste0(path_datos,"Bases de Datos/Database Compilation/PISCO/District/PISCO variables district level.RDS"))
