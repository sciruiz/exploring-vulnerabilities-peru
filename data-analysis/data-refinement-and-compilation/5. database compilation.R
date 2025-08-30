##
### 5. - Database compilation
##

#Leonardo Doig-Alba
#Creation Date: 08/12/2021
#Last Modification: 17/07/2025

#We aggregate all databases from previous scripts: ENAHO, ENDES (including "Wasting & Stunting" and "Adolescent mothers" from MINSA database),
#PISCO, Dengue and diarrhea

#Libraries
library(tidyverse)
library(lubridate)

#Path
path_databases <- "Data Analysis/Bases de Datos/Database Compilation/"
path_output <- "Data Analysis/Bases de Datos/"

#Import databases and normalize some variables.
db_pisco <- readRDS(paste0(path_databases, "PISCO/PISCO variables district level.RDS")) %>% 
  select(-c(sd_tmin, area_m2, sd_tmax)) %>%                                     #We discard some variables from the PISCO database
  group_by(year, department, province, district, district_ID, ubigeo) %>%       #We reduce the database so it presents one value per year
  summarise(min_temp = min(mean_tmin, na.rm = T), max_temp = max(mean_tmax, na.rm = T),
            max_accum_rain_m2 = max(accum_rain_per_m2, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(min_temp = (min_temp - min(min_temp)) / (max(min_temp) - min(min_temp)),
       max_accum_rain_m2 = (max_accum_rain_m2 - min(max_accum_rain_m2)) / (max(max_accum_rain_m2) - min(max_accum_rain_m2)),
       max_temp = (max_temp - min(max_temp)) / (max(max_temp) - min(max_temp))) 
  
db_enaho <- readRDS(paste0(path_databases, "ENAHO/standardized ENAHO 2012-2016.RDS"))

#Normalize all variables
# db_enaho <- db_enaho %>% 
#   ungroup() %>% 
#   mutate(across(.cols = c(7:22),
#                 .fns = ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) )))

db_enaho <- db_enaho %>%
  ungroup() %>%
    mutate(across(.cols = c(woman_household_lead, afroamerican_descendent_pop,
                deaf_mute_pop, female_population, children_under5),
      .fns = ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) )))

db_endes <- readRDS(paste0(path_databases, "ENDES/Standardized ENDES 2012-2016.RDS"))

db_endes <- db_endes %>% 
  ungroup() %>% 
  mutate(across(.cols = c(7:13),
                .fns = ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) )))


#Not normalized because we use a GLM with a Poisson distribution
db_dengue_diarrhea <- readRDS(paste0(path_databases,"CDC-MINSA/dengue diarrhea cases CDC-MINSA.RDS")) %>%
  select(-est_population) %>% rename(dengue = dengue_cases, diarrhea = diarrhea_cases)

#
### Comparing districts between databases
#

#We first make sure the districts between data sources match.

(anti_join(db_enaho[,unique("district_ID")],
           db_endes[,unique("district_ID")] ))

#ENDES has 411 unique districts more than ENAHO
(distinct(anti_join(db_endes[,unique("district_ID")],
                    db_enaho[,unique("district_ID")] )))

(anti_join(db_dengue_diarrhea[,unique("district_ID")],
           db_endes[,unique("district_ID")]))

#ENDES has 1 more unique district than dengue/diarrhea: "LIMA - YAUYOS - SAN JOAQUIN"
(anti_join(db_endes[,unique("district_ID")],
  db_dengue_diarrhea[,unique("district_ID")] ))

#ENDES has 354 more districts than PISCO
(distinct(anti_join(db_endes[,unique("district_ID")],
           db_pisco[,unique("district_ID")] )))

#PISCO has 0 more districts than ENDES
(distinct(anti_join(db_pisco[,unique("district_ID")],
                    db_endes[,unique("district_ID")] )))

#PISCO was estimated from a 2015 shapefile. What are the 354 missing points from?
#We visualize the missing districts in the 2015 map which we used to estimate the PISCO values.
list <- distinct(anti_join(db_endes[,unique("district_ID")], db_pisco[,unique("district_ID")] )) 

# district_map_wgs84 <- sf::st_read(paste0("Bases de Datos/Maps and Shapes/2015 (INEI)/DISTRITO_15.shp")) %>% 
#   select(-c(IDDPTO, IDPROV, NOM_CAP)) %>% 
#   rename(department = NOMBDEP, province= NOMBPROV, district = NOMBDIST, ubigeo = IDDIST) 
# 
# ER <- district_map_wgs84 %>% mutate(district_ID = paste(department,province,district,sep = "-")) %>% 
#   filter(district_ID %in% unique(list$district_ID))   #Four districts from the ENDEs database were not in the 2015 map, all other districts should have PISCO values
# 
# #We plot the districts with missing data.
# ggplot()+
#   geom_sf(data= district_map_wgs84 %>% summarise())+
#   geom_sf(data = ER, fill = "tomato", color = "tomato") +
#   theme_bw()
# 
# #All of them are "small" districts. We estimate their overall areas.
# R <- R %>% mutate(area = as.numeric(st_area(st_transform(geometry, 32718))/1000000)); summary(R$area) #Bigges
# summary(sf::st_area(district_map_wgs84 %>% mutate(district_ID = paste(department,province,district,sep = "-")) %>% 
#                filter(!(district_ID %in% unique(list$district_ID)) ))/1000000)

#We review the overall districts per data source
n_distinct(db_endes$ubigeo)            # 1834  
n_distinct(db_dengue_diarrhea$ubigeo)  # 1833
n_distinct(db_enaho$ubigeo)            # 1423  (~400 less)
n_distinct(db_pisco$ubigeo)            # 1480  (~360 less)

#We then explore the number of NA values per data source
(db_endes[rowSums(is.na(db_endes)) > 0,]) #2 NA values for various ENDES variables (only data for adolescent mothers)
(db_enaho[rowSums(is.na(db_enaho)) > 0,])
(db_dengue_diarrhea[rowSums(is.na(db_dengue_diarrhea)) > 0,])
(db_pisco[rowSums(is.na(db_pisco)) > 0,])

db_annual_complete <- full_join(db_enaho, db_endes,
                                by = c("department", "province", "district", "district_ID", "ubigeo", "year"))

db_annual_complete <- full_join(db_annual_complete, db_dengue_diarrhea,
                                by = c("department", "province", "district", "district_ID", "ubigeo", "year"))

db_annual_complete <- full_join(db_annual_complete, db_pisco, 
                                by = c("department", "province", "district", "district_ID", "ubigeo", "year"))

#Additionally we generate a database with only shared districts.

dfs <- list(db_enaho, db_endes, db_pisco, db_dengue_diarrhea)
shared_districts <- Reduce(intersect, lapply(dfs, function(df) df[["district_ID"]]))

db_annual_shared_distr <- db_annual_complete %>% 
  filter(district_ID %in% shared_districts)

#We export these two resulting databases as .RDS files.

#Annual dengue and diarrhea databases - with NA districts
write_rds(db_annual_complete, paste0(path_databases, "07.25 - Annual DB complete.RDS"))

#Annual dengue and diarrhea databases - without unshared districts
write_rds(db_annual_shared_distr, paste0(path_databases, "07.25 - Annual DB without unshared districts.RDS"))



#
### Reduction of databases: 5-year average
#

#PISCO
db_pisco <- db_pisco %>% 
  select(-c(year)) %>% 
  group_by(department, province, district, district_ID, ubigeo) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ungroup() 

#We validate that there are no non-infinite values ("NA", Inf" or "NaN")
db_pisco_infinit <- do.call(data.frame,                      # Replace Inf and NaN in data by NA
                                        lapply(db_pisco,
                                               function(x) replace(x, !is.finite(x), NA)))

table(is.na(db_pisco_infinit[6:8])); rm(db_pisco_infinit)

#ENAHO
db_enaho <- db_enaho %>% 
  ungroup() %>% 
  select(-c(year)) %>% 
  group_by(department, province, district, district_ID, ubigeo) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ungroup()

#We validate that there are no non-infinite values ("NA", Inf" or "NaN")
db_enaho_infinit <- do.call(data.frame,                      # Replace Inf and NaN in data by NA
                            lapply(db_enaho,
                                   function(x) replace(x, !is.finite(x), NA)))

table(is.na(db_enaho_infinit[6:21])); rm(db_enaho_infinit)

#ENDES
db_endes <- db_endes %>% 
  ungroup() %>% 
  select(-c(year)) %>% 
  group_by(department, province, district, district_ID, ubigeo) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ungroup() 

#We validate that there are no non-infinite values ("NA", Inf" or "NaN")
db_endes_infinit <- do.call(data.frame,                      # Replace Inf and NaN in data by NA
                            lapply(db_endes,
                                   function(x) replace(x, !is.finite(x), NA)))

table(is.na(db_endes_infinit[c(6:8)])); rm(db_endes_infinit)

#dengue & diarrhea
db_dengue_diarrhea <- db_dengue_diarrhea %>% 
  ungroup() %>% 
  select(-c(year)) %>% 
  group_by(department, province, district, district_ID, ubigeo) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ungroup()

#We validate that there are no non-infinite values ("NA", Inf" or "NaN")
db_dengue_diarrhea_infinit <- do.call(data.frame,                      # Replace Inf and NaN in data by NA
                            lapply(db_dengue_diarrhea,
                                   function(x) replace(x, !is.finite(x), NA)))

table(is.na(db_dengue_diarrhea_infinit[c(6, 7)])); rm(db_dengue_diarrhea_infinit)


#
### Database Merge: database with all possible districts (filled with NAs)
#

#We join the ENAHO and ENDES databases
db_aggreg_complete <- full_join(db_enaho, db_endes,
                    by = c("department", "province", "district", "district_ID", "ubigeo"))

#We review the NAs
table(is.na(db_aggreg_complete))  #6576 / 44776  NAs (0.15 %)
(data.frame(sapply(db_aggreg_complete, function(y) sum(length(which(is.na(y)))))))
#411 NAs per variable from the ENAHO database.
#This is expected given that this database has ~400 districts less than the ENDES data base. 

db_aggreg_complete <- full_join(db_aggreg_complete, db_dengue_diarrhea,
                    by = c("department", "province", "district", "district_ID", "ubigeo"))

#We review the NAs
table(is.na(db_aggreg_complete))   #6578  / 48442 NAs (0.14 %)
(data.frame(sapply(db_aggreg_complete, function(y) sum(length(which(is.na(y)))))))
#One district was not in the dengue/diarrhea database (2 additional NAs in the database)

db_aggreg_complete <- full_join(db_aggreg_complete, db_pisco, 
                    by = c("department", "province", "district", "district_ID", "ubigeo"))

#We review the NAs
table(is.na(db_aggreg_complete))  #7640 / 52882 NAs (0.14 %)
(data.frame(sapply(db_aggreg_complete, function(y) sum(length(which(is.na(y)))))))
#357 districts were not found in the pisco database.


#
### Database Merge: database with only shared districts
#

#We join the ENAHO and ENDES databases
db_aggreg_shared_distr <- inner_join(db_enaho, db_endes,
                             by = c("department", "province", "district", "district_ID", "ubigeo"))

db_aggreg_shared_distr <- inner_join(db_aggreg_shared_distr, db_dengue_diarrhea,
                             by = c("department", "province", "district", "district_ID", "ubigeo"))

db_aggreg_shared_distr <- inner_join(db_aggreg_shared_distr, db_pisco, 
                             by = c("department", "province", "district", "district_ID", "ubigeo"))

table(is.na(db_aggreg_shared_distr));nrow(db_aggreg_shared_distr)  #No NAs, but only 1191 rows


#
### Database exportation 
#

#We export resulting databases as .RDS files.

#Complete (aggregated) dengue and diarrhea databases - with NA districts
write_rds(db_aggreg_complete, paste0(path_output, "07.25 - Aggregated DB (2012-2016) with NA districts.RDS"))

#Shared (aggregated) dengue and diarrhea databases - without NAs
write_rds(db_aggreg_shared_distr, paste0(path_output, "07.25 - Aggregated DB (2012-2016) without NAs.RDS"))


#
### Ramon Castilla data (Caballococha)
#

db_rc <- db_annual_complete %>% filter(district == "RAMON CASTILLA")

db_rc2 <- db_rc %>% gather(parameter, value, c(7:34)) %>% 
  spread(year, value) %>% ungroup() %>% dplyr::select(-c("department","province","district_ID", "ubigeo"))

db_rc3 <- db_rc %>% gather(parameter, value, c(7:34)) %>% 
  ungroup() %>% dplyr::select(-c("department","province","district_ID", "ubigeo"))

ggplot(db_rc3, aes(year, value)) + geom_point() + geom_line()+ facet_wrap(~parameter, scales = "free")

write_rds(db_rc2, paste0(path_output, "07.25 - Annual values Ramon Castilla.RDS"))  


-------------------------------------------
  
#We estimate a percentage of change for each parameter (base line value == 2012 value)
#formula:   2012 value - value / 2012 value x 100
#To avoid NaN, when any value is "0" it is changed to "0.00001")
db2 <- db %>% 
  filter(parameter != "diabetes") %>% 
  gather(year, value, c("2012":"2016")) %>% 
  group_by(parameter) %>% 
  mutate(year = as.numeric(year),
         value = ifelse((value == 0), value + 0.001, value),
        val_2012 = #ifelse((value[which(year == 2012)] != 0), 
                          value[which(year == 2012)],
#                          value[which(year == 2013)]), #Specifically for adolescent mothers
         perc_change = (value - val_2012) / val_2012 * 100) %>% 
  ungroup()

ggplot(db2, aes(year, perc_change)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::comma, limits = c(-100, 100)) + 
  facet_wrap(~parameter, scales = "free")

#####
#Variables that have not been associated with any specific response variable but are considered to be importante are renamed
#with an added "Ind_" at the beginning of their label .
# db_aggreg_complete_complete <- db_aggreg_complete_complete %>% 
#   rename(single_mother = single_mother,
#          ind_woman_violence = woman_violence, 
#          ind_adolescent_mothers = adolescent_mothers,
#         ind_diabetes  = diabetes, 
#         ind_children_under5 = children_under5,
#         ind_adults_over65 = adults_over65,
#         ind_non_spanish_speaking_pop = non_spanish_speaking_pop, 
#         ind_no_adult15_literarcy = no_adult15_literarcy,
#         ind_no_internet_access =  no_internet_access, 
#         ind_age_dependecy = age_dependecy,
#         ind_female_population = female_population,
#         ind_woman_household_lead = woman_household_lead, 
#         ind_amazonic_indigenous_pop = amazonic_indigenous_pop, 
#         ind_afroamerican_descendent_pop = afroamerican_descendent_pop,
#         ind_aymara_quechua_amazonic_pop = aymara_quechua_amazonic_pop, 
#         ind_deaf_mute_pop = deaf_mute_pop,
#         ind_no_electricity_access = no_electricity_access)
