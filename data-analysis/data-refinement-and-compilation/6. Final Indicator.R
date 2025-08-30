#
## Models cuali-cuanti
#

#Created: 19-07-2024
#Last Modified: 

# *Datos distritos de loreto
# *selección de variables marcadas en el excel
# *normalizar (min-max)/value-min)
# *indicador previo (literatura) solo para loreto
# Dos indicadores:
  # DDS - incluyee todas las varaibles en base a lo que las 
  # literatura - solo las que son muy significativa

#Libraries
library(tidyverse)
library(lme4)

#We import the databases
path_import <- "Data Analysis/Bases de Datos/"
path_inei <- "Data Analysis/Bases de Datos/INEI/"


inei_population <- readRDS(paste0(path_inei, "est. population 2012-2016 (district).RDS")) %>% 
  group_by(department, province, district, district_ID, ubigeo) %>% summarise(est_population = mean(est_population))


##
###  MODELO LITERATURA
##

#db_anual_shared <- readRDS(paste0(path_import, "/Database Compilation/07.25 - Annual DB without unshared districts.RDS"))
db_aggregated <- readRDS(paste0(path_import, "07.25 - Aggregated DB (2012-2016) without NAs.RDS"))

db_dengue <- left_join(db_aggregated, inei_population) %>% 
  select(-c(stunting, wasting, diarrhea)) %>% 
  filter(!is.na(dengue), dengue != 0)

db_diarrhea <- left_join(db_aggregated, inei_population) %>% 
    select(-c(pregnant_woman, no_healthcare_access, dengue)) %>%
    filter(!is.na(diarrhea))
  

#Selección entre variables con alta correlación (>= 0.7)
deng_corr <- as.data.frame(cor(db_dengue %>% select(-c(dengue, department, province, district, ubigeo, district_ID))))
deng_corr <- deng_corr %>% filter(c(1:25) <= - 0.7, c(1:25) >= 0.7,  c(1:25) != 1)
#(non-spanish - Aymara/quechua/amazonic 0.886); (min temp - max temp 0.796); (electricity - water access 0.72)
db_dengue <- db_dengue %>% select(-c(non_spanish_speaking_pop, no_electricity_access, max_temp))

diarr_corr <- as.data.frame(cor(db_diarrhea %>% select(-c(diarrhea, department, province, district, ubigeo, district_ID))))
diarr_corr <- diarr_corr %>% filter(c(1:25) <= - 0.7, c(1:25) >= 0.7,  c(1:25) != 1)
db_diarrhea <- db_diarrhea %>% select(-c(non_spanish_speaking_pop))

#MODELOS CON TODAS LAS VARIABLES
lm_dengue_literatura_1 <- glm(dengue ~ offset(log(est_population)) + children_under5 + adults_over65 + female_population +
                          no_water_access + no_basic_sanitation_access + no_healthcare_access + age_dependecy + no_adult15_literarcy
                        + woman_household_lead + deaf_mute_pop + migrant_pop + aymara_quechua_amazonic_pop + amazonic_indigenous_pop
                        + afroamerican_descendent_pop + diabetes + woman_violence + single_mother + pregnant_woman + adolescent_mothers
                        + min_temp  + max_accum_rain_m2, family = quasipoisson, data = db_dengue) 

summary(lm_dengue_literatura_1)
drop1(lm_dengue_literatura_1, test ="F")

#Variables not to drop:
#1 *adults_over65
#2 *No water access
#3 *no_adult15_literarcy
#4 *woman_household_lead
#5 *migrant_pop
#6 *aymara_quechua_amazonic_pop
#7 *amazonic_indigenous_pop
#8 *afroamerican_descendent_pop
#9 *pregnant_woman
#10 *adolescent_mothers
#11 *min_temp
#12 *max_accum_rain_m2


lm_diarrhea_literatura_1 <- glm(diarrhea ~ offset(log(est_population)) + children_under5 + adults_over65 + female_population +
                                  no_water_access + no_basic_sanitation_access + age_dependecy + no_adult15_literarcy
                                  + woman_household_lead + deaf_mute_pop + migrant_pop + aymara_quechua_amazonic_pop + amazonic_indigenous_pop
                                  + afroamerican_descendent_pop + diabetes + woman_violence + single_mother + stunting + wasting + adolescent_mothers
                                  + min_temp  + max_accum_rain_m2, family = quasipoisson, data = db_diarrhea) 

summary(lm_diarrhea_literatura_1)
drop1(lm_diarrhea_literatura_1, test ="F")

#Variables not to drop:
#*adults_over65
#*migrant_pop
#*amazonic_indigenous_pop
#*afroamerican_descendent_pop
#*diabetes
#*woman_violence
#*single_mother
#*stunting
#*adolescent_mothers

#MODELOS CON VARIABLES SIGNIFICATIVAS
lm_dengue_literatura_2 <- glm(dengue ~ offset(log(est_population)) + adults_over65 + no_water_access + no_adult15_literarcy + woman_household_lead +
                                migrant_pop + aymara_quechua_amazonic_pop + amazonic_indigenous_pop + afroamerican_descendent_pop +
                                pregnant_woman + adolescent_mothers + min_temp + max_accum_rain_m2, family = quasipoisson, data = db_dengue)

#Formula for R-adj values from deviances: 1 - ((residual dev. /df.res)/(null dev. / df.null))
#Formula for R values from deviances: 1 - (residual dev / null dev)

summary(lm_dengue_literatura_2)

round(cbind(Estimate = coef(lm_dengue_literatura_2),
            confint(lm_dengue_literatura_2)), 2)

#Significnant variables with select model (dengue):
#1 *no_adult15_literarcy           p = 0.007;  weight: -4.6
#2 *woman_household_lead           p = 0.036;  weight:  5.3
#3 *migrant_pop                    p = 0.003;  weight: -2.0
#4 *aymara_quechua_amazonic_pop    p = 0.002;  weight:  2.5
#5 *amazonic_indigenous_pop        p = 0.008;  weight: -6.3
#6 *afroamerican_descendent_pop    p = 0.006;  weight:  3.5
#7 *pregnant_woman                 p = 0.005;  weight:  2.1
#8 *adolescent_mothers             p = 0.043;  weight:  4.9
#9 *min_temp                       p = 0.036;  weight:  3.4
#-----------------------------------------------------------
#10 *adults_over65                  p = 0.052;  weight: -4.5
#11 *No water access;               p = 0.067;  
#12 *max_accum_rain_m2             p = 0.198;

lm_diarrhea_literatura_2 <- glm(diarrhea ~ offset(log(est_population)) + adults_over65 + migrant_pop + amazonic_indigenous_pop +
                                  afroamerican_descendent_pop + diabetes + woman_violence + single_mother + stunting + adolescent_mothers,
                                  family = quasipoisson, data = db_diarrhea)

summary(lm_diarrhea_literatura_2) 

round(cbind(Estimate = coef(lm_diarrhea_literatura_2),
            confint(lm_diarrhea_literatura_2)), 2)

#Significnant variables with select model (diarrhea):
#1. *migrant_pop                 p = 0.003;  weight: -0.65
#2. *amazonic_indigenous_pop     p < 0.001;  weight:  1.57
#3. *diabetes                    p = 0.003;  weight:  1.2
#4. *woman_violence              p < 0.001;  weight:  1.0
#5. *single_mother               p < 0.001;  weight:  1.0
#6. *stunting                    p = 0.036;  weight: -0.8
#7. *adolescent_mothers          p = 0.027;  weight:  2.0 
#--------------------------------------------------------------------
#8. *adults_over65                  p = 0.069
#9. *afroamerican_descendent_pop    p = 0.274


##
### Visualizing the National Vulnerability Indicator
#

db_aggregated <- db_aggregated %>%
  mutate(region = case_when(department %in% c("TUMBES", "PIURA", "LAMBAYEQUE",
                                              "LA LIBERTAD", "ANCASH", "LIMA",
                                              "ICA", "AREQUIPA", "MOQUEGUA",
                                              "TACNA", "CALLAO") ~ "Coastal region",
                            department %in% c("CAJAMARCA", "AMAZONAS", "HUANUCO",
                                              "PASCO", "JUNIN", "HUANCAVELICA",
                                              "AYACUCHO", "APURIMAC", "CUSCO", 
                                              "PUNO") ~ "Highland region",
                            department %in% c("LORETO", "SAN MARTIN", "UCAYALI", 
                                              "MADRE DE DIOS") ~ "Jungle region"),
         north_south = case_when(department %in% c("TUMBES", "PIURA", "LAMBAYEQUE", "LA LIBERTAD",
                                                   "AMAZONAS", "CAJAMARCA", "SAN MARTIN",
                                                   "LORETO", "UCAYALI", "HUANUCO", "PASCO", "JUNIN") ~ "North",
                                 department %in% c("ANCASH", "LIMA", "CALLAO", "ICA",
                                                   "AREQUIPA", "MOQUEGUA", "TACNA",
                                                   "HUANCAVELICA", "AYACUCHO",
                                                   "APURIMAC", "CUSCO", "PUNO",
                                                   "MADRE DE DIOS") ~ "South"))


db_aggregated <- db_aggregated %>%
  mutate(region = case_when(department %in% c("TUMBES", "PIURA", "LAMBAYEQUE",
                                              "LA LIBERTAD", "ANCASH", "LIMA",
                                              "ICA", "AREQUIPA", "MOQUEGUA",
                                              "TACNA", "CALLAO") ~ "Coastal region",
                            department %in% c("CAJAMARCA", "AMAZONAS", "HUANUCO",
                                              "PASCO", "JUNIN", "HUANCAVELICA",
                                              "AYACUCHO", "APURIMAC", "CUSCO", 
                                              "PUNO") ~ "Highland region",
                            department %in% c("LORETO", "SAN MARTIN", "UCAYALI", 
                                              "MADRE DE DIOS") ~ "Jungle region"),
         north_south = case_when(department %in% c("TUMBES", "PIURA", "LAMBAYEQUE", "LA LIBERTAD",
                             "AMAZONAS", "CAJAMARCA", "SAN MARTIN",
                             "LORETO", "UCAYALI", "HUANUCO", "PASCO", "JUNIN") ~ "North",
           department %in% c("ANCASH", "LIMA", "CALLAO", "ICA",
                             "AREQUIPA", "MOQUEGUA", "TACNA",
                             "HUANCAVELICA", "AYACUCHO",
                             "APURIMAC", "CUSCO", "PUNO",
                             "MADRE DE DIOS") ~ "South"))

dept_order <- c("LORETO", "AMAZONAS", "SAN MARTIN", "UCAYALI", "MADRE DE DIOS",
                "CAJAMARCA", "HUANUCO", "PASCO", "JUNIN", 
                "HUANCAVELICA", "AYACUCHO", "APURIMAC", "CUSCO", "PUNO",
                "TUMBES", "PIURA", "LAMBAYEQUE", "LA LIBERTAD", "ANCASH", 
                "LIMA", "ICA", "AREQUIPA", "MOQUEGUA", "TACNA", "CALLAO")

db_index_diarr <-db_aggregated %>%  
  select(c("north_south", "region", "department", "province", "district", "diarrhea",
           "adolescent_mothers", "stunting", "single_mother", 
           "woman_violence", "diabetes", "amazonic_indigenous_pop", 
           "migrant_pop"))

db_index_diarr_fin <- db_index_diarr %>% 
  group_by(north_south, region, department, province, district, diarrhea) %>% 
  summarise(adolescent_mothers = adolescent_mothers * 2,
            stunting = stunting * (-0.8),
            single_mother = single_mother * 1.0,
            woman_violence = woman_violence * (1.02),
            diabetes = diabetes * (1.2),
            amazonic_indigenous_pop = amazonic_indigenous_pop * 1.57,
            migrant_pop = migrant_pop * (-0.65),
            diarrhea_NVI =  (adolescent_mothers + stunting + single_mother + 
                               woman_violence + diabetes + amazonic_indigenous_pop + 
                               migrant_pop) / 7) %>% 
  ungroup() %>% 
  mutate(diarrhea_NVI = (diarrhea_NVI - min(diarrhea_NVI)) / (max(diarrhea_NVI) - min(diarrhea_NVI)))

db_index_deng <- db_aggregated %>% 
  select(c("north_south", "region", "department", "province", "district", "dengue",
           "min_temp", "adolescent_mothers", "pregnant_woman", 
           "afroamerican_descendent_pop", "amazonic_indigenous_pop", "aymara_quechua_amazonic_pop",
           "migrant_pop", "woman_household_lead", "no_adult15_literarcy"))

db_index_deng_fin <- db_index_deng %>% 
  group_by(north_south, region, department, province, district, dengue) %>% 
  summarise(no_adult15_literarcy = no_adult15_literarcy*(-4.6),
            woman_household_lead = woman_household_lead * (5.3),
            migrant_pop = migrant_pop * (-2.0),
            aymara_quechua_amazonic_pop = aymara_quechua_amazonic_pop * 2.5,
            amazonic_indigenous_pop = amazonic_indigenous_pop * (-6.3),
            afroamerican_descendent_pop = afroamerican_descendent_pop * 3.5,
            pregnant_woman = pregnant_woman * 2.1,
            adolescent_mothers = adolescent_mothers * 4.9,
            min_temp = min_temp * 3.4,
            dengue_NVI = (no_adult15_literarcy + woman_household_lead + migrant_pop +
                             aymara_quechua_amazonic_pop + amazonic_indigenous_pop +
                             afroamerican_descendent_pop + pregnant_woman +
                            adolescent_mothers + min_temp) / 9)%>% 
  ungroup() %>% 
  mutate(dengue_NVI = (dengue_NVI - min(dengue_NVI)) / (max(dengue_NVI) - min(dengue_NVI)))

#
### Plot 3: National Vulnerability map
#

path_import <- "Data Analysis/Bases de Datos/"

map_wgs84 <- sf::st_read(paste0(path_import,"/Mapa - 2015 (INEI)/DISTRITO_15.shp")) %>%
  select(-c(IDDPTO, IDPROV, NOM_CAP, IDDIST)) %>%
  rename(department = NOMBDEP, province= NOMBPROV, district = NOMBDIST)

db_index_deng_map <- left_join(db_index_deng_fin, map_wgs84)
db_index_diarr_map <- left_join(db_index_diarr_fin, map_wgs84)

ggplot(db_index_deng_map) + 
#  geom_sf(data = map_wgs84, aes(geometry)) +
  viridis::scale_fill_viridis(begin = 0.7, end = 0, option = "G", discrete = F) +
  geom_sf(aes(geometry = geometry, fill = dengue_NVI)) + 
  theme_bw()

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

neighbors_layer <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(admin != "Peru")

peru <- ne_countries(country = "Peru", scale = "medium", returnclass = "sf")

bbox <- st_bbox(map_wgs84)

expand_bbox <- function(bbox, pct = 0.05) {
  bbox <- as.numeric(bbox) |> setNames(names(bbox))  # convert bbox to numeric named vector
  dx <- (bbox["xmax"] - bbox["xmin"]) * pct
  dy <- (bbox["ymax"] - bbox["ymin"]) * pct
  bbox["xmin"] <- bbox["xmin"] - dx
  bbox["xmax"] <- bbox["xmax"] + dx
  bbox["ymin"] <- bbox["ymin"] - dy
  bbox["ymax"] <- bbox["ymax"] + dy
  return(bbox)
}

expanded_bbox <- expand_bbox(st_bbox(map_wgs84), pct = 0.05)

ggplot() +
  viridis::scale_fill_viridis(begin = 0.7, end = 0, option = "G", discrete = F) +
  geom_sf(data = neighbors_layer, fill = "white", color = "grey50", alpha=1, size = 0.3) +
  geom_sf(data = map_wgs84, fill = "white", color = "grey", size = 0.3) +
  geom_sf(data = db_index_deng_map, aes(geometry = geometry, fill = dengue_NVI), color = "grey30", size = 0.2) +
  coord_sf(xlim = c(expanded_bbox["xmin"], expanded_bbox["xmax"]),
           ylim = c(expanded_bbox["ymin"], expanded_bbox["ymax"]), expand = FALSE)+
  theme_bw() +
  theme(
        panel.background = element_rect(fill = scales::alpha("lightblue", 0.7)), # lighter + transparent
    panel.grid = element_blank()) +
  labs(fill = "Dengue NVI", title = "Dengue Index by District")


ggplot() +
  viridis::scale_fill_viridis(begin = 0.7, end = 0, option = "G", discrete = F) +
  geom_sf(data = neighbors_layer, fill = "white", color = "grey50", alpha=1, size = 0.3) +
  geom_sf(data = map_wgs84, fill = "white", color = "grey", size = 0.3) +
  geom_sf(data = db_index_diarr_map, aes(geometry = geometry, fill = diarrhea_NVI), color = "grey30", size = 0.2) +
  coord_sf(xlim = c(expanded_bbox["xmin"], expanded_bbox["xmax"]),
           ylim = c(expanded_bbox["ymin"], expanded_bbox["ymax"]), expand = FALSE)+
  theme_bw() +
  theme(
    panel.background = element_rect(fill = scales::alpha("lightblue", 0.7)), # lighter + transparent
    panel.grid = element_blank()) +
  labs(fill = "Diarrhea NVI", title = "Diarrhea Index by District")


#
## Plot 1
#

dengue_region_means <- db_index_deng_fin %>%
  mutate(region = factor(region, levels = c("Coastal region", "Highland region", "Jungle region"))) %>% 
  group_by(region) %>%
  summarise(mean_NVI = mean(dengue_NVI, na.rm = TRUE))

my_colors <- c("Coastal region" = "#FFD343", "Highland region" = "#E66100", "Jungle region" = "#1A9850")
# #FDE725FF - coast
# #C43C4EFF  - highland
# #2DB27DFF - jungle


ggplot(db_index_deng_fin %>% 
         mutate(region = factor(region, levels = c("Coastal region", "Highland region", "Jungle region")),
                department = factor(department, levels = dept_order)),
       aes(x = department, y = dengue_NVI, fill = region)) +
  scale_fill_manual(values = my_colors) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), color = "grey", aes(shape = north_south)) +
  geom_hline(data = dengue_region_means, aes(yintercept = mean_NVI, linetype = "Regional mean"), 
             color = "tomato", cex= 0.7) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x") +  # visually separates regions
  theme_bw() +
  scale_linetype_manual(name = "Reference", values = c("Regional mean" = "dashed")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # rotate labels for readability
    panel.spacing = unit(1, "lines"),                   # space between facets
    strip.background = element_rect(fill = "grey90"),   # facet strip background
    strip.text = element_text(face = "bold")) +
  labs(x = "Department", y = "dengue NVI", fill = "Region", shape = "North / South")

diarrhea_region_means <- db_index_diarr_fin %>%
  mutate(region = factor(region, levels = c("Coastal region", "Highland region", "Jungle region"))) %>% 
  group_by(region) %>%
  summarise(mean_NVI = mean(diarrhea_NVI, na.rm = TRUE))

ggplot(db_index_diarr_fin %>% 
         mutate(region = factor(region, levels = c("Coastal region", "Highland region", "Jungle region")),
                department = factor(department, levels = dept_order)),
       aes(x = department, y = diarrhea_NVI, fill = region)) +
  scale_fill_manual(values = my_colors) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), color = "grey", aes(shape = north_south)) +
  geom_hline(data = diarrhea_region_means, aes(yintercept = mean_NVI, linetype = "Regional mean"), 
              color = "tomato", cex= 0.7) +
  facet_grid(. ~ region, scales = "free_x", space = "free_x") +  # visually separates regions
  theme_bw() +
  scale_linetype_manual(name = "Reference", values = c("Regional mean" = "dashed")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # rotate labels for readability
    panel.spacing = unit(1, "lines"),                   # space between facets
    strip.background = element_rect(fill = "grey90"),   # facet strip background
    strip.text = element_text(face = "bold")) +
  labs(x = "Department", y = "diarrhea NVI", fill = "Region", shape = "North / South")


#
## Plot 2 
#

db_diarrea_summary <- db_index_diarr_fin %>%
  mutate(region = factor(region, levels = c("Coastal region", "Highland region", "Jungle region"))) %>% 
  group_by(north_south, region, department) %>%
  summarise(across(
      c("adolescent_mothers", "stunting", "single_mother", "woman_violence",
        "diabetes", "amazonic_indigenous_pop", "migrant_pop"), 
      ~ mean(.x, na.rm = TRUE),  # calculate average, ignoring NA
      .names = "{.col}"))      # prefix "avg_" to new column names

names(db_diarrea_summary)

clean_names <- names(db_diarrea_summary) %>%
  gsub("_", " ", .) %>%
#  gsub("average", "avg.", ., ignore.case = TRUE) %>%   # optional tweak
  stringr::str_wrap(width = 15)                      # wrap text at ~15 chars

names(db_diarrea_summary) <- clean_names

my_colors <- c("Coastal region" = "#FFD343", "Highland region" = "#E66100", "Jungle region" = "#1A9850")

ggplot(db_diarrea_summary %>% ungroup %>% 
         gather(key = "vuln_var", value= "value", c(4:10)),
       aes(x = vuln_var, y = value, color = region)) +
  scale_color_manual(values = my_colors) +
  geom_boxplot() +
  geom_point(aes (shape = `north south`), position = position_jitterdodge()) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # rotate labels for readability
    panel.spacing = unit(1, "lines"),                   # space between facets
    strip.background = element_rect(fill = "grey90"),   # facet strip background
    strip.text = element_text(face = "bold")) +
  labs(x = "diarrhea vulnerability factors", y = "factor values * weight", color = "Region", shape = "North / South")

db_dengue_summary <- db_index_deng_fin %>%
  mutate(region = factor(region, levels = c("Coastal region", "Highland region", "Jungle region"))) %>% 
  group_by(north_south, region, department) %>%
  summarise(across(
    c("no_adult15_literarcy", "woman_household_lead", "migrant_pop", 
      "aymara_quechua_amazonic_pop", "amazonic_indigenous_pop", "afroamerican_descendent_pop", 
      "pregnant_woman", "adolescent_mothers", "min_temp"),
    ~ mean(.x, na.rm = TRUE),  # calculate average, ignoring NA
    .names = "{.col}"))      # prefix "avg_" to new column names

clean_names <- names(db_dengue_summary) %>%
  gsub("_", " ", .) %>%
  stringr::str_wrap(width = 15)                      # wrap text at ~15 chars

names(db_dengue_summary) <- clean_names

ggplot(db_dengue_summary %>% 
         gather(key = "vuln_var", value= "value", c(4:12)),
       aes(x = vuln_var, y = value, color = region)) +
  scale_color_manual(values = my_colors) +
  geom_boxplot() +
  geom_point(aes (shape = `north south`), position = position_jitterdodge())  + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # rotate labels for readability
    panel.spacing = unit(1, "lines"),                   # space between facets
    strip.background = element_rect(fill = "grey90"),   # facet strip background
    strip.text = element_text(face = "bold")) +
  labs(x = "dengue vulnerability factors", y = "factor values * weight",  shape = "North / South", color = "Region")




#-----------------------------------------------------------------------------------

##
### MODELOS DDS
#

#*Las variables ya están normalizadas*

db_no_NAs <- read_rds(paste0(path_import, "JULY 2024 - DATABASE WITH NO NAs (spatial agregation and filtering rows between dbses).RDS")) %>% 
  mutate(min_temp = (min_temp - min(min_temp)) / (max(min_temp) - min(min_temp)),
         max_accum_rain_m2 = (max_accum_rain_m2 - min(max_accum_rain_m2)) / (max(max_accum_rain_m2) - min(max_accum_rain_m2)),
         dengue = round(dengue), diarrhea = round(diarrhea)) %>% 
  filter(department == "LORETO")

db_no_NAs <- left_join(db_no_NAs, inei_population)


lm_dengue_DDS <- glm(dengue ~ offset(log(est_population)) + migrant_pop + 
                       no_water_access + min_temp + max_accum_rain_m2 + 
                       no_healthcare_access + stunting + wasting,
                     data = db_no_NAs)

summary(lm_dengue_DDS)
drop1(lm_dengue_DDS, test ="F")


lm_diarrhea_DDS <- glm(diarrhea ~ offset(log(est_population)) + migrant_pop + 
                        no_water_access + no_adult15_literarcy + no_healthcare_access,
                      data = db_no_NAs)


summary(lm_diarrhea_DDS)
drop1(lm_diarrhea_DDS, test ="F")

#-------------------------------------------------------------------------------------------------

#
## CREACIÓN DEL INDICADOR
#


#Indicador 1a: variables significativas del DDS sin peso
#Indicador 1b: todas las variables del DDS sin peso


#Indicador 2a: variables significativas del DDS con peso (coeficiente del modelo)
#Indicador 2b: todas las variables del DDS con peso (coeficiente del modelo)

#VARIABLES Y SUS COEFICIENTES DEL MODELO DDS

#dengue
# no_water_access   -114.64 (p-value <0.05)
# min_temp            1805.97 (p-value <0.05)
# migrant_pop         43.34 (p-value 0.49)
# max_accum_rain_m2  -105.20 (p-value 0.584) 
# no_healthcare_access -95.33 (p-value 0.502)
# stunting -255.68     (0.102)
# wasting  629.64      (0.626)

#diarrhea 
#migrant_pop 936.1 (p-value < 0.05)
#no_water_access -127.9 (p-value 0.520)
#no_adult15_literarcy 775.0 (p-value 0.300)
#no_healthcare access 300.6 (p-value 0.718)

#VARIABLES Y SUS COEFICIENTES DEL MODELO LITERATURA

# dengue
# no_basic_sanitation_access    -6.2320 (p-value <0.05)
# min_temp                     -21.4340 (p-value <0.05)
# deaf_mute_pop                119.2743 (p-value <0.05)
# woman_household_lead          12.9447 (p-value 0.091)
# afroamerican_descendent_pop   19.5442 (p-value 0.074)
# adolescent_mothers           419.8957 (p-value 0.088)
# max_accum_rain_m2             -3.9651 (p-value 0.062)

# diarrhea
# no_water_access               -1.66718 (p-value <0.05)
# woman_household_lead          11.14846 (p-value <0.05)
# min_temperature               25.2039  (p-value <0.05)
# max_accum_rain_m2             -4.58560 (p-value 0.051)
# aymara_quechua_amazonic_pop    4.1977 (p-value 0.086)


#Indicador 3a: literatura con peso (coeficiente del modelo p<0.1)
#Indicador 3b: literatura con peso (coeficiente del modelo p<0.05)
##Variable weights and significance:


indicadores_dengue <- db_no_NAs %>% 
  group_by(department, province, district, dengue) %>% 
  summarise(
    indicador_1a = (((-no_water_access) + min_temp + migrant_pop - max_accum_rain_m2 - 
                       no_healthcare_access - stunting + wasting)/7),
    # indicador_1b = ((no_water_access + min_temp + migrant_pop)/3),
    indicador_2a = ((no_water_access*(-114.64) + min_temp*1805.97 + migrant_pop*43.34 + max_accum_rain_m2*(-105.20) + 
                       no_healthcare_access*(-95.33) + stunting*(-255.68) + wasting*629.64)/7),
    # indicador_2b = ((no_water_access*(-114.64) + min_temp*1805.97 + migrant_pop*43.34)/3),
    indicador_3a = ((no_basic_sanitation_access*(-6.2320) + min_temp*(-21.4340) + deaf_mute_pop*119.2743 + 
                      woman_household_lead*12.9447 + afroamerican_descendent_pop*19.5442 + 
                      adolescent_mothers*419.8957 + max_accum_rain_m2*(-3.9651))/7),
    indicador_3b = ((no_basic_sanitation_access*(-6.2320) + min_temp*(-21.4340) + deaf_mute_pop*119.2743)/3)) %>% 
  ungroup() %>% 
  mutate(indicador_1a = indicador_1a + 0.213,
    indicador_1a = (indicador_1a - min(indicador_1a))/(max(indicador_1a) - min(indicador_1a)),
    indicador_2a = (indicador_2a - min(indicador_2a))/(max(indicador_2a) - min(indicador_2a)),
    indicador_3a = indicador_3a + 3.99,      
    indicador_3a = (indicador_3a - min(indicador_3a))/(max(indicador_3a) - min(indicador_3a)),
    indicador_3b = indicador_3b + 9.23,     
    indicador_3b = (indicador_3b - min(indicador_3b))/(max(indicador_3b) - min(indicador_3b)))

    
indicadores_diarrea <- db_no_NAs %>% 
  group_by(department, province, district, diarrhea) %>% 
  summarise(
    indicador_1a = ((migrant_pop - no_water_access + no_adult15_literarcy + no_healthcare_access)/4),
    # indicador_1b = (migrant_pop),
    indicador_2a = ((migrant_pop*936.1 + no_water_access*(-127.9) +
                       no_adult15_literarcy*775.0 + no_healthcare_access*300.6)/4),
    # indicador_2b = (migrant_pop*0.718),
    indicador_3a = ((no_water_access*(-1.66718) + woman_household_lead*11.14846 + min_temp*25.2039 +
                      max_accum_rain_m2*(-4.58560) + aymara_quechua_amazonic_pop*4.1977)/5),
    indicador_3b = ((no_water_access*(-1.66718) + woman_household_lead*11.14846 + min_temp*25.2039)/3)) %>% 
  ungroup() %>% 
  mutate(indicador_1a = (indicador_1a - min(indicador_1a))/(max(indicador_1a) - min(indicador_1a)),
         indicador_2a = (indicador_2a - min(indicador_2a))/(max(indicador_2a) - min(indicador_2a)),
         indicador_3a = (indicador_3a - min(indicador_3a))/(max(indicador_3a) - min(indicador_3a)),
         indicador_3b = (indicador_3b - min(indicador_3b))/(max(indicador_3b) - min(indicador_3b)))


#
## Mapas
#
library(sf)

map_wgs84 <- sf::st_read(paste0("Bases de Datos/Maps and Shapes/2015 (INEI)/DISTRITO_15.shp")) %>% 
  select(-c(IDDPTO, IDPROV, NOM_CAP, IDDIST)) %>% 
  rename(department = NOMBDEP, province= NOMBPROV, district = NOMBDIST)

indicadores_diarrea <- left_join(indicadores_diarrea, map_wgs84)
indicadores_dengue <- left_join(indicadores_dengue, map_wgs84)

#indicadores_dengue <-  dropna(indicadores_dengue) 
#prueba <-  indicadores_dengue %>% filter(!is.na(dengue))

(mapa_indicadores_diarrea <- ggplot(indicadores_diarrea %>% 
                                      rename("Inx. DDS no-weight" = indicador_1a,
                                             "Inx. DDS with weight" = indicador_2a,
                                             "Inx. literature (p-value<0.1)" = indicador_3a,
                                             "Inx. literature (p-value<0.05)" = indicador_3b) %>%  
                                      gather(indicador, valor, c(5:8))) + 
  viridis::scale_fill_viridis(begin = 1, end =0.4, option = "F", discrete = F) +
  geom_sf(aes(geometry = geometry, fill = valor)) + 
  facet_wrap(~indicador) + 
  theme_bw())

(mapa_vulnerabilidad_dengue <- ggplot(indicadores_dengue%>% 
                                     filter(!is.na(dengue)) %>% 
                                     mutate(dengue = ((dengue + 1)- (min(dengue) + 1)) / ((max(dengue) + 1) - (min(dengue)+1)))) + 
    viridis::scale_fill_viridis(begin = 1, end =0.4, option = "F", discrete = F) +
    geom_sf(aes(geometry = geometry, fill = dengue)) + 
    theme_bw())

(mapa_vulnerabilidad_diarrhea <- ggplot(indicadores_diarrea%>% 
                                        mutate(diarrhea = ((diarrhea + 1)- (min(diarrhea) + 1)) / ((max(diarrhea) + 1) - (min(diarrhea)+1)))) + 
    viridis::scale_fill_viridis(begin = 1, end =0.4, option = "F", discrete = F) +
    geom_sf(aes(geometry = geometry, fill = diarrhea)) + 
    theme_bw())



(mapa_indicadores_dengue <- ggplot(indicadores_dengue%>% 
                                     rename("Inx. DDS no-weight" = indicador_1a,
                                            "Inx. DDS with weight" = indicador_2a,
                                            "Inx. literature (p-value<0.1)" = indicador_3a,
                                            "Inx. literature (p-value<0.05)" = indicador_3b) %>%
                                     gather(indicador, valor, c(5:8))) + 
  viridis::scale_fill_viridis(begin = 1, end =0.4, option = "F", discrete = F) +
  geom_sf(aes(geometry = geometry, fill = valor)) + 
  facet_wrap(~indicador) +
  theme_bw())
