library(haven)
library(tidyverse)
library(SUMMER)
library(sf)
library(readstata13)
library(INLA)

################################################################################
################################################################################
# This file takes all the sepreate DHS survey for a country and combines then 
# into country level count data. 
# It looks the way it does because all the DHS surveys had there own kinks and 
# I worked around them on the fly while creating the data for each country. 
# I Sadly had not time to make it look pretty or put it into a nice for-loop with 
# if statements 
# The code also includes the creation of strata weights. These were not used in 
# analysis because they are only needed for precise estimations.


## Nigeria 

rm(list = ls())

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Nigeria")

dir("../")

# Loading Data 
Nigeria_1_shp <- st_read("../Nigeria/GPS_files/NGGE23FL.shp")
Nigeria_2_shp <- st_read("../Nigeria/GPS_files/NGGE4BFL.shp")
Nigeria_3_shp <- st_read("../Nigeria/GPS_files/NGGE52FL.shp")
Nigeria_4_shp <- st_read("../Nigeria/GPS_files/NGGE6AFL.shp")

Nigeria_1 <- read_dta("../Nigeria/NGBR21FL.dta")
Nigeria_2 <- read_dta("../Nigeria/NGBR4BFL.dta")
Nigeria_3 <- read_dta("../Nigeria/NGBR53FL.dta")
Nigeria_4 <- read_dta("../Nigeria/NGBR6AFL.dta")


# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Nigeria/Admin/nga_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")

table(A1$adm1_name)
table(Nigeria_1_shp$ADM1NAME)

shp_list <- list(Nigeria_1_shp, Nigeria_2_shp, Nigeria_3_shp, Nigeria_4_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Nigeria_shp_", 1:4)

rm(Nigeria_1_shp, Nigeria_2_shp, Nigeria_3_shp, Nigeria_4_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Nigeria_shp_1, Nigeria_shp_2, Nigeria_shp_3, Nigeria_shp_4)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:4)

# Getting the births data using the get births function 
births_list_raw <- list(Nigeria_1, Nigeria_2, Nigeria_3, Nigeria_4)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}

names(births_pm) <- paste0("Nigeria_PM_", 1:4)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Nigeria_survey_", i),
           country = "Nigeria")
  
  counts_all <- bind_rows(counts_all, counts)
}
summary(counts_all$Y)
table(Nigeria_1$b5)
# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(A1$shapeName)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )


weights.strata_Nigeria <- weights.strata
counts_all_Nigeria <- counts_all
save(counts_all_Nigeria, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Nigeria.rda")
save(weights.strata_Nigeria, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Nigeria.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Nigeria.rda")

rm(list = ls())


################################################################################
################################################################################

##Ethiopia

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Ethiopia")

dir("../")

# Loading Data 
Ethiopia_1_shp <- st_read("../Ethiopia/GPS_files/ETGE42FL.shp")
Ethiopia_2_shp <- st_read("../Ethiopia/GPS_files/ETGE52FL.shp")
Ethiopia_3_shp <- st_read("../Ethiopia/GPS_files/ETGE61FL.shp")
Ethiopia_4_shp <- st_read("../Ethiopia/GPS_files/ETGE71FL.shp")
Ethiopia_5_shp <- st_read("../Ethiopia/GPS_files/ETGE81FL.shp")

Ethiopia_1 <- read_dta("../Ethiopia/ETBR41FL.dta")
Ethiopia_2 <- read_dta("../Ethiopia/ETBR51FL.dta")
Ethiopia_3 <- read_dta("../Ethiopia/ETBR61FL.dta")
Ethiopia_4 <- read_dta("../Ethiopia/ETBR71FL.dta")
Ethiopia_5 <- read_dta("../Ethiopia/ETBR81FL.dta")



# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Ethiopia/Admin/eth_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(Ethiopia_1_shp, Ethiopia_2_shp, Ethiopia_3_shp, Ethiopia_4_shp,
                 Ethiopia_5_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Ethiopia_shp_", 1:5)

rm(Ethiopia_1_shp, Ethiopia_2_shp, Ethiopia_3_shp, Ethiopia_4_shp,
   Ethiopia_5_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Ethiopia_shp_1, Ethiopia_shp_2, Ethiopia_shp_3, Ethiopia_shp_4,
                 Ethiopia_shp_5)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:5)

# Getting the births data using the get births function 
births_list_raw <- list(Ethiopia_1, Ethiopia_2, Ethiopia_3, Ethiopia_4,
                        Ethiopia_5)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Ethiopia_PM_", 1:5)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Ethiopia_survey_", i),
           country = "Ethiopia")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_Ethiopia <- weights.strata
counts_all_Ethiopia <- counts_all
save(counts_all_Ethiopia, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Ethiopia.rda")
save(weights.strata_Ethiopia, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Ethiopia.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Ethiopia.rda")

rm(list = ls())


################################################################################
################################################################################

## Sierra Leone 

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Sierra_Leone")

dir("../")

# Loading Data 
SL_1_shp <- st_read("../Sierra_Leone/GPS_files/SLGE7AFL.shp")
SL_2_shp <- st_read("../Sierra_Leone/GPS_files/SLGE53FL.shp")
SL_3_shp <- st_read("../Sierra_Leone/GPS_files/SLGE61FL.shp")


SL_1 <- read_dta("../Sierra_Leone/SLBR7AFL.dta")
SL_2 <- read_dta("../Sierra_Leone/SLBR51FL.dta")
SL_3 <- read_dta("../Sierra_Leone/SLBR61FL.dta")



# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Sierra_Leone/Admin/sle_admin2.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(SL_1_shp, SL_2_shp, SL_3_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm2_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("SL_shp_", 1:3)

rm(SL_1_shp, SL_2_shp, SL_3_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm2_name)


# Creating the clusters:
shp_list <- list(SL_shp_1, SL_shp_2, SL_shp_3)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm2_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm2_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:3)

# Getting the births data using the get births function 
births_list_raw <- list(SL_1, SL_2, SL_3)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("SL_PM_", 1:3)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Sierra_Leone_survey_", i),
           country = "Sierra Leone")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_SL <- weights.strata
counts_all_SL <- counts_all
save(counts_all_SL, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_SL.rda")
save(weights.strata_SL, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_SL.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_SL.rda")

rm(list = ls())


################################################################################
################################################################################

## Chad 

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Chad")

dir("../")

# Loading Data 
Chad_1_shp <- st_read("../Chad/GPS_files/TDGE71FL.shp")
Chad_1 <- read_dta("../Chad/TDBR71FL.dta")



# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Chad/Admin/tcd_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(Chad_1_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Chad_shp_", 1:1)

rm(Chad_1_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Chad_shp_1)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:1)

# Getting the births data using the get births function 
births_list_raw <- list(Chad_1)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Chad_PM_", 1:1)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Chad_survey_", i),
           country = "Chad")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(is.na(counts_all$total))
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_Chad <- weights.strata
counts_all_Chad <- counts_all
save(counts_all_Chad, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Chad.rda")
save(weights.strata_Chad, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Chad.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Chad.rda")

rm(list = ls())



################################################################################
################################################################################

## Burundi

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Burundi")

dir("../")

# Loading Data 
Burundi_1_shp <- st_read("../Burundi/GPS_files/BUGE61FL.shp")
Burundi_2_shp <- st_read("../Burundi/GPS_files/BUGE71FL.shp")



Burundi_1 <- read_dta("../Burundi/BuBR61FL.dta")
Burundi_2 <- read_dta("../Burundi/BuBR71FL.dta")



# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Burundi/Admin/bdi_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(Burundi_1_shp, Burundi_2_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Burundi_shp_", 1:2)

rm(Burundi_1_shp, Burundi_2_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Burundi_shp_1, Burundi_shp_2)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:2)

# Getting the births data using the get births function 
births_list_raw <- list(Burundi_1, Burundi_2)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Burundi_PM_", 1:2)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Burundi_survey_", i),
           country = "Burundi")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_Burundi <- weights.strata
counts_all_Burundi <- counts_all
save(counts_all_Burundi, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Burundi.rda")
save(weights.strata_Burundi, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Burundi.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Burundi.rda")

rm(list = ls())



################################################################################
################################################################################

## Guinea

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Guinea")

dir("../")

# Loading Data 
Guinea_1_shp <- st_read("../Guinea/GPS_files/GNGE42FL.shp")
Guinea_2_shp <- st_read("../Guinea/GPS_files/GnGE52FL.shp")
Guinea_3_shp <- st_read("../Guinea/GPS_files/GNGE61FL.shp")
Guinea_4_shp <- st_read("../Guinea/GPS_files/GnGE71FL.shp")


Guinea_1 <- read_dta("../Guinea/GNBR41FL.dta")
Guinea_2 <- read_dta("../Guinea/gnbr52fl.dta")
Guinea_3 <- read_dta("../Guinea/GNBR62FL.dta")
Guinea_4 <- read_dta("../Guinea/GNBR71FL.dta")


# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Guinea/Admin/gin_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(Guinea_1_shp, Guinea_2_shp, Guinea_3_shp, Guinea_4_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Guinea_shp_", 1:4)

rm(Guinea_1_shp, Guinea_2_shp, Guinea_3_shp, Guinea_4_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Guinea_shp_1, Guinea_shp_2, Guinea_shp_3, Guinea_shp_4)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:4)

# Getting the births data using the get births function 
births_list_raw <- list(Guinea_1, Guinea_2, Guinea_3, Guinea_4)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Guinea_PM_", 1:4)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Guinea_survey_", i),
           country = "Guinea")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_Guinea <- weights.strata
counts_all_Guinea <- counts_all
save(counts_all_Guinea, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Guinea.rda")
save(weights.strata_Guinea, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Guinea.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Guinea.rda")

rm(list = ls())


################################################################################
################################################################################

## Angola

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Angola")

dir("../")

# Loading Data 
Angola_1_shp <- st_read("../Angola/GPS_files/AOGE61FL.shp")
Angola_2_shp <- st_read("../Angola/GPS_files/AOGE71FL.shp")



Angola_1 <- read_dta("../Angola/AOBR62FL.dta")
Angola_2 <- read_dta("../Angola/AOBR71FL.dta")



# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Angola/Admin/ago_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(Angola_1_shp, Angola_2_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Angola_shp_", 1:2)

rm(Angola_1_shp, Angola_2_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Angola_shp_1, Angola_shp_2)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:2)

# Getting the births data using the get births function 
births_list_raw <- list(Angola_1, Angola_2)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Angola_PM_", 1:2)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Angola_survey_", i),
           country = "Angola")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_Angola <- weights.strata
counts_all_Angola <- counts_all
save(counts_all_Angola, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Angola.rda")
save(weights.strata_Angola, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Angola.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Angola.rda")

rm(list = ls())


################################################################################
################################################################################

## Lesotho

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Lesotho")

dir("../")

# Loading Data 
Lesotho_1_shp <- st_read("../Lesotho/GPS_files/LSGE42FL.shp")
Lesotho_2_shp <- st_read("../Lesotho/GPS_files/LSGE62FL.shp")
Lesotho_3_shp <- st_read("../Lesotho/GPS_files/LSGE71FL.shp")


Lesotho_1 <- read_dta("../Lesotho/LSBR41FL.dta")
Lesotho_2 <- read_dta("../Lesotho/LSBR61FL.dta")
Lesotho_3 <- read_dta("../Lesotho/LSBR71FL.dta")


# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Lesotho/Admin/geoBoundaries-LSO-ADM1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = shapeName), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(Lesotho_1_shp, Lesotho_2_shp, Lesotho_3_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(shapeName, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Lesotho_shp_", 1:3)

rm(Lesotho_1_shp, Lesotho_2_shp, Lesotho_3_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$shapeName)


# Creating the clusters:
shp_list <- list(Lesotho_shp_1, Lesotho_shp_2, Lesotho_shp_3)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, shapeName, LATNUM, LONGNUM) %>%
    mutate(
      admin = shapeName ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:3)

# Getting the births data using the get births function 
births_list_raw <- list(Lesotho_1, Lesotho_2, Lesotho_3)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Lesotho_PM_", 1:3)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Lesotho_survey_", i),
           country = "Lesotho")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_Lesotho <- weights.strata
counts_all_Lesotho <- counts_all
save(counts_all_Lesotho, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Lesotho.rda")
save(weights.strata_Lesotho, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Lesotho.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Lesotho.rda")

rm(list = ls())


################################################################################
################################################################################

## Liberia

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Liberia")

dir("../")

# Loading Data 
Liberia_1_shp <- st_read("../Liberia/GPS_files/LBGE52FL.shp")
Liberia_2_shp <- st_read("../Liberia/GPS_files/LBGE6AFL.shp")
Liberia_3_shp <- st_read("../Liberia/GPS_files/LBGE7AFL.shp")


Liberia_1 <- read_dta("../Liberia/LBBR51FL.dta")
Liberia_2 <- read_dta("../Liberia/LBBR6AFL.dta")
Liberia_3 <- read_dta("../Liberia/LBBR7AFL.dta")


# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Liberia/Admin/lbr_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province")


shp_list <- list(Liberia_1_shp, Liberia_2_shp, Liberia_3_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Liberia_shp_", 1:3)

rm(Liberia_1_shp, Liberia_2_shp, Liberia_3_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Liberia_shp_1, Liberia_shp_2, Liberia_shp_3)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:3)

# Getting the births data using the get births function 
births_list_raw <- list(Liberia_1, Liberia_2, Liberia_3)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Liberia_PM_", 1:3)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Liberia_survey_", i),
           country = "Liberia")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )
weights.strata_Liberia <- weights.strata
counts_all_Liberia <- counts_all
save(counts_all_Liberia, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Liberia.rda")
save(weights.strata_Liberia, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Liberia.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Liberia.rda")

rm(list = ls())


################################################################################
################################################################################

## Uganda

setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/DHS/Uganda")

dir("../")

# Loading Data 
Uganda_1_shp <- st_read("../Uganda/GPS_files/UGGE43FL.shp")
Uganda_2_shp <- st_read("../Uganda/GPS_files/UGGE53FL.shp")
Uganda_3_shp <- st_read("../Uganda/GPS_files/UGGE61FL.shp")
Uganda_4_shp <- st_read("../Uganda/GPS_files/UGGE7AFL.shp")


Uganda_1 <- read_dta("../Uganda/UGBR41FL.dta")
Uganda_2 <- read_dta("../Uganda/UGBR52FL.dta")
Uganda_3 <- read_dta("../Uganda/UGBR61FL.dta")
Uganda_4 <- read_dta("../Uganda/UGBR7BFL.dta")

# Create adjacency matrix (Amat) for admin regions

# loading Admin 1  grid 
A1 <- st_read("../Uganda/Admin/UGA_admin1.geojson")

ggplot(data = A1) +
  geom_sf(aes(fill = adm1_name), color = "white", size = 0.2) +
  scale_fill_viridis_d(name = "Province") +
  theme(legend.position="none")


shp_list <- list(Uganda_1_shp, Uganda_2_shp, Uganda_3_shp, Uganda_4_shp)
region_shp <- list()
for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  
  A1_merge <- A1 %>% select(adm1_name, geometry)
  
  region_shp[[i]] <- shp %>% st_join(A1_merge, join = st_within)
  
}
names(region_shp) <- paste0("Uganda_shp_", 1:4)

rm(Uganda_1_shp, Uganda_2_shp, Uganda_3_shp, Uganda_4_shp)
list2env(region_shp, envir = .GlobalEnv)


#creating the amat
mat <- getAmat(geo = A1$geometry, names = A1$adm1_name)


# Creating the clusters:
shp_list <- list(Uganda_shp_1, Uganda_shp_2, Uganda_shp_3, Uganda_shp_4)
cluster_maps <- list()

for(i in seq_along(shp_list)){
  shp <- shp_list[[i]]
  shp_df <- as.data.frame(shp)
  cluster_maps[[i]] <- shp_df %>%
    distinct(DHSCLUST, adm1_name, LATNUM, LONGNUM) %>%
    mutate(
      admin = adm1_name ,
      v001 = as.integer(DHSCLUST) 
    ) %>%
    select(v001, admin, LATNUM, LONGNUM)
}

names(cluster_maps) <- paste0("cluster_map_", 1:4)

# Getting the births data using the get births function 
births_list_raw <- list(Uganda_1, Uganda_2, Uganda_3, Uganda_4)
births_pm <- list()
for(i in seq_along(births_list_raw)){
  bp <- births_list_raw[[i]]
  cm <- cluster_maps[[i]]
  
  cm <- cm %>% select(v001, admin)
  
  bp <- bp %>% left_join(cm, by = c("v001"))
  
  bp <- bp %>% 
    mutate(b5 = ifelse(b5 == 1, "YES", "NO"))
  
  births_pm[[i]] <- getBirths(data = bp,
                              month.cut = c(1, 12, 24, 36, 48, 60),
                              year.cut  = seq(1989, 2014, by = 1),
                              strata    = c("admin", "v025")
                              
  )
}
names(births_pm) <- paste0("Uganda_PM_", 1:4)




# Merging the bith data with the geographic data to get the final data for estimation 
counts_all <- NULL
for(i in seq_along(births_pm)){
  b <- births_pm[[i]]
  
  cm <- cluster_maps[[i]]
  b <- b %>% left_join(cm, by = c("v001", "admin"))
  
  b <- b %>%
    mutate(
      clustid = v001,
      region  = admin,  
      strata  = v025
    )
  vars <- c("clustid", "region", "time", "age", "strata")
  counts <- getCounts(b[, c(vars, "died")],
                      variables = "died",
                      by = vars,
                      drop = TRUE)
  
  counts <- counts %>%
    mutate(
      cluster = clustid,
      years   = time,
      region  = region,
      strata  = strata,
      Y       = died
    ) %>%
    select(cluster, years, region, strata, age, total, Y) %>%
    mutate(survey = paste0("Uganda_survey_", i),
           country = "Uganda")
  
  counts_all <- bind_rows(counts_all, counts)
}

# Quick checks to seef if i have missings
table(is.na(counts_all$cluster))
table(is.na(counts_all$region))

head(counts_all)


table(counts_all$region)
table(counts_all$total)
table(counts_all$Y)


# Using this data to try to make the strata wheigts needed for posterior sampling
weights.strata <- counts_all %>%
  mutate(strata = case_when(
    strata == "Rural" ~ "rural",
    strata == "Urban" ~ "urban",
    T ~ strata
  )) %>% 
  group_by(region, years, strata) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(region, years, strata = c("urban", "rural"), fill = list(n = 0)) %>%
  group_by(region, years) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

weights.strata <- weights.strata %>% filter(is.na(prop) == FALSE)

weights.strata <- weights.strata %>%
  select(region, years, strata, prop) %>%     # keep only needed vars
  pivot_wider(
    names_from = strata,                      # create columns "urban" and "rural"
    values_from = prop                        # fill them with the proportion values
  )

weights.strata_Uganda <- weights.strata
counts_all_Uganda <- counts_all
save(counts_all_Uganda, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/counts_all_Uganda.rda")
save(weights.strata_Uganda, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Uganda.rda")
save(mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/mat_Uganda.rda")


rm(list = ls())


