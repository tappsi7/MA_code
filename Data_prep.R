library(tidyverse)
library(sf)
library(haven) 
library(readxl)

rm(list= ls())
setwd("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data")
dir("../")

# Combining the different Shape files of the countries into one.

countries <- list("Angola","Burundi","Chad","Ethiopia","Guinea","Lesotho",
                  "Liberia","Nigeria","Sierra_Leone","Uganda")
admpath <- list()
admfilename <- list()
paths <- list()
admfiles <- list()
for(i in seq_along(countries)){
  admpath[i] <- paste0("../Data/DHS/", countries[i],"/Admin")
  
  m <- admpath[[i]]
  admfilename[i] <- list.files(path = m)
  
  f <- admfilename[[i]]
  paths[i] <- paste0(m,"/",f )
 
}

adm <- lapply(paths, st_read)
adm[[4]] <- adm[[4]] %>% select(adm1_name, adm0_name, geometry)
adm  <- bind_rows(adm)

adm1 <- c("Angola", "Liberia", "Nigeria", "Tchad","Ethiopia", "Burundi",
          "Uganda", "Guinée")
adm2 <- c( "Sierra Leone")

adm <- adm %>% mutate(adm = case_when(
  adm0_name %in% adm1 ~ adm1_name,
  adm0_name %in% adm2 ~ adm2_name,
  shapeGroup == "LSO" ~ shapeName
), Country = case_when(
  adm0_name %in% adm1 | adm0_name %in% adm2 ~ adm0_name,
  shapeGroup == "LSO" ~ "Lesotho"
))

adm <- adm %>% select(adm, Country, geometry)

saveRDS(adm, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/adm.rds") 

## Prepping the Aiddata by dividing into aid forms and prepping the merge with the
## region files

folder_path <- "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/Aiddata"


files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

aiddata <- bind_rows(
  lapply(files, function(f) {
    read_excel(f, col_types = "text")
  })
)

aiddata <- aiddata %>%
  mutate(across(c(year, lat, long, precision, usdcr, usdco, crspcode), 
                as.numeric))

#  1 = Humanitarian - 2 = Development - 3 = Security         
aiddata <- aiddata %>% mutate(aidtype = case_when(crspcode > 70000 & crspcode < 80000 ~ 1,
                                                  crspcode > 15200 & crspcode < 16000 ~ 3,
                                                  TRUE ~ 2 
))

aiddata <- aiddata %>% filter(precision < 6)

country_year_aid <- aiddata |>
  distinct(rname, year)

country_year_aid <- country_year_aid %>% filter(is.na(rname) == F) %>% 
  mutate(conflict = 1)


aiddata <- aiddata %>% filter(is.na(long) == F) %>% 
  st_as_sf( coords = c("long", "lat"), crs = 4326)


hum_aid <- aiddata %>%  select(year, rname, usdco, aidtype, geometry, precision) %>% 
  filter(aidtype == 1)

dev_aid <- aiddata %>% select(year, rname, usdco, aidtype, geometry) %>% 
  filter(aidtype == 2)
table(hum_aid$precision)

hum_aid <- st_transform(hum_aid, st_crs(adm))

adm_hum <- hum_aid %>%
  st_join(adm)

adm_hum <- adm_hum %>%
  group_by(adm, year) %>% 
  summarise(hum_aid = sum(usdco, na.rm = TRUE))


dev_aid <- st_transform(dev_aid, st_crs(adm))

adm_dev <- dev_aid %>%
  st_join(adm)

adm_dev <- adm_dev %>%
  group_by(adm, year) %>% 
  summarise(dev_aid = sum(usdco, na.rm = TRUE))


save(adm_dev, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Covariate Data/dev_counts.rda")
save(adm_hum, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Covariate Data/hum_counts.rda")

## Prepping the Event data from the UCDP GED by filtering out unprecise events
## and getting the geometry right 

event_data <- readRDS("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/GEDEvent_v25_1.rds")

event_data <- event_data %>% st_as_sf( coords = c("longitude", "latitude"), 
                                       crs = 4326)
event_data <- event_data %>% filter(where_prec < 5 ) %>% select(geometry, best, year) %>% 
  st_join(adm) %>% filter(is.na(adm) == F) %>% group_by(adm, year) %>%
  summarise(deaths  = sum(best, na.rm = T))

event_data <- event_data %>% filter(year <= 2009)

save(event_data, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Covariate Data/event_counts.rda")
save(adm, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Covariate Data/admin_base.rda")


## Combining the Covariates and my main data, by expanding the region data with years
## and then merging in the aid data and the violence data to create the region level
## covariates 

covariates <- adm  %>%
  tidyr::expand_grid(year = 1989:2009)

covariates <- left_join(covariates, adm_hum, by = c("year", "adm"))
covariates <- left_join(covariates, adm_dev, by = c("year", "adm"))
covariates <- left_join(covariates, event_data, by = c("year", "adm"))

covariates <- covariates %>% mutate(Country = str_to_lower(Country),
                                    Country = dplyr::recode(Country, 
                                                     "guinée" = "guinea",
                                                     "tchad"  = "chad"))
                                      
table(covariates$Country)

country_year_aid <- country_year_aid %>% mutate(rname = str_to_lower(rname))

covariates <- left_join(covariates, country_year_aid, by = c("Country" = "rname", "year"))


covariates <- covariates %>% mutate(conflict = case_when(conflict == 1 | conflict == 1 ~ 1,
                                                         is.na(conflict) == T & is.na(conflict) == T ~ 0))
covariates <- covariates %>% mutate(log_death = log1p(deaths), 
                                    intense = case_when(log_death > 4.5 ~ 1,
                                                        log_death <= 4.5 ~ 0,
                                                        TRUE ~ 0))
table(covariates$intense)


covariates <- covariates %>% select(adm, Country, year, hum_aid, dev_aid,
                                    deaths, geometry.x, conflict, intense)
covariates <- covariates %>% mutate(log_hum_aid = log10(hum_aid),
                                    log_dev_aid = log10(dev_aid),
                                    hum_scaled = scale(hum_aid) %>% as.numeric(),
                                    dev_scaled = scale(dev_aid) %>% as.numeric())



covariates[is.na(covariates)] <- 0

covariates <- covariates %>%
  mutate(log_hum_lag = lag(log_hum_aid, n = 1),
         log_dev_lag = lag(log_dev_aid, n = 1),
         hum_scaled_lag = lag(hum_scaled, n = 1),
         dev_scaled_lag = lag(dev_scaled, n = 1),
         hum_lag = lag(hum_aid, n = 1)) 

saveRDS(covariates, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Data/covariates.rds")

### Loading the counts data and combining it 

files <- list.files(
  path = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data",
  pattern = "^counts_all_.*\\.rda$",  
  full.names = TRUE
)
files

data_list <- lapply(files, function(f) {
  e <- new.env()
  load(f, envir = e)

  e[[ls(e)[1]]]
})
  
counts_all <- bind_rows(data_list)

table(counts_all$country)

## Merging the covariate data onto the count data


counts_all$years <- as.numeric(as.character(counts_all$years))
counts_all <- left_join(counts_all, covariates, by = c("region" = "adm",
                                                       "years" = "year"))

counts_all$y2 <- sample(counts_all$Y, size = length(counts_all$Y), replace = TRUE)

counts_all <- counts_all %>% mutate(y2 = case_when(y2 > total ~ 1,
                                                   TRUE ~ y2))
counts_all <- counts_all %>% select(cluster, years, strata, total, Y, region,
                                    country, hum_aid, dev_aid, deaths, age, 
                                    conflict, log_hum_lag, hum_scaled, log_dev_lag, hum_scaled_lag,
                                    dev_scaled_lag, hum_aid, dev_aid, hum_lag, intense)

## last checks, filtering and saving

counts_all$strata <- tolower(counts_all$strata)

counts_all <- counts_all %>% filter(years <= 2009)
test_counts_all <- counts_all %>% filter(years >= 2004)
Ethiopia_counts_all <- counts_all %>% filter(country == "Ethiopia")

saveRDS(counts_all, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/all_counts.rds", compress = FALSE)
saveRDS(test_counts_all, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/test_all_counts.rds", compress = FALSE)
saveRDS(Ethiopia_counts_all, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Ethiopia_all_counts.rds", compress = FALSE)


## Getting the strata weights, which where not needed in the analysis

files <- list.files(
  path = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data",
  pattern = "^strata_weights_.*\\.rda$",  
  full.names = TRUE
)

data_list <- lapply(files, function(f) {
  e <- new.env()
  load(f, envir = e)
  
  e[[ls(e)[1]]]
})

weights_all <- bind_rows(data_list)

weights_all$years <- as.numeric(as.character(weights_all$years))
test_weights <- weights_all %>% filter(years >= 2004 & years <= 2009)

load("C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data/strata_weights_Ethiopia.rda")
Ethiopia_weights <- weights.strata_Ethiopia

Ethiopia_weights$years <- as.numeric(as.character(Ethiopia_weights$years))
Ethiopia_weights <- Ethiopia_weights %>% filter(years <= 2009)

saveRDS(weights_all, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/weights_all.rds", compress = FALSE)
saveRDS(test_weights, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/test_weights.rds", compress = FALSE)
saveRDS(Ethiopia_weights, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/Ethiopia_weights.rds", compress = FALSE)

## Loading the adjacency matricies and combining them into one. 

files <- list.files(
  path = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/fitting_data",
  pattern = "^mat_.*\\.rda$",  
  full.names = TRUE
)

data_list <- lapply(files, function(f) {
  e <- new.env()
  load(f, envir = e)
  
  e[[ls(e)[1]]]
})

### Now to the fucking matricies 

library(Matrix)

# Get all unique region names across all matrices
all_regions <- unique(unlist(lapply(data_list, rownames)))

# Create one big empty matrix
combined <- matrix(0, 
                   nrow = length(all_regions), 
                   ncol = length(all_regions),
                   dimnames = list(all_regions, all_regions))

# Fill in each sub-matrix
for (mat in data_list) {
  regions <- rownames(mat)
  combined[regions, regions] <- mat
}

big.mat <- combined

## taking care of this one region in Angola thats in another country by assigning it
## one boarder. If i dont do this the model crashes

big.mat["Cabinda", "Zaire"] <- 1
big.mat["Zaire", "Cabinda"] <- 1

saveRDS(big.mat, file = "C:/Users/fabia/OneDrive/Desktop/Master Thesis/R-Stuff/big_mat.rds", compress = FALSE )

