library(here)
library(tidyverse)
library(tidycensus)
library(sf)
library(spdep)
# 2017 data
a <- get_acs(geography = "tract",
             state = "DC",
             variable = "B08101_025",
             geometry = TRUE) %>% # Count of transit commuters
  mutate_at(vars(estimate), funs(na_if(., 0))) %>%
  mutate(se = moe / 1.645,
         cv = se / estimate * 100,
         year = 2017) %>%
  drop_na(.)
# Link matrix
a_spatial <- as(a, "Spatial")
a_nb <- poly2nb(a_spatial, queen = TRUE)
# High percentage difference from neighbors
flag <- vector()
for(j in 1:length(a_nb)){
  sub <- a_nb[[j]]
  nb_properties <- a %>% slice(sub) %>% pull(cv) %>% mean(.)
  self_properties <- a %>% slice(j) %>% pull(cv) %>% mean(.)
  diff <- (self_properties - nb_properties) /
    ((self_properties + nb_properties) / 2) * 100
  if(diff >= 40){
    flag <- c(flag, 1)
  } else {
    flag <- c(flag, 0)
  }
}
a$sp_outlier <- flag

# Effect on CV for study area
a %>% st_set_geometry(NULL) %>%
  summarize(mean(cv), median(cv), max(cv)) # 17.62, 16.21, 64.17
a %>% filter(sp_outlier == 0) %>%
  st_set_geometry(NULL) %>%
  summarize(mean(cv), median(cv), max(cv)) # 16.62, 15.92, 32.28

# Must check this method with several geos and variables.
# Write s.t. you can use CTPP downloads
# GEO: bg tract county
# VARIABLE: B03002_012 (Hispanic or Latino)
#           B08303_013 (Supercommuters)
# EXPANDING AND SHRINKING STUDY AREA: DC vs TPB planning area

transcom <- list()
for(item in c("block group", "tract")){
  threshold <- NULL
  if(item == "block group"){
    threshold <- 100
  } else {
    threshold <- 80
  }
  a <- get_acs(geography = item,
               state = "DC",
               variable = "B08303_013",
               geometry = TRUE) %>%
    # ZERO-DROPPING IS A PROBLEM. CREATES NO-NEIGHBOR SPATIAL UNITS
    mutate_at(vars(estimate), funs(na_if(., 0))) %>%
    mutate_at(vars(moe), funs(replace_na(., 0))) %>%
    mutate(se = moe / 1.645,
           cv = se / estimate * 100,
           year = 2017) %>%
    drop_na(.)
  transcom[[item]] <- a
  a_spatial <- as(a, "Spatial")
  a_nb <- poly2nb(a_spatial, queen = TRUE)
  flag <- vector()
  for(j in 1:length(a_nb)){
    sub <- a_nb[[j]]
    nb_properties <- a %>% slice(sub) %>% pull(cv) %>% mean(.)
    self_properties <- a %>% slice(j) %>% pull(cv) %>% mean(.)
    diff <- (self_properties - nb_properties) /
      ((self_properties + nb_properties) / 2) * 100
    if(diff >= 40){
      flag <- c(flag, 1)
    } else {
      flag <- c(flag, 0)
    }
  }
  a$sp_outlier <- flag
  a$sp_outlier <- ifelse(a$cv > threshold, 1, 0)
  plot(a["cv"])
  plot(a["sp_outlier"])
  no_adj <- a %>% st_set_geometry(NULL) %>%
    summarize(mean(cv), median(cv), max(cv))
  adj <- a %>% filter(sp_outlier == 0) %>%
    st_set_geometry(NULL) %>%
    summarize(mean(cv), median(cv), max(cv))
}
