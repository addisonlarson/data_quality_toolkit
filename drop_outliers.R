library(here)
library(tidyverse)
library(tidycensus)
library(sf)
library(spdep)
library(tigris)
options(tigris_class = "sf")
# VARIABLES
# B03002_012 (Hispanic or Latino)
# B08303_008 - 013 (Commutes 30+ minutes)
# B08303_013 (Supercommuters)
# GEOGRAPHIES
# Block Group
# Tract
# County
# STUDY AREA
# District of Columbia
# TBP Planning Area

var_list <- list("No. Hispanic Residents" = "B03002_002",
                 "No. Residents, Commutes 30+ Minutes" = c("B08303_008",
                                                           "B08303_009",
                                                           "B08303_010",
                                                           "B08303_011",
                                                           "B08303_012",
                                                           "B08303_013"),
                 "No. Residents, Commutes 60+ Minutes" = "B08303_013")
geo_list <- c("tract")
a_bg <- block_groups("DC", cb = TRUE)
a_trct <- tracts("DC", cb = TRUE)
a_cty <- counties("DC", cb = TRUE)

for(item in 1:length(var_list)){
  for(geo in geo_list){
    threshold <- NULL
    if(geo == "block group"){
      threshold <- 100
      study_area <- a_bg
    } else {
      threshold <- 80
      study_area <- a_trct
    }
    a <- get_acs(geography = geo,
                 state = "DC",
                 variables = var_list[[item]]) %>%
      group_by(GEOID) %>%
      summarize(sum_est = sum(estimate),
                sum_moe = moe_sum(moe, estimate)) %>%
      mutate_at(vars(sum_moe), funs(replace_na(., 0))) %>%
      mutate(se = sum_moe / 1.645,
             cv = case_when(sum_est == 0 ~ 100,
                            sum_est != 0 ~ se / sum_est * 100),
             year = 2017)
    a <- left_join(study_area, a)
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
    print(summary(a$cv))
    a$sp_outlier <- flag
    a <- a %>%
      mutate_at(vars(sp_outlier), funs(ifelse(cv > threshold, 1, 0))) %>%
      mutate(cv_cat = case_when(cv <= 15 ~ "0-15",
                                cv > 15 & cv <= 30 ~ "15.1-30",
                                cv > 30 & cv <= 60 ~ "30.1-60",
                                cv > 60 ~ "60.1+"))
    print(table(a$sp_outlier))
    print(table(a$cv_cat))
    plot(a["cv"])
    plot(a["sp_outlier"])
    no_adj <- a %>% st_set_geometry(NULL) %>%
      summarize(mean(cv), median(cv), max(cv))
    adj <- a %>% filter(sp_outlier == 0) %>%
      st_set_geometry(NULL) %>%
      summarize(mean(cv), median(cv), max(cv))
  }
}
