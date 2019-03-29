# for CTPP you're going to have to dl and save all input data
# simulate that here
library(here)
library(tidyverse)
library(tidycensus)
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

var_list <- list("a" = "B03002_002",
                 "b" = c("B08303_008",
                         "B08303_009",
                         "B08303_010",
                         "B08303_011",
                         "B08303_012",
                         "B08303_013"),
                 "c" = "B08303_013")
geo_list <- c("tract", "block group")

for(item in 1:length(var_list)){
  for(geo in geo_list){
    threshold <- NULL
    if(geo == "block group"){
      threshold <- 100
    } else {
      threshold <- 80
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
                            sum_est != 0 ~ se / sum_est * 100))
    write_csv(a, here("dl_data", paste0(names(var_list)[[item]], "_", geo,".csv")))
  }
}

block_groups("DC", cb = TRUE) %>%
  dplyr::select(-ALAND, -AWATER) %>%
  st_write(., here("dl_geo", "a_bg.shp"))
tracts("DC", cb = TRUE) %>%
  dplyr::select(-ALAND, -AWATER) %>%
  st_write(., here("dl_geo", "a_trct.shp"))
counties("DC", cb = TRUE) %>%
  dplyr::select(-ALAND, -AWATER) %>%
  st_write(., here("dl_geo", "a_cty.shp"))
