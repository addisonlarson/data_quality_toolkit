library(here); library(tidyverse); library(sf)
options(stringsAsFactors = FALSE)

# Split residence-based and workplace-based .shps into .csvs with CV data
# One field per .csv because we use these to generate summary sheets
types <- c("residence", "workplace")
for (type in types){
  type_key <- ifelse(type == "residence", "rac", "wac")
  if (type_key == "residence"){
    geogs <- c("cty", "puma", "tad", "tract", "taz")
  } else {
    geogs <- c("cty", "powpuma", "tad", "tract", "taz")
  }
  for (g in geogs){
    tabs <- read_csv(here(paste("inputfile", type_key, "xwalk.csv", sep = "_"))) %>%
      filter(geo == g) %>%
      select(tableid) %>%
      distinct(.) %>%
      pull(.)
    for (t in tabs){
      dat <- st_read(here("raw", type, paste0(g, "/", t, ".shp"))) %>%
        st_set_geometry(NULL)
      detector <- ncol(dat) - 4 # Tells us how many additional columns we need to plan for -- some datasets are very wide
      iterator <- detector / 2
      for (u in seq(from = 5, by = 2, length.out = iterator)) {
        temp_dat <- dat %>% select(geoid, u, u + 1)
        names(temp_dat) <- c("GEOID", "sum_est", "sum_moe")
        temp_dat <- temp_dat %>%
          mutate(se = sum_moe / 1.645,
                 cv = case_when(sum_est == 0 ~ 100,
                                sum_est != 0 ~ se / sum_est * 100)) %>%
          write_csv(., here("dl_data", paste0(t, "_", u, "_", g, ".csv")))
      }
    }
  }
}

# Flows data has different schema, and we don't download tract or TAZ OD matrices
type <- "flows"
geogs <- c("cty", "puma", "tad")
for (g in geogs){
  tabs <- read_csv(here("inputfile_od_xwalk.csv")) %>%
    filter(geo == g) %>%
    select(tableid) %>%
    distinct(.) %>%
    pull(.)
  for (t in tabs){
    dat <- st_read(here("raw", type, paste0(g, "/", t, ".shp"))) %>%
      st_set_geometry(NULL)
    detector <- ncol(dat) - 2
    iterator <- detector / 2
    for (u in seq(from = 3, by = 2, length.out = iterator)) {
      temp_dat <- dat %>% select(res_id, u, u + 1)
      names(temp_dat) <- c("GEOID", "sum_est", "sum_moe")
      temp_dat <- temp_dat %>%
        mutate(se = sum_moe / 1.645,
               cv = case_when(sum_est == 0 ~ 100,
                              sum_est != 0 ~ se / sum_est * 100)) %>%
        write_csv(., here("dl_data", paste0(t, "_", u, "_", g, ".csv")))
    }
  }
}
