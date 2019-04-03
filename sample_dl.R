# for CTPP you're going to have to dl and save all input data
# simulate that here
library(here)
library(tidyverse)
library(tidycensus)
library(sf)
options(stringsAsFactors = FALSE)

# Will be easy to iterate by dataset ID

# A101100 - Total Population
dat <- st_read(here("raw", "tract", "A101100.shp")) %>%
  st_set_geometry(NULL)
detector <- ncol(dat) - 4 # Tells us how many additional columns we need to plan for -- some datasets are very wide
iterator <- detector / 2

for (u in seq(from = 5,
              by = 2,
              length.out = iterator)) {
  temp_dat <- dat %>% select(geoid, u, u + 1)
  names(temp_dat) <- c("GEOID", "sum_est", "sum_moe")
  temp_dat <- temp_dat %>%
    mutate(se = sum_moe / 1.645,
           cv = case_when(sum_est == 0 ~ 100,
                          sum_est != 0 ~ se / sum_est * 100)) %>%
    write_csv(., here("dl_data", paste0("A101100",
                                        "_",
                                        u,
                                        "_tract.csv")))
}

# A101103 - Hispanic or Latino Origin
dat <- st_read(here("raw", "tract", "A101103.shp")) %>%
  st_set_geometry(NULL)
detector <- ncol(dat) - 4 # Tells us how many additional columns we need to plan for -- some datasets are very wide
iterator <- detector / 2

for (u in seq(from = 5,
              by = 2,
              length.out = iterator)) {
  temp_dat <- dat %>% select(geoid, u, u + 1)
  names(temp_dat) <- c("GEOID", "sum_est", "sum_moe")
  temp_dat <- temp_dat %>%
    mutate(se = sum_moe / 1.645,
           cv = case_when(sum_est == 0 ~ 100,
                          sum_est != 0 ~ se / sum_est * 100)) %>%
    write_csv(., here("dl_data", paste0("A101103",
                                        "_",
                                        u,
                                        "_tract.csv")))
}

