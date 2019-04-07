# for CTPP you're going to have to dl and save all input data
# simulate that here
library(here)
library(tidyverse)
library(sf)
options(stringsAsFactors = FALSE)
g <- "tad"

tabs <- read_csv(here("inputfile_xwalk.csv")) %>%
  filter(geo == g) %>%
  select(tableid) %>%
  distinct(.) %>%
  pull(.)

for (t in tabs){
  dat <- st_read(here("raw", paste0(g, "/", t, ".shp"))) %>%
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
