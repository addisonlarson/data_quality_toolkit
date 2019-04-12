library(knitr)
library(rmarkdown)
library(here)
library(tidyverse)
inputfile_xwalk <- read_csv(here("inputfile_od_xwalk.csv")) %>%
  mutate(order = case_when(geo == "cty"   ~ "a",
                           geo == "puma"  ~ "b",
                           geo == "tad"   ~ "c",
                           geo == "tract" ~ "d",
                           geo == "taz"   ~ "e"),
         tableno = as.numeric(str_sub(file, 9, -1))) %>%
  arrange(tableid, tableno, order)
# Only keep tables available at all geos
keep_vars <- inputfile_xwalk %>%
  group_by(tableid, tableno) %>%
  summarize(available = n()) %>%
  filter(available == 5) %>%
  pull(tableid) %>%
  unique(.)
inputfile_xwalk <- inputfile_xwalk %>%
  filter(tableid %in% keep_vars)

for(n in 1:nrow(inputfile_xwalk)){
  file_id <- paste0(inputfile_xwalk$file[n], "_", inputfile_xwalk$geo[n], ".csv")
  render("report_template.Rmd",
         params = list(a = file_id,
                       b = inputfile_xwalk$variable[n],
                       c = inputfile_xwalk$tableid[n],
                       d = n),
         output_file = here("repts",
                            paste0(inputfile_xwalk$file[n], "_", inputfile_xwalk$order[n], ".pdf")))
  }
