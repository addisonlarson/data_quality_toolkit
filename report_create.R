library(knitr)
library(rmarkdown)
library(here)
library(tidyverse)
inputfile_xwalk <- read_csv(here("inputfile_xwalk.csv")) %>%
  mutate(order = case_when(inputfile_xwalk$geo == "cty" ~ "a",
                           inputfile_xwalk$geo == "puma" ~ "b",
                           inputfile_xwalk$geo == "tad" ~ "c",
                           inputfile_xwalk$geo == "tract" ~ "d",
                           inputfile_xwalk$geo == "taz" ~ "e")) %>%
  arrange(file, order)

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
