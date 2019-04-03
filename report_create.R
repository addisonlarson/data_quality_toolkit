library(knitr)
library(rmarkdown)
library(here)
library(tidyverse)
inputfile_xwalk <- read_csv(here("inputfile_xwalk.csv"))

for(n in 1:nrow(inputfile_xwalk)){
  file_id <- paste0(inputfile_xwalk$file[n], "_", inputfile_xwalk$geo[n], ".csv")
  render("report_template.Rmd",
         params = list(a = file_id,
                       b = inputfile_xwalk$variable[n],
                       c = inputfile_xwalk$tableid[n]),
         output_file = here("repts",
                            paste0(inputfile_xwalk$file[n], "_", inputfile_xwalk$geo[n], ".pdf")))
  }

# Note that the dl_geo file will need to be updated with each geography type
