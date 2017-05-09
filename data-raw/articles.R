library(readr)
library(dplyr)
library(stringr)

articles <- read_csv(
    "data-raw/cybergeo.csv"
  ) %>% 
  mutate(authors = str_replace(authors, "^[[:space:]]*;", "")) %>% 
  select(id, date, title, title_en, authors, langue)
use_data( articles, overwrite = TRUE )
