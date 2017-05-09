library(readr)
library(dplyr)
library(stringr)

articles <- read_csv(
    "data-raw/cybergeo.csv"
  ) %>% 
  mutate(authors = str_replace(authors, "^[[:space:]]*;", "")) %>% 
  select(id, date, title, title_en, authors, langue)
use_data( articles, overwrite = TRUE )

sentences <- read_delim( "data-raw/sentences.csv", delim = "|", col_types = "ic") %>% 
  rename( article_id = id) %>% 
  mutate( id = row_number() )
use_data( sentences, overwrite = TRUE)
