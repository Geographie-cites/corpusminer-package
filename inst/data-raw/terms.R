library("dplyr")
library("readr")
library("tidytext")
library("tm")

blacklist <- tolower(unique( c(
  readLines( "https://raw.githubusercontent.com/stopwords-iso/stopwords-fr/master/stopwords-fr.txt" ), 
  stopwords("fr"), stopwords("en"), stopwords_fr, 
  stop_words$word, 
  "al", "el", "los", "pro", "del", "con", "est-à-dire", "una"
)))

terms <- read_csv2( system.file( "data-raw/terms.csv", package = "corpusminer"), col_types = "ici" ) %>% 
  rename(article_id = id ) %>% 
  mutate( id = row_number() ) %>% 
  select(id, article_id, term, count) %>% 
  filter(  ! tolower(term) %in% blacklist )

use_data(terms, overwrite = TRUE)
