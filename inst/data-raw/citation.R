library(dplyr)
library(RSQLite)
library(stringr)
library(tidyr)

# reorganise citation data
tb_kws <- tbl( src_sqlite( system.file("sqlite", "CitationKeywords.sqlite3", package = "corpusminer") ), "keywords" )
tb_cit <- tbl( src_sqlite( system.file("sqlite", "CitationNetwork.sqlite3", package = "corpusminer") ), "edges" )

# first keywords, we do the str_split now and use 
# a list column for the keywords

get_edges <- function(tb_cit){
  select( tb_cit, from, to ) %>% 
    collect(n = Inf) %>% 
    mutate_at( vars(from, to), as.numeric )
}

get_data <- function(tb_cit, tb_kws) {
  from <- tb_cit %>% 
    select(from, fromtitle, fromyear, fromcyb) %>% 
    collect(n=Inf) %>% 
    rename(id = from, title = fromtitle, year = fromyear, cyb = fromcyb) %>% 
    distinct( id, .keep_all = TRUE)
  
  to <- tb_cit %>% 
    select(to, totitle, toyear, tocyb) %>% 
    collect(n=Inf) %>% 
    rename( id = to, title = totitle, year = toyear, cyb = tocyb) %>% 
    distinct( id, .keep_all = TRUE)
  
  keywords <- collect(tb_kws, n = Inf) %>% 
    mutate( id = as.numeric(id), keyword = str_split(keywords, ";") ) %>% 
    select( id, keyword)
  
  data <- bind_rows(from, to) %>% 
    distinct( id, .keep_all = TRUE) %>%
    mutate( cyb = cyb == 1, id = as.numeric(id) ) %>% 
    right_join(keywords, by = "id") 

  data
}

citation_edges <- get_edges(tb_cit)
citation_data <- get_data(tb_cit, tb_kws)

use_data( citation_edges, overwrite = TRUE )
use_data( citation_data, overwrite = TRUE )
