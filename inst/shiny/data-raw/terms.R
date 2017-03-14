library("dplyr")

terms <- read.table( "data-raw/terms.csv",
  sep = ";", quote = "", comment.char = "", header = TRUE, stringsAsFactors = FALSE
  ) %>%
  tbl_df() %>%
  mutate(
    article_id = id,
    id = row_number()
  ) %>%
  select(id, article_id, term, count)

save( terms, file = "data/terms.rda" )

sentences <- read.table( "data-raw/sentences.csv",
  sep = "|", quote = "", comment.char = "", header = TRUE, stringsAsFactors=FALSE
  ) %>%
  tbl_df() %>%
  mutate(
    article_id = id,
    id = row_number()
  )
save( sentences, file = "data/sentences.rda")

cybergeo <- read.table(
  "data-raw/cybergeo.csv",
  sep = ",", quote = "\"", comment.char = "", header = TRUE ) %>%
  tbl_df() %>%
  rename(titre = title_en, auteurs = authors) %>%
  mutate(citation = paste(sep = ". ", auteurs, substr(date,1,4), titre)) %>%
  select(id, date, citation, langue)
save( cybergeo, file = "data/cybergeo.rda" )
