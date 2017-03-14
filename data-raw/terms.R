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
