# tokenize and lemmatize records

library(pacman)
p_load(tidyverse, udpipe, groupdata2, future.apply)

options(future.globals.onReference = "warning")


###
# prepare data for parallel processing
###

lemma = read_csv("data/csv/full_ref.csv") %>%
  groupdata2::group(120)

lemma %>%
  select(rowname, text_c, .groups) %>%
  split(.$.groups) %>%
  walk(~(write_csv(., str_c("data/to_udpipe/", unique(.$.groups), "_psp.csv"))))


###
# udpipe set up
###

lemmatize <- function(filename) {
  
  part <- read_csv(paste0("data/to_udpipe/", filename))
  
  ud_model <- udpipe_load_model("czech-pdt-ud-2.3-181115.udpipe")
  x <- udpipe_annotate(ud_model, x = part$text_c, doc_id = part$rowname)
  
  x <- as_tibble(x) %>%
    select(doc_id, token, lemma) 
  
  x <- x %>%
    mutate(doc_id = as.numeric(doc_id)) %>%
    filter(!is.na(lemma)) %>%
    group_by(doc_id) %>%
    summarise(text_c = str_c(lemma, collapse = " "))
  
  write_csv(x, path = paste0("data/from_udpipe/", filename))
  
}

ud_model <- udpipe_load_model("czech-pdt-ud-2.3-181115.udpipe")


###
# run tokenization and lemmatization
###

lfull <- list.files(path = "data/to_udpipe/", pattern = "psp", full.names = F)

plan(multiprocess)
future_lapply(lfull, lemmatize,
              future.packages = c("tidyverse", "udpipe", "zoo"))