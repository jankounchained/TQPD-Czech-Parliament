# structuring vw output

library(pacman)
p_load(tidyverse, entropy, multidplyr, parallel, groupdata2)


# VW IN
doc_id = read_lines("models/doc_id")

# load in topic distribuiton across docs
doc = read_delim("models/doc_topic.model", delim = " ",
                 col_names = FALSE,
                 col_types = cols())

# take only last pass
n_topics = ncol(doc)
n_docs = nrow(doc) / 10
doc = tail(doc, n_docs)


subset_var <- grep("s_.*?", names(.GlobalEnv), value = T)

subset_start <- sapply(subset_var, get, USE.NAMES = F) %>%
  as.numeric() %>%
  sort()

# manually +1 to subset starts from psp2013 and psp2017
doc2 <- doc %>%
  mutate(doc_id = doc_id) %>%
  group(n = c(1, 36744, 81355, 124089, 171200, 229401, 314649, 362983),
        method = "l_starts",
        starts_col = "doc_id")

doc2 %>%
  split(.$.groups) %>%
  walk(~(write_csv(., str_c("data/ntr/", unique(.$.groups), "_to.csv"))))

