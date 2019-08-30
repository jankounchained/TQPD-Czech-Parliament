# prepare lemmatized transcripts for LDA & create yearly subsets

library(pacman)
p_load(tidyverse, entropy, multidplyr, parallel, groupdata2)


###
# prepare for LDA
###

unn <- read_csv("data/csv/unn.csv")

out <- count(unn, rowname, word) %>%
  ungroup() %>%
  mutate(hash = as.integer(as.factor(word))) %>%
  arrange(as.numeric(rowname, desc(n))) %>%
  unite("freq", hash, n, sep = ":", remove = FALSE)


out %>%
  distinct(hash, word) %>%
  write_csv("data/vw/hash_full.csv")


out2 <- out %>%
  split(out$rowname) %>%
  map_chr(~str_c(.$freq, collapse = " ")) %>%
  str_c("| ", .)


write_lines(out2, "data/vw/psp_full_lda.vw")


# doc_id
out3 = unite(out, "freq", hash, n, sep = ":", remove = FALSE)

out3$rowname %>%
  unique() %>%
  write_lines("models/doc_id")


###
# create yearly subsets
###

subset_year_new <- function(year, sub_range) {
  
  match <- str_c(
    paste0(
      as.character(year), "...., ", sub_range, "\\. ", "schůze"), 
    collapse = "|")
  
  return(match)
  
}

get_subset_start_pos <- function(year, subrange) {
  
  sp <- fli %>%
    filter(str_detect(doc, subset_year_new(year, subrange))) %>%
    select(rowname) %>%
    head(1) %>%
    as.character()
  
  return(sp)
}

fli <- read_csv("data/csv/full_lemma_in.csv")

# 2010: 1-19, 20-40, 41-59
# 2013: 1-22, 23-38, 39-58
# 2017: 1-23, 24-29

ex101 <- c(paste0(0, 0, 1:9, "schuz"), paste0(0, 10:19, "schuz"))
ex101 <- str_c(ex101, collapse = "|")
s_101 <- fli %>%
  filter(str_detect(doc, ex101)) %>%
  select(rowname) %>%
  head(1) %>%
  as.character()

ex102 <- str_c(paste0(0, 20:40, "schuz"), collapse = "|")
s_102 <- fli %>%
  filter(str_detect(doc, ex102)) %>%
  select(rowname) %>%
  head(1) %>%
  as.character()

ex103 <- str_c(paste0(0, 41:59, "schuz"), collapse = "|")
s_103 <- fli %>%
  filter(str_detect(doc, ex103)) %>%
  select(rowname) %>%
  head(1) %>%
  as.character()

rm(ex101, ex102, ex103)

# psp2013
s_131 <- get_subset_start_pos("2013", 1:22)
s_132 <- get_subset_start_pos("2013", 23:38)
s_133 <- get_subset_start_pos("2013", 38:58)


# psp2017
s_171 <- get_subset_start_pos("2017", 1:23)
s_172 <- get_subset_start_pos("2017", 24:29)


# get all variables and write_csv
subsets <- grep("s_.*?", names(.GlobalEnv), value = T)

for (i in 1:8) {
   
   write_csv(get(subsets[i]), path = paste0("data/ntr/", subsets[i], ".csv"))
   
 }