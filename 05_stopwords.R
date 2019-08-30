# merge lemmatized subsets, filter stopwords and turn back into tidy format

library(pacman)
p_load(tidyverse, tidytext)


###
# merge back
###

ufull <- list.files(path = "data/from_udpipe/", pattern = "psp", full.names = T)

from_ud <- map_df(ufull, read_csv) %>%
  rename(rowname = doc_id) %>%
  arrange(rowname) %>%
  filter(text_c != "0")


###
# stopwords - MP names
###

poslanec <- read_delim("data/misc/poslanec.unl", 
                       delim = "|", escape_double = T,
                       col_names = c("id_poslanec", "id_osoba", "id_kraj", "id_kandidatka", 
                                     "id_obdobi", "web", "ulice", "obec", "psc", "email", 
                                     "telefon", "fax", "psp_telefon", "facebook", "foto"),
                       locale = locale(encoding = 'windows-1250'))

osoby <- read_delim("data/misc/osoby.unl", 
                    delim = "|", escape_double = T,
                    col_names = c("id_osoba", "pred", "prijmeni", "jmeno", "za",
                                  "narozeni", "pohlavi", "zmena", "umrti"),
                    locale = locale(encoding = 'windows-1250'))

obdobi_of_interest <- c("170", "171", "172")

names <- poslanec %>%
  filter(id_obdobi %in% obdobi_of_interest) %>%
  left_join(osoby, by = "id_osoba") %>%
  select(prijmeni, jmeno) %>%
  gather() %>%
  mutate(value = tolower(value))


###
# stopwords - typical
###
stopwords_cs <- 
  # stopwords-iso
  c(stopwords::stopwords("cs", source = "stopwords-iso")) %>%
  # MP names and surnames
  c(names$value) %>%
  # month names
  c("leden", "únor", "březen", "duben", "květen", 
    "červen", "červenec", "srpen", "září", "říjen", "listopad", "prosinec") %>%
  # addressing
  c("kolega", "kolegyně", 
    "poslanec", "poslankyně", 
    "pán", "pan", "paní", 
    "vážený", "vážené", "vážená",
    "ministr", "ministrině", "premiér",
    "předseda", "předsedkyně", "předsedající",
    "místopředseda", "místopředsedkyně") %>%
  unlist() %>%
  tibble(word = .)


unn <- from_ud %>%
  unnest_tokens(word, text_c, token = "words") %>%
  filter(!word %in% stopwords_cs$word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  select(rowname, word) #%>%
# group_by(rowname) %>%
# summarise(text_c = str_c(word, collapse = " "))

write_csv(unn, "data/csv/unn.csv")