# merge records, append metadata about MPs

library(pacman)
p_load(tidyverse, tidytext)


###
# merge
###

psp2010 <- read_csv("data/csv/psp2010_tidy.csv") %>%
  arrange(rowname)

psp2013 <- read_csv("data/csv/psp2013_tidy.csv") %>%
  arrange(rowname) %>%
  mutate(rowname = rowname + max(psp2010$rowname))

psp2017 <- read_csv("data/csv/psp2017_tidy.csv") %>%
  arrange(rowname) %>%
  mutate(rowname = rowname + max(psp2013$rowname))

full_psp <- psp2010 %>%
  full_join(psp2013) %>%
  full_join(psp2017)


###
# load MP metadata
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

organy <- read_delim("data/misc/organy.unl", 
                     delim = "|", escape_double = T,
                     col_names = c("id_organ", "organ_id_organ", "id_typ_organu", 
                                   "zkratka", "nazev_organu_cz", "nazev_organu_en", 
                                   "od_organ", "do_organ", 
                                   "priorita", "cl_organ_base"),
                     locale = locale(encoding = 'windows-1250'))


# only include psp2010, psp2013 and psp2017
obdobi_of_interest <- c("170", "171", "172")

# make a list of party affiliation
mp_list <- poslanec %>%
  filter(id_obdobi %in% obdobi_of_interest) %>%
  left_join(osoby, by = "id_osoba") %>%
  select(id_osoba, prijmeni, jmeno, id_kandidatka, id_obdobi) %>%
  left_join(organy, by = c("id_kandidatka" = "id_organ")) %>%
  mutate(speaker_name = paste(jmeno, prijmeni, sep = " "),
         speaker_name = str_remove_all(speaker_name, "[:punct:]")) %>%
  select(speaker_name, zkratka, id_obdobi) %>%
  unique()


###
# append metadata
###

# tokenized names
osoby_unn <- osoby %>%
  mutate(speaker = paste(jmeno, prijmeni, sep = " ")) %>%
  select(speaker) %>%
  unnest_tokens(word_name, speaker, token = "words", to_lower = F)

# extract subset of titles to be used as stopwords
sp_stopwords <- enframe(unique(fli$speaker)) %>%
  select(name = value) %>%
  unnest_tokens(word_full, name, token = "words", to_lower = F) %>%
  filter(!word_full %in% osoby_unn$word_name) %>%
  unique() %>%
  filter(!is.na(word_full))

# remove stopwords from transcripts
sp_name <- fli %>%
  select(rowname, speaker) %>%
  unnest_tokens(word, speaker, token = "words", to_lower = F) %>%
  filter(!word %in% sp_stopwords$word_full) %>%
  group_by(rowname) %>%
  summarise(speaker_name = str_c(word, collapse = " "))


###
# export
###

# make the final merge
fli2 <- fli %>%
  # replace speaker name by clean speaker name
  full_join(sp_name, by = "rowname") %>%
  ungroup() %>%
  # add partisanship
  full_join(mp_list, by = c("speaker_name", "id_obdobi"))

write_csv(fli2, "data/csv/full_ref.csv")