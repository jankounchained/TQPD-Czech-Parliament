# scrapes transcripts of speeches and relevant metadate from http://www.psp.cz/eknih/ 

library(pacman)
p_load(tidyverse, rvest)


###
# part 1: scraping links to dowload records from
###

# read links from Parliamentary Digital Library
home <- read_html("http://www.psp.cz/eknih/index.htm") %>%
  html_nodes("a") %>%
  html_attrs() %>%
  as.character() %>%
  as_tibble()

# filter links containing number + "ps" (chamber of deputies seasons)
ps_index <- home %>%
  filter(str_detect(value, "\\dps")) %>%
  mutate(value = paste0("http://www.psp.cz", value))

# parliamentary seasons outside of interest
old_psp <- "2006|2002|1998|1996|1993"

# links to transcript folders
zip_links <- ps_index %>%
  filter(!str_detect(value, old_psp)) %>%
  mutate(value = str_remove(value, "/index.htm"),
         value = paste0(value, "/stenprot/zip/"))


get_link_zf <- function(html_link) {
  
  link <- read_html(html_link)
  
  on_page <- link %>%
    html_nodes("a") %>%
    html_attrs() %>%
    as.character() %>%
    as_tibble() %>%
    filter(str_detect(value, "zip")) %>%
    # 2010 uses a different link system
    mutate(value = case_when(
      str_detect(value, "2010ps") ~ paste0("http://www.psp.cz", value),
      TRUE ~ paste0(html_link, value))) %>%
    filter(!str_detect(value, "href ="))
  
  return(on_page)
}

all_zip_links <- map_df(zip_links$value, get_link_zf)


###
# part 2: download records
###

for (link in all_zip_links$value) {
  
  # random pauses between downloads
  timeout = sample(5, 1) + exp(rnorm(1))
  
  download.file(url = link, 
                destfile = paste0("data/",
                                  substr(link, 25, 30),
                                  "_",
                                  substr(link, 45, 56)))
  
  Sys.sleep(timeout)
  
}


###
# part 3: unzip and sort
###

f2010 <- list.files("data/zipfiles/", pattern = "2010.*?\\.zip", full.names = T)
lapply(f2010, unzip, exdir = "C:\\Users\\jan\\Documents\\_git\\CZpar\\data\\2010ps")

f2013 <- list.files("data/zipfiles/", pattern = "2013.*?\\.zip", full.names = T)
lapply(f2013, unzip, exdir = "C:\\Users\\jan\\Documents\\_git\\CZpar\\data\\2013ps")

f2017 <- list.files("data/zipfiles/", pattern = "2017.*?\\.zip", full.names = T)
lapply(f2017, unzip, exdir = "C:\\Users\\jan\\Documents\\_git\\CZpar\\data\\2017ps")