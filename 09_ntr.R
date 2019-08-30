# calculating novelty, transience, resonance

library(pacman)
p_load(tidyverse, entropy, future.apply)


###
# functions
###

vdiff <- function(x,n,fun) sapply(n, function(i) fun(x,i))
vlag  <- function(x,n) vdiff(x,0:n,dplyr::lag)
vlead <- function(x,n) vdiff(x,0:n,dplyr::lead)

novelty <- function(mat, w) {
  vlag(1:nrow(mat), w) %>%          # produce the lags (same shape as document)
    apply(1, function(idx) {        # then for each row (document)
      lapply(idx[2:length(idx)], function(i) { #for each lag
        if (is.na(i)) return(NA)             #check if it's na (we're at beginning / end of data)
        ## calculate surprise from past to present
        KL.plugin(mat[i,], mat[idx[1],], unit = "log2")
      }) %>%
        unlist() %>%
        mean()
    })}


transience <- function(mat, w) {
  vlead(1:nrow(mat), w) %>%         # produce the leads (same shape as document)
    apply(1, function(idx) {        # then for each row (document)
      lapply(idx[2:length(idx)], function(i) { #for each lead
        if (is.na(i)) return(NA)             #check if it's na (we're at beginning / end of data)
        ## calculate surprise from present to future
        KL.plugin(mat[idx[1],], mat[i,], unit = "log2")
      }) %>%
        unlist() %>%
        mean()
    })}


z <- function(d) (d - mean(d)) / sd(d)

calculate_ntr <- function(doc_subset_path, w) {
  
  import = read_csv(doc_subset_path)
  
  id = import$doc_id
  
  doc = import %>%
    select(-doc_id, -.groups) %>%
    as.matrix(., ncol = 100)
  
  ntr_output = tibble(doc_id = id) %>%
    mutate(
      novelty = novelty(doc, w),
      transience = transience(doc, w),
      resonance = novelty - transience
    ) %>%
    filter(complete.cases(.)) %>%
    mutate(z_novelty = z(novelty),
           z_transience = z(transience),
           z_resonance = z(resonance)
    )
  
  res_nov_model = lm(z_resonance ~ z_novelty, data = ntr_output)
  ntr_output$delta_R = ntr_output$z_resonance - predict(res_nov_model)
  
  ntr_output %>%
    write_csv(paste0("O:/ARTS_Cognitive-Science/2017-Cognitive Science/EM4/", 
                     str_extract(doc_subset_path, "\\d"), "_from_matrix.csv"))
  
}


####
# run
####

sub_path <- list.files(path = ".", pattern = "_to", full.names = T)

plan(multiprocess)
future_lapply(sub_path, FUN = calculate_ntr, w = 27)