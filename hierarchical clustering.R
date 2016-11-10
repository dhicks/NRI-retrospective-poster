## Use hierarchical clustering (dendrograms) to try to cluster abstracts
## This doesn't work: projects are too far apart to form meaningful clusters

library(tidyverse)
library(cowplot)
library(stringr)
library(reshape2)
library(tidytext)

## Load the data
load('cleaned awards.Rdata')
dataf = dataf %>% 
    transmute(project.id, 
              ## Handle some encoding errors
              abstract = stringi::stri_enc_toutf8(Abstract, 
                                                  validate = TRUE)) %>%
    ## Select the longest abstract for each project
    filter(!duplicated(.)) %>%
    mutate(length = str_length(abstract)) %>%
    group_by(project.id) %>%
    slice(which.min(length)) %>%
    ungroup() %>%
    select(-length)

## Count tokens across project abstracts
token_counts = dataf %>%
    unnest_tokens(token, abstract) %>% 
    group_by(project.id, token) %>%
    summarize(n = n()) %>%
    ## Remove stopwords
    anti_join(stop_words, by = c('token' = 'word')) %>%
    ## Remove numbers
    filter(!str_detect(token, '^[0-9]')) %>%
    ungroup()

dtm = token_counts %>% dcast(project.id ~ token)
dtm[is.na(dtm)] = 0

clusters = hclust(dist(dtm[,-1], method = 'binary'))
plot(clusters)
