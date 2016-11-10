## Use principal components on high-tf-idf terms and k-means clustering to cluster abstracts
## This doesn't work: need many, many components to capture a substantial amount of the variance in the DTM

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

## Identify the terms of interest
terms = token_counts %>% 
    bind_tf_idf(token, project.id, n) %>%
    top_n(180, wt = tf_idf) %>%
    .$token %>%
    unique

dtm = token_counts %>% 
    filter(token %in% terms) %>%
    dcast(project.id ~ token)
dtm[is.na(dtm)] = 0

project.id = dtm[,1]
dtm = dtm[,-1]
rownames(dtm) = project.id
decomp = prcomp(dtm)

screeplot(decomp)
cumsum((decomp$sdev)^2) / sum(decomp$sdev^2)

rotated_dtm = cbind(project.id, decomp$x[,]) %>% as_tibble
fit = kmeans(rotated_dtm, 3)
cluster::clusplot(rotated_dtm, fit$cluster)

