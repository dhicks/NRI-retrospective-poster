library(tidyverse)
library(tidytext)
library(stringr)
library(igraph)

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
	slice(which.max(length)) %>%
	ungroup() %>%
	select(-length)

term_project_df = dataf %>% 
	unnest_tokens(token, abstract, format = 'html') %>%
	anti_join(stop_words, by = c('token' = 'word')) %>%
	group_by(project.id, token) %>%
	summarize(n = n()) %>%
	ungroup() %>% 
	bind_tf_idf(token, project.id, n)
# ecdf(term_project_df$idf) %>% plot
# term_project_df %>%
# 	filter(idf > 5) %>%
# 	.$token %>%
# 	unique %>% length

term_project_df_f = term_project_df %>% 
	filter(idf > 4.25)

term_project_net = graph_from_data_frame(term_project_df_f, directed = FALSE)
V(term_project_net)$type = str_detect(V(term_project_net)$name, '^[0-9]')
E(term_project_net)$weight = 100 * E(term_project_net)$tf_idf

clusters_start = proc.time()
clusters = cluster_walktrap(term_project_net)
clusters_end = proc.time()
clusters_end - clusters_start
clusters

# dendPlot(clusters)

V(term_project_net)$cluster = membership(clusters)

proj_net = bipartite_projection(term_project_net)$proj2

plot(term_project_net,
	 vertex.color = V(proj_net)$cluster,
	 vertex.size = 3,
	 vertex.label = '')
