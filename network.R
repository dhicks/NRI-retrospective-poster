library(tidyverse)
library(igraph)
library(stringr)
library(tidytext)
library(tikzDevice)

load('cleaned awards.Rdata')
dataf = dataf %>% select(pi = PrincipalInvestigator, 
                         copis = `Co-PIName(s)`,
                         organization = Organization, 
                         title = title.clean, 
                         project.id)

## ----------
## Network of projects and PIs
prj_pi_net = dataf %>% select(project.id, pi) %>% 
    filter(complete.cases(.), !duplicated(.)) %>%
    graph_from_data_frame(directed = FALSE)
## Network of projects and CoPIs
copi_df = dataf %>% 
    select(copis, project.id) %>%
    ## Use unnest_tokens to split out CoPIs
    unnest_tokens(copi, copis, token = 'regex', pattern = ', ', to_lower = FALSE) %>%
    ## Remove many NAs
    filter(complete.cases(.), !duplicated(.))
## Turn into a graph
prj_copi_net =  graph_from_data_frame(copi_df, directed = FALSE)
## Network of PIs and organizations
pi_org_net = dataf %>% select(pi, organization) %>%
    filter(complete.cases(.), !duplicated(.)) %>%
    graph_from_data_frame(directed = FALSE)

## ----------
## Big combined network
combined_net = prj_pi_net + prj_copi_net + pi_org_net

n_projects = length(unique(dataf$project.id))
n_individuals = c(dataf$pi, copi_df$copi) %>% unique %>% length
n_organizations = length(unique(dataf$organization))
V(combined_net)$type = c(rep('project', n_projects),
                         rep('individual', n_individuals),
                         rep('organization', n_organizations))
type_colors = c('red', 'blue', 'green')
names(type_colors) = c('project', 'individual', 'organization')
type_shapes = c('square', 'circle', 'circle')
names(type_shapes) = c('project', 'individual', 'organization')

combined_net_gc = combined_net %>%
    induced_subgraph(components(combined_net)$membership == 1)

combined_net_gc %>%
plot(layout = layout_with_fr(.),
     vertex.label = ifelse(V(.)$type == 'organization',
                           {V(.)$name %>% str_replace_all('([ \\-][^ \\-]*)[ \\-]', '\\1\n')},
                           ''),
     vertex.color = type_colors[V(.)$type],
     vertex.shape = type_shapes[V(.)$type],
     vertex.frame.color = NA,
     vertex.label.cex = 1,
     vertex.label.color = 'black',
     vertex.size = 3,
     edge.width = 3)

## ----------
## Individuals, linked by organization and project
indiv_by_prj_net = combined_net %>% 
    induced_subgraph(which(V(.)$type != 'organization')) %>%
    bipartite_projection(types = V(.)$type == 'individual') %>%
    .$proj2
indiv_by_org_net = combined_net %>%
    induced_subgraph(which(V(.)$type != 'project')) %>%
    bipartite_projection(types = V(.)$type == 'individual') %>%
    .$proj2

individ_net = indiv_by_prj_net + indiv_by_org_net

## Color edges by type:
## red = project connection
## blue = organization connection
E(individ_net)$type = ifelse(E(individ_net) %in% E(indiv_by_prj_net), 
                             'project', 
                             'organization')
E(individ_net)$color = ifelse(E(individ_net)$type == 'project', 'red', 'blue')

components(individ_net)$csize %>% table

## Plot the whole net, and the largest connected component
plot(individ_net,
     vertex.label = '',
     vertex.size = 3,
     edge.width = 3)

tikz(standAlone = TRUE, file = 'individuals.tex')
par(mar = c(0,0,0,0))
individ_net %>%
    induced_subgraph(components(.)$membership == 1) %>%
    plot(vertex.label = '',
         vertex.size = 3,
         edge.width = 3)
dev.off()

## Centrality
# indiv_centrality = betweenness(individ_net)
# dataf = dataf %>% 
#     mutate(pi_centrality = indiv_centrality[dataf$pi])
# dataf %>% select(pi, organization, pi_centrality) %>%
#     filter(!duplicated(.)) %>% View

## ----------
## Network of organizations
org_net = dataf %>% 
    select(organization, project.id) %>%
    graph_from_data_frame(directed = FALSE) %>%
    set_vertex_attr('type', value = V(.)$name %in% dataf$organization) %>%
    bipartite_projection() %>%
    .$proj2

components(org_net)$csize %>% table

tikz(standAlone = TRUE, sanitize = TRUE, file = 'organizations.tex')
par(mar = c(0,0,0,0))
org_net %>%
    induced_subgraph(components(.)$membership == 1) %>%
    plot(
        layout = layout_with_kk(.), #layout_with_fr(.),
        vertex.label = {V(.)$name %>% str_replace_all('([ \\-][^ \\-]*)[ \\-]', '\\1\n')},
        vertex.label.cex = .7,
        #vertex.size = 50 * betweenness(., normalized = TRUE),
        vertex.size = 5,
        vertex.label.color = 'black',
        vertex.color = 'light blue',
        vertex.frame.color = NA,
        edge.width = 3, 
        edge.color = 'grey80')
dev.off()

# bc = betweenness(org_net)
# ec = eigen_centrality(org_net) %>% .$vector
# deg = degree(org_net)
# qplot(deg, bc) + geom_smooth(method = 'lm')

