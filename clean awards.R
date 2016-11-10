library(tidyverse)
library(cowplot)
library(lubridate)
library(stringr)

dataf.unfiltered = read_csv('Awards. 2016-09-21 8013 ecode.csv') %>%
    mutate(StartDate = mdy(StartDate), EndDate = mdy(EndDate))
dataf = dataf.unfiltered %>% filter(str_detect(Title, 'NRI|EAGER|RAPID'))
dataf.excluded = dataf.unfiltered %>% filter(!AwardNumber %in% dataf$AwardNumber)


## Remove metadata flags in the title
meta_flags = c('CAREER', 'CPS', 'EAGER', 'INSPIRE', 'INSPIRE Track 1',
			   'NRI', 'NRI-Large', 'NRI-Small',
			   'Collaborative Research', 'Large', 'Small', 
			   'NRI/Collaborative Research',
			   'RAPID', 'RI', 'RUI', 'Medium', 
			   'Synergy')
meta_clean = function (title.col) str_replace(title.col, 
											  str_c(meta_flags, ': ', collapse = '|'), 
											  '')
dataf = dataf %>% 
	## Run through meta_clean 3 times to catch everything
	mutate(title.clean = meta_clean(Title)) %>% 
	mutate(title.clean = meta_clean(title.clean)) %>%
	mutate(title.clean = meta_clean(title.clean)) %>%
	## Catch (Continuation) titles
	mutate(title.clean = str_replace(title.clean, ' \\(Continuation\\)', '')) %>%
	## Account for differences in capitalization and punctuation
	mutate(title.norm = {str_replace_all(title.clean, '[^[:alnum:]]', ' ') %>% tolower}) 
## Define an ID for each project
project.id = dataf %>% group_by(title.norm) %>% group_indices
dataf = dataf %>% mutate(project.id = project.id)

save(dataf, file = 'cleaned awards.Rdata')
stop()

## 165 distinct project titles
length(unique(dataf$project.id))



## ----------
## Organizations
## 91 organizations
dataf %>% .$Organization %>% unique %>% length
## The list doesn't have spurious repeats
dataf %>% group_by(Organization) %>% summarize(n = n()) %>% View

## Very sparse
ggplot(dataf, aes(project.id, Organization, fill = active)) + geom_tile() 

## Organization-project network
# org_project_net = dataf %>% 
# 	select(project.id, Organization, StartDate, EndDate) %>% 
# 	graph_from_data_frame(directed = FALSE)
# V(org_project_net)$type = ifelse(str_detect(V(org_project_net)$name, '[0-9]'), 
# 								 'project', 
# 								 'organization')
# 
# giant_comp = induced_subgraph(org_project_net, 
# 				 which(components(org_project_net)$membership == 1))
# plot(#org_project_net, 
# 	giant_comp,
# 	 vertex.label = ifelse(V(org_project_net)$type == 'project', 
# 	 					  '', 
# 	 					  V(org_project_net)$name),
# 	 vertex.label.cex = 1,
# 	 vertex.size = 3, 
# 	 edge.width = 3)

## ----------
## PIs and co-PIs
pis = dataf %>% 
	group_by(PrincipalInvestigator) %>% 
	## NB to unpact the list columns, use tidyr::unnest
	summarize(n.awards.pi = n(),
			  awards.pi = list(AwardNumber), 
			  projects.pi = list(project.id),
			  projects.pi.title = list(title.clean),
			  organization.pi = list(unique(Organization)), 
			  email = unique(PIEmailAddress), 
			  active.pi = sum(active) > 0, 
			  is.pi = TRUE) %>%
	rename(name = PrincipalInvestigator)

copis = dataf %>% 
	mutate(copi = str_split(`Co-PIName(s)`, ', ', simplify = FALSE)) %>%
	unnest(copi) %>% 
	filter(copi != PrincipalInvestigator) %>%
	group_by(copi) %>%
	summarize(n.awards.copi = n(),
			  awards.copi = list(AwardNumber), 
			  projects.copi = list(project.id),
			  projects.copi.title = list(title.clean),
			  organization.copi = list(unique(Organization)), 
			  active.copi = sum(active) > 0, 
			  is.copi = TRUE) %>%
	rename(name = copi) %>%
	filter(!is.na(name))
								   
investigators = full_join(pis, copis, by = 'name') %>% 
	group_by(name) %>%
	mutate(n.awards = sum(n.awards.pi, n.awards.copi, na.rm = TRUE),
		   awards = Map(c, awards.pi, awards.copi), 
		   projects = Map(c, projects.pi, projects.copi), 
		   projects.title = Map(c, projects.pi.title, projects.copi.title), 
		   organization = {Map(c, organization.pi, organization.copi) %>% Map(unique, .)},
		   active = active.pi | active.copi, 
		   is.pi = ifelse(!is.na(is.pi), is.pi, FALSE), 
		   is.copi = ifelse(!is.na(is.copi), is.copi, FALSE))

## Write a CSV to use to find missing emails
investigators %>% 
	mutate(organization = {Map(paste, organization, collapse = '; ') %>% 
							unlist}, 
		   search.string = paste(name, organization)) %>%
	select(name, organization, search.string, email) #%>% 
	#write.csv('to_match.csv', row.names = FALSE)

## To get a list of investigators for each project:  
# investigators %>% 
# 	mutate(organization = {Map(paste, organization, collapse = '; ') %>% unlist}) %>%
# 	unnest(projects, projects.title) %>% 
# 	rename(project.id = projects, project.title = projects.title) %>%
# 	select(project.id, project.title, name, email, organization) %>%
# 	arrange(project.id) %>%
# 	View

## Investigator-project network
# invest_net = investigators %>% select(name, projects) %>% unnest(projects) %>%
# 	graph_from_data_frame(directed = FALSE)
# 
# components(invest_net)
# 
# plot(invest_net, 
# 	 vertex.label = '', 
# 	 vertex.size = 3, 
# 	 edge.width = 3)
