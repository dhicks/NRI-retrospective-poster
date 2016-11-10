library(tidyverse)
library(cowplot)
library(lubridate)
library(stringr)

load('cleaned awards.Rdata')

## Timeline of projects
dataf %>% group_by(project.id) %>% 
    summarize(start = min(StartDate), 
              end = max(EndDate), 
              title = first(title.clean)) %>% 
              {ggplot(., aes(reorder(title, start))) + 
                      geom_linerange(aes(ymin = start, ymax = end)) +
                      geom_hline(yintercept = as.numeric(ymd('2016-11-29'))) +
                      coord_flip() + xlab('') +
                      scale_y_date(date_breaks = '1 year', date_labels = '%Y') +
                      theme(axis.text.y = element_text(size = 3))}