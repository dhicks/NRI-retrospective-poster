library(tidyverse)
library(cowplot)
library(lubridate)
library(stringr)
library(tikzDevice)

load('cleaned awards.Rdata')

## Timeline of projects
dataf = dataf %>% group_by(project.id) %>% 
    summarize(start = min(StartDate), 
              end = max(EndDate), 
              title = first(title.clean)) %>%
    ungroup() %>%
    arrange(start) %>%
    mutate(title = factor(title, ordered = TRUE, levels = title))
    
dataf$color = rep_len(scales::brewer_pal(palette = 'Set1')(4), nrow(dataf))

tikz(height = 25, width = 12, sanitize = TRUE, standAlone = TRUE, 
     file = 'stripplot.tex')
ggplot(dataf, aes(title)) + 
    geom_linerange(aes(ymin = start, ymax = end), color = dataf$color) +
    scale_color_manual(values = dataf$color, guide = FALSE) +
    geom_hline(yintercept = as.numeric(ymd('2016-11-29'))) +
    coord_flip() + 
    xlab('') +
    scale_y_date(date_breaks = '1 year', date_labels = '%Y') +
    theme(axis.text.y = element_text(size = 6, color = dataf$color))
dev.off()
