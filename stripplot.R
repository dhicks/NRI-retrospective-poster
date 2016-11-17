## Timeline of projects
library(tidyverse)
library(cowplot)
library(lubridate)
library(stringr)
library(tikzDevice)

## Load the data
load('cleaned awards.Rdata')
dataf = dataf %>% group_by(project.id) %>% 
    summarize(start = min(StartDate), 
              end = max(EndDate), 
              title = first(title.clean)) %>%
    ungroup() 

## Find the middle blank in each title
midblanks = dataf$title %>% str_locate_all('[:blank:]') %>%
    sapply(function(x) x[,1]) %>% #.[156] %>%
    sapply(function (x) x[ceiling(length(x)/2)])

## Split the titles at these midpoints, and paste back together with a newline
dataf = dataf %>% 
    mutate(title.front = substr(title, 1, midblanks), 
           title.back = substr(title, midblanks + 1, nchar(title)), 
           title.format = str_c(title.front, '\n', title.back))

## Arrange in order of start time
dataf = dataf %>% arrange(start) %>%
    mutate(title.format = factor(title.format, ordered = TRUE, levels = title.format))

## Color the y-axis labels
dataf$color = rep_len(scales::brewer_pal(palette = 'Set1')(4), nrow(dataf))

## Plot
tikz(height = 34, width = 8, sanitize = TRUE, standAlone = TRUE, 
     file = 'stripplot.tex')
ggplot(dataf, aes(title.format)) + 
    geom_linerange(aes(ymin = start, ymax = end), color = dataf$color, size = 2) +
    scale_color_manual(values = dataf$color, guide = FALSE) +
    geom_hline(yintercept = as.numeric(ymd('2016-11-29'))) +
    coord_flip() + 
    xlab('') +
    scale_y_date(date_breaks = '2 year', date_labels = '%Y') +
    theme(axis.text.y = element_text(size = 6, color = dataf$color, face = 'bold'), 
          plot.margin = margin(t = 7, r = 3, l = -15, b = 7))
dev.off()
