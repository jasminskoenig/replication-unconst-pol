rm(list=ls())


##############
# DATA
##############

vfgh_decisions <- readRDS("data/vfgh_decisions.RDS")


##############
# TABLE
##############

vfgh_decisions %>%
  filter(!is.na(normnumber)) %>%
  add_count(norm, sort = TRUE) %>% 
  select(norm, government, policyarea, pop_origin, n) %>%
  distinct(norm, .keep_all = TRUE) %>%
  slice(1:20) %>%
  kable("latex", booktabs = T) -> tab

writeLines(tab, 'results/tab_suedlaws.tex')