rm(list = ls())

# theme
theme_gridY <- theme_ipsum_tw(axis_text_size = 12, 
                              axis_title_size = 12,
                              strip_text_size = 12,
                              grid = "Y") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"))

# set theme
theme_set(theme_gridY)

##############
# DATA
##############

vfgh_decisions <- readRDS("data/vfgh_decisions.RDS")
laws <- readRDS("data/laws.RDS")


##############
# FIGURE
##############

# count laws by populists and non-populists
laws %>%
  filter(year > 1979) %>%
  group_by(pop_origin) %>%
  count()

# df only including a norm once
vfgh_decisions_distinct <- vfgh_decisions %>%
  filter(government != "NA")  %>% 
  distinct(normnumber, year, .keep_all = TRUE) 

# plot
vfgh_decisions %>% 
  filter(!is.na(norm))  %>% 
  bind_rows(vfgh_decisions_distinct, .id = "id") %>% #bind with dataset that only includes norm once to compare attempts of review and actual reviews 
  filter(year > 1979) %>%
  mutate(populist = if_else(pop_origin == 1, "Populist", "Non-populist")) %>% 
  mutate(lawstotal = ifelse(populist == "Populist", 196, 5316)) %>% #insert count from before
  mutate(id = case_when(
    id == 1 ~ "Initiation of Review",
    id == 2 ~ "Share of Total Laws Reviewed"
  )) %>%
  group_by(populist, lawstotal, id) %>%
  dplyr::count() %>%  #count number of reviews and review attempts for populists and non-populists
  mutate(x = as.numeric(n/lawstotal)) %>% #calculate shares
  ggplot(aes(y = populist, x = x)) + 
  facet_wrap(~ id) +
  geom_bar(position= "dodge", stat="identity", width= 0.5) +
  coord_flip() +
  xlab('Share of Total Laws Under Review') +
  ylab('') +
  xlim(0,0.5)

ggsave("results/fig_lawsreviewed_bypop.pdf",width=13,height=10, units = "cm",device = cairo_pdf)
ggsave("results/fig_lawsreviewed_bypop.tiff",width=13,height=10, units = "cm",compression = "lzw", dpi = 400)
