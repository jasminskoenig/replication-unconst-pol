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

##############
# FIGURE
##############

vfgh_decisions %>%
  filter(!is.na(plaintiff_category)) %>%
  filter(!is.na(decision)) %>%
  filter(type == "erkenntnis") %>%
  filter(plaintiff_category != "Other") %>%
  mutate(plaintiff_category = ifelse(plaintiff_category == "Politician", "Abstract Review", plaintiff_category)) %>%
  mutate(plaintiff_category = ifelse(plaintiff_category == "Person", "Individual", plaintiff_category)) %>%
  mutate(date = year(date_2)) %>%
  group_by(date, plaintiff_category) %>%
  count() %>%
  ungroup() %>%
  complete(date, plaintiff_category,
           fill = list(n = 0)) %>%
  group_by(date) %>%
  mutate(n_date = sum(n))%>%
  mutate(share = n/n_date) %>% 
  ggplot() +
  geom_line(aes(x=date, y=share, linetype = plaintiff_category)) +
  scale_linetype_manual(values=c("dotted", "longdash", "solid")) +
  ylab("Share") +
  xlab("Decision Year") +
  theme(legend.title=element_blank())

ggsave("results/fig_review2.pdf",width=18,height=10, units = "cm", device = cairo_pdf)
ggsave("results/fig_review2.tiff",width=18,height=10, units = "cm",compression = "lzw", dpi = 400)
