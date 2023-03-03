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

# share of plaintiffs
vfgh_decisions %>%
  filter(!is.na(plaintiff_category)) %>%
  filter(!is.na(decision)) %>% 
  group_by(plaintiff_category) %>%
  count() %>%
  mutate(share = n/2583) #calculate share of plaintiffs overall

# calculate and plot
vfgh_decisions %>%
  filter(plaintiff_category != "Other") %>%
  mutate(plaintiff_category = if_else(plaintiff_category == "Politician", "Abstract Review", plaintiff_category)) %>%
  group_by(pop_origin) %>%
  mutate(count = n()) %>% #count reviews by populists and non-populists
  ungroup() %>%
  group_by(pop_origin, plaintiff_category, count) %>%
  count() %>% #count frequency of plaintiffs within populist and non-populist group
  mutate(share = n/count) %>% #calculate share
  ggplot(aes(x = plaintiff_category, y = share, fill = as.factor(pop_origin))) + #plot
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c("#999999", "#333333"), 
                    name = NULL, 
                    labels = c("Non-Populist", "Populist")) +
  ylab("Share of Reviews Per Plaintiff Type") +
  xlab("") +
  ylim(0.0, 0.6)

ggsave("results/fig_lawsreviewed_byplaintiff.pdf",width=12,height=10, units = "cm", device = cairo_pdf)
ggsave("results/fig_lawsreviewed_byplaintiff.tiff",width=12,height=10, units = "cm",compression = "lzw", dpi = 400)
