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
  mutate(date = str_extract(date, "\\d{4}")) %>%
  filter(!is.na(decision)) %>%
  mutate(date = as.numeric(date)) %>%
  group_by(decision, date) %>%
  dplyr::count() %>% #count decisions per year
  ggplot(aes(x = date, y = n, group = as.factor(decision), fill = as.factor(decision))) + #plot
  geom_area() +
  xlab('Year') +
  ylab('Number of Laws Under Review') +
  scale_fill_manual(values=c("#999999", "#333333"), 
                    name = NULL, 
                    labels = c("Constitutional", "Invalidated")) 

ggsave("results/fig_totalcomplaints.pdf",width=18,height=9, units = "cm",device = cairo_pdf)
ggsave("results/fig_totalcomplaints.tiff",width=18,height=9, units = "cm",compression = "lzw", dpi = 400)
