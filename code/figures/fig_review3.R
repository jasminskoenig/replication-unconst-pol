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

Governments <- read_excel("Governments.xlsx") %>%
  mutate(Regierung = str_squish(Regierung))

# Figure 11: Share of Total Laws Reviewed per Government
vfgh_decisions %>% 
  filter(year > 1979) %>%
  filter(!is.na(decision)) %>% 
  mutate(government = factor(government, levels = Governments$Regierung)) %>% 
  filter(government != "NA")  %>%
  group_by(government, decision, lawstotal) %>%
  dplyr::count() %>% 
  mutate(percentage = n/lawstotal*100) %>% 
  ggplot(aes(fill = decision, y = percentage, x = government)) + 
  geom_bar(position="stack", stat="identity") +
  xlab('') +
  ylab('Share of Total Laws Reviewed') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_flip() +
  scale_fill_manual(name = NULL, 
                    labels = c("Overruled", "Sustained"),
                    values = c("#999999", "#333333")) 

ggsave("results/fig_review3.pdf",width=18,height=10, units = "cm", device = cairo_pdf)  
ggsave("results/fig_review3.tiff",width=18,height=10, units = "cm",compression = "lzw", dpi = 400)

