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

vdem <- vdemdata::vdem

##############
# FIGURE
##############

vdem %>%
  filter(country_name == "Austria") %>%
  select(v2juhcind_osp, year, v2juhcind_osp_codehigh, v2juhcind_osp_codelow) %>% 
  filter(year > 1969) %>%
  ggplot() +
  geom_line(aes(x = year, y = v2juhcind_osp)) +
  geom_ribbon(aes(x = year, ymin = v2juhcind_osp_codelow, ymax = v2juhcind_osp_codehigh), alpha=0.5) +
  xlim(1970, 2020) +
  ylim(0,4) +
  ylab("V-Dem High Court Independence") +
  xlab("Year")

ggsave("results/fig_vdem.pdf",width=18,height=10, units = "cm", device = cairo_pdf)  
ggsave("results/fig_vdem.tiff",width=18,height=10, units = "cm",compression = "lzw", dpi = 400)
