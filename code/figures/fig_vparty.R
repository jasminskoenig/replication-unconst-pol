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

vparty <- vdemdata::vparty

##############
# FIGURE
##############

vparty %>%
  filter(v2pashname == "FPO") %>%
  filter(year > 1945) %>% 
  ggplot() +
  geom_line(aes(x=year, y=v2xpa_popul)) +
  geom_ribbon(aes(x=year, ymin=v2xpa_popul_codelow, ymax=v2xpa_popul_codehigh), alpha=0.5)+
  xlim(1970, 2020)+
  ylim(0,1)+
  xlab("Year") +
  ylab("V-Party Populism Score FPÖ") 

ggsave("results/fig_vparty.pdf",width=18,height=10, units = "cm", device = cairo_pdf) 
ggsave("results/fig_vparty.tiff",width=18,height=10, units = "cm",compression = "lzw", dpi = 400)
