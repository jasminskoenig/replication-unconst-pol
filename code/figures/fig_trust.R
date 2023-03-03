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

trust <- read_excel("data/Vertrauensindex_Institutionen_Zeitreihe_2011-2021.xlsx")


##############
# FIGURE
##############

trust %>%
  mutate(year = year(Erhebungszeitraum)) %>%
  rename(Government = Regierung, Parliament = Parlament, Judiciary = Justiz) %>%
  pivot_longer(cols = c(Government, Parliament, Judiciary, VfGH), names_to = "Institution", values_to = "saldo") %>%
  ggplot() +
  geom_line(aes(x = year, y = saldo, linetype = Institution)) +
  ylim(-50, 50) +
  ylab("Trust Saldo") +
  xlab("Year") +
  theme(legend.position = "right") +
  scale_x_continuous(breaks= c(2011, 2012, 2016, 2019, 2021))

ggsave("results/fig_trust.pdf",width=18,height=9, units = "cm", device = cairo_pdf)  
ggsave("results/fig_trust.tiff",width=18,height=9, units = "cm",compression = "lzw", dpi = 400)
