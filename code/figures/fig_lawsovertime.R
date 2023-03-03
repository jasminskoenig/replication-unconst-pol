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

#count number of reviews and invalidations per year
vfgh_decisions %>%
  filter(year > 1979) %>%
  group_by(year, decision) %>%
  count() %>%
  ungroup() %>%
  complete(year, decision, fill = list(n= 0)) %>%
  group_by(year) %>%
  mutate(reviewed = sum(n)) %>% #reviews per year
  filter(decision == "sustained") -> #invalidations per year
  reviews

# aggregate laws per year
laws %>%
  filter(year > 1979) %>%
  group_by(year, populism_rough) %>%
  count() %>% 
  ungroup() %>% 
  left_join(reviews, by = "year") %>% 
  rename(legislated = n.x, invalidated = n.y) %>%
  pivot_longer(c(legislated, reviewed, invalidated), names_to = "laws", values_to = "n") ->
  laws_aggregated

# separate datasets for ggplot for legislated, invalidated and reviewed laws
laws_aggregated %>%
  filter(laws == "legislated") -> 
  legislated

laws_aggregated %>% 
  filter(laws == "invalidated") ->
  invalidated

laws_aggregated %>% 
  filter(laws == "reviewed") ->
  reviewed

# Figure 3: Legislative Activity over Time
ggplot() +
  geom_segment(aes(x=year, y = n, xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_rough), color = "black"), data = legislated)+ # laws legislated per year
  geom_segment(aes(x=year, y = n, xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_rough), color = "#666666"), data = invalidated)+ # laws from that year invalidated
  geom_segment(aes(x=year, y = n, xend=lead(year),
                   yend=lead(n),
                   lty=lead(populism_rough), color = "#CCCCCC"), data = reviewed)+ # laws from that year reviewed
  labs(linetype = "") +
  scale_linetype_discrete(na.translate=FALSE)+
  scale_color_manual(values = c("#666666", "black", "#CCCCCC"),
                     labels = c("Invalidated", "Reviewed", "Legislated"),
                     name = "")+
  xlab("Year") +
  ylab("Laws")

ggsave("results/fig_lawsovertime.pdf",width=16,height=8, units = "cm",device = cairo_pdf)
ggsave("results/fig_lawsovertime.tiff",width=16,height=8, units = "cm",compression = "lzw", dpi = 400)
