# 2026-02-17
# JVS

library(tidyverse)
library(palmerpenguins)

head(penguins)

# make my first ggplot
ggplot(data = penguins) + 
  geom_point(aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
  geom_smooth(aes(x = flipper_length_mm, y = body_mass_g)) +
  xlab("Flipper Length (mm)") +
  ylab("Body Mass (g)") +
  ggtitle("Penguins") +
  theme_bw()

# line graph
penguins_ts = penguins %>%
  group_by(species, year) %>%
  summarize(n_penguins = n())

ggplot(data = penguins_ts) +
  geom_line(aes(x = year, y = n_penguins, color = species)) +
  theme_bw()

# univariant plotting using histogram
# data is stacked ontop of eachother
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species)) +
  theme_bw()

# data is plotted infront of eachother
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), 
                      position = "identity", alpha = 0.75, binwidth = 3) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  theme_bw()

# box plots
ggplot(data = penguins) +
  geom_boxplot(aes(y = flipper_length_mm, x = species)) +
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), width = 0.2) +
  xlab("") +
  ylab("Flipper Length (mm)") +
  theme_bw()

# bar plot
ggplot(data = penguins %>% filter(!is.na(sex))) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species) +
  theme_bw()
ggsave(filename = "figures/penguins_plot.png", 
        width = 5, height = 4, units = "in", dpi = 300)
