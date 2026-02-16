# 2026-02-12
# JVS

install.packages("tidyverse")
install.packages("palmerpenguins")

library(tidyverse)
library(palmerpenguins)

head(penguins)
summary(penguins)
dim(penguins)

# filter
gentoo = filter(penguins, species == "Gentoo")
summary(gentoo)

gentoo_ladies = filter(penguins, species == "Gentoo", sex == "female")
summary(gentoo_ladies)

# introduce the pipe
gentoo_ladies = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female")

gentoo_ladies_body_mass_g = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female") %>%
  summarize(gentoo_ladies_body_mass_g = mean(body_mass_g))

gentoo_gents_body_mass_g = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "male") %>%
  summarize(gentoo_gents_body_mass_g = mean(body_mass_g))

# use group_by()
species_mean_mass = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = T),
            sd_body_mass_g = sd(body_mass_g, na.rm = T),
            n_penguins = n())

write_csv(species_mean_mass, file = "data/processed/penguin_bodies")
temp = read_csv(file = "data/processed/penguin_bodies")
temp

# mutate
penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) # 0.0022 lb/g
head(penguins_for_america)

penguins %>% distinct(species)

temp = penguins %>%
  select(species, sex, body_mass_g)
head(temp)

temp = penguins %>%
  select(-body_mass_g) # - removes column
head(temp)

temp = penguins %>%
  arrange(body_mass_g)
head(temp)
tail(temp)
