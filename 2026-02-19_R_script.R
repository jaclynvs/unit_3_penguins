# 2026-02-19
# JVS

library(tidyverse)
library(palmerpenguins)
install.packages("rstatix")
library(rstatix)

head(penguins)
gentoo = penguins %>%
  filter(species == "Gentoo") %>%
  droplevels()
head(gentoo)
summary(gentoo)
dim(gentoo)
mean(gentoo$body_mass_g, na.rm = T)
sd(gentoo$body_mass_g, na.rm = T)

ggplot() +
  geom_histogram(data = gentoo, aes(x = body_mass_g))

# QQ plot
ggplot() +
  stat_qq(aes(sample = body_mass_g), data = gentoo)

gentoo_body_mass_g_symonds = 5500 # from Symonds and Tattersall 2010

my_t_test = t.test(gentoo$body_mass_g, mu = gentoo_body_mass_g_symonds)
class(my_t_test)
summary(my_t_test)
my_t_test$p.value

# try a pipe friendly t test from the rstatix package
my_t_test2 = gentoo %>%
  t_test(body_mass_g ~ 1, mu = gentoo_body_mass_g_symonds)
my_t_test2

# independent sample t test
data_for_t_test = penguins %>%
  dplyr::filter(species %in% c("Gentoo", "Adelie"), !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()

summary(data_for_t_test)

ggplot(data = data_for_t_test) +
  geom_histogram(aes(x = body_mass_g)) +
  facet_wrap(~species, scales = "free")

ggplot(data = data_for_t_test) +
  stat_qq(aes(sample = body_mass_g)) +
  facet_wrap(~species, scales = "free")

# check equality of variance
# if true, use students t test
# if false, use welch's

data_for_t_test %>% levene_test(body_mass_g ~ species) # if p-value is < 0.05, variances are NOT equal, use welch's

my_ind_t_test = t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal = T)
class(my_ind_t_test)

# using rstatix
# using welch's
data_for_t_test %>%
  t_test(body_mass_g ~ species)

head(data_for_t_test)
dim(data_for_t_test)
summary(data_for_t_test)

# using students
data_for_t_test %>%
  t_test(body_mass_g ~ species, var.equal = T)

#########################
# correlations

ggplot(data = gentoo) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm))

ggplot(data = gentoo) +
  stat_qq(aes(sample = bill_length_mm))

cor(x = gentoo$bill_depth_mm, y = gentoo$bill_length_mm, use = "complete.obs")

cor.test(x = gentoo$bill_length_mm, y = gentoo$bill_depth_mm, use = "complete.obs")

gentoo %>%
  cor_test(bill_depth_mm, bill_length_mm)

install.packages("GGally")
library(GGally)

gentoo %>%
  select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) %>%
  GGally::ggpairs()

head(gentoo)
gentoo = gentoo %>%
  filter(!is.na(body_mass_g))
cor(gentoo[,3:6])

penguins %>%
  select(species, where(is.numeric)) %>%
  GGally::ggpairs(aes(color = species))
