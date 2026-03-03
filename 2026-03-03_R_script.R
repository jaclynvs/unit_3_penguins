# 2026-03-03
# JVS

library(tidyverse)
library(palmerpenguins)
library(GGally)

head(penguins)
summary(penguins)

ggpairs(data = penguins %>% select(where(is.numeric)))
ggpairs(data = penguins %>% select(bill_depth_mm, bill_length_mm))

# build a bad linear model
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data = penguins)
summary(lm_1)

str(lm_1)
lm_1$coefficients
class(lm_1)

ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

plot(lm_1)

# try just 1 species

gentoo = penguins %>%
  filter(species == "Gentoo",
          !is.na(bill_length_mm))
head(gentoo)
summary(gentoo)

ggpairs(data = gentoo %>% select(bill_length_mm, bill_depth_mm))

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
summary(lm_2)
plot(lm_2)

ggplot(data = gentoo, aes(y = bill_depth_mm, x = bill_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# quickly plot lm for all 3 species

ggplot(data = penguins) +
  geom_point(aes(x = bill_length_mm, bill_depth_mm, color = species)) +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm, color = species), method = "lm") +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm), color = "black", method = "lm")

# exercise 5.1
lm_3 = lm(bill_depth_mm ~ flipper_length_mm, data = gentoo)
summary(lm_3)
plot(lm_3)

ggplot(data = gentoo, aes(y = bill_depth_mm, x = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()
