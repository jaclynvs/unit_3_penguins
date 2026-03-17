# 2026-03-05
# JVS

library(tidyverse)
library(palmerpenguins)

summary(penguins)
unique(penguins$year)
penguins %>% distinct(year)

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_length_mm))

# build my first multiple regression model

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm_3)
summary(lm_3)
anova(lm_3)

lm_3_table = broom::tidy(lm_3, conf.int = T, conf.level = 0.95)
write_csv(lm_3_table, file = "figures/lm_3_table.csv")

install.packages("ggiraph")
install.packages("ggiraphExtra")
library(ggiraph)
library(ggiraphExtra)

ggPredict(lm_3, se = T, interactive = T)

lm_3_predictions = predict(lm_3, interval = "confidence", level = 0.95)
head(lm_3_predictions)
dim(lm_3_predictions)
dim(penguins_lm_3)

penguins_lm_3_predictions = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predictions)
dim(penguins_lm_3_predictions)

ggplot(data = penguins_lm_3_predictions, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = 0.5) +     #alpha = transparrency
  theme_bw()

# generate new data so we can extrapolate beyond the data feeding the model

newdata_bill_length_mm = seq(from = min(penguins_lm_3$bill_length_mm), to = max(penguins_lm_3$bill_length_mm), by = 0.1)
newdata_bill_length_mm
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, species = unique(penguins_lm_3$species))
head(newdata)
tail(newdata)

newdata_predict_lm_3 = cbind(newdata, predict(lm_3, newdata = newdata, interval = "confidence"))
head(newdata_predict_lm_3)

ggplot() +
  geom_point(data = penguins_lm_3, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(data = newdata_predict_lm_3, aes(x = bill_length_mm, y = fit, color = species)) +
  geom_ribbon(data = newdata_predict_lm_3, aes(x = bill_length_mm, ymin = lwr, ymax = upr, fill = species), alpha = 0.5)

lm_3_predict = lm_3 %>%
  broom::augment(data = penguins_lm_3, se_fit = T, interval = "confidence", conf.level = 0.95)

head(lm_3_predict)
glimpse(lm_3_predict)

newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)

lm_3_predict = lm_3 %>%
  broom::augment(newdata = newdata, se_fit = T, interval = "confidence", conf.int = 0.95)

head(lm_3_predict)

lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm * species, data = penguins_lm_3)
# short hand for above line 
lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data = penguins_lm_3)

summary(lm_4)

AIC(lm_3, lm_4)

# step function
best_model = step(lm_4)
best_model

lm_4_predict = lm_4 %>%
  broom::augment(se_fit = T, interval = "confidence")

head(lm_4_predict)

ggplot(data = lm_4_predict) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species))+
  geom_line(aes(x = bill_length_mm, y = .fitted, color = species)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species), alpha = 0.3) +
  theme_classic()

# 2 continuous variable predictors
library(car) #vif()

gentoo = penguins %>%
  filter(species == "Gentoo")
summary(gentoo)

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)
step(lm_gentoo_3)
summary(lm_gentoo_3)

vif(lm_gentoo_3)

newdata = gentoo %>%
  select(body_mass_g) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm = T)) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm = T))
  
head(newdata)
head(gentoo)

lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata = newdata, se_fit = T, interval = "confidence")
head(lm_gentoo_3_predict)

ggplot(data = lm_gentoo_3_predict) +
  geom_point(aes(x = body_mass_g, y = bill_depth_mm), data = gentoo) +
  geom_line(aes(x = body_mass_g, y = .fitted)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = body_mass_g), alpha = 0.3) +
  annotate("text", x = 4250, y = 17, label = paste0("flipper length = ", median(gentoo$flipper_length_mm, na.rm = T))) +
  annotate("text", x = 4250, y = 16.5, label = paste0("bill length = ", median(gentoo$bill_length_mm, na.rm = T))) +
  theme_bw()

# ????? exercise not working
newdata2 = gentoo %>%
  select(flipper_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm = T)) %>%
  mutate(bill_depth_mm = median(gentoo$bill_depth_mm, na.rm = T))

lm_gentoo_3_predict2 = lm_gentoo_3 %>%
  broom::augment(newdata = newdata2, se_fit = T, interval = "confidence")
head(lm_gentoo_3_predict2)

ggplot(data = lm_gentoo_3_predict) +
  geom_point(aes(x = bill_depth_mm, y = flipper_length_mm), data = gentoo) +
  geom_line(aes(x = bill_depth_mm, y = .fitted)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_depth_mm), alpha = 0.3) +
  annotate("text", x = 4250, y = 17, label = paste0("body mass = ", median(gentoo$body_mass_g, na.rm = T))) +
  annotate("text", x = 4250, y = 16.5, label = paste0("bill length = ", median(gentoo$bill_length_mm, na.rm = T))) +
  theme_bw()
###

# ANOVA
penguins_lm = lm(body_mass_g ~ species + sex, data = penguins)
summary(penguins_lm)
anova(penguin_lm)

penguins %>%
  group_by(sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = T))

penguins_anova = aov(body_mass_g ~ sex + species, data = penguins)
summary(penguins_anova)

TukeyHSD(penguins_anova)
