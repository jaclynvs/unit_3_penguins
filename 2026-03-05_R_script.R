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
