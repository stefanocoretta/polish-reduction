# Stefano Coretta
# 2023/04/27
# Code for presentation 'Reconsidering the IPA vowel quadrilateral'

library(tidyverse)
library(ggrepel)
library(Manu)
theme_set(theme_light(base_family = "DejaVu Sans"))
update_geom_defaults("text", list(family = theme_get()$text$family))
update_geom_defaults("label", list(family = theme_get()$text$family))
library(mgcv)
library(rticulate)

hoiho_7 <- c(get_pal("Hoiho"), "#490B0A")
options(ggplot2.discrete.fill = hoiho_7)

midpoint_long <- readRDS("~/repos/polish-reduction/data/derived/midpoint_long.rds") %>%
  filter(
    speaker == "PL08",
    stress == "stressed",
    rate == "normal",
    item %in% c("papa", "popa", "bywa", "mimo", "sumo", "rzepa"),
    fan_line > 4, fan_line < 36
  ) %>%
  mutate(V1 = as.factor(V1), speaker = as.factor(speaker))

midpoint_long %>%
  ggplot(aes(X, Y, colour = fan_line)) +
  geom_point(alpha = 1) +
  coord_fixed() +
  facet_wrap(~ speaker)

midpoint_long %>%
  ggplot(aes(X, Y, colour = V1)) +
  geom_point(alpha = 0.5) +
  coord_fixed() +
  facet_wrap(~ V1)

# GAM ----

vowels_gam <- polar_gam(
  Y ~
    s(X, V1, bs = "fs", k = 8),
  data = midpoint_long,
  fan_lines = c(8, 25)
)

summary(vowels_gam)

# Plot GAM ----

v_gams_preds <- predict_polar_gam(vowels_gam, length_out = 100) %>%
  mutate(
    V1 = str_replace(V1, "y", "ɨ"),
    V1 = str_replace(V1, "e", "ɛ"),
    V1 = str_replace(V1, "o", "ɔ")
  )

v_highest <- v_gams_preds %>%
  group_by(V1) %>%
  filter(Y == max(Y))

v_gams_preds %>%
  ggplot(aes(X, Y, colour = V1)) +
  geom_path(linewidth = 2.5) +
  geom_point(size = 0.01, colour = "white") +
  geom_label(
    data = v_highest,
    aes(label = V1, fill = V1),
    colour = c("white", "white", "white", "black", "white", "white"),
    size = 6,
    label.r = unit(0.3, "lines"),
    label.padding = unit(0.3, "lines")
  ) +
  scale_color_manual(values = hoiho_7) +
  coord_fixed() +
  labs(
    x = "Back to front (mm)", y = "Low to high (mm)",
    caption = "Polar GAM smoothing, k = 8"
  ) +
  theme(legend.position = "none")

ggsave("figures/uti-vowels-gam.png", width = 7, height = 5)


