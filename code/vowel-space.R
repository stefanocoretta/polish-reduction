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
    speaker != "PL09",
    stress == "stressed",
    rate == "normal",
    fan_line > 9, fan_line < 31
  ) %>%
  mutate(V1 = as.factor(V1))

# midpoint_long %>%
#   ggplot(aes(X, Y, colour = fan_line)) +
#   geom_point()

vowels_gam <- polar_gam(
  Y ~
    s(X, V1, bs = "fs", k = 8),
  data = midpoint_long,
  # fan_lines = c(5, 25),
  origin = c(15, -50)
)

summary(vowels_gam)

v_gams_preds <- predict_polar_gam(vowels_gam, length_out = 100)

v_highest <- v_gams_preds %>%
  group_by(V1) %>%
  filter(Y == max(Y))

v_gams_preds %>%
  ggplot(aes(X, Y, colour = V1)) +
  geom_path(linewidth = 2.5) +
  geom_point(size = 0.01, colour = "white") +
  geom_label_repel(
    data = v_highest,
    aes(label = V1, fill = V1),
    colour = c("white", "white", "white", "black", "white", "white"),
    size = 6,
    label.r = unit(0.3, "lines"),
    label.padding = unit(0.3, "lines"),
    min.segment.length = 15
  ) +
  scale_color_manual(values = hoiho_7) +
  labs(
    x = "Back to front (mm)", y = "Low to high (mm)",
    caption = "Polar GAM smoothing, k = 8"
  ) +
  theme(legend.position = "none")

ggsave("figures/uti-vowels-gam.png", width = 7, height = 5)
