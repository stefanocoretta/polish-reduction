knitr::opts_chunk$set(echo = FALSE)
knitr::opts_knit$set(root.dir = here::here())
library(tidyverse)
library(ggrepel)
library(patchwork)
theme_set(theme_minimal(base_family = "Lato"))
options(
  ggplot2.discrete.fill = list(
    # 2 cols
    c("#264653", "#e76f51"),
    # 3 cols
    c("#B09B37", "#955F47", "#A8B9CB"),
    # 6 cols
    c("#7ACCD7", "#115896", "#7C6C65", "#4C4C53", "#BA2F00", "#21282F")
  )
)


midpoint_rotated <- readRDS("./data/derived/midpoint_rotated.rds") %>%
  mutate(
    rate = factor(rate, levels = c("fast", "normal", "slow")),
    stress = factor(stress, levels = c("unstressed", "stressed")),
    item = factor(item),
    V1 = str_replace(V1, "y", "ɨ"),
    V1 = factor(V1, levels = c("a", "e", "i", "o", "u", "ɨ")),
    frame = item,
    duration.log = log(duration),
    z1 = as.numeric(z1),
    z2 = as.numeric(z2),
    f1.z = as.numeric(f1.z),
    f2.z = as.numeric(f2.z),
    f0.z = as.numeric(f0.z)
  )

levels(midpoint_rotated$frame) <- c(
  "b_b", "b_b", "b_w", "b_w", "s_m", "drz_w", "drz_w",
  "k_m", "k_m", "m_m", "m_m", "ni_b", "ni_b", "n_w",
  "n_w", "p_p", "p_p", "p_p", "p_p", "rz_p", "rz_p",
  "s_m", "z_m", "z_m", "z_w", "z_w"
)

contrasts(midpoint_rotated$V1) <- "contr.sum"

midpoint_long <- readRDS("./data/derived/midpoint_long.rds") %>%
  mutate(
    rate = factor(rate, levels = c("fast", "normal", "slow")),
    stress = factor(stress, levels = c("unstressed", "stressed")),
    V1 = factor(V1, levels = c("a", "e", "i", "o", "u", "y")),
    V1 = recode_factor(V1, i = "i", e = "e", y = "ɨ", a = "a", o = "o", u = "u")
  )


f1 <- midpoint_rotated %>%
  # mutate(V1 = factor(V1, levels = c("i", "ɨ", "e", "a", "o", "u"))) %>%
  mutate(V1 = factor(V1, levels = c("i", "ɨ", "u", "e", "o", "a"))) %>%
  ggplot(aes(duration, f1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_reverse() +
  facet_wrap(~ V1)

f2 <- midpoint_rotated %>%
  mutate(V1 = factor(V1, levels = c("i", "ɨ", "e", "a", "o", "u"))) %>%
  # mutate(V1 = factor(V1, levels = c("i", "ɨ", "u", "e", "o", "a"))) %>%
  ggplot(aes(duration, f2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ V1)

f1 + f2

midpoint_rotated %>%
  ggplot(aes(f2, f1, colour = duration, size = duration, alpha = duration)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_point()
