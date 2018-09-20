# reproduced with permission from Earo

library(gganimate)
library(tsibble)
library(tidyverse)

theme_remark <- function() {
  theme_grey() +
    theme(
      axis.text = element_text(size = 14), 
      strip.text = element_text(size = 16), 
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 16), 
      legend.text = element_text(size = 16),
      legend.position = "bottom"
    )
}

enquiry <- read_rds("data/enquiry.rds") %>%
  filter(channel != "Other") %>%
  mutate(channel = fct_drop(channel)) %>%
  arrange(service, category, channel, date)

enquiry_ma <- enquiry %>%
  as_tsibble(
    key = id(service | category, channel), index = date
  ) %>%
  # need CRAN rlang
  fill_na(volume = 0L) %>%
  summarise(ttl_volume = sum(volume)) %>%
  mutate(ma = slide_dbl(ttl_volume, mean, .size = 7))

slide_window <- slider(enquiry_ma$date, .size = 60) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())

p_slide <- ggplot() +
  geom_line(aes(date, ttl_volume), colour = "grey80", data = enquiry_ma) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = slide_window, fill = "#e6550d", alpha = 0.6) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark() +
  transition_manual(group)

animate(p_slide, 100, 5, width = 800, height = 200)
anim_save("img/anim-slide.gif")

tile_window <- tiler(enquiry_ma$date, .size = 60) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())

p_tile <- ggplot() +
  geom_line(aes(date, ttl_volume), colour = "grey80", data = enquiry_ma) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = tile_window, fill = "#e6550d", alpha = 0.6) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark() +
  transition_manual(group)

animate(p_tile, 100, 5, width = 800, height = 200)
anim_save("img/anim-tile.gif")

stretch_window <- stretcher(enquiry_ma$date, .init = 60) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())

p_stretch <- ggplot() +
  geom_line(aes(date, ttl_volume), colour = "grey80", data = enquiry_ma) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = stretch_window, fill = "#e6550d", alpha = 0.6) +
  xlab("Date") +
  ylab("Total volume") +
  theme_remark() +
  transition_manual(group)

animate(p_stretch, 100, 5, width = 800, height = 200)
anim_save("img/anim-stretch.gif")