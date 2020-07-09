library(hexSticker)
library(here)

img_path <- here("hex", "origami-crane.png")
sticker_path <- here("hex", "origami-sticker.pdf")

sticker(img_path,
  package = "origami",
  p_size = 8, s_x = 1, s_y = 0.75, s_width = 0.6,
  h_fill = "#242A30", h_color = "#99A5Ab",
  filename = sticker_path
)
