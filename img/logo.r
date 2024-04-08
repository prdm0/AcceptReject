library(hexSticker)
library(ggplot2)
library(glue)
library(magick)
library(fs)

img <- image_read("img/logo_transparency.png")
logo <- image_ggplot(img, interpolate = TRUE)

sticker(
  logo,
  package = "",
  p_size = 3,
  s_width = 1.9,
  s_height = 1.9,
  s_x = 1,
  s_y = 1,
  h_fill =  "#BCA0CB",
  h_color = "#F990C2",
  p_color = "#335955",
  p_family = "Aller_Rg",
  h_size = 2,
  white_around_sticker = F,
  filename = "logo.png",
  url = "https://prdm0.github.io/AcceptReject/",
  u_size = 4,
  spotlight = T,
  l_alpha = 0.4,
  dpi = 300,
  u_color = "#0F2536"
)
