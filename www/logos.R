library(magick)

image_read("www/logo_original/PNG logo_blue.png") %>%
  image_resize(geometry = "300x150") %>%
  image_extent(geometry = "300x150", gravity = "center") %>%
  image_background(color = "white") %>%
  image_write("www/iiasa_logo.png")

image_read("www/logo_original/oeaw_logo_446x192.png") %>%
  image_resize(geometry = "300x150") %>%
  image_extent(geometry = "300x150", gravity = "center") %>%
  image_background(color = "white") %>%
  image_write("www/oeaw_logo.png")


image_read("www/logo_original/adri.png") %>%
  image_resize(geometry = "300x150") %>%
  image_extent(geometry = "300x150", gravity = "center") %>%
  image_background(color = "white") %>%
  image_write("www/adri_logo.png")


image_read("www/logo_original/Uni_Logo_2016_crop.png") %>%
  image_resize(geometry = "300x150") %>%
  image_extent(geometry = "300x150", gravity = "center") %>%
  image_background(color = "white") %>%
  image_write("www/uniwien_logo.png")