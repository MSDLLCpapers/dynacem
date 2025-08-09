# Create hex sticker logo
# =======================

# Logo is created externally to R as 'sticker.png'
# The following code then converts this to the 'logo.png' image file that we will use
# With thanks to Nelson Gonzabato, https://nelson-gon.github.io/12/06/2020/hex-sticker-creation-r/

library(magick)
library(hexSticker)

magick::image_read("sticker.png") |>
  hexSticker::sticker(
    package="",
    h_color = "#ffffff", # White around edges
    s_x = 1, # Position the logo centrally
    s_y = 1,
    s_width = 2.8, # Make the logo bigger
    s_height = 2.8, 
    filename="logo.png" # Save file
  )

# You then need to move the 'logo.png' file into the docs folder