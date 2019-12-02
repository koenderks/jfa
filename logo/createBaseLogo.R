require(hexSticker)
require(ggplot2)
require(svglite)

subplot <- ggplot() + theme_void() + theme_transparent()
sticker <- hexSticker::sticker(subplot = subplot, 
                               package = "jfa", 
                               p_color = "white", 
                               p_size = 25, 
                               p_x = 1, 
                               p_y = 0.6,
                               h_size = 3, 
                               h_fill = "#223f87", 
                               h_color = "#152c52", 
                               url = "www.github.com/koenderks/jfa", 
                               u_color = "white", 
                               u_size = 3)
sticker

# Save sticker as .svg file
ggplot2::ggsave(plot = sticker, filename = "baseLogo.svg")
