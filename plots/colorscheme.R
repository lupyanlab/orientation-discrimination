
#error_bar_color <- "#fc8d62"  # orange, from colorbrewer
dark_gray <- "#636363"

error_bar_color <- dark_gray # dark gray
line_color <- dark_gray

light_blue <- "#9ecae1"
dark_blue <- "#08519c"
light_green <- "#a1d99b"
dark_green <- "#006d2c"

color_scheme <- list(
  visualmask = dark_blue,
  visualnomask = light_blue,
  nonvisualmask = dark_green,
  nonvisualnomask = light_green
)

imagery_color_scheme <- list(mask = dark_blue, nomask = light_blue)
facts_color_scheme <- list(mask = dark_green, nomask = light_green)

magnet_color_scheme <- list(invalid = "#d7191c", valid = "#a6d96a",
                            noise = dark_gray)
