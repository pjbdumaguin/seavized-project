theme_sv_d <- function(
  base_size = 11,
  base_family = "Cambria",
  header_family = NULL,
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  ink = "#b5dde1ff",
  paper = "#173436ff",
  accent = "#ffb7b2ff"
) {
  half_line <- base_size / 2
  t <- theme(
    line = element_blank(),
    rect = element_rect(
      fill = paper,
      colour = NA,
      linewidth = 0,
      linetype = 1,
      inherit.blank = FALSE,
      linejoin = "round"
    ),
    polygon = element_blank(),
    point = element_blank(),
    text = element_text(
      family = base_family,
      face = "plain",
      colour = ink,
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    title = element_text(family = header_family),
    spacing = unit(half_line, "pt"),
    margins = margin_auto(half_line),
    geom = element_geom(
      ink = ink,
      paper = paper,
      accent = accent,
      linewidth = base_line_size,
      borderwidth = base_line_size,
      linetype = 1L,
      bordertype = 1L,
      family = base_family,
      fontsize = base_size,
      pointsize = (base_size / 11) * 1.5,
      pointshape = 19
    ),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks.length = rel(0),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = NULL,
    legend.box = NULL,
    legend.key.size = unit(1.2, "lines"),
    legend.position = "right",
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(hjust = 0),
    legend.key.spacing = rel(1),
    legend.margin = margin_auto(0),
    legend.box.margin = margin_auto(0),
    legend.box.spacing = unit(0.2, "cm"),
    legend.ticks.length = rel(0.2),
    legend.background = element_blank(),
    legend.frame = element_blank(),
    legend.box.background = element_blank(),
    strip.clip = "on",
    strip.text = element_text(size = rel(0.8)),
    strip.switch.pad.grid = rel(0.5),
    strip.switch.pad.wrap = rel(0.5),
    strip.background = element_blank(),
    panel.ontop = FALSE,
    panel.spacing = NULL,
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin_auto(base_size),
    plot.title = element_text(
      face = "bold",
      size = rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = margin(t = half_line)
    ),
    plot.title.position = "panel",
    plot.subtitle = element_text(
      hjust = 0,
      vjust = 1,
      margin = margin(t = half_line)
    ),
    plot.caption = element_text(
      size = rel(0.4),
      hjust = 0,
      vjust = 0,
      margin = margin(t = half_line)
    ),
    plot.caption.position = "plot",
    plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "bottomright",
    plot.tag.location = "plot",
    plot.background = element_rect(color = paper), # fills the white border artifact
    complete = TRUE
  )
  return(t)
}
