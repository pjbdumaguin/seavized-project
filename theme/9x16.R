to_pt <- function(px, res = 72) {
  pt <- px * (72 / res)
  return(pt)
}

theme_sea <- function() {
  ggplot2::theme(
    text = element_text(
      family = "Cambria",
      color = "#b5dde1ff",
      size = 72,
      hjust = 0
    ),
    geom = element_geom(
      family = "Cambria",
      fontsize = 72,
      color = "#b5dde1ff"
    ),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#173436ff", color = "#173436ff"),
    plot.title = element_text(size = 59),
    plot.subtitle = element_text(
      size = 96,
      margin = margin_part(t = 39, b = 59)
    ),
    plot.title.position = "plot",
    plot.caption = element_text(size = 22, margin = margin_part(t = 67)),
    plot.caption.position = "plot",
    plot.tag = element_text(size = 42, margin = margin_part(t = 67)),
    plot.tag.position = "bottomright",
    plot.tag.location = "plot",
    # margin based on short form video "safezone"
    plot.margin = margin(
      to_pt(250),
      to_pt(120),
      to_pt(380),
      to_pt(120),
      unit = "pt"
    ),
    validate = TRUE
  )
}
