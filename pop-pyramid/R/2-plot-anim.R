# sourcing the data script will give us a `pop_data` object
source(file.path("pop-pyramid", "R", "1-data.R"))

library(gganimate)
library(ggfx)

# note: explicitly register the family if setting (device) type = windows
windowsFonts(Cambria = windowsFont("Cambria"))

# load custom theme
source(file.path("theme", "theme.R"))
theme_set(theme_sv_d())

# anim data----
countries <- c("Philippines")

anim_dat <- filter(pop_dat, country %in% countries) |>
  mutate(age_grp_lbl = str_replace(age_grp, "-.+$", "\u2ba5"))

rect_dat <- mutate(
  anim_dat,
  xmin = min(pop_age_sex),
  xmax = max(pop_age_sex),
  .by = c("country", "year"),
  .keep = "none"
)

text_dat <- mutate(
  anim_dat,
  pop_work = sum(abs(pop_age_sex)[age_start >= 15 & age_start < 65]),
  pop_dep = sum(abs(pop_age_sex)[!(age_start >= 15 & age_start < 65)]),
  prop_work_dep = (pop_work / (pop_work + pop_dep)) * 100,
  prop_work_dep = round(prop_work_dep, 2),
  .by = c(country, year),
  .keep = "none"
) |>
  bind_cols(select(rect_dat, xmin))

# animation----
anim <- ggplot(anim_dat) +
  as_reference(
    geom_col(
      aes(pop_age_sex, age_grp, fill = sex),
      width = 0.8,
      color = NA,
      show.legend = FALSE
    ),
    id = "bars"
  ) +
  scale_fill_manual(values = c("male" = "#97f2ffff", "female" = "#ffb7b2ff")) +
  labs(
    title = anim_dat$country,
    subtitle = "{frame_time}",
    caption = "Data source: United Nations, Department of\nEconomic and Social Affairs, Population Division (2024).\nWorld Population Prospects 2024, Online Edition.",
    tag = "seavized"
  ) +
  # working population section----
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = 3.5, ymax = 13.5),
    rect_dat,
    color = "#b5dde1ff",
    fill = NA,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  with_blend(
    geom_label(
      mapping = aes(
        x = xmin,
        y = 13.5,
        label = paste0("Working-age\npopulation: ", prop_work_dep, "%")
      ),
      text_dat,
      size = 11,
      hjust = 0,
      vjust = 1,
      border.color = NA,
      fill = NA,
      inherit.aes = FALSE
    ),
    bg_layer = "bars",
    blend_type = "xor"
  ) +
  # alt y-axis----
  annotate(
    "text",
    x = 0,
    y = unique(anim_dat$age_grp),
    label = unique(anim_dat$age_grp_lbl),
    color = "#173436ff",
    size = 9
  ) +
  coord_cartesian(clip = "off") +
  theme_sv_d(base_size = 88) +
  theme(
    plot.caption = element_text(size = 20, margin = margin_part(t = 100)),
    plot.tag = element_text(size = 48),
    # margin based on short form video "safezone"
    plot.margin = margin(250, 120, 380-100, 120, unit = "pt")
  ) +
  # animation section----
  transition_time(year) +
  view_follow()

# preview----
animate(
  anim,
  nframes = 4,
  renderer = file_renderer(file.path("pop-pyramid", "data"), "anim", TRUE),
  # grDevices::png args----
  device = 'bmp',
  height = 1920,
  width = 1080,
  units = "px",
  pointsize = 12 * 2.834645669, # mm to pt
  type = "windows",
  family = "Cambria"
)

# generate ffmpeg executable path
base_dir <- dir(file.path("C:", "Program Files"), "ffmpeg.+", full.names = TRUE)
ffmpeg_dir <- dir(file.path(base_dir, "bin"), "ffmpeg.exe", full.names = TRUE)
# renduh
anim_save(
  paste0(countries[1], ".mp4"),
  animation = anim,
  path = file.path("pop-pyramid", "data"),
  # animate() args----
  fps = 15,
  duration = 1,
  renderer = ffmpeg_renderer(ffmpeg = ffmpeg_dir),
  ## grDevices::bmp() args----
  device = 'bmp',
  height = 1920,
  width = 1080,
  units = "px",
  pointsize = 12 * 2.834645669, # mm to pt
  type = "windows",
  family = "Cambria"
)
