# sourcing the data script will give us a `pop_data` object
source(file.path("pop-pyramid", "R", "1-data.R"))

library(gganimate)
library(ggfx)
library(glue)

# note: explicitly register the family if setting (device) type = windows
windowsFonts(Cambria = windowsFont("Cambria"))

# load custom theme
source(file.path("theme", "theme.R"))
theme_set(theme_sv_d())

# anim data----
sea <- c(
  "Brunei Darussalam",
  "Cambodia",
  "Indonesia",
  "Lao People's Democratic Republic",
  "Malaysia",
  "Myanmar",
  "Philippines",
  "Singapore",
  "Thailand",
  "Timor-Leste",
  "Viet Nam"
)

anim_dat <- filter(pop_dat, country %in% sea) |>
  mutate(age_grp_lbl = str_replace(age_grp, "-.+$", "\u2ba5")) |>
  pivot_wider(names_from = sex, values_from = pop_age_sex) |>
  mutate(surplus = male + female) |> # addition because male population is already in negative
  mutate(
    male_sp = if_else(surplus < 0, surplus, 0),
    female_sp = if_else(surplus > 1, surplus, 0)
  ) |>
  # record the total population by age group and gender (with surplus)
  mutate(
    m_total = male,
    f_total = female
  ) |>
  pivot_longer(
    cols = ends_with("_total"),
    names_to = "temp",
    values_to = "pop_age_sex_w_sp"
  ) |>
  select(!temp) |>
  # subtract the surplus from gender
  mutate(male = male - male_sp, female = female - female_sp) |>
  pivot_longer(
    cols = contains("male"),
    names_to = "sex",
    values_to = "pop_age_sex_wo_sp"
  ) |>
  mutate(sex = fct_relevel(sex, "male_sp", "male", "female_sp", "female"))

rect_dat <- mutate(
  anim_dat,
  xmin = min(pop_age_sex_w_sp),
  xmax = max(pop_age_sex_w_sp),
  .by = c("country", "year"),
  .keep = "none"
)

text_dat <- mutate(
  anim_dat,
  pop_work = sum(abs(pop_age_sex_w_sp)[age_start >= 15 & age_start < 65]),
  pop_dep = sum(abs(pop_age_sex_w_sp)[!(age_start >= 15 & age_start < 65)]),
  prop_work_dep = (pop_work / (pop_work + pop_dep)) * 100,
  prop_work_dep = round(prop_work_dep, 2),
  .by = c(country, year),
  .keep = "none"
) |>
  bind_cols(select(rect_dat, xmin))

gen_anim <- function(country_arg) {
  # TODO refactor
  anim_dat <- filter(anim_dat, country == country_arg)
  rect_dat <- filter(rect_dat, country == country_arg)
  text_dat <- filter(text_dat, country == country_arg)
  anim <- ggplot(anim_dat) +
    as_reference(
      geom_col(
        aes(pop_age_sex_wo_sp, age_grp, fill = sex),
        position = "stack",
        stat = "sum",
        width = 0.8,
        color = NA,
        show.legend = FALSE
      ),
      id = "bars"
    ) +
    scale_fill_manual(
      values = c(
        "male_sp" = "#33e4ff",
        "male" = "#97f2ff",
        "female" = "#ffb7b2ff",
        "female_sp" = "#ff584d"
      )
    ) +
    labs(
      title = case_when(
        str_detect(anim_dat$country[1], "Brunei") ~ "Brunei",
        str_detect(anim_dat$country[1], "Lao") ~ "Lao PDR",
        .default = anim_dat$country[1]
      ),
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
          label = glue("Working-age\npopulation: {prop_work_dep}%")
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
      plot.margin = margin(250, 120, 380 - 100, 120, unit = "pt")
    ) +
    # gganimate section----
    transition_time(year) +
    view_follow()

  return(anim)
}

# locate ffmpeg executable path
base_dir <- dir(
  file.path("C:", "Program Files"),
  "ffmpeg.+",
  full.names = TRUE
)
ffmpeg_dir <- dir(file.path(base_dir, "bin"), "ffmpeg.exe", full.names = TRUE)

loc_audio <- function(country) {
  dirs <- list.files(file.path("pop-pyramid", "data", "audio"))
  return(dirs[agrep(country, dirs, ignore.case = TRUE)])
}

render_anim <- function(anim, country) {
  audio <- loc_audio(country)
  anim_save(
    glue("{country}.mp4"),
    animation = anim,
    path = file.path("pop-pyramid", "data", "anim"),
    # animate() args----
    fps = 30,
    duration = 60,
    renderer = ffmpeg_renderer(
      ffmpeg = ffmpeg_dir,
      options = list(
        "-i" = file.path("pop-pyramid", "data", "audio", audio),
        "-af" = "afade=t=out:st=58:d=2", # audio fade hehe
        "-shortest" = NULL
      )
    ),
    ## grDevices::bmp() args----
    device = 'bmp',
    height = 1920,
    width = 1080,
    units = "px",
    pointsize = 12 * 2.834645669, # mm to pt
    type = "windows",
    family = "Cambria"
  )
}

# main function----
walk(sea, function(country) {
  c_anim <- gen_anim(country)
  render_anim(c_anim, country)
})

# test preview----
animate(
  anim,
  nframes = 2,
  renderer = file_renderer(
    file.path("pop-pyramid", "data", "test-preview"),
    "anim",
    TRUE
  ),
  # grDevices::png args----
  device = 'bmp',
  height = 1920,
  width = 1080,
  units = "px",
  pointsize = 12 * 2.834645669, # mm to pt
  type = "windows",
  family = "Cambria"
)
