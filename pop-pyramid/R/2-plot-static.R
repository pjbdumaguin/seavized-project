# sourcing the data script will give us a `pop_data` object
source(file.path("pop-pyramid", "R", "1-data.R"))
library(ggfx)

windowsFonts(Cambria = windowsFont("Cambria"))

targ_c <- "Brunei Darussalam"
targ_y <- format(Sys.Date(), "%Y")

static_dat <- pop_dat |>
  filter(country %in% targ_c) |>
  filter(year %in% targ_y) |>
  mutate(
    pop_work = sum(abs(pop_age_sex)[age_start >= 15 & age_start < 65]),
    pop_dep = sum(abs(pop_age_sex)[!(age_start >= 15 & age_start < 65)]),
    prop_work_dep = (pop_work / (pop_work + pop_dep)) * 100,
    prop_work_dep = round(prop_work_dep, 2),
    .by = c(country, year)
  ) |>
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

source(file.path("theme", "theme.R"))
theme_set(theme_sv_d())

plot <- static_dat |>
  ggplot(aes(pop_age_sex_wo_sp, age_grp, fill = sex)) +
  as_reference(
    geom_col(
      position = "stack",
      stat = "sum",
      width = 0.8,
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
  # working-age population
  geom_rect(
    aes(
      xmin = min(pop_age_sex_w_sp),
      xmax = max(pop_age_sex_w_sp),
      ymin = 3.5,
      ymax = 13.5
    ),
    color = "#b5dde1ff",
    fill = NA,
    linewidth = 0.1
  ) +
  with_blend(
    annotate(
      geom = "label",
      x = min(static_dat$pop_age_sex_w_sp),
      y = "60-64",
      label = paste0(
        "Working-age population: ",
        unique(static_dat$prop_work_dep),
        "%"
      ),
      size = 5,
      size.unit = "pt",
      hjust = 0,
      border.color = NA,
      fill = NA
    ),
    bg_layer = "bars",
    blend_type = "xor"
  ) +
  # alt y-axis----
  annotate(
    "text",
    x = 0,
    y = unique(static_dat$age_grp),
    label = str_replace(unique(static_dat$age_grp), "-.+$", "\u2ba5"),
    color = "#173436ff",
    size = 5,
    size.unit = "pt"
  ) +
  labs(
    title = targ_c,
    subtitle = targ_y,
    caption = paste0(
      "Data source: United Nations,\n",
      "Department of Economic and Social Affairs, Population Division (2024).\n",
      "World Population Prospects 2024, Online Edition."
    ),
    tag = "seavized"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "plot.png",
  plot = plot,
  path = file.path("pop-pyramid", "data"),
  width = 1080,
  height = 1080,
  units = "px"
)
