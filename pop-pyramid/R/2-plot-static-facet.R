# sourcing the data script will give us a `pop_data` object
source(file.path("pop-pyramid", "R", "1-data.R"))

windowsFonts(Cambria = windowsFont("Cambria"))

source(file.path("theme", "theme.R"))
theme_set(theme_sv_d())

facet_path <- file.path("pop-pyramid", "data", "facet")
if (!dir.exists(facet_path)) {
  dir.create(facet_path, recursive = TRUE)
}

lbl_yr <- function(yrs) {
  txt <- map_chr(yrs, \(yr) {
    if (yr > 2024) paste0(yr, "*") else yr
  })
}

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

static_dat <- pop_dat |>
  select(!loc_type) |>
  filter(country %in% sea) |>
  pivot_wider(names_from = sex, values_from = pop_age_sex) |>
  mutate(surplus = male + female) |> # addition because male population is already in negative
  mutate(
    male_sp = if_else(surplus < 0, surplus, 0),
    female_sp = if_else(surplus > 1, surplus, 0)
  ) |>
  mutate(male = male - male_sp, female = female - female_sp) |>
  pivot_longer(
    cols = contains("male"),
    names_to = "sex",
    values_to = "pop_age_sex"
  ) |>
  mutate(sex = fct_relevel(sex, "male_sp", "male", "female_sp", "female"))


gen_plot <- function(country_arg, yrs = NULL) {
  if(is.null(yrs)) yrs <- c(seq(1950, 2100, 10))

  country_dat <- static_dat |>
    filter(country %in% country_arg) |>
    filter(year %in% yrs)

  plot <- ggplot(aes(pop_age_sex, age_grp, fill = sex), data = country_dat) +
    geom_col(
      width = .8,
      show.legend = FALSE
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
      title = country_dat$country[1],
      subtitle = "Population pyramid every decade",
      caption = paste0(
        "Data source: United Nations,\n",
        "Department of Economic and Social Affairs, Population Division (2024).\n",
        "World Population *Prospects 2024, Online Edition."
      ),
      tag = "seavized"
    ) +
    theme(
      plot.margin = margin_auto(14),
      plot.subtitle = element_text(size = 8),
      strip.text = element_text(size = 7)
    ) +
    facet_wrap(
      vars(year),
      labeller = labeller(year = lbl_yr),
      strip.position = "bottom"
    )

  ggsave(
    glue::glue("{country_dat$country[1]}.png"),
    plot = plot,
    path = facet_path,
    width = 1080,
    height = 1080,
    units = "px"
  )
}

walk(sea, gen_plot)
