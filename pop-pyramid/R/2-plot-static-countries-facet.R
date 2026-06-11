# sourcing the data script will give us a `pop_data` object
source(file.path("pop-pyramid", "R", "1-data.R"))

windowsFonts(Cambria = windowsFont("Cambria"))

source(file.path("theme", "theme.R"))
theme_set(theme_sv_d())

facet_path <- file.path("pop-pyramid", "data", "facet", "country")
if (!dir.exists(facet_path)) {
  dir.create(facet_path, recursive = TRUE)
}

lbl_c <- function(cs) {
  cs |> replace_values(
    "Brunei Darussalam" ~ "Brunei",
    "Lao People's Democratic Republic" ~ "Lao PDR"
  )
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


gen_plot <- function(countries, yr) {

  country_dat <- static_dat |>
    filter(country %in% countries) |>
    filter(year == yr)

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
      title = "Southeast Asia",
      subtitle = glue::glue("Population pyramid, {yr}"),
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
      vars(country),
      scales = "free_x",
      labeller = labeller(country = lbl_c),
      strip.position = "bottom"
    )

  ggsave(
    "sea.png",
    plot = plot,
    path = facet_path,
    width = 1080,
    height = 1080,
    units = "px"
  )
}

gen_plot(sea, 2026)
