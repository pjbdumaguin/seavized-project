library(tidyverse)

# prepare directory
base_path <- file.path("pop-pyramid", "data")
if (!dir.exists(base_path)) {
  dir.create(base_path, recursive = TRUE)
}

# download the csv file
# "Population on 01 January, by 5-year age groups"
pop_url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryByAge5GroupSex_Medium.csv.gz"
pop_destfile <- file.path(base_path, basename(pop_url))
if (!file.exists(pop_destfile)) {
  download.file(pop_url, pop_destfile)
}

# inspect headers before loading the full file
colnames(read_csv(pop_destfile, n_max = 0, show_col_types = FALSE))

# we're interested in Location, Time, AgeGrp, PopMale, PopFemale, and PopTotal
pop_obj <- read_csv(
  file = pop_destfile,
  col_select = c(
    "loc_type" = "LocTypeName",
    "country" = "Location",
    "year" = "Time",
    "age_grp" = "AgeGrp",
    "age_start" = "AgeGrpStart",
    "male" = "PopMale",
    "female" = "PopFemale",
    "pop_age" = "PopTotal"
  ),
  col_types = cols(
    "LocTypeName" = "c",
    "Location" = "c",
    "Time" = "i",
    "AgeGrp" = "f",
    "AgeGrpStart" = "i",
    "PopMale" = "d",
    "PopFemale" = "d",
    "PopTotal" = "d"
  ),
  lazy = TRUE
)

# some more inspection
# pop_obj |> slice_sample(n = 10)

# prep data for plotting
pop_dat <- pop_obj |>
  filter(loc_type == "Country/Area") |> 
  mutate(male = -male) |> # left side
  pivot_longer(
    cols = ends_with("male"),
    names_to = "sex",
    values_to = "pop_age_sex"
  ) |>
  mutate(across(c(pop_age_sex, pop_age), \(col) col * 1e3)) |> # data was in thousands
  select(!loc_type)
  
rm(list = grepv("pop_dat", ls(), invert = TRUE))
