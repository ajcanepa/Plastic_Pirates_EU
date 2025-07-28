# Single HeatMap ----------------------------------------------------------
library(tidyHeatmap)

Data_prop_eu %>%
  heatmap(
    .row           = Litter_type,
    .column        = CountryResponsible,
    .value         = Prop_of_Europe,
    scale          = "column", # can be "none" (by default) or "row", "column" or "both". (#column keeps the z‑scaling)
    # a three‑colour gradient: low→mid→high
    palette_value  = c("#F7FBFF", "#6BAED6", "#08306B"),
    name           = "Deviation % from \nEU total",
    column_title   = "Country",
    row_title      = "Litter type",
    show_row_names = TRUE,
    show_column_names = TRUE
  )


# Faceted Heatmaps --------------------------------------------------------
library(dplyr)
library(tidyHeatmap)
library(ComplexHeatmap)

# Heatmap for Spring
ht_spring <- Data_prop_eu %>%
  filter(Season == "Spring") %>%
  heatmap(
    .row          = Litter_type,
    .column       = CountryResponsible,
    .value        = Prop_of_Europe,
    scale         = "none",
    name          = "Spring\n% of EU total",
    palette_value = c("#F7FBFF", "#6BAED6", "#08306B"),
    show_row_names    = TRUE,
    show_column_names = TRUE
  )

# Heatmap for Summer
ht_summer <- Data_prop_eu %>%
  filter(Season == "Summer") %>%
  heatmap(
    .row          = Litter_type,
    .column       = CountryResponsible,
    .value        = Prop_of_Europe,
    scale         = "none",
    name          = "Summer\n% of EU total",
    palette_value = c("#F7FBFF", "#6BAED6", "#08306B"),
    show_row_names    = FALSE,  # hide repeat of row names
    show_column_names = TRUE
  )

# Combine side by side
ht_spring + ht_summer



