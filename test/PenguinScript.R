
# ============================================================
# PENGUIN INFOGRAPHIC
# "Not all penguins are built the same!"
#
# Requirements:
#   - R
#   - tidyverse
#
# The dataset is downloaded automatically.
# ============================================================


# 1. INSTALL AND LOAD PACKAGES -------------------------------

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(tidyverse)


# 2. DOWNLOAD AND LOAD DATA ----------------------------------

penguin_url <- paste0(
  "https://raw.githubusercontent.com/",
  "allisonhorst/palmerpenguins/main/",
  "inst/extdata/penguins.csv"
)

penguins <- read_csv(
  penguin_url,
  show_col_types = FALSE
)


# 3. CLEAN AND PREPARE DATA ----------------------------------

penguins_clean <- penguins |>
  filter(
    !is.na(species),
    !is.na(body_mass_g),
    !is.na(flipper_length_mm)
  ) |>
  mutate(
    species = factor(
      species,
      levels = c("Adelie", "Chinstrap", "Gentoo")
    )
  )


# 4. SUMMARISE DATA ------------------------------------------

penguin_summary <- penguins_clean |>
  group_by(species) |>
  summarise(
    mean_body_mass = mean(body_mass_g),
    mean_flipper_length = mean(flipper_length_mm),
    n = n(),
    .groups = "drop"
  )

print(penguin_summary)


# 5. DEFINE SPECIES COLOURS ----------------------------------

species_colours <- c(
  "Adelie" = "#E69F00",
  "Chinstrap" = "#56B4E9",
  "Gentoo" = "#009E73"
)


# 6. CREATE SUMMARY DATA FOR THE INFOGRAPHIC -----------------

summary_long <- penguin_summary |>
  select(
    species,
    mean_body_mass,
    mean_flipper_length
  ) |>
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "measurement",
    values_to = "value"
  ) |>
  mutate(
    measurement = recode(
      measurement,
      mean_body_mass = "Average body mass (g)",
      mean_flipper_length = "Average flipper length (mm)"
    )
  )


# 7. CREATE SUMMARY PLOT -------------------------------------

summary_plot <- ggplot(
  summary_long,
  aes(
    x = species,
    y = value,
    fill = species
  )
) +
  geom_col(
    width = 0.65,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = round(value)),
    vjust = -0.6,
    size = 4
  ) +
  facet_wrap(
    ~measurement,
    scales = "free_y",
    ncol = 2
  ) +
  scale_fill_manual(
    values = species_colours
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.20))
  ) +
  labs(
    title = "Not all penguins are built the same!",
    subtitle = "Comparing the three penguin species of the Palmer Archipelago",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 24,
      face = "bold",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 20)
    ),
    strip.text = element_text(
      size = 13,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 25, 20, 25)
  )


# 8. CREATE SCATTER PLOT -------------------------------------

scatter_plot <- ggplot(
  penguins_clean,
  aes(
    x = flipper_length_mm,
    y = body_mass_g,
    colour = species
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.75
  ) +
  scale_colour_manual(
    values = species_colours
  ) +
  labs(
    title = "Bigger flippers, heavier penguins?",
    subtitle = "Relationship between flipper length and body mass",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    colour = "Species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      margin = margin(b = 12)
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 25, 20, 25)
  )


# 9. CREATE OUTPUT INFOGRAPHIC -------------------------------

# Use grid to arrange the two ggplots vertically.
# grid is included with R, so no additional package is needed.

output_file <- "Penguin_infographic.png"

png(
  filename = output_file,
  width = 2400,
  height = 2800,
  res = 200
)

grid::grid.newpage()

grid::pushViewport(
  grid::viewport(
    layout = grid::grid.layout(
      nrow = 2,
      ncol = 1,
      heights = grid::unit(
        c(0.48, 0.52),
        "null"
      )
    )
  )
)

# Upper section: species comparisons
print(
  summary_plot,
  vp = grid::viewport(
    layout.pos.row = 1,
    layout.pos.col = 1
  )
)

# Lower section: scatter plot
print(
  scatter_plot,
  vp = grid::viewport(
    layout.pos.row = 2,
    layout.pos.col = 1
  )
)

grid::popViewport()

dev.off()


# 10. CONFIRM COMPLETION -------------------------------------

cat(
  "\nInfographic successfully created!\n",
  "File: ", output_file, "\n",
  "Location: ", getwd(), "\n"
)
