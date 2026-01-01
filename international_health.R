# File: international_health.R
# Desc: Uses OECD data to compare health spending and outcomes
# Date: 2025-12-05
# Auth: John Schmitt

library(tidyverse)
library(readr)
library(readxl)
library(fs)
library(janitor)
library(patchwork)
library(here)

# Useful functions --------------------------------------------------------

colors <- c("#acc8e5", "#6699CC")

my_theme <- theme(
  text = element_text(family = "Inconsolata"),
  axis.text.x = element_text(size = 12, face = "bold"), # tick marks
  axis.text.y = element_text(size = 12, face = "bold"), # tick marks
  axis.title.x = element_text(size = 13, face = "bold"),
  axis.title.y = element_text(size = 13, face = "bold"),
  plot.title = element_text(size = 16, color = "#2C6FB2", face = "bold"),
  plot.subtitle = element_text(size = 14, color = "#2C6FB2", face = "bold"),
  plot.caption = element_text(size = 10)
)

save_plot <- function(
    plot,
    file,
    dpi = 1200,
    width = 6,
    height = 4,
    units = "in") {
  ggsave(file, plot, dpi = dpi, width = width, height = height, units = units)
}

# Read raw data from OECD and WHO ----------------------------------------------

  #OECD

  # Downloaded from https://data-explorer.oecd.org/  
  # December 3, 2025

    # rename long OECD file name

      # health_expenditures

file_copy(
  path = here("raw-data", "OECD.ELS.HD,DSD_SHA@DF_SHA,1.0+.A.EXP_HEALTH.PT_B1GQ._T.._T.._T....csv"),
  new_path = here("raw-data", "health_expenditures.csv"),
  overwrite = TRUE
)

      # life_expectancy

file_copy(
  path = here("raw-data", "OECD.ELS.HD,DSD_HEALTH_STAT@DF_LE,1.1+.A...Y0.csv"),
  new_path = here("raw-data", "life_expectancy.csv"),
  overwrite = TRUE
)

      # low_birth_weight

file_copy(
  path = here("raw-data", "OECD.ELS.HD,DSD_HEALTH_STAT@DF_IH_LB,1.1+.A.csv"),
  new_path = here("raw-data", "low_birth_weight.csv"),
  overwrite = TRUE
)

      # infant_mortality

file_copy(
  path = here("raw-data", "OECD.ELS.HD,DSD_HEALTH_STAT@DF_MIM,1.1+.A.INM.csv"),
  new_path = here("raw-data", "infant_mortality.csv"),
  overwrite = TRUE
)

      # maternal mortality oecd

        # see below

  # WHO

    # Downloaded from https://www.who.int/data/gho/whs-annex/  
    # December 3, 2025

      # maternal_mortality who

file_copy(
  path = here("raw-data", "WHS2025_DATADOWNLOAD.csv"),
  new_path = here("raw-data", "maternal_mortality_who.csv"),
  overwrite = TRUE
)

# health_expenditures (percent of GDP) ------------------------------------

    # read data

health_expenditures <- read_csv(here("raw-data", "health_expenditures.csv")) |>
  clean_names() |> 
  select(
    country_short = ref_area,
    country = reference_area,
    share_gdp = obs_value,
    year = time_period
  ) |> 
  mutate(country = case_when(
    str_detect(country, "China") ~ "China",
    str_detect(country, "Korea") ~ "South Korea",
    TRUE ~ country
  )) |> 
  select(country, country_short, year, share_gdp) |> 
  arrange(country, year)

      # organize data

health_expenditures_recent <- health_expenditures |> 
  slice_max(order_by = year, n = 1, by = country) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, share_gdp))

      # select countries for plot

countries_to_keep <- c("United States", 
                       "Germany", 
                       "Austria",
                       "Switzerland",
                       "France", 
                       "Sweden",
                       "Canada",
                       "United Kingdom",
                       "Belgium",
                       "Japan",
                       "Finland",
                       "Portugal",
                       "New Zealand",
                       "Netherlands",
                       "Norway",
                       "Denmark",
                       "Spain",
                       "Iceland",
                       "Italy",
                       "South Korea",
                       "Greece",
                       "Ireland",
                       "Chile",
                       "China")

health_expenditures_plot <- health_expenditures_recent |>
  filter(country %in% countries_to_keep)

      # plot data

health_expenditures_png <- ggplot(
  data = health_expenditures_plot,
  aes(
    x = share_gdp,
    y = country,
    fill = country == "United States"
  )
) + # highlights US
  geom_col() +
  geom_text(aes(label = scales::number(share_gdp, accuracy = 0.1)), # rounds to one decimal
    hjust = -0.1, # moves text to right of bar
    size = 3,
    family = "inconsolata" # apply font to the geom_text labels too
  ) + # adjusts size of value label
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  # allow x access to expand to prevent labels from being cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Healthcare expenditures",
    subtitle = "In 2024",
    caption = "Source: OECD",
    x = "Share of GDP (%)",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text (size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
health_expenditures_png

      # save plot

ggsave(here("plots", "health_expenditures.png"), 
       plot = health_expenditures_png, 
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

# life expectancy (years at birth) set-up --------------------------------------

      # read data

life_expectancy <- read_csv(here("raw-data", "life_expectancy.csv")) |>
  clean_names() |>
filter(measure == "LFEXP") |>
  select(
    country_short = ref_area,
    country = reference_area,
    life_expectancy = obs_value,
    gender = sex,
    year = time_period
  ) |> 
  mutate(country = case_when(
    str_detect(country, "China") ~ "China",
    str_detect(country, "Korea") ~ "South Korea",
    TRUE ~ country
  )) |> 
  mutate(gender = recode(gender, "_T" = "T")) |> 
  select(country, country_short, gender, year, life_expectancy) |>
  arrange(country, gender, year)

# organize data
 
life_expectancy_T_recent <- life_expectancy |> 
  filter(gender == "T") |> 
  slice_max(order_by = year, n = 1, by = country) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, life_expectancy)) |> 
  arrange(desc(life_expectancy))

life_expectancy_F_recent <- life_expectancy |> 
  filter(gender == "F") |> 
  slice_max(order_by = year, n = 1, by = country) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, life_expectancy)) |> 
  arrange(desc(life_expectancy))

life_expectancy_M_recent <- life_expectancy |> 
  filter(gender == "M") |> 
  slice_max(order_by = year, n = 1, by = country) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, life_expectancy)) |> 
  arrange(desc(life_expectancy))

    # select countries for plot

countries_to_keep <- c(
"United States",
"Chile",
"Germany",
"Austria",
"Switzerland",
"France",
"Sweden",
"Canada",
"United Kingdom",
"Belgium",
"Japan",
"Finland",
"Portugal",
"New Zealand",
"Netherlands",
"Norway",
"Denmark",
"Spain",
"Iceland",
"Italy",
"South Korea",
"Greece",
"Ireland",
"China")

life_expectancy_T_recent_plot <- life_expectancy_T_recent |>
  filter(country %in% countries_to_keep)

life_expectancy_F_recent_plot <- life_expectancy_F_recent |>
  filter(country %in% countries_to_keep)

life_expectancy_M_recent_plot <- life_expectancy_M_recent |>
  filter(country %in% countries_to_keep)

# plot data

#     life expectancy for combined female and male population-------------------

life_expectancy_T_png <- ggplot(
  data = life_expectancy_T_recent_plot,
  aes(
    x = life_expectancy,
    y = country,
    fill = country == "United States"
  )
) + # highlights US
  geom_col() +
  # use aes(label = scales::comma(life_expectancy, accuracy = 0.1)) to
  # include comma as a separator for thousands
  geom_text(aes(label = scales::number(life_expectancy, accuracy = 0.1)), 
            hjust = -0.1,
            size = 3,
            family = "inconsolata") + 
  # adjusts size of value label
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  # allow x access to expand to prevent labels from being cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Life expectancy, total population",
    subtitle = "Most recent available year, 2020-2024",
    caption = "Source: OECD",
    x = "Years",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text (size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
life_expectancy_T_png

      # save plot

ggsave(here("plots", "life_expectancy_T.png"), 
       plot = life_expectancy_T_png, 
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

#     life expectancy for female population ----------------------------------------

life_expectancy_F_png <- ggplot(
  data = life_expectancy_F_recent_plot,
  aes(
    x = life_expectancy,
    y = country,
    fill = country == "United States"
  )
) + # highlights US
  geom_col() +
  # use aes(label = scales::comma(life_expectancy, accuracy = 0.1)) to
  # include comma as a separator for thousands
  geom_text(aes(label = scales::number(life_expectancy, accuracy = 0.1)), 
            hjust = -0.1,
            size = 3,
            family = "inconsolata") + 
  # adjusts size of value label
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  # allow x access to expand to prevent labels from being cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Life expectancy, female population",
    subtitle = "Most recent available year, 2020-2024",
    caption = "Source: OECD",
    x = "Years",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text (size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
life_expectancy_F_png

      # save plot

ggsave(here("plots", "life_expectancy_F.png"), 
       plot = life_expectancy_F_png, 
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

#     life expectancy for male population --------------------------------------
life_expectancy_M_png <- ggplot(
  data = life_expectancy_M_recent_plot,
  aes(
    x = life_expectancy,
    y = country,
    fill = country == "United States"
  )
) + # highlights US
  geom_col() +
  # use aes(label = scales::comma(life_expectancy, accuracy = 0.1)) to
  # include comma as a separator for thousands
  geom_text(aes(label = scales::number(life_expectancy, accuracy = 0.1)), 
            hjust = -0.1,
            size = 3,
            family = "inconsolata") + 
  # adjusts size of value label
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  # allow x access to expand to prevent labels from being cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Life expectancy, male population",
    subtitle = "In years, most recent available 2020-2024",
    caption = "Source: OECD",
    x = "Years",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text (size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
life_expectancy_M_png

# save plot

ggsave(here("plots", "life_expectancy_M.png"), 
       plot = life_expectancy_M_png, 
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

# low birth weight (percent of live births) ------------------------------------

      # read data

low_birth_weight <- read_csv(here("raw-data", "low_birth_weight.csv")) |>
  clean_names() |> 
  select(
    country_short = ref_area,
    country = reference_area,
    low_birth_weight = obs_value, # percentage of live births
    year = time_period
  ) |> 
  mutate(country = case_when(
    str_detect(country, "China") ~ "China",
    str_detect(country, "Korea") ~ "South Korea",
    TRUE ~ country
  )) |>
  select(country, country_short, year, low_birth_weight) |>
  arrange(country, year)

      # organize data

low_birth_weight_recent <- low_birth_weight |> 
  slice_max(order_by = year, n = 1, by = country) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, low_birth_weight)) |> 
  arrange(desc(low_birth_weight))

    # select countries for plot

countries_to_keep <- c(
"Australia",
"Austria",
"Belgium",
"Chile",
"China",
"Denmark",
"Finland",
"France",
"Germany",
"Greece",
"Iceland",
"Ireland",
"Italy",
"Japan",
"South Korea",
"Netherlands",
"New Zealand",
"Norway",
"Portugal",
"Spain",
"Sweden",
"Switzerland",
"United Kingdom",
"United States"
)

low_birth_weight_recent_plot <- low_birth_weight_recent |>
  filter(country %in% countries_to_keep) |>
  mutate(country = case_when(
    str_detect(country, "Germany") ~ "Germany (2013)",
    TRUE ~ country
  )) |>
  mutate(country = fct_reorder(country, low_birth_weight)) |>
  arrange(desc(low_birth_weight))

# plot data

low_birth_weight_png <- ggplot(
  data = low_birth_weight_recent_plot,
  aes(
    x = low_birth_weight,
    y = country,
    fill = country == "United States"
  )
) + # highlights US
  geom_col() +
  # use aes(label = scales::comma(low_birth_weight, accuracy = 0.1)) to
  # include comma as a separator for thousands
  geom_text(aes(label = scales::number(low_birth_weight, accuracy = 0.1)), 
            hjust = -0.1,
            size = 3,
            family = "inconsolata") + 
  # adjusts size of value label
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  # allow x access to expand to prevent labels from being cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Low birth weight",
    subtitle = "Most recent available year, 2020-2024",
    caption = "Source: OECD",
    x = "Percent of live births",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text (size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
low_birth_weight_png

      # save plot

ggsave(here("plots", "low_birth_weight.png"), 
       plot = low_birth_weight_png,
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

# infant_mortality (per 1,000 live births) -------------------------------------

      # read data

threshold_yes <- "Minimum threshold of 22 weeks (or 500 grams birthweight)"
threshold_no <- "No minimum threshold of gestation period or birthweight"

infant_mortality <- read_csv(here("raw-data", "infant_mortality.csv")) |>
  clean_names() |> 
  select(
    country_short = ref_area,
    country = reference_area,
    measure = measure,
    gestation = gestation_period_threshold, 
    # 1 Minimum threshold of 22 weeks (or 500 grams birthweight)   367
    # 2 No minimum threshold of gestation period or birthweight    706
    # 22 week threshold may improve international comparability
    # due to differences across countries in what constitutes a 
    # live birth versus a "still birth"
    infant_mortality = obs_value, # deaths per 1,000 live births
    year = time_period
  ) |>
  mutate(threshold = case_when(
    gestation == threshold_yes ~ 1,
    gestation == threshold_no  ~ 0,
    TRUE ~ NA_real_ # Assign NA (or another value) if it matches neither
  )) |> 
  mutate(country = case_when(
    str_detect(country, "China") ~ "China",
    str_detect(country, "Korea") ~ "South Korea",
    TRUE ~ country
  )) |>
  select(country, country_short, year, infant_mortality, threshold) |>
  arrange(threshold, country, year)

      # organize data

infant_mortality_threshold_recent <- infant_mortality |>
  filter(threshold == 1) |> 
  slice_max(order_by = year, n = 1, by = country) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, infant_mortality)) |> 
  arrange(desc(infant_mortality))

infant_mortality_nothreshold_recent <- infant_mortality |>
  filter(threshold == 0) |> 
  slice_max(order_by = year, n = 1, by = country) |> 
  ungroup() |> 
  mutate(country = fct_reorder(country, infant_mortality)) |> 
  arrange(desc(infant_mortality))

      # version with threshold applied

        # select countries for plot with threshold

countries_threshold <- c(
  "Chile",
  "Iceland",
  "Sweden",
  "Japan",
  "Finland",
  "Denmark",
  "Norway",
  "Austria",
  "South Korea",
  "Portugal",
  "Spain",
  "Belgium",
  "Switzerland",
  "Germany",
  "United Kingdom",
  "Netherlands",
  "New Zealand",
  "France",
  "United States"
)

infant_mortality_threshold_recent_plot <- infant_mortality_threshold_recent |>
  filter(country %in% countries_threshold)  |>
  mutate(country = case_when(
    str_detect(country, "Netherlands") ~ "Netherlands (2016)",
    str_detect(country, "Germany") ~ "Germany (2013)",
    str_detect(country, "United Kingdom") ~ "UK (2015)",
    str_detect(country, "Portugal") ~ "Portugal (2017)",
    TRUE ~ country
  ))  |> 
  mutate(country = fct_reorder(country, infant_mortality)) |> 
  arrange(desc(infant_mortality))

      # plot threshold version

infant_mortality_threshold_png <- ggplot(
  data = infant_mortality_threshold_recent_plot,
  aes(
    x = infant_mortality,
    y = country,
    fill = country == "United States"
  )
) + # highlights US
  geom_col() +
  # use aes(label = scales::comma(life_expectancy, accuracy = 0.1)) to
  # include comma as a separator for thousands
  geom_text(aes(label = scales::number(infant_mortality, accuracy = 0.1)),
    hjust = -0.1,
    size = 3,
    family = "inconsolata"
  ) +
  # adjusts size of value label
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  scale_x_continuous(
    # allow x access to expand to prevent labels from being cut off
    expand = expansion(mult = c(0, 0.1)),
    breaks = seq(0, 6, 1),
    limits = c(0, 6.2) # highest value is 6.1
  ) +
  labs(
    title = "Infant mortality",
    subtitle = "Most recent available year, 2020-2024",
    caption = "Source: OECD",
    x = "Per 1,000 live births",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
infant_mortality_threshold_png

ggsave(here("plots", "infant_mortality_threshold.png"), 
       plot = infant_mortality_threshold_png, 
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

      # version with threshold applied

          # select countries for plot with threshold

countries_nothreshold <- c(
  "Chile",
  "Iceland",
  "Finland",
  "Japan",
  "Norway",
  "Sweden",
  "Denmark",
  "Italy",
  "South Korea",
  "Spain",
  "Austria",
  "Ireland",
  "Portugal",
  "Belgium",
  "Australia",
  "Germany",
  "Switzerland",
  "Greece",
  "Netherlands",
  "France",
  "United Kingdom",
  "China",
  "Canada",
  "New Zealand",
  "United States"
)

infant_mortality_nothreshold_recent_plot <- infant_mortality_nothreshold_recent |>
  filter(country %in% countries_nothreshold)  |>
  mutate(country = case_when(
    str_detect(country, "Netherlands") ~ "Netherlands (2016)",
    str_detect(country, "Germany") ~ "Germany (2013)",
    str_detect(country, "United Kingdom") ~ "UK (2015)",
    str_detect(country, "Portugal") ~ "Portugal (2017)",
    TRUE ~ country
  )) |> 
  mutate(country = fct_reorder(country, infant_mortality)) |> 
  arrange(desc(infant_mortality))

      # plot threshold version

infant_mortality_nothreshold_png <- ggplot(
  data = infant_mortality_nothreshold_recent_plot,
  aes(
    x = infant_mortality,
    y = country,
    fill = country == "United States" # highlights US
  )
) +
  geom_col() +
  # use aes(label = scales::comma(infant_mortality, accuracy = 0.1)) to
  # include comma as a separator for thousands
  geom_text(aes(label = scales::number(infant_mortality, accuracy = 0.1)),
    hjust = -0.1,
    size = 3,   # adjusts size of value label
    family = "inconsolata"
  ) +
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  scale_x_continuous(
    # allow x access to expand to prevent labels from being cut off
    expand = expansion(mult = c(0, 0.1)),
    breaks = seq(0, 6, 1),
    limits = c(0, 6.2) # highest value is 6.1
  ) +
  labs(
    title = "Infant mortality",
    subtitle = "Most recent available year, 2020-2024",
    caption = "Source: OECD",
    x = "Per 1,000 live births",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
infant_mortality_nothreshold_png

ggsave(here("plots", "infant_mortality_nothreshold.png"), 
       plot = infant_mortality_nothreshold_png, 
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

# display both using patchwork
infant_mortality_nothreshold_png + infant_mortality_threshold_png

# maternal mortality (per 100,000 live births) oecd ----------------------------

# read data

maternal_mortality_oecd <- read_excel(
  path = here("raw-data", "bcqmh9.xlsx"),
  sheet = "g3-17",
  range = "A27:B74",
  col_names = FALSE
) |>
  set_names(c("country", "maternal_mortality")) |>
  # remove superscripts related to country-specific footnotes
  mutate(country = str_remove_all(country, "[\\p{No}*#]") |> str_trim()) |> 
  mutate(maternal_mortality = na_if(maternal_mortality, 0)) |>
  select(country, maternal_mortality) |> 
  arrange(desc(maternal_mortality))

# organize data

maternal_mortality_oecd_recent <- maternal_mortality_oecd |> 
  mutate(country = fct_reorder(country, maternal_mortality )) |> 
  arrange(desc(maternal_mortality))

# select countries for plot

countries_to_keep <- c(
  "Norway",
  "Iceland",
  "Australia",
  "Spain",
  "Japan",
  "Netherlands",
  "Germany",
  "Sweden",
  "Italy",
  "Denmark",
  "Belgium",
  "Ireland",
  "Austria",
  "New Zealand",
  "Switzerland",
  "Greece",
  "France",
  "South Korea",
  "Finland",
  "United Kingdom",
  "Canada",
  "Portugal",
  "Chile",
  "United States",
  "China"
)

maternal_mortality_plot <- maternal_mortality_oecd_recent |>
  filter(country %in% countries_nothreshold)

# plot data

maternal_mortality_png <- ggplot(
  data = maternal_mortality_plot,
  aes(
    x = maternal_mortality,
    y = country,
    fill = country == "United States"
  )
) + # highlights US
  geom_col() +
  # use aes(label = scales::comma(maternal_mortality, accuracy = 0.1)) to
  # include comma as a separator for thousands
  geom_text(aes(label = scales::number(maternal_mortality, accuracy = 0.1)),
            hjust = -0.1,
            size = 3,
            family = "inconsolata"
  ) +
  # adjusts size of value label
  scale_fill_manual(values = c("FALSE" = "#acc8e5", "TRUE" = "#6699CC")) +
  # allow x access to expand to prevent labels from being cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Maternal mortality",
    subtitle = "In 2020",
    caption = "Source: OECD",
    x = "Per 1,000 live births",
    y = NULL
  ) +
  theme_classic() +
  my_theme +
  theme(
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.position = "none", # Hide the legend
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    axis.text.x = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 11, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain")
  )
maternal_mortality_png

# save plot

ggsave(here("plots", "maternal_mortality.png"), 
       plot = maternal_mortality_png, 
       width = 6.4, 
       height = 6.9,
       units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web

#   # maternal mortality (who)
# 
# matmort <- "Maternal mortality ratio (per 100,000 live births)"
# 
# maternal_mortality_who <- read_csv(here("raw-data", "maternal_mortality_who.csv")) |>
#   filter(IndicatorName == matmort) |> 
#   clean_names() |> 
#   select(
#     country_short = location_code,
#     country = location,
#     maternal_mortality = numeric_value, 
#     # Maternal mortality ratio (per 100,000 live births)
#     year
#   ) |>
#   select(country, country_short, year, maternal_mortality) |>
#   arrange(country, year)

# patchwork plot ----------------------------------------------------------

early_look <- health_expenditures_png +
  life_expectancy_T_png +
  infant_mortality_threshold_png +
  maternal_mortality_png +
  plot_layout(ncol = 2)
early_look

ggsave(here("plots", "early_look.png"), 
       plot = early_look, 
       # width = 6.4, 
       # height = 6.9,
       # units = "in",
       # scale = 0.8,
       dpi = 600)    # High resolution for print/web
