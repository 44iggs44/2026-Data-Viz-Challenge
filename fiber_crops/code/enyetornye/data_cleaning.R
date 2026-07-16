


library(tidyverse)
library(readr)
library(stringr)

path <- "/Users/fraser103/Library/CloudStorage/OneDrive-WestVirginiaUniversity/Fame/Fame_fiber/master_data"

target_naics <- c("3131", "3132", "3133", "3141", "3149", "3252")

all_files <- list.files(
  path = path,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

fiber_files <- all_files[
  str_detect(
    basename(all_files),
    paste0("^\\d{4}\\.annual (", paste(target_naics, collapse = "|"), ")\\d* ")
  )
]

length(fiber_files)


data_1990_2025 <- rbindlist(
  lapply(fiber_files, function(x) {
    dt <- fread(x)
    dt[, source_file := basename(x)]
    dt
  }),
  fill = TRUE
)


state_data <- data_1990_2025[
  agglvl_code == 56
] %>%
  mutate(
    state = str_remove(area_title, " -- Statewide")
  )

county_data <- data_1990_2025[
  agglvl_code == 76
] %>%
  mutate(
    county = str_extract(area_title, "^[^,]+"),
    state = str_extract(area_title, "(?<=, ).*$")
  )


write_csv(
  data_1990_2025,
  file.path(path, "fiber_manufacturing_all.csv")
)





state_trends <- state_data %>%
  group_by(state, year, industry_code, industry_title) %>%
  summarise(
    employment = sum(annual_avg_emplvl, na.rm = TRUE),
    establishments = sum(annual_avg_estabs_count, na.rm = TRUE),
    avg_weekly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE),
    .groups = "drop"
  )


state_trends %>%
  filter(year %in% c(1990, 2025)) %>%
  group_by(year, state) %>%
  summarise(total_employment = sum(employment), .groups = "drop") %>%
  arrange(year, desc(total_employment))


library(tidyverse)

top_states <- state_trends %>%
  filter(year %in% c(1990, 2025)) %>%
  group_by(year, state) %>%
  summarise(
    total_employment = sum(employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  slice_max(total_employment, n = 15)

ggplot(
  top_states,
  aes(
    x = reorder(state, total_employment),
    y = total_employment
  )
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  labs(
    title = "Top States by Fiber Manufacturing Employment",
    x = "",
    y = "Employment"
  ) +
  theme_minimal()






#testing plots .. 
state_index <- state_trends %>%
  group_by(state, year) %>%
  summarise(
    employment = sum(employment, na.rm = TRUE),
    establishments = sum(establishments, na.rm = TRUE),
    avg_weekly_wage = mean(avg_weekly_wage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(state) %>%
  filter(any(year == 1990)) %>%
  mutate(
    base_employment = employment[year == 1990][1],
    base_establishments = establishments[year == 1990][1],
    base_wage = avg_weekly_wage[year == 1990][1],
    employment_index = employment / base_employment * 100,
    establishments_index = establishments / base_establishments * 100,
    wage_index = avg_weekly_wage / base_wage * 100
  ) %>%
  ungroup()


top_states_1990 <- state_index %>%
  filter(year == 1990) %>%
  arrange(desc(employment)) %>%
  slice_head(n = 10) %>%
  pull(state)

state_index %>%
  filter(state %in% top_states_1990) %>%
  select(state, year, employment_index, establishments_index, wage_index) %>%
  pivot_longer(
    cols = c(employment_index, establishments_index, wage_index),
    names_to = "measure",
    values_to = "index_value"
  ) %>%
  ggplot(aes(x = year, y = index_value, color = measure)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~state) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(
    title = "Fiber Manufacturing Change Over Time for Top 1990 States",
    subtitle = "Employment, establishments, and wages indexed to 1990 = 100",
    x = "Year",
    y = "Index, 1990 = 100",
    color = "Measure"
  ) +
  theme_minimal()




#seperately 

top_states <- state_trends %>%
  filter(year == 1990) %>%
  group_by(state) %>%
  summarise(
    employment = sum(employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(employment, n = 10) %>%
  pull(state)

state_trends %>%
  filter(state %in% top_states) %>%
  group_by(state, year) %>%
  summarise(
    employment = sum(employment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(year, employment, color = state)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~state) +
  theme_minimal() +
  labs(
    title = "Fiber Manufacturing Employment",
    x = "Year",
    y = "Employment"
  )



state_trends %>%
  filter(state %in% top_states) %>%
  group_by(state, year) %>%
  summarise(
    establishments = sum(establishments, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(year, establishments, color = state)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~state) +
  theme_minimal() +
  labs(
    title = "Fiber Manufacturing Establishments",
    x = "Year",
    y = "Establishments"
  )



state_trends %>%
  filter(state %in% top_states) %>%
  group_by(state, year) %>%
  summarise(
    avg_weekly_wage = mean(avg_weekly_wage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(year, avg_weekly_wage, color = state)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~state) +
  theme_minimal() +
  labs(
    title = "Fiber Manufacturing Weekly Wages",
    x = "Year",
    y = "Average Weekly Wage ($)"
  )






