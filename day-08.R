# Madi Schartz
# 2/23/2025
# Purpose of script is to create a cumulative cases and deaths by USA region faceted plot.

# URL for COVID-19 data
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv"

# Read the CSV file into a data frame
covid_data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv")

# Create a data frame with region, state abbreviation, and state name
state_info <- data.frame(
  state_abb = state.abb,
  state_name = state.name,
  region = state.region)

# View the first few rows to check if everything is correct
head(state_info)

# Join state information to the COVID-19 data
covid_data_with_region <- covid_data %>%
  left_join(state_info, by = c("state" = "state_name"))

# View the joined data to ensure it's correct
head(covid_data_with_region)

# Calculate daily and cumulative cases and deaths
covid_data_cumulative <- covid_data_with_region %>%
  group_by(region, date) %>%
  mutate(
    cumulative_cases = cumsum(cases),
    cumulative_deaths = cumsum(deaths)) %>%
  ungroup()

# Pivot the data to long format
covid_data_long <- covid_data_cumulative %>%
  pivot_longer(
    cols = c(cumulative_cases, cumulative_deaths),
    names_to = "metric",
    values_to = "count")

# View the long format data
head(covid_data_long)

# Create the plot
ggplot(covid_data_long, aes(x = as.Date(date), y = count)) +
  geom_line(col = 'gray80') +
  geom_point(aes(col = region)) +
  facet_grid(metric ~ region, scales = "free_y") +
  labs(
    x = "Date",
    y = "Cumulative Count",
    title = "Cumulative COVID-19 Cases & Deaths by USA Region",
    subtitle = "Tracking Cases & Deaths over Time",
    caption = "Data: COVID-19 Dataset"
  ) +
  theme_linedraw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, face = "bold")) +
  theme(plot.subtitle = element_text(color = "navy", face = "bold")) +
  theme(plot.caption = element_text(color = "gray50", face = "italic"))

# Save the plot to a file
ggsave("img/covid_cases_deaths_by_region.png", width = 10, height = 8)
