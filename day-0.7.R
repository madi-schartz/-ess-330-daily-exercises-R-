# Name: [Madi Schartz]
# Date: [02/19/2025]
# Purpose: This script reads in COVID-19 data from the provided URL and processes it.
# Load necessary libraries
library(readr)

# URL for COVID-19 data
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv"


# Read the CSV file into a data frame
covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv")

# Check the first few rows to confirm it's loaded
head(covid)


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Identify the six states with the most current cases
top_states <- covid_data %>%
  arrange(desc(current_cases)) %>%
  slice_head(n = 6) %>%
  pull(state)

# Filter the raw data to include only the top 6 states
filtered_data <- covid_data %>%
  filter(state %in% top_states)

# Create a ggplot to visualize the data
plot <- ggplot(filtered_data, aes(x = state, y = current_cases, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 6 States with Most Current COVID-19 Cases",
       x = "State",
       y = "Number of Current Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot to the img directory
ggsave("img/top_states_covid_cases.png", plot, width = 8, height = 6)
