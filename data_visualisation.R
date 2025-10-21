library(ggplot2)
library(tidyverse)
library(readr)

url <- "https://raw.githubusercontent.com/RobbenWijanathan/drug-consumption-regression/main/drug_consumption.csv"
data <- read_csv(url)

head(data)

library(tidyverse)

# Convert to long format first
data_long <- data %>%
  pivot_longer(
    cols = c(Alcohol, Amphet, Amyl, Benzos, Caff, Cannabis, Choc, Coke,
             Crack, Ecstasy, Heroin, Ketamine, Legalh, LSD, Meth,
             Mushrooms, Nicotine, Semer, VSA),
    names_to = "Drug",
    values_to = "Usage"
  )

# Recode CL0 = "No", others = "Yes"
data_long <- data_long %>%
  mutate(UsageBinary = ifelse(Usage == "CL0", "No", "Yes"))

# Summarize proportion of users per ethnicity for each drug
summary_data <- data_long %>%
  group_by(Ethnicity, Drug) %>%
  summarise(
    percent_yes = mean(UsageBinary == "Yes") * 100,
    .groups = "drop"
  )

# Plot as a heatmap
ggplot(summary_data, aes(x = Drug, y = Ethnicity, fill = percent_yes)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(
    title = "Percentage of Drug Users by Ethnicity",
    x = "Drug",
    y = "Ethnicity",
    fill = "% Yes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

