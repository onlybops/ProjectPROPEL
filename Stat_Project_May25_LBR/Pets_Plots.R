# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# Read the data
pets <- read.csv("sf_clean.csv")

# View column names and sample data
str(pets)
head(pets)

# Use original data frame named 'pets'
pets <- pets %>%
  mutate(pets_clean = sub("^\\([a-z]\\)\\s*", "", pets))

pet_colors <- c(
  "both" = "#1f77b4",
  "cats" = "#ff7f0e",
  "dogs" = "#2ca02c",
  "no pets" = "#d62728"
)

# Bar plot of pet policies
ggplot(pets, aes(x = pets_clean, fill = pets_clean)) +
  geom_bar() +
  scale_fill_manual(values = pet_colors) +
  theme_minimal() +
  labs(title = "Number of Listings by Pet Policy",
       x = "Pet Policy",
       y = "Count of Listings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summarize counts and calculate percentages
pet_summary_clean <- pets %>%
  count(pets_clean) %>%
  mutate(percentage = round(100 * n / sum(n), 1),
         label = paste0(percentage, "%"))

# Create pie chart with labels and legend
ggplot(pet_summary_clean, aes(x = "", y = n, fill = pets_clean)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = pet_colors) +
  labs(title = "Pet Policy Distribution", fill = "Pet Policy") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )


# Convert 'pets' to a factor if it's not already
pets$pets <- as.factor(pets$pets)

# Box plot for price by pet acceptance
ggplot(pets, aes(x = pets, y = price, fill = pets)) +
  geom_boxplot() +
  labs(
    title = "Listing Price by Pet Acceptance",
    x = "Pet Acceptance",
    y = "Listing Price (USD)",
    fill = "Pets Allowed"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)





