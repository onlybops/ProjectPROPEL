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
  "both" = "#E76F51",
  "cats" = "#8AC926",
  "dogs" = "#2EC4B6",
  "no pets" = "#B388EB"
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



#box plot showing the price by acceptance 
# Step 1: Clean the 'pets' column
pets <- pets %>%
  mutate(pets_clean = sub("^\\([a-z]\\)\\s*", "", pets))

# Step 2: Filter data to include only listings priced between $0 and $7000
pets_filtered <- pets %>%
  filter(price >= 0 & price <= 7000)

# Step 3: Calculate percentages for each cleaned pet acceptance group
pet_percentages <- pets_filtered %>%
  count(pets_clean) %>%
  mutate(percentage = round(n / sum(n) * 100, 1),
         pets_label = paste0(pets_clean, " (", percentage, "%)"))

# Step 4: Merge percentage labels back into main filtered data
pets_filtered <- pets_filtered %>%
  left_join(pet_percentages, by = "pets_clean")

# Step 5: Create box plot
ggplot(pets_filtered, aes(x = pets_label, y = price, fill = pets_label)) +
  geom_boxplot() +
  labs(
    title = "Listing Price by Pet Acceptance", #(Up to $7,000)
    x = "Pet Acceptance",
    y = "Listing Price (USD)",
    fill = "Pets Allowed"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar, limits = c(0, 7000)) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )




