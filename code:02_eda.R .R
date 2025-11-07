library(tidyverse)
library(janitor)

# Read and clean data
df <- read_csv("data/health_dataset.csv") %>% clean_names()

# Ensure numeric columns
df <- df %>%
  mutate(
    obesity_rate = as.numeric(obesity_rate),
    mental_health_index = as.numeric(mental_health_index)
  )

# Confirm columns
cat("✅ Columns:", paste(names(df), collapse = ", "), "\n")
cat("✅ Rows:", nrow(df), "\n")

# Create output folder if missing
dir.create("output", showWarnings = FALSE)

# -----  Histogram of Obesity Rates -----
p1 <- ggplot(df, aes(x = obesity_rate)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Obesity Rates Across Countries") +
  xlab("Obesity Rate (%)") +
  ylab("Number of Countries")

ggsave("output/obesity_hist.png", p1, width = 6, height = 4)

# -----  Scatter plot: Obesity vs Mental Health -----
p2 <- ggplot(df, aes(x = obesity_rate, y = mental_health_index)) +
  geom_point(color = "darkblue", size = 2) +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Obesity Rate vs Mental Health Index") +
  xlab("Obesity Rate (%)") +
  ylab("Mental Health Index (Simulated)")
ggsave("output/obesity_vs_mental.png", p2, width = 6, height = 4)
cat("✅ EDA complete. Plots saved in 'output' folder.\n")
