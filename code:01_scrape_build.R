# -------------------------------
# Seminar 1 - Web Scraping Script
# -------------------------------

library(rvest)
library(dplyr)
library(readr)
library(janitor)
library(stringr)

# Step 1: read page
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_obesity_rate"
page <- read_html(url)

# Step 2: extract all tables
tables <- html_table(page, fill = TRUE)
cat("Tables found on page:", length(tables), "\n")

# Step 3: choose table with 'country' + 'obesity' columns
candidate_idx <- which(sapply(tables, function(tbl) {
  nms <- tolower(names(tbl))
  has_country <- any(grepl("country|region|nation", nms))
  has_obesity <- any(grepl("obes|estimate|percent|rate|prevalence", nms))
  has_country && has_obesity
}))

if (length(candidate_idx) == 0) {
  sel_idx <- 1
  cat("No match found, using table 1\n")
} else {
  sel_idx <- candidate_idx[1]
  cat("Selected table:", sel_idx, "\n")
}

tbl_raw <- tables[[sel_idx]]

# Step 4: clean column names
tbl <- tbl_raw %>% clean_names()

# Step 5: identify the right columns
country_col <- names(tbl)[grepl("country|region|nation", names(tbl))]
obesity_col <- names(tbl)[grepl("obes|estimate|percent|rate|prevalence", names(tbl))]

if (length(country_col) == 0) {
  country_col <- names(tbl)[1]
  cat("No 'country' column found; using first column:", country_col, "\n")
}
if (length(obesity_col) == 0) {
  obesity_col <- names(tbl)[2]
  cat("No 'obesity' column found; using second column:", obesity_col, "\n")
}

# Step 6: rename and clean
tbl <- tbl %>%
  rename(Country = all_of(country_col),
         ObesityRate = all_of(obesity_col)) %>%
  mutate(
    Country = str_to_title(as.character(Country)),
    ObesityRate = parse_number(as.character(ObesityRate))
  ) %>%
  filter(!is.na(ObesityRate)) %>%    # remove "Unknown" or NA rows
  select(Country, ObesityRate) %>%
  distinct()


# Step 7: add mental health index
mental_health <- data.frame(
  Country = tbl$Country,
  MentalHealthIndex = runif(nrow(tbl), 30, 80)
)

# Step 8: merge
final_data <- tbl %>%
  left_join(mental_health, by = "Country") %>%
  drop_na() %>%
  distinct()

# Step 9: save
dir.create("data", showWarnings = FALSE)
write_csv(final_data, "data/health_dataset.csv")

cat("✅ Saved data/health_dataset.csv with", nrow(final_data), "rows\n")

                                            