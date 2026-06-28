library(tidyverse)

#Reading the csv file
data <- read_csv("data/raw/caffeine_sensitivity_english_raw.csv")

#Get a summary of the data 
glimpse(data)

#Cleaning the data; changing variables into numeric ones and removing the first two rows
data_clean <- data %>%
  slice(-(1:2)) %>%
  filter(!if_all(starts_with("caff_intox"), is.na)) %>%
  mutate(across(
    .cols = c(gender, occupation, caff_sens, starts_with("caff_intox")),
    .fns = parse_number
  ))

#Saving the cleaned data set
write_csv(data_clean, "data/clean/caffeine_sensitivity_english_clean.csv")
