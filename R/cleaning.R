library(tidyverse)
library(qualtRics)

caff_raw <- read_survey("data/raw/caffeine_sensitivity_english_raw.csv")

caff <- 
  caff_raw |> 
  select(PROLIFIC_PID,
         age, gender, occupation, caff_sens, caffeine_perday,
         starts_with("caff_intox")
         ) |> 
  mutate(across(starts_with("caff_intox"), parse_number),
         across(c("gender", "occupation", "caff_sens"), ~str_remove(.x, "^\\d.") |> str_squish())
         )

write_excel_csv(caff, "data/clean/caffeine_sensitivity_english_clean.csv")
