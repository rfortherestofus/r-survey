# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)

# Import Data -------------------------------------------------------------

survey_data <- read_tsv("2020-combined-survey-final.tsv") |> 
  clean_names()

survey_data |> 
  select(contains("enjoy"))

survey_data |> 
  filter(is.na(qr_enjoyment)) |> 
  select(qr_enjoyment)

survey_data |> 
  glimpse()

avg_r_enjoyment <- survey_data |> 
  drop_na(qr_enjoyment) |> 
  group_by(qcountry) |> 
  summarize(avg_enjoyment = mean(qr_enjoyment),
            n = n()) |> 
  filter(n >= 10) |> 
  arrange(desc(avg_enjoyment))

