# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(scales)

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
  filter(n >= 50) |> 
  arrange(desc(avg_enjoyment)) |> 
  drop_na(qcountry) |> 
  mutate(avg_enjoyment_two_digits = number(avg_enjoyment, accuracy = 0.01))

# Data Visualization ------------------------------------------------------

ggplot(data = avg_r_enjoyment,
       mapping = aes(x = avg_enjoyment,
                     y = qcountry,
                     label = avg_enjoyment_two_digits)) +
  geom_col(fill = "#6cabdd") +
  geom_text(hjust = 1.2,
            color = "white") +
  theme_minimal() +
  labs(title = "Average Enjoyment of R on a 5-Point Scale Among Users Around the World",
       subtitle = "Only countries with 50 or more responses included",
       y = NULL,
       x = NULL)
