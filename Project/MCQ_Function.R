# Loading the packages

library(dplyr)
library(tidyr)

# Reading the data

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# Creating the function

handle_mcqs <- function(QN)
{
  pattern <- paste0("^", QN, "_")
  
  selected_columns <- grep(pattern, names(data), value = TRUE)
  
  data_long <- data %>%
    select(all_of(selected_columns)) %>%
    pivot_longer(cols = everything(), names_to = "Options", values_to = "Selected")
  
  data_long <- data_long %>%
    filter(!is.na(Selected))
  
  result <- data_long %>%
    group_by(Selected) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = round((Count / sum(Count)) * 100, 2)) %>%
    select(-2)
  
  return (result)
}
