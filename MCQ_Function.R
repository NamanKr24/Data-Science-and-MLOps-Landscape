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

handle_mcqs2 <- function(QN1, QN2)
{
  pattern1 <- paste0("^", QN2, "_")
  pattern2 <- paste0("^", QN2)
  
  selected_columns1 <- grep(pattern1, names(data), value = TRUE)
  selected_columns2 <- grep(pattern2, names(data), value = TRUE)
  
  data_long1 <- data %>%
    select(all_of(selected_columns1)) %>%
    pivot_longer(cols = everything(), names_to = "Options1", values_to = "Selected1")
  data_long2 <- data %>%
    select(all_of(selected_columns2)) %>%
    pivot_longer(cols = everything(), names_to = "Options2", values_to = "Selected2")
  
  data_long1 <- data_long1 %>%
    filter(!is.na(Selected1))
  data_long2 <- data_long2 %>%
    filter(!is.na(Selected2))
  
  df_combined <- bind_cols(data_long1, data_long2)
  
  result <- df_combined %>%
    group_by(Selected1, Selected2) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / sum(Count)) * 100) %>%
    select(Selected1, Selected2, Percentage) %>%
    ungroup()
  
  return (result)
}