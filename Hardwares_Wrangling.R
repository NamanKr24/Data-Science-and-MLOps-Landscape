# Loading the packages

library(readr)
library(dplyr)
library(tidyr)

source("MCQ_Function.R")

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# Programming languages
  
      data1 <- handle_mcqs("Q42")
      
# No. of times of TPU usage
      
      data2 <- data %>%
        select(Q43) %>%
        filter(!is.na(Q43)) %>%
        group_by(Q43) %>%
        summarise(Count = n()) %>%
        filter(Q43 != "Never") %>%
        mutate(Percentage = round((Count / sum(Count) *100), 2)) %>%
        select(-Count)