# Loading the packages

library(readr)
library(dplyr)
library(tidyr)

# Loading the data

data <- read_csv("kaggle_survey_2022_responses.csv")

source("MCQ_Function.R")

# Study platforms popularity

  # Manipulating the data
  
    data1 <- handle_mcqs("Q6")
    
    data1 <- data1 %>%
      transmute(Selected = ifelse(Selected == "Cloud-certification programs (direct from AWS, Azure, GCP, or similar)",
                                  "Cloud-certification programs", Selected), Percentage) %>%
      transmute(Selected = ifelse(Selected == "University Courses (resulting in a university degree)",
                                  "University Courses", Selected), Percentage) %>%
      transmute(Selected = ifelse(Selected == "Kaggle Learn courses",
                                  "Kaggle", Selected), Percentage)
    
# Study platforms helpfulness
    
    data2 <- handle_mcqs("Q7")
    
    data2 <- data2 %>%
      transmute(Selected = ifelse(Selected == "Kaggle (notebooks, competitions, etc)",
                                  "Kaggle", Selected), Percentage) %>%
      transmute(Selected = ifelse(Selected == "Online courses (Coursera, EdX, etc)",
                                  "Online courses", Selected), Percentage) %>%
      transmute(Selected = ifelse(Selected == "Video platforms (YouTube, Twitch, etc)",
                                  "Video platforms", Selected), Percentage) %>%
      transmute(Selected = ifelse(Selected == "None / I do not study data science",
                                  "None", Selected), Percentage) %>%
      transmute(Selected = ifelse(Selected == "Social media platforms (Reddit, Twitter, etc)",
                                  "Social media platforms", Selected), Percentage)