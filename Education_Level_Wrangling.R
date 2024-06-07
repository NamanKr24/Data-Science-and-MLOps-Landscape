# Loading the packages

library(readr)
library(dplyr)
library(tidyr)

# Loading the data

data <- read_csv("kaggle_survey_2022_responses.csv")

options(scipen = 999)

data <- data[2:23998, ]

# Education level

    data1 <- data %>%
      select(Q8) %>%
      filter(!is.na(Q8)) %>%
      group_by(Q8) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = (Count / sum(Count)) * 100) %>%
      select(-2)
    
    data1 <- data1 %>%
      mutate(Q8 = ifelse(Q8 == "No formal education past high school", "High School", Q8)) %>%
      mutate(Q8 = ifelse(Q8 == "Some college/university study without earning a bachelor’s degree", "Some study", Q8)) %>%
      mutate(Q8 = ifelse(Q8 == "I prefer not to answer", "Unsaid", Q8))
  
# Education levels by Roles
  
    data2 <- data %>%
      filter(!is.na(Q8)) %>%
      filter(!is.na(Q23)) %>%
      select(Q8, Q23)
    
    data2 <- data2 %>%
      mutate(Q8 = ifelse(Q8 == "No formal education past high school", "High School", Q8)) %>%
      mutate(Q8 = ifelse(Q8 == "Some college/university study without earning a bachelor’s degree", "Some study", Q8)) %>%
      mutate(Q8 = ifelse(Q8 == "I prefer not to answer", "Unsaid", Q8)) %>%
      mutate(Q23 = ifelse(Q23 == "Currently not employed", "Not employed", Q23)) %>%
      mutate(Q23 = ifelse(Q23 == "Engineer (non-software)", "Engineer", Q23)) %>%
      mutate(Q23 = ifelse(Q23 == "Teacher / professor", "Teacher", Q23)) %>%
      mutate(Q23 = ifelse(Q23 == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)",
                          "Data Analyst", Q23)) %>%
      mutate(Q23 = ifelse(Q23 == "Machine Learning/ MLops Engineer",
                          "MLops Engineer", Q23)) %>%
      mutate(Q23 = ifelse(Q23 == "Manager (Program, Project, Operations, Executive-level, etc)",
                          "Manager", Q23))
    
    data2 <- data2 %>%
      group_by(Q8, Q23) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = (Count / sum(Count)) * 100) %>%
      select(Q8, Q23, Percentage) %>%
      ungroup()
      
# Education level by researches
    
    selected_columns <- grep("Q10", names(data), value = TRUE)
    
    data_long <- data %>%
      select(all_of(selected_columns)) %>%
      pivot_longer(cols = everything(), names_to = "Options", values_to = "Selected")
    
    data_long2 <- data %>%
      select(Q8)
    
    data_long2 <- data_long2[rep(1:nrow(data_long2), each = 3), ]
    
    data3 <- bind_cols(data_long2, data_long)
    
    data3 <- data3 %>%
      filter(!is.na(Q8)) %>%
      filter(!is.na(Options)) %>%
      filter(!is.na(Selected)) %>%
      select(-Options)
   
    data3 <- data3 %>%
      mutate(Selected = ifelse(Selected == "Yes, the research made use of machine learning as a tool (applied research)",
                               "Applied research", Selected)) %>%
      mutate(Selected = ifelse(Selected == "Yes, the research made advances related to some novel machine learning method (theoretical research)",
                         "Theoretical research", Selected)) %>%
      mutate(Q8 = ifelse(Q8 == "Professional doctorate", "P.D.", Q8)) %>%
      mutate(Q8 = ifelse(Q8 == "Master’s degree", "Master’s", Q8)) %>%
      mutate(Q8 = ifelse(Q8 == "Doctoral degree", "Doctoral", Q8))
    
    data3 <- data3 %>%
      group_by(Q8, Selected) %>%
      summarise(Count = n()) %>%
      rename(Education = Q8) %>%
      rename(Research = Selected)
    
    contingency_table <- xtabs(Count ~ Education + Research, data = data3)