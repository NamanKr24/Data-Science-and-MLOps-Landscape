# Loading the packages

library(readr)
library(dplyr)
library(tidyr)

source("MCQ_Function.R")

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# Programming languages

      data1 <- handle_mcqs("Q12")
      
      data1 <- data1 %>%
        filter(Selected != "None")

# IDEs
      
      data2 <- handle_mcqs("Q13")
      
      data2 <- data2 %>%
        filter(Selected != "None") %>%
        filter(Selected != "nan") %>%
        filter(Selected != "Other")
      
      data2 <- data2 %>%
        mutate(Selected = ifelse(Selected == "Visual Studio Code (VSCode)", "VSCode", Selected))
      
# Programming languages by programming experience
      
      selected_columns <- grep("Q12", names(data), value = TRUE)
      
      data_long <- data %>%
        select(all_of(selected_columns)) %>%
        pivot_longer(cols = everything(), names_to = "Options", values_to = "Selected")
      
      data_long2 <- data %>%
        select(Q11)
      
      data_long2 <- data_long2[rep(1:nrow(data_long2), each = 15), ]
      
      data3 <- bind_cols(data_long2, data_long)
      
      data3 <- data3 %>%
        filter(!is.na(Q11)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        rename(Language = Selected) %>%
        rename(Experience = Q11) %>%
        select(-Options)
      
      data3 <- data3 %>%
        group_by(Language, Experience) %>%
        summarise(Count = n())
      
      total <- sum(data3$Count)
      
      data3 <- data3 %>%
        mutate(Percentage = (Count / total) * 100) %>%
        select(-Count)
      
      data3$Experience <- factor(data3$Experience, levels = c("< 1 years", "1-3 years",
                                "3-5 years", "5-10 years", "10-20 years", "20+ years"))

# Programming languages by ML experience

      data_long3 <- data %>%
        select(Q16)
      
      data_long3 <- data_long3[rep(1:nrow(data_long3), each = 15), ]
      
      data4 <- bind_cols(data_long3, data_long)
      
      data4 <- data4 %>%
        filter(!is.na(Q16)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        rename(Language = Selected) %>%
        rename(Experience = Q16) %>%
        select(-Options)
      
      data4 <- data4 %>%
        group_by(Language, Experience) %>%
        summarise(Count = n())
      
      total <- sum(data4$Count)
      
      data4 <- data4 %>%
        mutate(Percentage = (Count / total) * 100) %>%
        select(-Count)
      
      data4 <- data4 %>%
        filter(Experience != "I do not use machine learning methods") %>%
        mutate(Experience = ifelse(Experience == "20 or more years",
                                   "20+ years", Experience)) %>%
        mutate(Experience = ifelse(Experience == "Under 1 year",
                                   "< 1 year", Experience))
      
      data4$Experience <- factor(data4$Experience, levels = c("< 1 year", "1-2 years", "2-3 years",
                      "3-4 years", "4-5 years", "5-10 years", "10-20 years", "20+ years"))
      
# Programming languages by roles
      
      data_long4 <- data %>%
        select(Q23)
      
      data_long4 <- data_long4[rep(1:nrow(data_long4), each = 15), ]
      
      data5 <- bind_cols(data_long4, data_long)
      
      data5 <- data5 %>%
        filter(!is.na(Q23)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        rename(Language = Selected) %>%
        rename(Role = Q23) %>%
        select(-Options) %>%
        filter(Language != "None") %>%
        filter(Language != "Other") %>%
        filter(Role != "Currently not employed")
      
      data5 <- data5 %>%
        group_by(Role, Language) %>%
        summarise(Count = n()) 

      data_long5 <- data %>%
        select(Q23) %>%
        filter(!is.na(Q23)) %>%
        group_by(Q23) %>%
        summarise(Total = n()) %>%
        filter(Q23 != "Currently not employed") %>%
        rename(Role = Q23)

      data6 <- left_join(data5, data_long5, by = "Role")      

      data6 <- data6 %>%
        mutate(Proportion = round((Count / Total), 2)) %>%
        select(1, 2, 5)
        
      data6 <- data6 %>%
        mutate(Role = ifelse(Role == "Engineer (non-software)", "Engineer", Role)) %>%
        mutate(Role = ifelse(Role == "Teacher / professor", "Teacher", Role)) %>%
        mutate(Role = ifelse(Role == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)",
                             "Data Analyst", Role)) %>%
        mutate(Role = ifelse(Role == "Machine Learning/ MLops Engineer",
                             "MLops Engineer", Role)) %>%
        mutate(Role = ifelse(Role == "Manager (Program, Project, Operations, Executive-level, etc)",
                             "Manager", Role))