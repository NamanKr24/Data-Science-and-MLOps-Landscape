# Loading the packages

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)

source("MCQ_Function.R")

options(scipen = 999)

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# Roles
  
      data1 <- data %>%
        select(Q23) %>%
        filter(!is.na(Q23)) %>%
        filter(Q23 != "Currently not employed") %>%
        filter(Q23 != "Other") %>%
        group_by(Q23) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = (Count / sum(Count) * 100)) %>%
        select(-2)
      
      data1 <- data1 %>%
        mutate(Q23 = ifelse(Q23 == "Currently not employed", "Not employed", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Engineer (non-software)", "Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Teacher / professor", "Teacher", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)",
                            "Data Analyst", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Machine Learning/ MLops Engineer",
                            "MLops Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Manager (Program, Project, Operations, Executive-level, etc)",
                            "Manager", Q23))

# Industries by hiring
    
      data2 <- data %>%
        select(Q24) %>%
        filter(!is.na(Q24)) %>%
        group_by(Q24) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = (Count / sum(Count) * 100)) %>%
        select(-Count)
      
      data2 <- data2 %>%
        mutate(Q24 = ifelse(Q24 == "Academics/Education", "Education", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Accounting/Finance", "Finance", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Broadcasting/Communications", "Communications", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Computers/Technology", "Technology", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Energy/Mining", "Energy", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Government/Public Service", "Government", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Insurance/Risk Assessment", "Insurance", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Manufacturing/Fabrication", "Manufacturing", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Marketing/CRM", "Marketing", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Medical/Pharmaceutical", "Medical", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Non-profit/Service", "Non-profit", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Online Service/Internet-based Services", "Online", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Retail/Sales", "Sales", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Shipping/Transportation", "Transportation", Q24))
      
      data2 <- data2 %>%
        arrange(desc(Percentage)) %>%
        mutate(angle = seq(0, 2 * pi, length.out = n() + 1)[1:n()],
               label_pos = Percentage + 5, hjust = ifelse(angle < pi, 0, 1),
               angle = ifelse(angle < pi, angle * 180 / pi - 90, angle * 180 / pi + 90))
      
      data2 <- data2 %>%
        filter(Q24 != "Other")
      
      data2$Q24 <- factor(data2$Q24, levels = c("Technology", "Education", "Finance", "Manufacturing",
                          "Medical", "Government", "Online", "Sales", "Energy",
                          "Insurance", "Marketing", "Non-profit", "Communications", "Transportation"))
      
# Industry by roles
    
      data3 <- data %>%
        select(Q23, Q24) %>%
        filter(!is.na(Q23)) %>%
        filter(!is.na(Q24)) %>%
        filter(Q23 != "Currently not employed") %>%
        group_by(Q24, Q23) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = (Count / sum(Count) * 100)) %>%
        select(-Count)
      
      data3 <- data3 %>%
        mutate(Q24 = ifelse(Q24 == "Academics/Education", "Education", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Accounting/Finance", "Finance", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Broadcasting/Communications", "Communications", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Computers/Technology", "Technology", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Energy/Mining", "Energy", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Government/Public Service", "Government", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Insurance/Risk Assessment", "Insurance", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Manufacturing/Fabrication", "Manufacturing", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Marketing/CRM", "Marketing", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Medical/Pharmaceutical", "Medical", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Non-profit/Service", "Non-profit", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Online Service/Internet-based Services", "Online", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Retail/Sales", "Sales", Q24)) %>%
        mutate(Q24 = ifelse(Q24 == "Shipping/Transportation", "Transportation", Q24))
      
      data3 <- data3 %>%
        mutate(Q23 = ifelse(Q23 == "Currently not employed", "Not employed", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Engineer (non-software)", "Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Teacher / professor", "Teacher", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)",
                            "Data Analyst", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Machine Learning/ MLops Engineer",
                            "MLops Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Manager (Program, Project, Operations, Executive-level, etc)",
                            "Manager", Q23))
      
# Roles by experience in ML
      
      data4 <- data %>%
        select(Q16, Q23) %>%
        filter(!is.na(Q23)) %>%
        filter(!is.na(Q16)) %>%
        filter(Q23 != "Currently not employed") %>%
        group_by(Q16, Q23) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = (Count / sum(Count) * 100)) %>%
        select(-Count)
      
      data4 <- data4 %>%
        mutate(Q23 = ifelse(Q23 == "Currently not employed", "Not employed", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Engineer (non-software)", "Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Teacher / professor", "Teacher", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)",
                            "Data Analyst", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Machine Learning/ MLops Engineer",
                            "MLops Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Manager (Program, Project, Operations, Executive-level, etc)",
                            "Manager", Q23))
      
      data4 <- data4 %>%
        filter(Q16 != "I do not use machine learning methods") %>%
        mutate(Q16 = ifelse(Q16 == "20 or more years",
                                   "20+ years", Q16)) %>%
        mutate(Q16 = ifelse(Q16 == "Under 1 year",
                                   "< 1 year", Q16))
      
      data4$Q16 <- factor(data4$Q16, levels = c("< 1 year", "1-2 years", "2-3 years",
                                                              "3-4 years", "4-5 years", "5-10 years", "10-20 years", "20+ years"))

# ML compensation vs Yearly compensation by roles
    
      data5 <- data %>%
        select(Q23, Q29, Q30) %>%
        filter(!is.na(Q23)) %>%
        filter(!is.na(Q29)) %>%
        filter(!is.na(Q30))
    
      data5 <- data5 %>%
        mutate(Q30 = ifelse(Q30 == "$0 ($USD)", "$0-0", Q30)) %>%
        mutate(Q30 = ifelse(Q30 == "$100,000 or more ($USD)", "$99,999-$100,000", Q30))
      
      data5 <- data5 %>%
        mutate(Q30 = str_replace_all(Q30, "\\$", "")) %>%
        mutate(Q30 = str_replace_all(Q30, ",", "")) %>%
        separate(Q30, into = c("min_c", "max_c"), sep = "-") %>%
        mutate(min_c = as.numeric(min_c), max_c = as.numeric(max_c)) %>%
        mutate(ML_Compensation = (min_c + max_c) / 2 + 0.5) %>%
        filter(!is.na(ML_Compensation)) %>%
        mutate(ML_Compensation = round(ML_Compensation, 0)) %>%
        select(1, 2, 5)
      
      data5 <- data5 %>%
        mutate(Q29 = ifelse(Q29 == ">$1,000,000", "$999,999-$1,000,000", Q29))
      
      data5 <- data5 %>%
        mutate(Q29 = str_replace_all(Q29, "\\$", "")) %>%
        mutate(Q29 = str_replace_all(Q29, ",", "")) %>%
        separate(Q29, into = c("min_c", "max_c"), sep = "-") %>%
        mutate(min_c = as.numeric(min_c), max_c = as.numeric(max_c)) %>%
        mutate(Annual_Compensation = (min_c + max_c) / 2 + 0.5) %>%
        filter(!is.na(Annual_Compensation)) %>%
        mutate(Annual_Compensation = round(Annual_Compensation, 0)) %>%
        select(1, 4, 5)

      data5 <- data5 %>%
        group_by(Q23) %>%
        summarise(Avg_Yearly = round(mean(Annual_Compensation), 0),
                  Avg_ML = round(mean(ML_Compensation) / 5, 0))

      data5 <- data5 %>%
        mutate(Q23 = ifelse(Q23 == "Currently not employed", "Not employed", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Engineer (non-software)", "Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Teacher / professor", "Teacher", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)",
                            "Data Analyst", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Machine Learning/ MLops Engineer",
                            "MLops Engineer", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Manager (Program, Project, Operations, Executive-level, etc)",
                            "Manager", Q23)) %>%
        rename(Role = Q23) %>%
        rename(Expenses = Avg_ML) %>%
        rename(Earnings = Avg_Yearly)
      
# ML state by roles
      
      selected_columns <- grep("Q28", names(data), value = TRUE)
      
      data_long3 <- data %>%
        select(all_of(selected_columns)) %>%
        pivot_longer(cols = everything(), names_to = "Options", values_to = "Selected")
      
      data_long3 <- data_long3 %>%
        filter(Options != "Q28_7") %>%
        filter(Options != "Q28_8")
      
      data_long4 <- data %>%
        select(Q23)
      
      data_long4 <- data_long4[rep(1:nrow(data_long4), each = 6), ]
      
      data6 <- bind_cols(data_long4, data_long3)
      
      data6 <- data6 %>%
        filter(!is.na(Q23)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        select(-Options)
      
      data6 <- data6 %>%
        mutate(Q23 = ifelse(Q23 == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)",
                            "Data Analyst", Q23)) %>%
        mutate(Q23 = ifelse(Q23 == "Machine Learning/ MLops Engineer",
                            "MLops Engineer", Q23))
      
      data6 <- data6 %>%
        filter(Q23 %in% c("Data Scientist", "Data Analyst", "Research Scientist", "MLops Engineer"))
      
      data6 <- data6 %>%
        group_by(Q23, Selected) %>%
        summarise(Count = n())
      
      data6 <- data6 %>%
        mutate(Selected = ifelse(Selected == "Do research that advances the state of the art of machine learning",
                                 "ML Research", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Experimentation and iteration to improve existing ML models",
                                 "Improving models", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Build and/or run a machine learning service that operationally improves my product or workflows",
                                 "Build ML service", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Build prototypes to explore applying machine learning to new areas",
                                 "Build prototypes", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Build and/or run the data infrastructure that my business uses for storing, analyzing, and operationalizing data",
                                 "Build data infrastructure", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Analyze and understand data to influence product or business decisions",
                                 "Analyze data", Selected))
      
      node_labels <- unique(c(data6$Q23, data6$Selected))
      
      data6$source_index <- match(data6$Q23, node_labels) - 1
      
      data6$target_index <- match(data6$Selected, node_labels) - 1