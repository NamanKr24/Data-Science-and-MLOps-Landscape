# Loading the packages

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)

# Reading the data

data <- read_csv("kaggle_survey_2022_responses.csv")

options(scipen = 999)

# Manipulating the data

  data <- data[2:23998, ]
  
  data <- data %>%
            select(Q29, Q23:Q25, Q4, Q8, Q11, Q16) %>%
            filter(!is.na(Q29))
  
  data <- data %>%
    mutate(Q29 = str_replace_all(Q29, "\\$", "")) %>%
    mutate(Q29 = str_replace_all(Q29, ",", "")) %>%
    mutate(Q29 = str_replace_all(Q29, ">", "")) %>%
    separate(Q29, into = c("min_c", "max_c"), sep = "-") %>%
    mutate(min_c = as.numeric(min_c), max_c = as.numeric(max_c)) %>%
    mutate(Mean_Compensation = (min_c + max_c) / 2 + 0.5)
  
  data <- data %>%
    filter(!is.na(Mean_Compensation)) %>%
    select(Q23:Mean_Compensation)
  
# Yearly compensation by Roles
    
      data1 <- data %>%
        select(Q23, Mean_Compensation) %>%
        filter(!is.na(Q23))
      
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
      
      data1 <- data1 %>%
        filter(Q23 != "Other")

# Yearly compensation by Roles
      
      data2 <- data %>%
        select(Q24, Mean_Compensation) %>%
        filter(!is.na(Q24))
      
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
        filter(Q24 != "Other")
      
# Yearly compensation by education level
      
      data3 <- data %>%
        select(Q8, Mean_Compensation) %>%
        filter(!is.na(Q8))
      
      data3 <- data3 %>%
        mutate(Q8 = ifelse(Q8 == "No formal education past high school", "High School", Q8)) %>%
        mutate(Q8 = ifelse(Q8 == "Some college/university study without earning a bachelor’s degree", "Some study", Q8)) %>%
        mutate(Q8 = ifelse(Q8 == "I prefer not to answer", "Unsaid", Q8))

# Yearly compensation by Company size
      
      data4 <- data %>%
        select(Q25, Mean_Compensation) %>%
        filter(!is.na(Q25))
      
      data4 <- data4 %>%
        mutate(Q25 = str_remove_all(Q25, "employees")) %>%
        mutate(Q25 = str_replace_all(Q25, " or more", "+"))
      
# Yearly compensation by continents
      
      data5 <- data %>%
        select(Q4, Mean_Compensation) %>%
        filter(!is.na(Q4))
      
      data5$continent <- countrycode(data5$Q4, "country.name", "continent")
      
      data5 <- data5 %>%
        select(-Q4)

# Yearly compensation by programming and machine learning experiences
      
      data6 <- data %>%
        filter(!is.na(Q11)) %>%
        filter(!is.na(Q16)) %>%
        filter(Q16 != "I do not use machine learning methods") %>%
        select(Q11, Q16, Mean_Compensation) %>%
        rename(Programming = Q11) %>%
        rename(ML = Q16) %>%
        group_by(Programming, ML) %>%
        summarise(Compensation = mean(Mean_Compensation))

      data6$Programming <- factor(data6$Programming, levels = c("< 1 years", "1-3 years",
                                                            "3-5 years", "5-10 years", "10-20 years", "20+ years"))
      
      data6 <- data6 %>%
        mutate(ML = ifelse(ML == "20 or more years",
                                   "20+ years", ML)) %>%
        mutate(ML = ifelse(ML == "Under 1 year",
                                   "< 1 year", ML))
      
      data6$ML <- factor(data6$ML, levels = c("< 1 year", "1-2 years", "2-3 years",
                                                              "3-4 years", "4-5 years", "5-10 years", "10-20 years", "20+ years"))