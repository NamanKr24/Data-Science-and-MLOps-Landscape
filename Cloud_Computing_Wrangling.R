# Loading the packages

library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# Cloud computing platforms by the sectors

  # Data manipulation
      
      selected_columns <- grep("Q31", names(data), value = TRUE)
      
      data_long <- data %>%
        select(all_of(selected_columns)) %>%
        pivot_longer(cols = everything(), names_to = "Options", values_to = "Selected")
      
      data_long2 <- data %>%
        select(Q24)
      
      data_long2 <- data_long2[rep(1:nrow(data_long2), each = 12), ]
      
      data1 <- bind_cols(data_long2, data_long)
      
      data1 <- data1 %>%
        filter(!is.na(Q24)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        rename(Platform = Selected) %>%
        rename(Industry = Q24) %>%
        select(-Options)
      
      data1 <- data1 %>%
        filter(Industry != "None") %>%
        filter(Industry != "Other")
      
      data1 <- data1 %>%
        mutate(Industry = ifelse(Industry == "Academics/Education", "Education", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Accounting/Finance", "Finance", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Broadcasting/Communications", "Communications", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Computers/Technology", "Technology", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Energy/Mining", "Energy", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Government/Public Service", "Government", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Insurance/Risk Assessment", "Insurance", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Manufacturing/Fabrication", "Manufacturing", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Marketing/CRM", "Marketing", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Medical/Pharmaceutical", "Medical", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Non-profit/Service", "Non-profit", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Online Service/Internet-based Services", "Online", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Retail/Sales", "Sales", Industry)) %>%
        mutate(Industry = ifelse(Industry == "Shipping/Transportation", "Transportation", Industry))
      
      data1 <- data1 %>%
        group_by(Industry, Platform) %>%
        summarise(Count = n())
      
      total <- sum(data1$Count)
      
      data1 <- data1 %>%
        mutate(Percentage = (Count / total) * 100) %>%
        select(-Count)

# Cloud computing platforms by countries
      
      data_long3 <- data %>%
        select(Q4)
      
      data_long3 <- data_long3[rep(1:nrow(data_long3), each = 12), ]
      
      data2 <- bind_cols(data_long3, data_long)
      
      data2 <- data2 %>%
        filter(!is.na(Q4)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        select(-Options)
      
      data2 <- data2 %>%
        group_by(Q4, Selected) %>%
        summarise(Count = n())
      
      data2 <- data2 %>%
        filter(Selected != "None")
      
      data2 <- data2 %>%
        group_by(Q4) %>%
        arrange(desc(Count))  %>%
        slice(1)  %>%
        ungroup()
      
      data2 <- data2 %>%
        rename(country = Q4) %>%
        rename(Platform = Selected) %>%
        select(-3) %>%
        mutate(country = str_to_lower(country))
      
      world <- ne_countries(scale = "medium", returnclass = "sf") %>%
        mutate(country = str_to_lower(admin))
      
      world_data <- left_join(world, data2, by = "country")