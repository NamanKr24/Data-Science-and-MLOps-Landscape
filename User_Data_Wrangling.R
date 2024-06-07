# Loading the packages

library(readr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

# Reading the data

data <- read_csv("kaggle_survey_2022_responses.csv")

# Manipulating the data

data <- data[2:23998, ]

data <- data %>%
          select(Q2, Q3, Q4, Q5)

# Users as students

        data1 <- data %>%
                  group_by(Q5) %>%
                  summarise(percentage = (n()/nrow(data)) * 100) %>%
                  transmute(Q5 = ifelse(Q5 == "Yes", "Student", "Not student"), percentage)
        
        data1$Q5 <- factor(data1$Q5, levels = c("Student", "Not student"))

# Users by gender

        data2 <- data %>%
          transmute(Q3 = ifelse(Q3 %in% c("Nonbinary",
                              "Prefer not to say", "Prefer to self-describe"), "Others", Q3))  
          
        data2 <- data2 %>%
          group_by(Q3) %>%
          summarise(percentage = (n()/nrow(data)) * 100) %>%
          rename(Gender = Q3)
        
        data2$Gender <- factor(data2$Gender, levels = c("Man", "Woman", "Others"))

# Gender by students
        
        data3 <- data %>%
          filter(!is.na(Q3))
        
        data3 <- data3 %>%
          group_by(Q5, Q3) %>%
          summarise(percentage = (n()/nrow(data)) * 100) %>%
          rename(Gender = Q3, Status = Q5) %>%
          transmute(Status = ifelse(Status == "Yes", "Student", "Not student"), Gender, percentage)
          
# Users by their origin
        
        data4 <- data %>%
          group_by(Q4) %>%
          summarise(Users = n()) %>%
          rename(country = Q4) %>%
          transmute(country = str_to_lower(country), Users = Users)
        
        world <- ne_countries(scale = "medium", returnclass = "sf") %>%
                  mutate(country = str_to_lower(admin))
        
        world_data <- left_join(world, data4, by = "country")