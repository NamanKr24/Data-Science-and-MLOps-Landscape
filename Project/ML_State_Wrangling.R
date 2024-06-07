# Loading the packages

library(readr)
library(dplyr)
library(tidyr)
library(reshape2)
library(fmsb)

source("MCQ_Function.R")

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# ML state in companies

      data1 <- data %>%
        select(Q27) %>%
        filter(!is.na(Q27)) %>%
        filter(Q27 != "I do not know") %>%
        group_by(Q27) %>%
        summarise(Count = n())
      
      data1 <- data1 %>%
        mutate(Q27 = ifelse(Q27 == "No (we do not use ML methods)", "Don't use", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We are exploring ML methods (and may one day put a model into production)",
                            "Exploring", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We have well established ML methods (i.e., models in production for more than 2 years)",
                            "Well-established", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We recently started using ML methods (i.e., models in production for less than 2 years)",
                            "Recently started", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We use ML methods for generating insights (but do not put working models into production)",
                            "Generating insights", Q27))
      
      data1 <- data1 %>%
        mutate(Percentage = Count / sum(Count) * 100) %>%
        select(-2)
      
      data1 <- data1 %>%
        arrange(desc(Percentage)) %>%
        mutate(fraction = Percentage / sum(Percentage),
               ymax = cumsum(fraction),
               ymin = c(0, head(ymax, n = -1)),
               labelPosition = (ymax + ymin) / 2,
               label = paste0(round(Percentage, 1), "%"))
      
# ML Activities

      data2 <- handle_mcqs("Q28")
      
      data2 <- data2 %>%
        filter(Selected != "Other") %>%
        filter(Selected != "None of these activities are an important part of my role at work")
      
      data2 <- data2 %>%
        mutate(Selected = ifelse(Selected == "Do research that advances the state of the art of machine learning",
                                 "Research to advance ML", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Experimentation and iteration to improve existing ML models",
                                 "Improving existing ML models", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Build and/or run a machine learning service that operationally improves my product or workflows",
                                 "Building ML service", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Build prototypes to explore applying machine learning to new areas",
                                 "Build prototypes for applying ML", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Build and/or run the data infrastructure that my business uses for storing, analyzing, and operationalizing data",
                                 "Building data infrastructures", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Analyze and understand data to influence product or business decisions",
                                 "Analyze and understand data", Selected))
      
      
# ML adoption by sectors

      selected_columns <- grep("Q28", names(data), value = TRUE)
      
      data_long <- data %>%
        select(all_of(selected_columns)) %>%
        pivot_longer(cols = everything(), names_to = "Options", values_to = "Selected")
      
      data_long2 <- data %>%
        select(Q24)
      
      data_long2 <- data_long2[rep(1:nrow(data_long2), each = 8), ]
      
      data3 <- bind_cols(data_long2, data_long)
      
      data3 <- data3 %>%
        filter(!is.na(Q24)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        select(-Options)
      
      data3 <- data3 %>%
        filter(Selected != "Other") %>%
        filter(Selected != "None of these activities are an important part of my role at work")
      
      data3 <- data3 %>%
        group_by(Q24, Selected) %>%
        summarise(Count = n())

      total <- sum(data3$Count)
      
      data3 <- data3 %>%
        mutate(Percentage = (Count / total) * 100) %>%
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

# ML team size by company size
     
      data4 <- data %>%
        select(Q25, Q26) %>%
        filter(!is.na(Q25)) %>%
        filter(!is.na(Q26)) %>%
        group_by(Q26, Q25) %>%
        summarise(Count = n())

      data4 <- data4 %>%
        mutate(Q25 = ifelse(Q25 == "0-49 employees", "0-49", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "50-249 employees", "50-249", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "250-999 employees", "250-999", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "1000-9,999 employees", "1000-9,999", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "10,000 or more employees", "10,000+", Q25))
      
      data4 <- data4 %>%
        mutate(Q26 = ifelse(Q26 == "1 to 2", "1-2", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "3 to 4", "3-4", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "5 to 9", "5-9", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "10 to 14", "10-14", Q26))
      
      data4 <- data4 %>%
        filter(Q26 != "0")
      
      data4$Q25 <- factor(data4$Q25, levels = c("0-49", "50-249", "250-999", "1000-9,999", "10,000+"))
      
      data4$Q26 <- factor(data4$Q26, levels = c("1-2", "3-4", "5-9", "10-14", "15-19", "20+"))
      
# ML adoption by ml team size
      
      data_long3 <- data %>%
        select(Q26)
      
      data_long3 <- data_long3[rep(1:nrow(data_long3), each = 8), ]
      
      data5 <- bind_cols(data_long3, data_long)
      
      data5 <- data5 %>%
        filter(!is.na(Q26)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        select(-Options)
      
      data5 <- data5 %>%
        filter(Selected != "Other") %>%
        filter(Selected != "None of these activities are an important part of my role at work")
      
      data5 <- data5 %>%
        group_by(Selected, Q26) %>%
        summarise(Count = n())
      
      total <- sum(data5$Count)
      
      data5 <- data5 %>%
        mutate(Percentage = round((Count / total) * 100, 2)) %>%
        select(-Count)

      data5 <- data5 %>%
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
      
      data5 <- data5 %>%
        mutate(Q26 = ifelse(Q26 == "1 to 2", "1-2", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "3 to 4", "3-4", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "5 to 9", "5-9", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "10 to 14", "10-14", Q26)) %>%
        filter(Q26 != "0")
      
      pivot_d1 <- dcast(data5, Q26 ~ Selected, value.var = "Percentage")
      
      pivot_d1 <- pivot_d1[, -1]
 
# ML state by company size by ML team size
      
      data6 <- data %>%
        select(Q27, Q25, Q26) %>%
        filter(!is.na(Q27)) %>%
        filter(!is.na(Q26)) %>%
        filter(!is.na(Q25)) %>%
        group_by(Q27, Q25)

      data6 <- data6 %>%
        filter(Q27 != "I do not know") %>%
        mutate(Q27 = ifelse(Q27 == "No (we do not use ML methods)", "Don't use", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We are exploring ML methods (and may one day put a model into production)",
                            "Exploring", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We have well established ML methods (i.e., models in production for more than 2 years)",
                            "Well-established", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We recently started using ML methods (i.e., models in production for less than 2 years)",
                            "Recently started", Q27)) %>%
        mutate(Q27 = ifelse(Q27 == "We use ML methods for generating insights (but do not put working models into production)",
                            "Generating insights", Q27))
      
      data6 <- data6 %>%
        mutate(Q25 = ifelse(Q25 == "0-49 employees", "0-49", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "50-249 employees", "50-249", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "250-999 employees", "250-999", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "1000-9,999 employees", "1000-9,999", Q25)) %>%
        mutate(Q25 = ifelse(Q25 == "10,000 or more employees", "10,000+", Q25))
      
      data6 <- data6 %>%
        mutate(Q26 = ifelse(Q26 == "1 to 2", "1-2", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "3 to 4", "3-4", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "5 to 9", "5-9", Q26)) %>%
        mutate(Q26 = ifelse(Q26 == "10 to 14", "10-14", Q26))
      
      data6 <- data6 %>%
        filter(Q27 != "Don't use") %>%
        filter(Q26 != "0")
      
      data6$Q26 <- factor(data6$Q26, levels = c("1-2", "3-4", "5-9", "10-14", "15-19", "20+"))

      data6$Q25 <- factor(data6$Q25, levels = c("0-49", "50-249", "250-999", "1000-9,999", "10,000+"))
      
      data6$Q27 <- factor(data6$Q27, levels = c("Generating insights", "Exploring", "Recently started", "Well-established"))