# Loading the packages

library(readr)
library(dplyr)
library(tidyr)

source("MCQ_Function.R")

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# Data visualization libraries

      data1 <- handle_mcqs("Q15")
      
      data1 <- data1 %>%
        filter(Selected != "None")  %>%
        filter(Selected != "Other")

# ML frameworks

      data2 <- handle_mcqs("Q17")
      
      data2 <- data2 %>%
        filter(Selected != "None")  %>%
        filter(Selected != "Other")

# ML algorithms

      data3 <- handle_mcqs("Q18")
      
      data3 <- data3 %>%
        filter(Selected != "None") %>%
        filter(Selected != "Other") %>%
        mutate(Selected = ifelse(Selected == "Autoencoder Networks (DAE, VAE, etc)",
                                 "Autoencoder Networks", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Dense Neural Networks (MLPs, etc)",
                                 "Dense Neural Networks", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Gradient Boosting Machines (xgboost, lightgbm, etc)",
                                 "Gradient Boosting Machines", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Transformer Networks (BERT, gpt-3, etc)",
                                 "Transformer Networks", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Decision Trees or Random Forests",
                                 "Decision Trees / Random Forests", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Linear or Logistic Regression",
                                 "Linear / Logistic Regression", Selected))

# ML algorithms by sectors

      selected_columns <- grep("Q18", names(data), value = TRUE)
      
      data_long <- data %>%
        select(all_of(selected_columns)) %>%
        pivot_longer(cols = everything(), names_to = "Options", values_to = "Selected")
      
      data_long2 <- data %>%
        select(Q24)
      
      data_long2 <- data_long2[rep(1:nrow(data_long2), each = 14), ]
      
      data4 <- bind_cols(data_long2, data_long)
      
      data4 <- data4 %>%
        filter(!is.na(Q24)) %>%
        filter(!is.na(Options)) %>%
        filter(!is.na(Selected)) %>%
        rename(Algorithm = Selected) %>%
        rename(Industry = Q24) %>%
        select(-Options)
      
      data4 <- data4 %>%
        filter(Industry != "None") %>%
        filter(Industry != "Other") %>%
        filter(Algorithm != "None") %>%
        filter(Algorithm != "Other")
      
      data4 <- data4 %>%
        mutate(Algorithm = ifelse(Algorithm == "Autoencoder Networks (DAE, VAE, etc)",
                                 "Autoencoder Networks", Algorithm)) %>%
        mutate(Algorithm = ifelse(Algorithm == "Dense Neural Networks (MLPs, etc)",
                                 "Dense Neural Networks", Algorithm)) %>%
        mutate(Algorithm = ifelse(Algorithm == "Gradient Boosting Machines (xgboost, lightgbm, etc)",
                                 "Gradient Boosting Machines", Algorithm)) %>%
        mutate(Algorithm = ifelse(Algorithm == "Transformer Networks (BERT, gpt-3, etc)",
                                 "Transformer Networks", Algorithm)) %>%
        mutate(Algorithm = ifelse(Algorithm == "Decision Trees or Random Forests",
                                 "Decision Trees / Random Forests", Algorithm)) %>%
        mutate(Algorithm = ifelse(Algorithm == "Linear or Logistic Regression",
                                 "Linear / Logistic Regression", Algorithm))
      
      data4 <- data4 %>%
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
      
      data4 <- data4 %>%
        group_by(Industry, Algorithm) %>%
        summarise(Count = n())
      
      total <- sum(data4$Count)
      
      data4 <- data4 %>%
        mutate(Percentage = (Count / total) * 100) %>%
        select(-Count)
 
# Image techniques

      data5 <- handle_mcqs("Q19")
      
      data5 <- data5 %>%
        filter(Selected != "None")  %>%
        filter(Selected != "Other") %>%
        mutate(Selected = ifelse(Selected == "Vision transformer networks (ViT, DeiT, BiT, BEiT, Swin, etc)",
                                 "Vision transformer networks", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Object detection methods (YOLOv6, RetinaNet, etc)",
                                 "Object detection methods", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Image segmentation methods (U-Net, Mask R-CNN, etc)",
                                 "Image segmentation methods", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Generative Networks (GAN, VAE, etc)",
                                 "Generative Networks", Selected)) %>%
        mutate(Selected = ifelse(Selected == "General purpose image/video tools (PIL, cv2, skimage, etc)",
                                 "General purpose image/video tools", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Image classification and other general purpose networks (VGG, Inception, ResNet, ResNeXt, NASNet, EfficientNet, etc)",
                                 "Image classification", Selected))

# NLP techniques

      data6 <- handle_mcqs("Q20") 

      data6 <- data6 %>%
        filter(Selected != "nan") %>%
        filter(Selected != "None")  %>%
        filter(Selected != "Other") %>%
        mutate(Selected = ifelse(Selected == "Contextualized embeddings (ELMo, CoVe)",
                                 "Contextualized embeddings", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Word embeddings/vectors (GLoVe, fastText, word2vec)",
                                 "Word embeddings", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Encoder-decoder models (seq2seq, vanilla transformers)",
                                 "Encoder-decoder models", Selected)) %>%
        mutate(Selected = ifelse(Selected == "Transformer language models (GPT-3, BERT, XLnet, etc)",
                                 "Transformer language models", Selected))

# Pre trained model weights
   
      data7 <- handle_mcqs("Q21")
      
      data7 <- data7 %>%
        filter(Selected != "No, I do not download pre-trained model weights on a regular basis") %>%
        filter(Selected != "Other storage services (i.e. google drive)")