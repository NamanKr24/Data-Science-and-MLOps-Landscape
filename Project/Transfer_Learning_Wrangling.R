# Loading the packages

library(readr)
library(dplyr)
library(tidyr)

source("MCQ_Function.R")

data <- read_csv("kaggle_survey_2022_responses.csv")

data <- data[2:23998, ]

# Image techniques

      data1 <- handle_mcqs("Q19")
      
      data1 <- data1 %>%
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

      data2 <- handle_mcqs("Q20") 

      data2 <- data2 %>%
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
   
      data3 <- handle_mcqs("Q21")
      
      data3 <- data3 %>%
        filter(Selected != "No, I do not download pre-trained model weights on a regular basis") %>%
        filter(Selected != "Other storage services (i.e. google drive)")