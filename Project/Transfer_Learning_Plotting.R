# Loading the packages

library(ggplot2)
library(forcats)

source("Transfer_Learning_Wrangling.R")

# Computer vision methods
    
      data1 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Popular computer vision methods",
             y = "Percentage", x = "Method") +
        scale_y_continuous(limits = c(0, 40)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "tan4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "darkslateblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "rosybrown3", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))

# NLP techniques
 
      data2 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Popular NLP techniques",
             y = "Percentage", x = "Technique") +
        scale_y_continuous(limits = c(0, 10)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "tan4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "darkslateblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "rosybrown3", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))

# Pre trained model weights

      data3 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Popular pre-trained model weights sources",
             y = "Percentage", x = "Platform") +
        scale_y_continuous(limits = c(0, 40)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "tan4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "darkslateblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "rosybrown3", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))