# Loading the packages

library(ggplot2)
library(forcats)

source("Hardwares_Wrangling.R")

# Programming languages

      data1 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Most popular hardware among the data science community",
             y = "Percentage", x = "Hardware") +
        scale_y_continuous(limits = c(0, 50)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "ivory", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "darkmagenta", size = 18, family = "serif", hjust = 1),
              axis.title.y = element_text(color = "blueviolet", size = 12, family = "serif"),
              axis.title.x = element_text(color = "darkcyan", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))
      
# No. of times of TPU usage

      data2 %>%
        ggplot (aes(x = reorder(Q43, Percentage), y = Percentage, fill = Q43)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Frequency of TPU usage", y = "Percentage", x = "Frequency") +
        scale_y_continuous(limits = c(0, 50)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "ivory", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "darkmagenta", size = 18, family = "serif"),
              axis.title.y = element_text(color = "blueviolet", size = 12, family = "serif"),
              axis.title.x = element_text(color = "darkcyan", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))