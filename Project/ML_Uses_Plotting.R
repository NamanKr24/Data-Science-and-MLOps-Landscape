# Loading the packages

library(ggplot2)
library(forcats)

source("ML_Uses_Wrangling.R")

# Data visualization libraries

      data1 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Popular data visualization libraries",
             y = "Percentage", x = "Library") +
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

# ML frameworks
          
      data2 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Popular ML frameworks",
             y = "Percentage", x = "Framework") +
        scale_y_continuous(limits = c(0, 30)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "tan4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "darkslateblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "rosybrown3", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))

# ML algorithms
      
      data3 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Popular ML algorithms",
             y = "Percentage", x = "Algorithm") +
        scale_y_continuous(limits = c(0, 25)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "tan4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "darkslateblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "rosybrown3", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))

# ML algorithms by sectors
      
      data4 %>%
        ggplot (aes(x = fct_reorder(Algorithm, Percentage, .fun = mean, .desc = TRUE), y = Industry, size = Percentage, color = Industry)) +
        geom_point(alpha = 0.6, show.legend = FALSE) +
        labs(title = "Which algorithms are popular in industries?",
             y = "Industry", x = "Algorithm") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "tan4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "darkslateblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "rosybrown3", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1))