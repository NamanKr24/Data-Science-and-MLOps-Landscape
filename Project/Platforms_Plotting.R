# Loading the packages

library(ggplot2)

source("Platforms_Wrangling.R")

# Study platforms popularity

    data1 %>%
      ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
      geom_bar(stat = "identity", width = 0.25, show.legend = FALSE) +
      coord_flip() +
      labs(title = "Popular platforms to study data science",
           y = "Percentage", x = "Platform") +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                vjust = -0.5, size = 4,
                position = position_dodge(width = 0.9)) +
      scale_y_continuous(limits = c(0, 25)) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "snow2", color = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            plot.title = element_text(color = "darkgoldenrod", size = 18, family = "serif"),
            axis.title.y = element_text(color = "forestgreen", size = 12, family = "serif"),
            axis.title.x = element_text(color = "mediumorchid", size = 12, family = "serif"),
            axis.text = element_text(color =  "black", face = "bold", family = "serif"))
    
# Study platforms helpfulness
 
    data2 %>%
      ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
      geom_bar(stat = "identity", width = 0.25, show.legend = FALSE) +
      coord_flip() +
      labs(title = "Most helpful resources for data science",
           y = "Percentage", x = "Platform") +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                vjust = -0.5, size = 4,
                position = position_dodge(width = 0.9)) +
      scale_y_continuous(limits = c(0, 30)) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "snow2", color = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            plot.title = element_text(color = "darkgoldenrod", size = 18, family = "serif"),
            axis.title.y = element_text(color = "forestgreen", size = 12, family = "serif"),
            axis.title.x = element_text(color = "mediumorchid", size = 12, family = "serif"),
            axis.text = element_text(color =  "black", face = "bold", family = "serif"))
