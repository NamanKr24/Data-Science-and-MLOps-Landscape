# Loading the packages

library(ggplot2)
library(vcd)

source("Education_Level_Wrangling.R")

# Education level
  
    data1 %>%
      ggplot (aes(x = reorder(Q8, Percentage), y = Percentage, fill = Q8)) +
      geom_bar(stat = "identity", width = 0.25, show.legend = FALSE) +
      coord_flip() +
      labs(title = "Education level of data science users",
           y = "Percentage", x = "Level") +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                vjust = -0.5, size = 4,
                position = position_dodge(width = 0.9)) +
      scale_y_continuous(limits = c(0, 50)) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "khaki", color = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            plot.title = element_text(color = "chartreuse4", size = 18, family = "serif"),
            axis.title.y = element_text(color = "plum4", size = 12, family = "serif"),
            axis.title.x = element_text(color = "coral", size = 12, family = "serif"),
            axis.text = element_text(color =  "black", face = "bold", family = "serif"))

# Education levels by Roles
    
    data2 %>%
      ggplot (aes(x = Q8, y = Q23, size = Percentage, color = Q8)) +
      geom_point(alpha = 0.6, show.legend = FALSE) +
      labs(title = "How much education for which job?",
           y = "Percentage", x = "Level") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "khaki", color = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            plot.title = element_text(color = "chartreuse4", size = 18, family = "serif"),
            axis.title.y = element_text(color = "plum4", size = 12, family = "serif"),
            axis.title.x = element_text(color = "coral", size = 12, family = "serif"),
            axis.text = element_text(color =  "black", face = "bold", family = "serif"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
# Education level by researches
    
      mosaic(contingency_table,
             shade = TRUE, labeling = labeling_values, gp = shading_hsv, legend = FALSE,
             main = "Data science researchers by their education",
            gp_labels = gpar(fontsize = 8))