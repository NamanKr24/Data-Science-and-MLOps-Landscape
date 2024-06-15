# Loading the packages

library(ggplot2)
library(forcats)
library(plotly)

source("Employments_Wrangling.R")

# Roles

    data1 %>%
      ggplot (aes(x = reorder(Q23, Percentage), y = Percentage, fill = Q23)) +
      geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
      coord_flip() +
      labs(title = "Top jobs in the data science community",
           y = "Percentage", x = "Jobs") +
      scale_y_continuous(limits = c(0, 40)) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightsteelblue1", color = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            plot.title = element_text(color = "burlywood3", size = 18, family = "serif"),
            axis.title.y = element_text(color = "violet", size = 12, family = "serif"),
            axis.title.x = element_text(color = "firebrick2", size = 12, family = "serif"),
            axis.text = element_text(color =  "black", face = "bold", family = "serif"))

# Industries by hiring
      
      data2 %>%
        ggplot (aes(x = reorder(factor(Q24), Percentage), y = Percentage, fill = Q24)) +
        geom_bar(stat = "identity", width = 1, show.legend = TRUE) +
        coord_polar(theta = "x", start = pi / 2) +
        labs(title = "Which sectors are hiring AI employees?", fill = "Sectors") +
        scale_y_continuous(limits = c(-max(data2$Percentage), max(data2$Percentage))) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "lightsteelblue1", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              plot.title = element_text(color = "burlywood3", size = 18, family = "serif"),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text = element_blank())
          
# Industry by roles
      
      data3 %>%
        ggplot (aes(x = fct_reorder(Q23, Percentage, .fun = mean, .desc = TRUE),
                    y = Q24, size = Percentage, color = Q23)) +
        geom_point(alpha = 0.6, show.legend = FALSE) +
        labs(title = "Which sectors are hiring for which roles?",
             y = "Sector", x = "Job") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "lightsteelblue1", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "burlywood3", size = 18, family = "serif"),
              axis.title.y = element_text(color = "violet", size = 12, family = "serif"),
              axis.title.x = element_text(color = "firebrick2", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# Roles by experience in ML
      
      data4 %>%
        ggplot (aes(x = fct_reorder(Q23, Percentage, .fun = mean, .desc = TRUE),
                    y = Q16, size = Percentage, color = Q23)) +
        geom_point(alpha = 0.6, show.legend = FALSE) +
        labs(title = "ML experience by each role",
             y = "Experience", x = "Job") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "lightsteelblue1", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "burlywood3", size = 18, family = "serif"),
              axis.title.y = element_text(color = "violet", size = 12, family = "serif"),
              axis.title.x = element_text(color = "firebrick2", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
# ML compensation vs Yearly compensation by roles
      
      p <- data5 %>%
        ggplot(aes(x = Earnings, y = Expenses, color = Role)) +
        geom_point() +
        labs(title = "How much of their earnings do they spend on ML?",
             x = "Earnings", y = "ML expenditure", fill = "") +
        theme_minimal() +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "lightsteelblue1", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "burlywood3", size = 18, family = "serif"),
              axis.title.y = element_text(color = "violet", size = 12, family = "serif"),
              axis.title.x = element_text(color = "firebrick2", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
     
# ML state by roles
      
      plot_ly(type = "sankey", orientation = "h",
              link = list(source = data6$source_index, target = data6$target_index, value = data6$Count),
              node = list(pad = 15, thickness = 20, line = list(color = "black", width = 0.5),
                          label = node_labels))