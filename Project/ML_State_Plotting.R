# Loading the packages

library(ggplot2)
library(forcats)
library(plotly)

source("ML_State_Wrangling.R")

# ML state in companies

      data1 %>%
        ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Q27)) +
        geom_rect() +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        geom_text(aes(x = 3.5, y = labelPosition, label = label), size = 3.5) +
        labs(title = "At what levels have the companies adopted ML usage?", fill = "") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "gray100", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              plot.title = element_text(color = "chocolate4", size = 18, family = "serif"),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text = element_blank())
      
# ML Activities

      plot_ly(type = "funnelarea",
              text = data2$Selected,
              values = data2$Percentage) %>%
        layout(title = list(text = "Activities performed at job",
        font = list(family = "serif", size = 18, color = "chocolate4")))
      
# ML adoption by sectors
      
      data3 %>%
        ggplot (aes(x = fct_reorder(Selected, Percentage, .fun = mean, .desc = TRUE),
                    y = Q24, size = Percentage, color = Selected)) +
        geom_point(alpha = 0.6, show.legend = FALSE) +
        labs(title = "What kind of roles are performed in the sectors?",
             y = "Industry", x = "Stage") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "gray100", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "chocolate4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "blue4", size = 12, family = "serif"),
              axis.title.x = element_text(color = "goldenrod4", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# ML team size by company size
     
      data4 %>%
        ggplot (aes(x = Q26, y = Count, fill = Q26)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        facet_wrap(~Q25, scales = "free") +
        labs(title = "How much priority do comapnies give to data science and ML?",
             y = "", x = "Data science team size") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "gray100", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "chocolate4", size = 18, family = "serif"),
              axis.title.y = element_text(color = "blue4", size = 12, family = "serif"),
              axis.title.x = element_text(color = "goldenrod4", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
# ML adoption by ml team size
      
      plot_ly(type = "scatterpolar", fill = "toself", mode = "marker") %>%
        add_trace(r = pivot_d1[, 1], theta = rownames(pivot_d1), name = colnames(pivot_d1)[1])  %>%
        add_trace(r = pivot_d1[, 2], theta = rownames(pivot_d1), name = colnames(pivot_d1)[2])  %>%
        add_trace(r = pivot_d1[, 3], theta = rownames(pivot_d1), name = colnames(pivot_d1)[3])  %>%
        add_trace(r = pivot_d1[, 4], theta = rownames(pivot_d1), name = colnames(pivot_d1)[4])  %>%
        add_trace(r = pivot_d1[, 5], theta = rownames(pivot_d1), name = colnames(pivot_d1)[5])  %>%
        add_trace(r = pivot_d1[, 6], theta = rownames(pivot_d1), name = colnames(pivot_d1)[6]) %>%
        layout(polar = list(radiaxis = list(visible = T, range = c(0, 50))),
               title = list(text = "How many people for a ML activity?",
                            font = list(family = "serif", size = 18, color = "chocolate4"),
                x = 0.5, xanchor = "center"))

# ML state by company size by ML team size

      plot_ly(data6, x = ~Q27, y = ~Q25, z = ~Q26, type = 'scatter3d') %>%
        layout(scene = list(xaxis = list(title = "ML state"),
                 yaxis = list(title = "Company size"),
                 zaxis = list(title = "ML team size")))