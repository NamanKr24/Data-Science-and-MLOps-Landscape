# Loading the packages

library(ggplot2)
library(ggrepel)
library(plotly)
library(RColorBrewer)

source("User_Data_Wrangling.R")

# Users as students

        data1 %>%
          ggplot (aes(x = Q5, y = percentage, fill = Q5)) +
          geom_bar(stat = "identity", width = 0.25, show.legend = FALSE) +
          labs(title = "Overall percentage of students in data science community",
               y = "Percentage", x = "") +
          geom_text(aes(label = paste0(round(percentage, 1), "%")),
                        vjust = -0.5, size = 4,
                        position = position_dodge(width = 0.9)) +
          scale_y_continuous(limits = c(0, 100)) +
          scale_fill_manual(values = c("green","red")) +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "lightgray",  color = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"),
                plot.title = element_text(color = "darksalmon", size = 18, family = "serif"),
                axis.title.y= element_text(color = "purple4", size = 12, family = "serif"),
                axis.text = element_text(color =  "black", face = "bold", family = "serif"))

# Users by gender

        data2 %>%
          ggplot(aes(x ="", y = percentage, fill = Gender)) +
          geom_bar(stat = "identity", width = 1, color = "black") +
          coord_polar(theta = "y") +
          scale_fill_manual(values = c("Man" = "green",
                              "Woman" = "red", "Others" = "orchid")) +
          geom_text(aes(label = paste0(round(percentage, 1), "%")),
                    vjust = -0.5, size = 4,
                    position = position_dodge(width = 0.9)) +
          theme_void() +
          labs(title = "Percentage of users by gender in data science community") +
          theme(plot.title = element_text(color = "darksalmon",
                                          size = 18, family = "serif"))
          
# Gender by students
       
        data3 %>%
          ggplot(aes(x = Gender, y = percentage, color = Status, group = Status)) +
          geom_line() +
          geom_point() +
          theme_minimal() +
          labs(title = "Which gender uses data science and ML?",
               y = "Percentage", x = "Gender", color = NULL) +
          guides(color = guide_legend(override.aes = list(title = NULL))) +
          geom_text(aes(label = paste0(round(percentage, 1), "%")),
                    vjust = -0.5, size = 4,
                    position = position_dodge(width = 0.9)) +
          scale_y_continuous(limits = c(0, 50)) +
          scale_color_manual(values = c("Student" = "green","Not student" = "red")) +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "black",  color = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"),
                plot.title = element_text(color = "darksalmon", size = 18, family = "serif"),
                axis.title.y= element_text(color = "purple4", size = 12, family = "serif"),
                axis.title.x = element_text(color = "darkturquoise", size = 12, family = "serif"),
                axis.text = element_text(color =  "black", face = "bold", size = 7.5, family = "serif"))
          
          
# Users by their origin

        p <-  world_data %>%
                ggplot() +
                geom_sf(aes(fill = Users)) +
                labs(title = "How much popular is data science across the globe?") +
                theme_minimal() +
                scale_fill_distiller(palette = "RdYlGn", na.value = "white") +
                theme(panel.background = element_rect(fill = "dodgerblue",  color = "white"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.line = element_line(color = "black"),
                      plot.title = element_text(color = "darksalmon",
                                                size = 18, family = "serif"),
                      axis.text = element_text(color =  "black", face = "bold", family = "serif"))
    
          ggplotly(p)
              
# Users by age category
          
        data %>%
          ggplot(aes(x = Q2)) +
          geom_histogram(stat = "count", binwidth = 1, fill = "yellow",  color = "white") +
          labs(title = "Number of users by age category",
               y = "Number of users", x = "Age category") +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "lightgray"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"),
                plot.title = element_text(color = "darksalmon", size = 18, family = "serif"),
                axis.title.y = element_text(color = "purple4", size = 14, family = "serif"),
                axis.title.x = element_text(color = "darkturquoise", size = 14, face = "bold", family = "serif"),
                axis.text = element_text(color =  "black", face = "bold", family = "serif"))