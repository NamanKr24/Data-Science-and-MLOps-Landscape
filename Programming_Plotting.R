# Loading the packages

library(ggplot2)
library(forcats)

source("Programming_Wrangling.R")

# Programming languages
   
      data1 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Top programming langauges in the data science community",
             y = "Percentage", x = "Language") +
        scale_y_continuous(limits = c(0, 40)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "azure", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "magenta", size = 18, family = "serif"),
              axis.title.y = element_text(color = "cadetblue4", size = 12, family = "serif"),
              axis.title.x = element_text(color = "midnightblue", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))
  
# IDEs

      data2 %>%
        ggplot (aes(x = reorder(Selected, Percentage), y = Percentage, fill = Selected)) +
        geom_bar(stat = "identity", width = 1, show.legend = FALSE) +
        coord_flip() +
        labs(title = "Top IDEs used by the data science community",
             y = "Percentage", x = "IDE") +
        scale_y_continuous(limits = c(0, 20)) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "azure", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "magenta", size = 18, family = "serif"),
              axis.title.y = element_text(color = "cadetblue4", size = 12, family = "serif"),
              axis.title.x = element_text(color = "midnightblue", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))
      
# Programming languages by programming experience

      data3 %>%
        ggplot (aes(x = fct_reorder(Language, Percentage, .fun = mean, .desc = TRUE),
                                    y = Experience, size = Percentage, color = Language)) +
        geom_point(alpha = 0.6, show.legend = FALSE) +
        labs(title = "Programming langauges according to the programming experience of the data science community",
             y = "Experience", x = "Language") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "magenta", size = 18, family = "serif"),
              axis.title.y = element_text(color = "cadetblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "midnightblue", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1))

# Programming languages by ML experience

      data4 %>%
        ggplot (aes(x = fct_reorder(Language, Percentage, .fun = mean, .desc = TRUE),
                    y = Experience, size = Percentage, color = Language)) +
        geom_point(alpha = 0.6, show.legend = FALSE) +
        labs(title = "Programming langauges according to the ML experience of the data science community",
             y = "Experience", x = "Language") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "magenta", size = 18, family = "serif"),
              axis.title.y = element_text(color = "cadetblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "midnightblue", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1)) 
      
# Programming languages by roles
      
      data6 %>%
        ggplot(aes(x = Role,
          y = fct_reorder(Language, Proportion, .fun = mean, .desc = TRUE), fill = Proportion)) +
        geom_tile() +
        scale_fill_gradient(low = "yellow", high = "brown") +
        labs(title = "Which jobs require the use of which languages?",
             y = "Language", x = "Role", fill ="") +
        geom_text(aes(label = Proportion), color = "black", size = 3) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "aliceblue", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              plot.title = element_text(color = "magenta", size = 18, family = "serif"),
              axis.title.y = element_text(color = "cadetblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "midnightblue", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 45, hjust = 1)) 