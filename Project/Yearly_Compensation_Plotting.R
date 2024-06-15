# Loading the packages

library(ggplot2)
library(GGally)

source("Yearly_Compensation_Wrangling.R")

# Yearly compensation by Roles

      data1 %>%
        ggplot(aes(x = Q23, y = Mean_Compensation, fill = Q23)) +
        geom_boxplot(show.legend = FALSE) +
        labs(title = "Yearly compensation of Data Science users by their roles",
             y = "Yearly compensation", x = "Role") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "azure1",  color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "seagreen", size = 18, family = "serif"),
              axis.title.y = element_text(color = "navyblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "brown", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", size = 6, family = "serif"),
              axis.text.x = element_text(angle = 90, hjust = 1, family = "serif"))
      
# Yearly compensation by Roles

      data2 %>%
        ggplot(aes(x = Q24, y = Mean_Compensation, fill = Q24)) +
        geom_boxplot(show.legend = FALSE) +
        labs(title = "Yearly compensation of Data Science users by their working sectors",
             y = "Yearly compensation", x = "Sector") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "azure1",  color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "seagreen", size = 18, family = "serif", hjust = 1),
              axis.title.y = element_text(color = "navyblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "brown", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", size = 6, family = "serif"),
              axis.text.x = element_text(angle = 90, hjust = 1, family = "serif"))

# Yearly compensation by education level

      data3 %>%
        ggplot(aes(x = Q8, y = Mean_Compensation, fill = Q8)) +
        geom_dotplot(binaxis = "y", stackdir = "center", fill = "blue",
                     alpha = 0.7, show.legend = FALSE, binwidth = 1) +
        labs(title = "Yearly compensation of Data Science users by their education",
             y = "Yearly compensation", x = "Education level") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "azure1",  color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "seagreen", size = 18, family = "serif"),
              axis.title.y = element_text(color = "navyblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "brown", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", size = 6, family = "serif"),
              axis.text.x = element_text(angle = 90, hjust = 1, family = "serif"))      

# Yearly compensation by Company size
     
      data4 %>%
        ggplot(aes(x = Q25, y = Mean_Compensation, fill = Q25)) +
        geom_violin(show.legend = FALSE) +
        labs(title = "Yearly compensation of Data Science users by their company size",
             y = "Yearly compensation", x = "Company size") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "azure1",  color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "seagreen", size = 18, family = "serif", hjust = 1),
              axis.title.y = element_text(color = "navyblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "brown", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", size = 6, family = "serif"),
              axis.text.x = element_text(hjust = 1, family = "serif"))
      
# Yearly compensation by continents

      data5 %>%
        ggplot(aes(x = continent, y = Mean_Compensation, fill = continent)) +
        geom_violin(show.legend = FALSE) +
        labs(title = "Yearly compensation of Data Science users by their continent",
             y = "Yearly compensation", x = "Continent") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "azure1",  color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "seagreen", size = 18, family = "serif"),
              axis.title.y = element_text(color = "navyblue", size = 12, family = "serif"),
              axis.title.x = element_text(color = "brown", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", size = 8, family = "serif"),
              axis.text.x = element_text(hjust = 1, family = "serif"))
      
# Yearly compensation by programming and machine learning experiences

      data6 %>%
        ggparcoord(columns = 1:2, groupColumn = 3,
                   scale = "globalminmax", alphaLines = 0.6) +
        labs(title = "Yearly compensation of Data Science users by their
             Programming and ML experiences", x = "Experiences", y = "Years") +
        scale_color_gradient(low = "green", high = "blue") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "black",  color = "white"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.line = element_line(color = "black"),
             plot.title = element_text(color = "seagreen", size = 18, family = "serif"),
             axis.title.y = element_text(color = "navyblue", size = 12, family = "serif"),
             axis.title.x = element_text(color = "brown", size = 12, family = "serif"),
             axis.text = element_text(color =  "black", face = "bold", size = 8, family = "serif"),
             axis.text.x = element_text(hjust = 1, family = "serif"),
             axis.text.y = element_blank())
      