# Loading the packages

library(ggplot2)
library(forcats)
library(plotly)

source("Cloud_Computing_Wrangling.R")

# Cloud computing platforms by the sectors

      data1 %>%
        ggplot (aes(x = fct_reorder(Platform, Percentage, .fun = mean, .desc = TRUE),
                    y = Industry, size = Percentage, color = Platform)) +
        geom_point(alpha = 0.6, show.legend = FALSE) +
        labs(title = "Usage of top cloud computing platforms in the working sectors",
             y = "Industry", x = "Platform") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "honeydew", color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "darkorange", size = 18, family = "serif"),
              axis.title.y = element_text(color = "darkorchid4", size = 12, family = "serif"),
              axis.title.x = element_text(color = "deeppink", size = 12, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"),
              axis.text.x = element_text(angle = 30, hjust = 1)) 

# Cloud computing platforms by countries

      p <- world_data %>%
        ggplot() +
        geom_sf(aes(fill = Platform)) +
        labs(title = "Country-wise most popular cloud computing platform") +
        scale_fill_manual(values = c("Alibaba Cloud" = "darksalmon", "Microsoft Azure" = "magenta",
                    "Amazon Web Services (AWS)" = "chartreuse", "Google Cloud Platform (GCP)" = "yellow"),
                    na.value = "white") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "dodgerblue",  color = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              plot.title = element_text(color = "darkorange",
                                        size = 18, family = "serif"),
              axis.text = element_text(color =  "black", face = "bold", family = "serif"))
      
      ggplotly(p)