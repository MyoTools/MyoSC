rm(list = ls())

library(tidyverse)
library(patchwork)
library(ggplot2)
library(ggpp)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf", bold = "timesbd.ttf")
showtext_auto()


true_data <- read_csv("E:/Fiji_segformer/Fiji/6.true_and_predict/true/Fiber/Fiber.csv")
pred_data <- read_csv("E:/Fiji_segformer/Fiji/6.true_and_predict/predict/Fiber/Fiber.csv")


clean_label <- function(data) {
  data %>%
    mutate(Cleaned_Label = str_extract(Label, "^[^\\.]+(?=\\.tif)"))
}

true_data <- clean_label(true_data) %>% mutate(Source = "Manual")
pred_data <- clean_label(pred_data) %>% mutate(Source = "Automated")


combined_data <- bind_rows(true_data, pred_data) %>% drop_na(Area)

prob_points <- ppoints(100) 

manual_quantiles <- quantile(true_data$Area, probs = prob_points, na.rm = TRUE)
automated_quantiles <- quantile(pred_data$Area, probs = prob_points, na.rm = TRUE)

qq_data <- data.frame(
  Manual = manual_quantiles,
  Automated = automated_quantiles
)

ppcc_value <- cor(qq_data$Manual, qq_data$Automated, method = "pearson")

qq_plot <- ggplot(qq_data, aes(x = Manual, y = Automated)) +

  geom_point(aes(color = "Q-Q plot"), alpha = 0.7, size = 2) +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "y = x", color = "y = x")) +
  geom_smooth(aes(color = "Linear fit"), method = "lm", se = FALSE) +

  scale_color_manual(
    name = NULL, 
    values = c(
      "Linear fit" = "#8CBCA6",    
      "Q-Q plot" = "#DFAD92",     
      "y = x" = "#81AABC"        
    ),
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid", "blank", "dashed"), 
        shape = c(NA, 16, NA),                  
        size = c(1, 3, 1)                        
      )
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("y = x" = "dashed"),  
    guide = "none"  
  ) +
  

  annotate(
    "text_npc",
    npcx = 0.01,
    npcy = 0.99,
    label = paste("Total PPCC: r =", round(ppcc_value, 3)),
    size = 28, 
    color = "#E97991", 
    fontface = "bold",
    hjust = 0,
    vjust = 1
  ) +
  scale_x_continuous(breaks = seq(0, max(qq_data$Manual), by = 2000)) +
  scale_y_continuous(breaks = seq(0, max(qq_data$Automated), by = 2000)) +
  

  labs(x = "Quantiles: Manual", y = "Quantiles: Automated") +
  theme_bw() +

  theme(
    text = element_text(family = "Times New Roman", face = "bold"), 
    axis.title = element_text(size = 84),
    axis.text = element_text(size = 80),
    legend.position = c(0.98, 0.3), 
    legend.justification = c(1, 1),  
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 80),
    legend.margin = margin(10, 10, 10, 10)
  )


qq_plot

ks_result <- ks.test(true_data$Area, pred_data$Area)
p_value_text <- ifelse(ks_result$p.value < 0.001, 
                       "P < 0.001", 
                       paste("P =", round(ks_result$p.value, 3)))

density_plot <- ggplot(combined_data, aes(x = Area, fill = Source)) +
  geom_density(alpha = 0.5, adjust = 0.8) +
  scale_fill_manual(values = c("Manual" = "#af99c8", "Automated" = "#9cb87d")) +
  

  annotate(
    "text_npc",
    npcx = 0.6,
    npcy = 0.75, 
    label = "K-S Test:",
    size = 28, 
    color = "#E97991", 
    fontface = "bold",
    hjust = 0,
    vjust = 1
  ) +
  annotate(
    "text_npc",
    npcx = 0.6,
    npcy = 0.65, 
    label = p_value_text,
    size = 28, 
    color = "#E97991", 
    fontface = "bold",
    hjust = 0,
    vjust = 1
  ) +
  
  labs(x = "CSA of Total Fibers (μm²)", y = "Probability Density") +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(size = 76),
    axis.text = element_text(size = 80),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_blank(),
    legend.text = element_text(size = 80),
    legend.margin = margin(10, 10, 10, 10)
  )
density_plot

combined_plot <- density_plot + qq_plot +
  plot_layout(ncol = 2)

print(combined_plot)