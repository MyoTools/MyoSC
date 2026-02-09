rm(list = ls())

library(tidyverse)
library(ggplot2)
library(patchwork) 
library(ggpp)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf", bold = "timesbd.ttf")
showtext_auto()

prepare_ba_data <- function(manual_path, automated_path) {
  manual_data <- read_csv(manual_path)
  automated_data <- read_csv(automated_path)
  clean_label <- function(data) {
    data %>%
      mutate(Cleaned_Label = str_extract(Label, "^[^\\.]+(?=\\.tif)"))
  }
  process_data_for_means <- function(df) {
    clean_label(df) %>%
      group_by(Cleaned_Label) %>%
      summarise(Mean_Area = mean(Area, na.rm = TRUE))
  }
  manual_means <- process_data_for_means(manual_data) %>% rename(Manual = Mean_Area)
  automated_means <- process_data_for_means(automated_data) %>% rename(Automated = Mean_Area)
  ba_data <- inner_join(manual_means, automated_means, by = "Cleaned_Label")
  ba_data <- ba_data %>%
    mutate(
      Average = (Manual + Automated) / 2, 
      Difference = Automated - Manual    
    )
  return(ba_data)
}

base_path <- "E:/Fiji_segformer/Fiji/6.true_and_predict/"
path_total_manual <- paste0(base_path, "true/Fiber/Fiber.csv")
path_total_auto <- paste0(base_path, "predict/Fiber/Fiber.csv")
path_I_manual <- paste0(base_path, "true/Type/I.csv")
path_I_auto <- paste0(base_path, "predict/Type/I.csv")
path_IIa_manual <- paste0(base_path, "true/Type/IIa.csv")
path_IIa_auto <- paste0(base_path, "predict/Type/IIa.csv")
path_IIb_manual <- paste0(base_path, "true/Type/IIb.csv")
path_IIb_auto <- paste0(base_path, "predict/Type/IIb.csv")

ba_total <- prepare_ba_data(path_total_manual, path_total_auto)
ba_type_I <- prepare_ba_data(path_I_manual, path_I_auto)
ba_type_IIa <- prepare_ba_data(path_IIa_manual, path_IIa_auto)
ba_type_IIb <- prepare_ba_data(path_IIb_manual, path_IIb_auto)

plot_bland_altman <- function(data, plot_title, ylab = NULL,xlab = NULL) {
  mean_diff <- mean(data$Difference)
  sd_diff <- sd(data$Difference)
  upper_limit <- mean_diff + 1.96 * sd_diff
  lower_limit <- mean_diff - 1.96 * sd_diff
  p <- ggplot(data, aes(x = Average, y = Difference)) +
    geom_point(alpha = 0.7, size = 3, color = "#338bcc") +
    
    geom_hline(yintercept = mean_diff, color = "#d92f2f", linetype = "solid", linewidth = 1) +
    geom_hline(yintercept = upper_limit, color = "#d92f2f", linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = lower_limit, color = "#d92f2f", linetype = "dashed", linewidth = 1) +
    
    ggpp::geom_text_npc(
      aes(npcx = 0.98, npcy = 0.85),
      label = paste("+1.96 SD:", round(upper_limit, 3)),
      hjust = 1, color = "#d92f2f", size = 55, family = "Times New Roman"
    ) +
    ggpp::geom_text_npc(
      aes(npcx = 0.98, npcy = 0.6),
      label = paste("Bias:", round(mean_diff, 3)),
      hjust = 1, color = "#d92f2f", size = 55, family = "Times New Roman"
    ) +
    ggpp::geom_text_npc(
      aes(npcx = 0.98, npcy = 0.1),
      label = paste("-1.96 SD:", round(lower_limit, 3)),
      hjust = 1, color = "#d92f2f", size = 55, family = "Times New Roman"
    ) +
    
    geom_text(
      aes(x = -Inf, y = Inf, label = plot_title),
      hjust = -0.1, 
      vjust = 1.1, 
      size = 55,   
      color = "#E97991",
      family = "Times New Roman",
      fontface = "bold"
    ) +
    
    labs(
      x = xlab,
      y = ylab 
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 80,family = "Times New Roman", face = "bold"),
      axis.title = element_text(size = 120, family = "Times New Roman", face = "bold"), 
      axis.text = element_text(size = 120, face = "bold", family = "Times New Roman")
    )
  
  return(p)
}

y_axis_title <- "Difference (Automated - Manual) (μm²)"
x_axis_title <- "Average (Automated + Manual) (μm²)"

plot_total <- plot_bland_altman(ba_total, "Total", ylab = y_axis_title,xlab= x_axis_title)
plot_I     <- plot_bland_altman(ba_type_I, "Type I", ylab = y_axis_title,xlab= x_axis_title)
plot_IIa   <- plot_bland_altman(ba_type_IIa, "Type IIa", ylab = y_axis_title,xlab= x_axis_title)
plot_IIb   <- plot_bland_altman(ba_type_IIb, "Type IIb", ylab = y_axis_title,xlab= x_axis_title)

combined_ba_plot <- plot_total + plot_I + plot_IIa + plot_IIb +
  plot_layout(nrow = 4) + 
  theme(
    text = element_text(
      family = "Times New Roman", 
      face = "bold",
      size = 40
    )
  )

print(combined_ba_plot)