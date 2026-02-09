rm(list=ls())

library(dplyr)
library(stringr)

library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf", bold = "timesbd.ttf")
showtext_auto()


process_summary_file <- function() {

  file_path <- "E:/Fiji_segformer/Fiji/6.true_and_predict/true/Fiber/Fiber.csv"
  if (!file.exists(file_path)) {
    stop("文件不存在: ", file_path)
  }
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  

  data$Cleaned_Label <- str_extract(data$Label, "^[^\\.]+(?=\\.tif)")
  

  result <- data %>%
    group_by(Cleaned_Label) %>%
    summarise(
      Mean_Area = mean(Area, na.rm = TRUE),
      Total_Count = n(),
      Total_Area = sum(Area, na.rm = TRUE)
    )
  

  print(result)
  return(result)
}


process_classified_file <- function() {

  file_path <- "E:/Fiji_segformer/Fiji/6.true_and_predict/predict/Fiber/Fiber.csv"
  if (!file.exists(file_path)) {
    stop("文件不存在: ", file_path)
  }
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  

  data$Cleaned_Label <- str_extract(data$Label, "^[^\\.]+(?=\\.tif)")
  

  result <- data %>%
    group_by(Cleaned_Label) %>%
    summarise(
      Mean_Area = mean(Area, na.rm = TRUE),
      Total_Count = n(),
      Total_Area = sum(Area, na.rm = TRUE)
    )
  


  print(result)
  return(result)
}


summary_results <- tryCatch(
  process_summary_file(),
  error = function(e) {
    message("Fail: ", e$message)
    return(NULL)
  }
)


classified_results <- tryCatch(
  process_classified_file(),
  error = function(e) {
    message("Fail: ", e$message)
    return(NULL)
  }
)



library(dplyr)

common_labels <- intersect(summary_results$Cleaned_Label, classified_results$Cleaned_Label)

summary_common <- summary_results %>% 
  filter(Cleaned_Label %in% common_labels) %>%
  select(Cleaned_Label, Mean_Area, Total_Count, Total_Area) %>%
  rename(Mean_Area_summary = Mean_Area, 
         Total_Count_summary = Total_Count, 
         Total_Area_summary = Total_Area)

classified_common <- classified_results %>% 
  filter(Cleaned_Label %in% common_labels) %>%
  select(Cleaned_Label, Mean_Area, Total_Count, Total_Area) %>%
  rename(Mean_Area_classified = Mean_Area, 
         Total_Count_classified = Total_Count, 
         Total_Area_classified = Total_Area)

combined_data <- inner_join(summary_common, classified_common, by = "Cleaned_Label")

col_pairs <- list(
  c(colnames(combined_data)[2], colnames(combined_data)[5]),  
  c(colnames(combined_data)[3], colnames(combined_data)[6]), 
  c(colnames(combined_data)[4], colnames(combined_data)[7])  
)

library(psych) 

results <- data.frame(
  Pair = sapply(col_pairs, paste, collapse = " vs "),
  Pearson_r = numeric(3),
  Pearson_p = numeric(3),
  Spearman_rho = numeric(3),
  Spearman_p = numeric(3)
)

for (i in seq_along(col_pairs)) {
  col1 <- combined_data[[col_pairs[[i]][1]]]
  col2 <- combined_data[[col_pairs[[i]][2]]]
  
  pearson_test <- cor.test(col1, col2, method = "pearson")
  results$Pearson_r[i] <- pearson_test$estimate
  results$Pearson_p[i] <- pearson_test$p.value
  
  spearman_test <- cor.test(col1, col2, method = "spearman")
  results$Spearman_rho[i] <- spearman_test$estimate
  results$Spearman_p[i] <- spearman_test$p.value
}

print(results)

library(ggplot2)
library(ggpmisc)

names(combined_data)

mae_area <- mean(abs(combined_data$Mean_Area_classified - combined_data$Mean_Area_summary))
mae_area_label <- paste("MAE =", sprintf("%.3f", mae_area))

mae_count <- mean(abs(combined_data$Total_Count_classified - combined_data$Total_Count_summary))
mae_count_label <- paste("MAE =", sprintf("%.3f", mae_count))

area_correlation_plot <- ggplot(combined_data, 
                                aes(x = Mean_Area_summary, y = Mean_Area_classified)) +
  
  geom_point(color = "#338bcc", alpha = 0.7, size = 3) +

  geom_smooth(method = "lm", se = TRUE, color = "#d92f2f",  alpha = 0.2) +

  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#888ecc") +

  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste("bold(atop(Total,", after_stat(rr.label), "))", sep = "")),
    parse = TRUE,
    label.x = "left",
    label.y = "top",
    size = 28,
    rr.digits = 3,  
    color = "#E97991"  
  ) +
  
  annotate(
    "text",
    x = Inf, y = -Inf, 
    label = mae_area_label,
    hjust = 1.1,   
    vjust = -0.5,  
    size = 28,
    family = "Times New Roman", 
    fontface = "bold",
    color = "black" 
  ) +

  labs(
    x = "Manual Mean CSA (μm²)",
    y = "Automated Mean CSA (μm²)"
  ) +

  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"), 
    axis.title = element_text(size = 84),
    axis.text = element_text(size = 80),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

print(area_correlation_plot)

count_correlation_plot <- ggplot(combined_data, 
                                aes(x = Total_Count_summary, y = Total_Count_classified)) +

  geom_point(color = "#338bcc", alpha = 0.7, size = 3) +

  geom_smooth(method = "lm", se = TRUE, color = "#d92f2f",  alpha = 0.2) +

  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#888ecc") +

  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste("bold(atop(Total,", after_stat(rr.label), "))", sep = "")),
    parse = TRUE,
    label.x = "left",
    label.y = "top",
    size = 28,
    rr.digits = 3, 
    color = "#E97991"  
  ) +
  
  annotate(
    "text",
    x = Inf, y = -Inf, 
    label = mae_count_label,
    hjust = 1.1,   
    vjust = -0.5,  
    size = 28,
    family = "Times New Roman", 
    fontface = "bold",
    color = "black" 
  ) +

  labs(
    x = "Manual Mean Count",
    y = "Automated Mean Count"
  ) +

  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"), 
    axis.title = element_text(size = 84),
    axis.text = element_text(size = 80),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

print(count_correlation_plot)

combined_plot <- area_correlation_plot + count_correlation_plot +
  plot_layout(ncol = 2)
combined_plot