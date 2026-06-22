# ====================================================================
# Configuration: Define Input/Output Files
# ====================================================================
# Set working directory to the location of these files before execution
INPUT_FILE <- "dataset_2026_Min.csv" 
OUTPUT_IMAGE <- "Figure_RF_Results.png"

# ====================================================================
# 1. Load Required Packages
# ====================================================================
if(!require(randomForest)) install.packages("randomForest")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(patchwork)) install.packages("patchwork") 

library(randomForest)
library(ggplot2)
library(patchwork)

# ====================================================================
# 2. Load and Preprocess Data
# ====================================================================
raw_data <- read.csv(INPUT_FILE, header = TRUE)
data <- raw_data[, c("pts", "road", "green", "svf", "building", "meanh", "temp", "ws", "humi")]

# ====================================================================
# 3. Train / Test Data Split (70:30)
# ====================================================================
set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[train_index, ]
test <- data[-train_index, ]

# ====================================================================
# 4. Train Random Forest Model
# ====================================================================
set.seed(123)
rf_model <- randomForest(as.factor(pts) ~ road + green + svf + building + meanh + temp + ws + humi, 
                         data = train, importance = TRUE)

# ====================================================================
# 5. Generate Panel A: Confusion Matrix
# ====================================================================
pred <- predict(rf_model, test)
cm <- table(Actual = test$pts, Predicted = pred)
cm_prop <- prop.table(cm, margin = 1) * 100

cm_df <- as.data.frame(cm)
cm_prop_df <- as.data.frame(cm_prop)
colnames(cm_df) <- c("Actual", "Predicted", "Freq")
colnames(cm_prop_df) <- c("Actual", "Predicted", "Percent")

cm_final <- merge(cm_df, cm_prop_df, by = c("Actual", "Predicted"))

# Convert numeric classes to categorical labels
level_nums <- c("1", "2", "3", "4")
level_names <- c("Comfortable", "Normal", "Hot", "Very Hot")
cm_final$Actual <- factor(cm_final$Actual, levels = level_nums, labels = level_names)
cm_final$Predicted <- factor(cm_final$Predicted, levels = level_nums, labels = level_names)

# Fix axis order for visualization
pts_levels <- c("Comfortable", "Normal", "Hot", "Very Hot")
cm_final$Predicted <- factor(cm_final$Predicted, levels = pts_levels)
cm_final$Actual <- factor(cm_final$Actual, levels = rev(pts_levels)) 

cm_final$Label <- ifelse(cm_final$Freq == 0, "0\n(0.0%)", 
                         sprintf("%d\n(%.1f%%)", cm_final$Freq, cm_final$Percent))
max_freq <- max(cm_final$Freq, na.rm = TRUE)
cm_final <- na.omit(cm_final)

p1 <- ggplot(cm_final, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +  
  geom_text(aes(label = Label, color = Freq > (max_freq / 2)), 
            size = 5, family = "sans", fontface = "bold") + 
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  scale_fill_gradient(low = "#F8FAFC", high = "#1E3A8A", guide = "none") + 
  scale_x_discrete(position = "top") + 
  coord_fixed() + 
  labs(x = "Predicted PTS", y = "Actual PTS") +
  theme_minimal(base_family = "sans") + 
  theme(
    axis.text.x.top = element_text(size = 12, color = "black", face = "bold", margin = margin(b = 10)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold", margin = margin(r = 10)),
    axis.title.x.top = element_text(size = 14, color = "black", face = "bold", margin = margin(b = 20)),
    axis.title.y = element_text(size = 14, color = "black", face = "bold", margin = margin(r = 20)),
    panel.grid = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# ====================================================================
# 6. Generate Panel B: Permutation Feature Importance (PFI)
# ====================================================================
imp <- importance(rf_model)
imp_df <- data.frame(Variable = rownames(imp), MDA = imp[, "MeanDecreaseAccuracy"])

imp_df$MDA_adj <- ifelse(imp_df$MDA < 0, 0, imp_df$MDA)
imp_df$Importance_pct <- (imp_df$MDA_adj / sum(imp_df$MDA_adj)) * 100

var_names <- c("temp" = "Air Temperature", "green" = "Green-area ratio (GA)",
               "road" = "Road-area ratio (RA)", "meanh" = "Avg. building height (ABH)",
               "svf" = "Sky View Factor (SVF)", "building" = "Avg. building-area ratio (ABA)",
               "humi" = "Relative Humidity", "ws" = "Wind Speed")
imp_df$Variable <- var_names[imp_df$Variable]
imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable[order(imp_df$Importance_pct)])

p2 <- ggplot(imp_df, aes(x = Variable, y = Importance_pct)) +
  geom_col(fill = "#1E3A8A", width = 0.55) + 
  geom_text(aes(label = sprintf("%.1f%%", Importance_pct)), 
            hjust = -0.15, size = 5, family = "sans", fontface = "bold", color = "black") +
  coord_flip() + 
  theme_minimal(base_family = "sans") +
  labs(x = "", y = "Relative Variable Importance (%)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  theme(
    axis.text.y = element_text(size = 12, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15)),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

# ====================================================================
# 7. Combine and Export Figure
# ====================================================================
combined_plot <- p1 + p2 + 
  plot_layout(widths = c(1, 0.65)) +  
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 20, face = "bold", family = "sans"))

ggsave(OUTPUT_IMAGE, plot = combined_plot, width = 12.5, height = 6, dpi = 300)

# ====================================================================
# 8. Model Performance Summary (Console Output)
# ====================================================================
actual_num <- as.numeric(as.character(test$pts))
pred_num <- as.numeric(as.character(pred))
total_n <- length(actual_num)

# Multi-class accuracy calculations
acc <- sum(actual_num == pred_num) / total_n * 100
over_est <- sum(pred_num > actual_num) / total_n * 100
under_est <- sum(pred_num < actual_num) / total_n * 100

# Binary accuracy calculation (Classes 1 & 2 vs Classes 3 & 4)
binary_actual <- ifelse(actual_num <= 2, 0, 1)
binary_pred <- ifelse(pred_num <= 2, 0, 1)
binary_acc <- sum(binary_actual == binary_pred) / total_n * 100

summary_text <- paste0(
  "\n========================================\n",
  " Model Performance Summary\n",
  "========================================\n",
  sprintf(" Total Samples        : %d\n", total_n),
  sprintf(" Multi-class Accuracy : %.1f%%\n", acc),
  sprintf(" Binary Accuracy      : %.1f%%\n", binary_acc),
  sprintf(" Overestimation       : %.1f%%\n", over_est),
  sprintf(" Underestimation      : %.1f%%\n", under_est),
  "========================================\n\n"
)

cat(summary_text)