# =========================================================
# LOAN DEFAULT PREDICTION – FULL PROJECT (R)
# =========================================================

# -------------------------
# 1. Load Libraries
# -------------------------
library(tidyverse)
library(readxl)
library(caret)
library(randomForest)
library(pROC)
library(corrplot)

# -------------------------
# 2. Set Working Directory
# -------------------------
setwd("D:/Loan_prediction_R/Loan_recovery")

# -------------------------
# 3. Load Dataset (Excel)
# -------------------------
loan_data <- read.csv("data/raw/credit_risk_dataset.csv")

# -------------------------
# 4. Basic Inspection
# -------------------------
str(loan_data)
summary(loan_data)
colSums(is.na(loan_data))

# -------------------------
# 5. Target Variable
# -------------------------
loan_data$loan_status <- as.factor(loan_data$loan_status)

# -------------------------
# 6. EDA – Class Distribution
# -------------------------
png("reports/figures/class_distribution.png")
barplot(table(loan_data$loan_status),
        main = "Loan Status Distribution",
        xlab = "Loan Status",
        ylab = "Count")
dev.off()

# -------------------------
# 7. Handle Missing Values
# -------------------------
loan_data <- loan_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "Unknown", .)))

# -------------------------
# 8. Encode Categorical Variables
# -------------------------
loan_data$person_home_ownership <- as.factor(loan_data$person_home_ownership)
loan_data$loan_intent <- as.factor(loan_data$loan_intent)
loan_data$loan_grade <- as.factor(loan_data$loan_grade)
loan_data$cb_person_default_on_file <- as.factor(loan_data$cb_person_default_on_file)

# -------------------------
# 9. Train-Test Split
# -------------------------
set.seed(123)
train_index <- createDataPartition(loan_data$loan_status, p = 0.8, list = FALSE)
train_data <- loan_data[train_index, ]
test_data  <- loan_data[-train_index, ]

# -------------------------
# 10. Logistic Regression
# -------------------------
log_model <- glm(loan_status ~ ., data = train_data, family = binomial)
summary(log_model)

log_pred_prob <- predict(log_model, test_data, type = "response")
log_pred <- ifelse(log_pred_prob > 0.5, 1, 0)

# -------------------------
# 11. Random Forest Model
# -------------------------
rf_model <- randomForest(loan_status ~ ., data = train_data, ntree = 200)
print(rf_model)

rf_pred <- predict(rf_model, test_data)
rf_prob <- predict(rf_model, test_data, type = "prob")[,2]

# -------------------------
# 12. Confusion Matrix
# -------------------------
conf_mat <- confusionMatrix(
  factor(rf_pred, levels = c(0, 1)),
  factor(test_data$loan_status, levels = c(0, 1))
)

print(conf_mat)

conf_matrix <- as.matrix(conf_mat$table)

png("reports/figures/confusion_matrix.png", width = 600, height = 500)

image(1:2, 1:2, t(conf_matrix),
      axes = FALSE,
      col = c("#d9ead3", "#f4cccc"),
      xlab = "Predicted",
      ylab = "Actual",
      main = "Confusion Matrix")

axis(1, at = 1:2, labels = colnames(conf_matrix))
axis(2, at = 1:2, labels = rownames(conf_matrix))

for (i in 1:2) {
  for (j in 1:2) {
    text(i, j, conf_matrix[j, i], cex = 1.6)
  }
}

dev.off()

# -------------------------
# 13. ROC Curve
# -------------------------
roc_obj <- roc(test_data$loan_status, rf_prob)

png("reports/figures/roc_curve.png")
plot(roc_obj, main = "ROC Curve - Random Forest")
dev.off()

# -------------------------
# 14. Correlation Heatmap
# -------------------------
numeric_data <- loan_data %>% select(where(is.numeric))
png("reports/figures/correlation_heatmap.png", width = 800, height = 800)
corrplot(cor(numeric_data), method = "color", type = "upper", tl.cex = 0.6)
dev.off()

# -------------------------
# 15. Save Model
# -------------------------
saveRDS(rf_model, "models/loan_default_rf_model.rds")

# -------------------------
# 16. Save Processed Dataset
# -------------------------
write.csv(loan_data, "data/processed/loan_cleaned.csv", row.names = FALSE)

# -------------------------
# 17. Final Message
# -------------------------
cat("PROJECT EXECUTED SUCCESSFULLY\n")
cat("Model, plots, and processed data saved.\n")
