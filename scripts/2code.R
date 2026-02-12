# =========================================================
# LOAN DEFAULT PREDICTION – SMOTE WITH DUMMY ENCODING
# =========================================================

# -------------------------
# 1. Load Libraries
# -------------------------
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)
library(recipes)
library(themis)

# -------------------------
# 2. Set Working Directory
# -------------------------
setwd("D:/Loan_prediction_R/Loan_recovery")

# -------------------------
# 3. Load Dataset
# -------------------------
loan_data <- read.csv("data/raw/credit_risk_dataset.csv")

# -------------------------
# 4. Target Variable
# -------------------------
loan_data$loan_status <- as.factor(loan_data$loan_status)

# -------------------------
# 5. Handle Missing Values
# -------------------------
loan_data <- loan_data %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character),
                ~ ifelse(is.na(.), "Unknown", .)))

# -------------------------
# 6. Encode Categorical Variables
# -------------------------
loan_data <- loan_data %>%
  mutate(across(where(is.character), as.factor))

# -------------------------
# 7. Train-Test Split
# -------------------------
set.seed(123)
train_index <- createDataPartition(
  loan_data$loan_status,
  p = 0.8,
  list = FALSE
)

train_data <- loan_data[train_index, ]
test_data  <- loan_data[-train_index, ]

# -------------------------
# 8. Check Imbalance (Before)
# -------------------------
cat("Class distribution BEFORE balancing:\n")
print(table(train_data$loan_status))

# -------------------------
# 9. RECIPE: DUMMY + SMOTE
# -------------------------
smote_recipe <- recipe(loan_status ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(loan_status, over_ratio = 1)

# Prep recipe on training data
smote_prep <- prep(smote_recipe)

# Apply to training data (SMOTE applied)
train_balanced <- bake(smote_prep, new_data = NULL)

# Apply SAME recipe to test data (NO SMOTE applied)
test_processed <- bake(smote_prep, new_data = test_data)

# -------------------------
# 10. Check Balance (After)
# -------------------------
cat("Class distribution AFTER balancing:\n")
print(table(train_balanced$loan_status))

# -------------------------
# 11. Random Forest Model
# -------------------------
rf_model <- randomForest(
  loan_status ~ .,
  data = train_balanced,
  ntree = 200
)

print(rf_model)

# -------------------------
# 12. Predictions
# -------------------------
rf_pred <- predict(rf_model, test_processed)
rf_prob <- predict(rf_model, test_processed, type = "prob")[, 2]



# -------------------------
# 13. Confusion Matrix
# -------------------------
conf_mat <- confusionMatrix(
  factor(rf_pred, levels = c(0, 1)),
  factor(test_data$loan_status, levels = c(0, 1))
)

print(conf_mat)

# -------------------------
# 14. ROC Curve
# -------------------------
roc_obj <- roc(test_data$loan_status, rf_prob)
plot(roc_obj, main = "ROC Curve – Balanced Random Forest")

# -------------------------
# 15. Save Outputs
# -------------------------
write.csv(
  train_balanced,
  "data/processed/loan_train_balanced.csv",
  row.names = FALSE
)

saveRDS(
  rf_model,
  "models/loan_default_rf_model_balanced.rds"
)

# -------------------------
# 16. Final Message
# -------------------------
cat("SMOTE APPLIED AFTER DUMMY ENCODING\n")
cat("MODEL TRAINED AND EVALUATED SUCCESSFULLY\n")
