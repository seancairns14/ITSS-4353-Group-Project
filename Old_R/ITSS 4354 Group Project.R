install.packages("caret")


knitr::opts_chunk$set(echo = TRUE)


# Load necessary libraries for logistic regression model
library(dplyr)
library(readr)
library(caret)
library(ggplot2)



## DATA READING
# Read the dataset
data <- read.csv("FNA_cancer.csv")
head(data)
print(length(unique(data$diagnosis)))
# View the dimensions of the dataset
dimensions <- dim(data)
print(paste("Data Dimensions: Rows =", dimensions[1], ", Columns =", dimensions[2]))


## DATA CLEANING
# Remove the 'X' column
data <- data %>% select(-X)



## DATA SPLITTING
# Setting a random seed for reproducibility
set.seed(4354)

# Splitting the data into training and test sets using the createDataPartition function from the caret package
# 80% of the data goes into training set
target <- data$diagnosis
train_indices <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Print the dimensions of the training and test data sets
train_dimensions <- dim(train_data)
test_dimensions <- dim(test_data)

print(paste("Train Data Dimensions: Rows =", train_dimensions[1], ", Columns =", train_dimensions[2]))
print(paste("Test Data Dimensions: Rows =", test_dimensions[1], ", Columns =", test_dimensions[2]))



## MODEL TRAINING
# Converting the factor levels to a binary format for logistic regression model
# 1 = 'M' (malignant) 0 = 'B' (benign)
train_data$diagnosis <- as.factor(ifelse(train_data$diagnosis == "M", 1, 0))
test_data$diagnosis <- as.factor(ifelse(test_data$diagnosis == "M", 1, 0))

# Train a logistic regression model using the glm function with binomial family parameter
logistic_model <- glm(diagnosis ~ ., data = train_data, family = "binomial")

# Summary of the model to view coefficients and statistics
summary(logistic_model)



## MODEL EVALUATION
# Make predictions on the test data
predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted probabilities to binary predictions based on a 0.5 cutoff
binary_predictions <- ifelse(predictions > 0.5, "1", "0")

# Create a confusion matrix to evaluate the model
confusion_matrix <- confusionMatrix(as.factor(binary_predictions), test_data$diagnosis)
print(confusion_matrix)


## PLOTTING CONFUSION MATRIX
cm = as.data.frame(confusion_matrix$table)

conf_plot <- ggplot(data = cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Actual", y = "Predicted", title = "Logistic Regression Confusion Matrix") +
  scale_fill_gradient(low = "white", high = "green") +  # Adjust colors as needed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))
print(conf_plot)




## PLOTTING FEATURE IMPORTANCE
# ***unfinished, i was trying to use sean's code to do it
importance <- as.data.frame(varImp(best_model))

importance_scores <- importance$Overall
importance_labels <- rownames(importance)
importance_data <- data.frame(importance_scores, importance_labels)

importance_plot <- ggplot(data = head(importance_data, 6), aes(x = reorder(importance_labels, -importance_scores), y = importance_scores)) + 
  labs(x = "Variables", y = "Importance", title = "Decision Tree Feature Importance Scores") +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))


## PLOTTING HEATMAP



