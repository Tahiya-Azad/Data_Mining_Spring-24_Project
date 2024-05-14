# Naive Bayes

library(rpart)
library(rpart.plot)
data <- ("~/Desktop/CMPT 436/DataMiningProject_2024/churn.txt")
churn_data <- read.csv(data, stringsAsFactors = TRUE)
names(churn_data) <- c("State", "Account_Length", "Area_Code", "Phone_Number", "International_Plan", "Voice_Mail_Plan", "Number_Of_Voice_Mail_Messages", "Total_Day_Minutes", "Total_Day_Calls", "Total_Day_Charge", "Total_Eve_Minutes", "Total_Eve_Calls", "Total_Eve_Charge", "Total_Night_Minutes", "Total_Night_Calls", "Total_Night_Charge", "Total_International_Minutes", "Total_International_Calls", "Total_International_Charge", "Number_Of_Calls_To_Customer_Service", "Churn")
df <- churn_data[-4]
churn_data$Churn <- factor(churn_data$Churn, levels = c("True", "False"))

set.seed(1234)
train <- sample(nrow(churn_data), 0.7 * nrow(df))
df.train <- df[train, ]
df.validate <- df[-train, ]
table(df.train$Churn)
table(df.validate$Churn)

library(e1071)

nb.model <- naiveBayes(Churn ~ ., data = df.train)
nb.pred <- predict(nb.model, df.validate)
nb.perf <- table(df.validate$Churn, nb.pred, dnn = c("Actual", "Predicted"))
nb.perf
tn <- nb.perf[1, 1]
fp <- nb.perf[1, 2]
fn <- nb.perf[2, 1]
tp <- nb.perf[2, 2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
print(accuracy)
error_rate <- (fp + fn) / (tp + tn + fp + fn)
print(error_rate)
sensitivity <- tp / (tp + fn)
print(sensitivity)
specificity <- tn / (tn + fp)
print(specificity)
precision <- tp / (tp + fp)
print(precision)
f_measure <- (2 * precision * sensitivity) / (precision + sensitivity)
print(f_measure)

# Decision Tree

# Set seed for reproducibility
set.seed(1234)
train_indices <- sample(nrow(df), 0.7 * nrow(df))
df.train <- df[train_indices, ]
df.validate <- df[-train_indices, ]

# Build a Decision Tree model
dtree <- rpart(Churn. ~ ., data = df.train, method = "class", parms = list(split = "information"))

# Prune the tree based on complexity parameter (this value would need tuning)
dtree_pruned <- prune(dtree, cp = dtree$cptable[which.min(dtree$cptable[, "xerror"]), "CP"])

# Plot the pruned decision tree
prp(dtree_pruned, type = 2, extra = 104, main = "Pruned Decision Tree for Churn Dataset")

# Predict on validation set
dtree_pred <- predict(dtree_pruned, newdata = df.validate, type = "class")

dtree_perf <- table(actual = df.validate$Churn., predicted = dtree_pred, dnn = c("Actual", "Predicted"))
# Extract true negatives, false positives, false negatives, and true positives
tn <- dtree_perf[1, 1]
fp <- dtree_perf[1, 2]
fn <- dtree_perf[2, 1]
tp <- dtree_perf[2, 2]

# Calculate metrics
accuracy <- (tp + tn) / (tp + tn + fp + fn)
print(accuracy)
error_rate <- (fp + fn) / (tp + tn + fp + fn)
print(error_rate)
sensitivity <- tp / (tp + fn) # Also known as recall
print(sensitivity)
specificity <- tn / (tn + fp)
print(specificity)
precision <- tp / (tp + fp)
print(precision)
f_measure <- (2 * precision * sensitivity) / (precision + sensitivity)
print(f_measure)

# Support Vector Machine Classifier

names(churn_data) <- c("State", "Account_Length", "Area_Code", "Phone_Number", "International_Plan", "Voice_Mail_Plan", "Number_Of_Voice_Mail_Messages", "Total_Day_Minutes", "Total_Day_Calls", "Total_Day_Charge", "Total_Eve_Minutes", "Total_Eve_Calls", "Total_Eve_Charge", "Total_Night_Minutes", "Total_Night_Calls", "Total_Night_Charge", "Total_International_Minutes", "Total_International_Calls", "Total_International_Charge", "Number_Of_Calls_To_Customer_Service", "Churn")

# remove 'Phone_Number' column
df <- churn_data[-4]

# Convert target variable 'Churn' to a factor
df$Churn <- factor(df$Churn, levels = c("False", "True"))

set.seed(1234)
train <- sample(nrow(df), 0.7 * nrow(df))
df.train <- df[train, ]
df.validate <- df[-train, ]
table(df.train$Churn)
table(df.validate$Churn)

library(e1071)

svm.model <- svm(Churn ~ ., data = df.train)
svm.pred <- predict(svm.model, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$Churn, svm.pred, dnn = c("Actual", "Predicted"))
svm.perf

tn <- svm.perf[1, 1]
fp <- svm.perf[1, 2]
fn <- svm.perf[2, 1]
tp <- svm.perf[2, 2]

# Calculate metrics
accuracy <- (tp + tn) / (tp + tn + fp + fn)
print(accuracy)
error_rate <- (fp + fn) / (tp + tn + fp + fn)
print(error_rate)
sensitivity <- tp / (tp + fn) # Also known as recall
print(sensitivity)
specificity <- tn / (tn + fp)
print(specificity)
precision <- tp / (tp + fp)
print(precision)
f_measure <- (2 * precision * sensitivity) / (precision + sensitivity)
print(f_measure)
