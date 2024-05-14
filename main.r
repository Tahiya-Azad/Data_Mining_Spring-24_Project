options(repos = c(CRAN = "http://cran.rstudio.com"))
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
update.packages("tidyverse")
install.packages("tidyr")
install.packages("e1071")
library(tidyr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(treemap)
library(e1071)

churn_data <- read.csv("/Users/tahiyaazad/Desktop/CMPT 436/DataMiningProject_2024/DM_2024/churn.txt")
churn_data <- as_tibble(churn_data)

options(width = 140)
print(churn_data)

## Pie Chart
library(RColorBrewer)

# Calculating total sums for Day Mins, Day Charge, Eve Mins, Eve Charge
Prop <- c(sum(churn_data$Day.Mins), sum(churn_data$Day.Charge), sum(churn_data$Eve.Mins), sum(churn_data$Eve.Charge))
lbls <- c("Day Mins", "Day Charge", "Eve Mins", "Eve Charge")
pct <- round(Prop / sum(Prop) * 100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
myPalatte <- brewer.pal(4, "Set2")
pie(Prop, labels = lbls2, border = "white", col = myPalatte, main = "The percentage of Calls and Charges based on Day and Evening")

## Bar Plot
int_l_plan_yes <- churn_data %>%
    filter(Int.l.Plan == "yes") %>%
    group_by(State, Int.l.Plan) %>%
    summarise(Count = n(), .groups = "drop")

##Creating the bar plot
plot <- ggplot(int_l_plan_yes, aes(x = State, y = Count, fill = Int.l.Plan)) +
    geom_bar(stat = "identity", width = 0.8) +
    scale_fill_manual(values = c("yes" = "#8346b4")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
    labs(x = "State", y = "Number of Customers", title = "International Plan of Customers by State")

print(plot)

## Histogram
Plotting a histogram of Customer Service Calls
plot <- ggplot(churn_data, aes(x = CustServ.Calls)) +
    geom_histogram(binwidth = 1, fill = "#c0ffe0", color = "black") +
    labs(x = "Customer Service Calls", y = "Frequency", title = "Distribution of Customer Service Calls") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
print(plot)

## Heatmap
state_vmail_plan <- churn_data %>%
    group_by(State, VMail.Plan) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(VMail.Plan = factor(VMail.Plan, levels = c("yes", "no"))) # Ensure proper factor levels


colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(100) # Creates 100 shades from Yellow to Orange to Red


plot <- ggplot(state_vmail_plan, aes(x = VMail.Plan, y = State, fill = count)) +
    geom_tile(color = "white") + # White borders for distinction
    scale_fill_gradientn(colors = colors) +
    labs(
        title = "Distribution of Voicemail Plan Usage by State",
        x = "Voicemail Plan",
        y = "State",
        fill = "Count"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
    )

print(plot)

## Treemap
data <- data.frame(
    category = c("Day Mins", "Day Charge", "Eve Mins", "Eve Charge"),
    value = c(sum(churn_data$Day.Mins), sum(churn_data$Day.Charge), sum(churn_data$Eve.Mins), sum(churn_data$Eve.Charge))
)
treemap(data,
    index = "category",
    vSize = "value",
    title = "Treemap of Daily and Evening Usage Metrics"
)


## Boxplot
plot <- ggplot(data = churn_data, aes(x = VMail.Message, y = State, fill = State)) +
    geom_boxplot(stat = "boxplot", coef = 0, outlier.shape = NA) +
    labs(title = "Boxplot of Voice Mail Messages by State", x = "VMail Messages", y = "State
    ") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
print(plot)

## Doughnut Chart
counts <- c(
    sum(churn_data$Day.Mins, na.rm = TRUE),
    sum(churn_data$Day.Charge, na.rm = TRUE),
    sum(churn_data$Night.Mins, na.rm = TRUE),
    sum(churn_data$Night.Charge, na.rm = TRUE)
)

categories <- c("Day Mins", "Day Charge", "Night Mins", "Night Charge")
data <- data.frame(Category = categories, Value = counts)

plot <- ggplot(data, aes(x = "", y = Value, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = c("Day Mins" = "#006aff", "Day Charge" = "#ffa600", "Night Mins" = "#fffb00", "Night Charge" = "#9500ff")) +
    labs(title = "Distribution of Calls and Charges Day vs Night") +

    # White circle
    annotate("text", x = 0, y = 0, label = "", size = 10, color = "white")

print(plot)
library(rpart)
library(rpart.plot)
data <- ("~/Desktop/CMPT 436/DataMiningProject_2024/churn.txt")
churn_data <- read.csv(data, stringsAsFactors = TRUE)
names(churn_data) <- c("State", "Account_Length", "Area_Code", "Phone_Number", "International_Plan", "Voice_Mail_Plan", "Number_Of_Voice_Mail_Messages", "Total_Day_Minutes", "Total_Day_Calls", "Total_Day_Charge", "Total_Eve_Minutes", "Total_Eve_Calls", "Total_Eve_Charge", "Total_Night_Minutes", "Total_Night_Calls", "Total_Night_Charge", "Total_International_Minutes", "Total_International_Calls", "Total_International_Charge", "Number_Of_Calls_To_Customer_Service", "Churn")
df <- churn_data[-4]
churn_data$Churn <- factor(churn_data$Churn, levels = c("True", "False"))

set.seed(1234)
train <- sample(nrow(churn_data), 0.7 * nrow(df))
df.train <- df[train,]
df.validate <- df[-train, ]
table(df.train$Churn)
table(df.validate$Churn)

library(e1071)

nb.model <- naiveBayes(Churn~ ., data = df.train)
nb.pred <- predict(nb.model, df.validate)
nb.perf <- table(df.validate$Churn, nb.pred, dnn=c("Actual", "Predicted"))
nb.perf
tn <- nb.perf[1, 1]
fp <- nb.perf[1, 2]
fn <- nb.perf[2, 1]
tp <- nb.perf[2, 2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
accuracy
error_rate <- (fp + fn) / (tp + tn + fp + fn)
error_rate
sensitivity <- tp / (tp + fn)
sensitivity
specificity <- tn / (tn + fp)
specificity
precision <- tp / (tp + fp)
precision
f_measure <- (2 * precision * sensitivity) / (precision + sensitivity)
f_measure
