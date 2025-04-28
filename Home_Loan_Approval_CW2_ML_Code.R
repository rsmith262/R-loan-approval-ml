############################################################################
# check and load packages 

# function for checking if packages are installed and if not installing them
# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[!req]
  if (length(need) > 0) { 
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
}

using("dplyr",
      "ggplot2",
      "readr",
      "tidyr",
      "caTools",
      "rprojroot",
      "rstudioapi",
      "caret",
      "randomForest",
      "glue",
      "smotefamily",
      "car",
      "pROC")

# sets the working directory to the source file location
setwd(dirname(getActiveDocumentContext()$path))


# loads data using relative path
file_path <- "clean_loan_data.csv"
loan_data_tb <- read_csv(file_path)

# check data
print(loan_data_tb)

############################################################################
# Summary of the Data
# most of this code came from CW1 - highlighting some key points from the EDA and data cleaning

#check data types - str() returns the structure of the datatype and key components
str(loan_data_tb)

summary(loan_data_tb)

loan_status_count <- loan_data_tb %>% count(Loan_Status)
print(loan_status_count)

###########

# pairplot to show relationship between numerical data

# first transform the columns that are numerical but should be factors

loan_data_tb <- loan_data_tb %>%
  mutate(
    Gender = as.factor(Gender),
    Married = as.factor(Married),
    Dependents = factor(Dependents, levels = c("0", "1", "2", "3+")), # sets the order
    Education = as.factor(Education),
    Self_Employed = as.factor(Self_Employed),
    Loan_Amount_Term = as.factor(Loan_Amount_Term),
    Credit_History = factor(Credit_History, levels = c("0","1")),
    Property_Area = as.factor(Property_Area),
    Loan_Status = factor(Loan_Status, levels = c("Y", "N"))
  )

# selects just numerical cols, pairs() only works on numeric
num_data_tb <- loan_data_tb %>% select_if(is.numeric)

# calls pairs
pairs(num_data_tb, main = "Pairs plot of numerical variables")

###########

# scatter plot to show relationship between loan per month and total income

# compares loan amount and total income (total income is monthly)
ggplot(loan_data_tb, aes(x = Loan_Per_Month, y = Total_Income, color = Loan_Status)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Loan Per Month vs. Total Income (Monthly)",
       x = "Loan Per Month",
       y = "Total Income (Monthly)",
       color = "Loan Status") +
  theme_minimal()

###########

# bar plot for summary of EDA

create_bar_plot <- function(data, filter_variable) {
  ggplot(data, aes_string(x = filter_variable, fill = filter_variable)) +
    geom_bar(color = "black") +
    scale_fill_brewer(palette = "Set2") +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
    labs(title = paste("Bar Plot of", filter_variable), x = filter_variable, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Create the bar plot for Loan_Status
create_bar_plot(loan_data_tb, "Loan_Status")

###########
# Now bar plots split by Loan_Status

# Create the grouped bar plot

create_grouped_bar_plot <- function(data, x_var) {
  ggplot(data, aes_string(x = x_var, fill = "Loan_Status")) +
    geom_bar(position = position_dodge(), color = "black") +
    geom_text(stat = "count", aes_string(label = "..count.."), vjust = -0.5, position = position_dodge(0.9), size = 3) +
    labs(title = paste("Grouped Bar Plot of Loan Status by", x_var), x = x_var, y = "Count") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

create_grouped_bar_plot(loan_data_tb, "Credit_History")


###########

# Statistical significance tests for categorical variables

chi_squared_function <- function(var) {
  
  var_contingency_table <- table(loan_data_tb[[var]], loan_data_tb$Loan_Status)
  chi_squared_test <- chisq.test(var_contingency_table)
  print(chi_squared_test)
  
}

chi_squared_function("Married")
chi_squared_function("Credit_History")
chi_squared_function("Education")
chi_squared_function("Self_Employed")
chi_squared_function("Gender")
chi_squared_function("Dependents")
chi_squared_function("Loan_Amount_Term")

###########

# significance tests for numerical data

# Statistical tests - t-tests
t_test_result <- t.test(Total_Income ~ Loan_Status, data = loan_data_tb)
print(t_test_result)

t_test_result <- t.test(Loan_Per_Month ~ Loan_Status, data = loan_data_tb)
print(t_test_result)

t_test_result <- t.test(Loan_To_Income_Ratio ~ Loan_Status, data = loan_data_tb)
print(t_test_result)

############################################################################
# Train test split

# https://www.statology.org/train-test-split-r/

set.seed(101)# setting the seed so it's reproducible

# create the split
split <- sample.split(loan_data_tb$Loan_Status, SplitRatio = 0.7)

# split the data
train_data <- subset(loan_data_tb, split == TRUE)
test_data <- subset(loan_data_tb, split == FALSE)

# check the dimensions
dim(train_data)

# check distribution of Loan_Status
train_loan_status_count <- train_data %>% count(Loan_Status)
print(train_loan_status_count)

# check the dimensions
dim(test_data)

# check distribution of Loan_Status
test_loan_status_count <- test_data %>% count(Loan_Status)
print(test_loan_status_count)

############################################################################

# Baseline Model

# predict all majority class

test_target_len <- length(test_data$Loan_Status)
predictions_baseline = factor(rep("Y", test_target_len), levels = c("Y", "N"))
  

############################################################################

# Preprocessing Pipeline

# avoiding data leakage by applying the same process to test as to train and everything applied to test is purely based on the
# training data

preprocess_data <- function(train_data, test_data) {
  # label encodes the features - it finds all columns that are factors and converts to numeric
  # all features except the target variable
  #https://www.geeksforgeeks.org/convert-factor-to-numeric-and-numeric-to-factor-in-r-programming/
  # https://dplyr.tidyverse.org/reference/across.html
  train_features <- train_data %>% select(-Loan_Status) %>% mutate(across(where(is.factor), as.integer))
  test_features <- test_data %>% select(-Loan_Status) %>% mutate(across(where(is.factor), as.integer))
  
  # scale the training data
  # https://topepo.github.io/caret/pre-processing.html
  preproc_values <- preProcess(train_features, method = c("center", "scale"))
  train_features <- predict(preproc_values, train_features)
  
  # apply same scaling as used on the train set to the test set
  test_features <- predict(preproc_values, test_features)
  
  # combine features back with the target variable
  train_data_preprocessed <- cbind(train_features, Loan_Status = train_data$Loan_Status)
  test_data_preprocessed <- cbind(test_features, Loan_Status = test_data$Loan_Status)
  
  # return the preprocessed data and the preprocessing object as a list so the same can be applied
  # to train as to test
  list(train_data = train_data_preprocessed, test_data = test_data_preprocessed, preproc_values = preproc_values)
}

# applies function to train and test data set
preprocessed_data <- preprocess_data(train_data, test_data)
train_data <- preprocessed_data$train_data
test_data <- preprocessed_data$test_data



############################################################################
# Train models

# set up train control - this uses stratified cross-val
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = defaultSummary, savePredictions = TRUE)

set.seed(101) # sets seed so it is reproducible
# Logistic Regression
model_lr <- train(Loan_Status ~ ., # specifies Loan_Status is the target variable
                  data = train_data, # specifies data set to use
                  method = "glm", # glm is the model type for LR
                  family = binomial, # binomial is correct for binary classification
                  trControl = train_control, # uses the trainControl set above
                  metric="Accuracy") # uses accuracy as the metric to help woith model training

set.seed(101) # sets seed so it is reproducible
# Train the decision tree model
model_dt <- train(Loan_Status ~ ., # specifies Loan_Status is the target variable
                  data = train_data, # specifies data set to use
                  method = "rpart", # rpart specifies to use decision tree
                  trControl = train_control, # uses the trainControl set above
                  metric = "Accuracy")

set.seed(101) # sets seed so it is reproducible
# Random Forest
model_rf <- train(Loan_Status ~ .,# specifies Loan_Status is the target variable
                  data = train_data, # specifies data set to use
                  method = "rf",# rf specifies to use random forest
                  trControl = train_control, # uses the trainControl set above
                  metric="Accuracy")


############################################################################
# Make predictions - using caret

# predict using Logistic Regression
predictions_lr <- predict(model_lr, newdata = test_data)


# predict using Decision Tree
predictions_dt <- predict(model_dt, newdata = test_data)


# predict using Random Forest
predictions_rf <- predict(model_rf, newdata = test_data)


############################################################################
# Evaluation

# Evaluation function

# https://topepo.github.io/caret/measuring-performance.html#class
# https://www.geeksforgeeks.org/how-to-calculate-auc-area-under-curve-in-r/
evaluate_model <- function(predictions, true_labels, positive_class = "Y", chart_title) {
  cm <- confusionMatrix(predictions, true_labels, mode = "prec_recall")
  
  # extract metrics from the confusion matrix
  # https://stackoverflow.com/questions/23433641/how-to-extract-accuracy-from-carets-confusionmatrix
  accuracy <- cm$overall["Accuracy"]
  sensitivity <- cm$byClass["Sensitivity"]
  precision <- cm$byClass["Precision"]
  recall <- sensitivity
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # calc AUC ROC
  # https://www.geeksforgeeks.org/how-to-calculate-auc-area-under-curve-in-r/
  roc_obj <- roc(true_labels, as.numeric(predictions == positive_class))
  auc_roc <- auc(roc_obj)
  
  
  # put all metrics calculated into a tibble
  metrics <- tibble(
    Metric = c("Accuracy", "F1 Score", "AUC ROC"),
    Value = c(accuracy, f1_score, auc_roc)
  )
  
  
  # visualise the confusion matrix
  # first create dataframe
  confmat_table <- as.data.frame(cm$table)
  
  # then plot with ggplot
  # https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot
  confmat_plot <-  ggplot(data = confmat_table, aes(x = Reference, y = Prediction)) +
                    geom_tile(aes(fill = Freq), color = "white") +
                    geom_text(aes(label = Freq), vjust = 1) +
                    scale_fill_gradient(low = "green", high = "blue") +
                    labs(title = glue("Confusion Matrix {chart_title}"), x = "Actual", y = "Predicted") +
                    theme_minimal()
 
  print(confmat_plot)

  return(metrics)

  
}

# evaluate baseline (dumb) model

evaluation_baseline <- evaluate_model(predictions_baseline, test_data$Loan_Status, chart_title = "Baseline")
print(evaluation_baseline)

# evaluate Logistic Regression
evaluation_lr <- evaluate_model(predictions_lr, test_data$Loan_Status, chart_title = "LR")
print(evaluation_lr)

# evaluate Decision Tree
evaluation_dt <- evaluate_model(predictions_dt, test_data$Loan_Status, chart_title = "DT")
print(evaluation_dt)

# evaluate Random Forest
evaluation_rf <- evaluate_model(predictions_rf, test_data$Loan_Status, chart_title = "RF")
print(evaluation_rf)

# put all results together
all_results <- bind_rows(
  evaluation_baseline %>% mutate(Model = "Baseline Dumb Model"),
  evaluation_lr %>% mutate(Model = "Logistic Regression"),
  evaluation_dt %>% mutate(Model = "Decision Tree"),
  evaluation_rf %>% mutate(Model = "Random Forest")
)

print(all_results)


############################################################################
# applying SMOTE

# apply SMOTE to the training data
# https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/SMOTE
smote_result <- SMOTE(train_data[,-ncol(train_data)], train_data$Loan_Status)
train_data_smote <- smote_result$data

# convert target back to factor
train_data_smote$class <- as.factor(train_data_smote$class)
train_data_smote <- train_data_smote %>% rename(Loan_Status = class)

# checks class distribution to make sure it is balanced
table(train_data_smote$Loan_Status)

############################################################################
# models with SMOTE

set.seed(101) # sets the seed
# train models on SMOTE data

# Logistic Regression
model_lr_smote <- train(Loan_Status ~ .,
                        data = train_data_smote, #uses smote data
                        method = "glm",
                        family = binomial,
                        trControl = train_control,
                        metric = "Accuracy")

set.seed(101) # Setting the seed
# Train the decision tree model
model_dt_smote <- train(Loan_Status ~ .,
                        data = train_data_smote, #uses smote data
                        method = "rpart",
                        trControl = train_control,
                        metric = "Accuracy")

set.seed(101) # Setting the seed
# Random Forest
model_rf_smote <- train(Loan_Status ~ .,
                        data = train_data_smote, #uses smote data
                        method = "rf",
                        trControl = train_control,
                        metric = "Accuracy")


############################################################################
# predict and evaluate with SMOTE

# make predictions
predictions_lr_smote <- predict(model_lr_smote, newdata = test_data)
predictions_dt_smote <- predict(model_lr_smote, newdata = test_data)
predictions_rf_smote <- predict(model_rf_smote, newdata = test_data)


# evaluate LR with SMOTE
evaluation_lr_smote <- evaluate_model(predictions_lr_smote, test_data$Loan_Status, chart_title = "LR (Smote)")
print(evaluation_lr_smote)

# evaluate DT with SMOTE
evaluation_dt_smote <- evaluate_model(predictions_dt_smote, test_data$Loan_Status, chart_title = "DT (Smote)")
print(evaluation_dt_smote)

# evaluate RF with SMOTE
evaluation_rf_smote <- evaluate_model(predictions_rf_smote, test_data$Loan_Status, chart_title = "RF (Smote)")
print(evaluation_rf_smote)

# put all results together
all_results_smote <- bind_rows(
  evaluation_lr_smote %>% mutate(Model = "Logistic Regression (smote)"),
  evaluation_dt_smote %>% mutate(Model = "Decision Tree (smote)"),
  evaluation_rf_smote %>% mutate(Model = "Random Forest (smote)")
)

print(all_results_smote)


###########################################################################
#Feature importance

# check which are the most important features in each algorithm

#LR

# gets the feature importance
importance_lr <- varImp(model_lr)

# prints it
print(importance_lr)

# plots it
plot(importance_lr, main = "Feature Importance for Logistic Regression Model")

#DT

importance_dt <- varImp(model_dt)

print(importance_dt)

plot(importance_dt, main = "Feature Importance for Decision Tree Model")

#RF

importance_rf <- varImp(model_rf)

print(importance_rf)

plot(importance_rf, main = "Feature Importance for Random Forest Model")


###########################################################################
# Tune Grid and feature selection

# remove least important features

# logistic regression

train_data_lr_fs = select(train_data, -c(Dependents))

test_data_lr_fs = select(test_data, -c(Dependents))

# decision tree

train_data_dt_fs = select(train_data, -c(Dependents, Loan_Amount_Term, Self_Employed, Education, Loan_Per_Month, Gender))

test_data_dt_fs = select(test_data, -c(Dependents, Loan_Amount_Term, Self_Employed, Education, Loan_Per_Month, Gender))

# random forest

train_data_rf_fs = select(train_data, -c(Married, Self_Employed, Education, Gender))

test_data_rf_fs = select(test_data, -c(Married, Self_Employed, Education, Gender)) 

########################
#train models

# logistic regression
set.seed(101) # Setting the seed

# Train models on feature sleected data

# Logistic Regression
model_lr_fs <- train(Loan_Status ~ .,
                     data = train_data_lr_fs,
                     method = "glm",
                     family = binomial,
                     trControl = train_control,
                     metric = "Accuracy")

# decision tree
set.seed(101) # Setting the seed
# Train the decision tree model
model_dt_fs <- train(Loan_Status ~ .,
                    data = train_data_dt_fs,
                    method = "rpart",
                    trControl = train_control,
                    tuneLength = 10, # number of different parameter values to try
                    metric = "Accuracy"
)

#random forest
# this defines the grid for the grid search
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10)
)

# Train the Random Forest model
set.seed(101) # Setting the seed
model_rf_fs <- train(Loan_Status ~ ., 
                    data = train_data_rf_fs, 
                    method = "rf", 
                    trControl = train_control,
                    tuneGrid = tune_grid, # using tunegrid
                    ntree = 50,
                    nodesize = 2, # min nodes
                    maxnodes = 20, #max nodes
                    metric = "Accuracy"
)

# Print the best model parameters
print(model_rf$bestTune)

#####################
#predictions

predictions_lr_fs <- predict(model_lr_fs, newdata = test_data_lr_fs)

predictions_dt_fs <- predict(model_dt_fs, newdata = test_data_dt_fs)

predictions_rf_fs <- predict(model_rf_fs, newdata = test_data_rf_fs)



evaluation_lr_fs <- evaluate_model(predictions_lr_fs, test_data_lr_fs$Loan_Status, chart_title = "LR (fs)")
print(evaluation_lr_fs)

evaluation_dt_fs <- evaluate_model(predictions_dt_fs, test_data_dt_fs$Loan_Status, chart_title = "DT (fs)")
print(evaluation_dt_fs)


evaluation_rf_fs <- evaluate_model(predictions_rf_fs, test_data_rf_fs$Loan_Status, chart_title = "RF (fs)")
print(evaluation_rf_fs)

all_results_fs <- bind_rows(
  evaluation_lr_fs %>% mutate(Model = "Logistic Regression (fs)"),
  evaluation_dt_fs %>% mutate(Model = "Decision Tree (fs)"),
  evaluation_rf_fs %>% mutate(Model = "Random Forest (fs)")
)

print(all_results_fs)

#############################

# join and pivot results into table

combined_results <- bind_rows(all_results, all_results_smote, all_results_fs)

# flatten results

# pivot results to create a table in a good format to read
pivot_results <- combined_results %>%
  pivot_wider(names_from = Metric, values_from = Value)

# printing the table
print(pivot_results)










