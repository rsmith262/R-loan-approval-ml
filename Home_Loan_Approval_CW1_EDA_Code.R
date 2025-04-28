##### check and load packages

# function for checking if packages are installed and if not installing them
# [1] https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[!req]
  if (length(need) > 0) { 
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
}

using("dplyr", "tidyr", "ggplot2","readr") # applying function

# # split the file path so the marker can just changer the folder they have saved the csv in 
folder = "C:\\Users\\richard.smith\\Desktop\\R_Scripts\\loan_data\\"
file = "loan_sanction_data.csv"
full_path = full_path <- paste0(folder, file)

# load data into a tibble
loan_data_tb <- read_csv(file=full_path)

# check data
print(loan_data_tb)

############################################################################
#### Summary

# variables for basic number of cols and rows
# no of cols
num_col <- ncol(loan_data_tb)
print(num_col)
# no of rows
num_row <- nrow(loan_data_tb)
print(num_row)
# etc

#check data types - str() returns the structure of the datatype and key components
str(loan_data_tb)

# statistical summary - explain in report
# shows outliers Applicant Income and loan amount
# shows there is missing data
summary(loan_data_tb)

# Number and percentage of missing data
sapply(loan_data_tb, function(x) sum(is.na(x)))

# Check for duplicate rows
duplicate_rows <- duplicated(loan_data_tb)
duplicates <- loan_data_tb[duplicate_rows, ]
print(duplicates)
# count the number
num_duplicates <- sum(duplicate_rows)

# using  dplyr library to get count of missing data
na_count <- loan_data_tb %>%
  summarise_all(~sum(is.na(.)))

# Convert to long format
na_count_long <- na_count %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "NA_Count")

# using  dplyr library to get percentage of missing data
# shows I will need to handle missing values later
na_percentage <- loan_data_tb %>%
  summarise_all(~mean(is.na(.)) * 100)

# bar plot showing missing vals
# [2] https://r-graph-gallery.com/218-basic-barplots-with-ggplot2.html
ggplot(na_count_long, aes(x = Variable, y = NA_Count, fill = Variable)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette = "Set3") + # brewer is colour blind friendly
  geom_text(aes(label = NA_Count), vjust = -0.5) +
  labs(title = "Bar Plot of Missing Values", x = "Variable", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# summary of relationship between categorical and numerical variables in uncleaned data

# create funnction to split a dataset into categorical and numerical fields
# [3] https://www.statology.org/r-function-return/
split_tibble_num_cat <- function(data) {
  # get names of categorical cols
  categorical_cols <- data %>%
    select_if(~is.factor(.) | is.character(.)) %>%
    names()
  
  # get names of categorical cols
  numeric_cols <- data %>%
    select_if(is.numeric) %>%
    names()
  
  # create new tb ofcategorical and numeric
  categorical_tb <- data %>% select(one_of(categorical_cols))
  numeric_tb <- data %>% select(one_of(numeric_cols))
  
  # return them as list - these are plit out after calling the function
  return(list(categorical_tb = categorical_tb, numeric_tb = numeric_tb))
}

# Call the function
split_tb_result <- split_tibble_num_cat(loan_data_tb)

# Assign cat and num tb's to variables
categorical_tb <- split_tb_result$categorical_tb
numeric_tb <- split_tb_result$numeric_tb

# plot to see relationship between numerical variables:
pairs_plot <- pairs(numeric_tb, main="Pairs plot of numerical variables (raw)")

# explore Loan_Amount_Term in more detail as looks discrete rather than contiuous:

# this gets count of values in the column (bit like value_counts)
# As shown there are 10 different term amounts so i think this would be better being a Factor too
value_counts_term <- loan_data_tb %>% count(Loan_Amount_Term)
print(value_counts_term) # results suggest this is also categorical

# summary of categorical columns - mode

# Function to calculate mode
# [4] https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
calculate_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# call function
cat_mode_summary <- categorical_tb %>%
  summarise(across(everything(), calculate_mode))

############################################################################
##### Data Cleaning

#start of transformations
# deal with datatypes
# All categorical should be factors - factors are better for categorical and faster - find references
# The numerical fields of Credit_History and Loan_Term_Amoun should be Factor too


#create a new dataframe so we can refer back to original data when needed
# convert to factors
clean_data_tb <- loan_data_tb %>%
  mutate(
    Gender = as.factor(Gender),
    Married = as.factor(Married),
    Dependents = factor(Dependents, levels = c("0", "1", "2", "3+")), # sets the order
    Education = as.factor(Education),
    Self_Employed = as.factor(Self_Employed),
    Loan_Amount_Term = as.factor(Loan_Amount_Term),
    Credit_History = factor(Credit_History, levels = c("0","1")),
    Property_Area = as.factor(Property_Area),
    Loan_Status = as.factor(Loan_Status),
  )

# check changes
str(clean_data_tb)


##### Delete LoanID

#remove Loan_ID from the clean data set
clean_data_tb <- clean_data_tb %>%
  select(-Loan_ID)

##### Standardise col names ApplicantIncome to Applicant_Income

# rename cols
clean_data_tb <- clean_data_tb %>%
  rename(
    Applicant_Income = ApplicantIncome,
    Coapplicant_Income = CoapplicantIncome,
    Loan_Amount = LoanAmount
  )

#### deal with missing values


# Gender/Married
# There is a relationship between these I will leverage for my method
# 28% of female applicants are married. 73% of males are married
# Rather than just using mode of each I will impute NA's in the Gender column based on Married and vice versa.


# group by gender and married
gender_married_group <- clean_data_tb %>%
  group_by(Gender, Married) %>%
  summarise(Count = n(), .groups = 'drop') # summarises count

# take group and make pivot table
gender_married_pivot <- gender_married_group %>%
  pivot_wider(names_from = Married, values_from = Count, values_fill = list(Count = 0))

# Statistically confirm correlation using chi-squared test

# [5] https://www.datacamp.com/tutorial/contingency-analysis-r

# create contingency table
contingency_table_gender_married <- table(clean_data_tb$Gender, clean_data_tb$Married)

chi_sq_gender_married <- chisq.test(contingency_table_gender_married)
print(chi_sq_gender_married)

# [6] https://stackoverflow.com/questions/68804434/how-can-i-fill-missing-values-based-on-a-condition-in-r
# vectorised conditional function. If gender is null and married is yes then male if no then female

# interpolate Gender nulls based on Married column
clean_data_tb <- clean_data_tb %>%
  mutate(Gender = case_when(
    is.na(Gender) & Married == "Yes" ~ "Male",
    is.na(Gender) & Married == "No" ~ "Female",
    TRUE ~ Gender
  ))

# interpolate Married nulls based on Gender column - if Male then married = yes, if female then married = no
clean_data_tb <- clean_data_tb %>%
  mutate(Married = case_when(
    is.na(Married) & Gender == "Male" ~ "Yes",
    is.na(Married) & Gender == "Female" ~ "No",
    TRUE ~ Married
  ))

# check above has worked
# group by gender and married
clean_gender_married_group <- clean_data_tb %>%
  group_by(Gender, Married) %>%
  summarise(Count = n(), .groups = 'drop')

# take group and make pivot table
clean_gender_married_pivot <- clean_gender_married_group %>%
  pivot_wider(names_from = Married, values_from = Count, values_fill = list(Count = 0))

# update nulls in loan amount with median categorised by property area

# [7] https://stackoverflow.com/questions/17435810/getting-median-of-a-column-where-value-of-another-column-is-1-in-r
median_loan_amount_Urban <- round(median(clean_data_tb$Loan_Amount[clean_data_tb$Property_Area == "Urban"], na.rm = TRUE))
median_loan_amount_semiurban <- round(median(clean_data_tb$Loan_Amount[clean_data_tb$Property_Area == "Semiurban"], na.rm = TRUE))
median_loan_amount_rural <- round(median(clean_data_tb$Loan_Amount[clean_data_tb$Property_Area == "Rural"], na.rm = TRUE))


print(median_loan_amount_Urban)
print(median_loan_amount_semiurban)
print(median_loan_amount_rural)


# replace nulls with median calculated above
clean_data_tb <- clean_data_tb %>%
  mutate(Loan_Amount = case_when(
    is.na(Loan_Amount) & Property_Area == "Urban" ~ median_loan_amount_Urban,
    is.na(Loan_Amount) & Property_Area == "Semiurban" ~ median_loan_amount_semiurban,
    is.na(Loan_Amount) & Property_Area == "Rural" ~ median_loan_amount_rural,
    TRUE ~ Loan_Amount
  ))


# interpolate Credit_History nulls based on Loan_Status, if yes then 1, if no then 0
clean_data_tb <- clean_data_tb %>%
  mutate(Credit_History = case_when(
    is.na(Credit_History) & Loan_Status == "Y" ~ "1",
    is.na(Credit_History) & Loan_Status == "N" ~ "0",
    TRUE ~ Credit_History
  ))

# interpolate Dependents, Self_Employed, Loan_Amount_Term
# nulls will be made mode of column

# custom mode function
# [8] https://dev.to/rapp2043/finding-the-mode-in-r-a-step-by-step-guide-49h1
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# replace NA values with the mode
# [9] https://www.statology.org/dplyr-replace-na-with-mean/

# replace NAs values with mode for multiple columns
clean_data_tb <- clean_data_tb %>%
  mutate(across(c(Dependents, Self_Employed, Loan_Amount_Term), ~replace_na(., get_mode(.[!is.na(.)]))))


# check clean data set for missing data
# using  dplyr library to get count of missing data
clean_na_count <- clean_data_tb %>%
  summarise_all(~sum(is.na(.)))

# Convert to long format
clean_na_count_long <- clean_na_count %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "NA_Count")

############################################################################
##### EDA

## Feature Engineering

# creating a total_income column
clean_data_tb <- clean_data_tb %>%
  mutate(Total_Income = Applicant_Income + Coapplicant_Income)

clean_data_tb <- clean_data_tb %>%
  mutate(Full_Loan_Amount = Loan_Amount * 1000)

# creating a loan per month column so it can be compared with income (income is monthly)
clean_data_tb <- clean_data_tb %>%
  mutate(
    Loan_Per_Month = Full_Loan_Amount / as.numeric(as.character(Loan_Amount_Term))
  )

# creating a loan to income ratio column
clean_data_tb <- clean_data_tb %>%
  mutate(Loan_To_Income_Ratio = Loan_Per_Month / Total_Income)

## Start Exploring the relationship between variables

# split the clean data set into numeric and categorical variables again
clean_split_tb_result <- split_tibble_num_cat(clean_data_tb)

# assign to variables
clean_categorical_tb <- clean_split_tb_result$categorical_tb
clean_numeric_tb <- clean_split_tb_result$numeric_tb

# exploring the numerical variables
# plot to see relationship between numerical variables:

clean_pairs_plot <- pairs(clean_numeric_tb, main="Pairs plot of numerical variables (clean)")

# Looks like the most interesting relationship is between the income columns and loan amount
# where there is a strong positive linear relationship

# histograms

# [10] https://r-graph-gallery.com/42-colors-names.html
hist(clean_numeric_tb$Loan_Amount, 
     main = "Histogram of Loan Amount", 
     xlab = "Loan Amount", 
     col = "darkslategray3", 
     border = "black")

hist(clean_numeric_tb$Loan_Per_Month, 
     main = "Histogram of Loan per Month", 
     xlab = "Total Income",
     col = "darkslategray3", 
     border = "black")

hist(clean_numeric_tb$Total_Income, 
     main = "Histogram of Total Income (Monthly)", 
     xlab = "Total Income",
     breaks = 20, # adjusts number of bins
     col = "darkslategray3", 
     border = "black")

# test for normal distribution

shapiro.test(clean_numeric_tb$Loan_Amount)
shapiro.test(clean_numeric_tb$Loan_Per_Month)
shapiro.test(clean_numeric_tb$Total_Income)
# both extremely low p-value so strong evidence that it is not normally distrubuted.


# [11] https://r-graph-gallery.com/50-51-52-scatter-plot-with-ggplot2.html
ggplot(clean_numeric_tb, aes(x = Loan_Per_Month, y = Total_Income)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatter Plot with Linear Fit - Loan Per Month vs Monthly Total Income", x = "Loan Per Month", y = "Monthly Total Income") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Pearson correlation
cor(clean_numeric_tb$Loan_Per_Month, clean_numeric_tb$Total_Income)

# evidence from the scatter plot and the correlation indicates a positive relationship
# with a weak correlation

# check how these factors influence loan status outcome
ggplot(clean_data_tb, aes(x = Loan_Amount, y = Total_Income, color = Loan_Status)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Loan Per Month vs. Total Income",
       x = "Loan Per Month",
       y = "Total Income",
       color = "Loan Status") +
  theme_minimal()

# check how these factors influence loan status outcome
ggplot(clean_data_tb, aes(x = Loan_Per_Month, y = Total_Income, color = Loan_Status)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Loan Per Month vs. Total Income",
       x = "Loan Per Month",
       y = "Total Income",
       color = "Loan Status") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5))

# checking loan to income ratio with a violin plot

ggplot(clean_data_tb, aes(x = Loan_Status, y = Loan_To_Income_Ratio, fill = Loan_Status)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.5) +  # boxplot with transparency
  geom_jitter(width = 0.2, alpha = 0.3) +   # Add jittered points
  labs(title = "Violin Plot of Loan to Income Ratio by Loan Status", x = "Loan Status", y = "Loan to Income Ratio") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5))


# checking distribution and outliers with boxplots

# convert to long format
clean_num_data_long <- clean_numeric_tb %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Create boxplots for numeric fields
# [12] https://r-graph-gallery.com/262-basic-boxplot-with-ggplot2.html
# doing incomes seperately to Loan_Amount as the scales were a bit out
ggplot(clean_num_data_long %>% filter(Variable %in% c("Applicant_Income", "Coapplicant_Income", "Total_Income")),
       aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplots of All Numerical Fields", x = "Variable", y = "Value") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(clean_num_data_long %>% filter(Variable == "Total_Income"),
       aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total Income", x = "Variable", y = "Value") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create boxplots for numeric fields
ggplot(clean_num_data_long %>% filter(Variable == "Loan_Per_Month"),
       aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of Loan Per Month", x = "Variable", y = "Value") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Statistical tests - t-tests
t_test_result <- t.test(Total_Income ~ Loan_Status, data = clean_data_tb)
print(t_test_result)

t_test_result <- t.test(Loan_Per_Month ~ Loan_Status, data = clean_data_tb)
print(t_test_result)

t_test_result <- t.test(Loan_To_Income_Ratio ~ Loan_Status, data = clean_data_tb)
print(t_test_result)



###### look at categorical columns

# convert to long format
clean_cat_data_long <- clean_categorical_tb %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# barplots
create_bar_plot <- function(data, filter_variable) {
  ggplot(data %>% filter(Variable == filter_variable), aes(x = Value, fill = Value)) +
    geom_bar(color = "black") +
    scale_fill_brewer(palette = "Set2") + # brewer is colour blind friendly
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
    labs(title = paste("Bar Plot of", filter_variable), x = filter_variable, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

create_bar_plot(clean_cat_data_long, "Gender")
create_bar_plot(clean_cat_data_long, "Married")
create_bar_plot(clean_cat_data_long, "Dependents")
create_bar_plot(clean_cat_data_long, "Education")
create_bar_plot(clean_cat_data_long, "Self_Employed")
create_bar_plot(clean_cat_data_long, "Loan_Amount_Term")
create_bar_plot(clean_cat_data_long, "Credit_History")
create_bar_plot(clean_cat_data_long, "Property_Area")
create_bar_plot(clean_cat_data_long, "Loan_Status")


# Now bar plots split by Loan_Status

# Create the grouped bar plot

create_grouped_bar_plot <- function(data, x_var) {
  ggplot(data, aes_string(x = x_var, fill = "Loan_Status")) +
    geom_bar(position = position_dodge(), color = "black") +
    geom_text(stat = "count", aes_string(label = "..count.."), vjust = -0.5, position = position_dodge(0.9), size = 3) +
    labs(title = paste("Grouped Bar Plot of Loan Status by", x_var), x = x_var, y = "Count") +
    scale_fill_brewer(palette = "Set2") + # brewer is colour blind friendly
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

create_grouped_bar_plot(clean_data_tb, "Gender")
create_grouped_bar_plot(clean_data_tb, "Married")
create_grouped_bar_plot(clean_data_tb, "Dependents")
create_grouped_bar_plot(clean_data_tb, "Education")
create_grouped_bar_plot(clean_data_tb, "Self_Employed")
create_grouped_bar_plot(clean_data_tb, "Loan_Amount_Term")
create_grouped_bar_plot(clean_data_tb, "Credit_History")
create_grouped_bar_plot(clean_data_tb, "Property_Area")


#### Statistical significance tests for categorical variables

chi_squared_function <- function(var) {
  
  var_contingency_table <- table(clean_data_tb[[var]], clean_data_tb$Loan_Status)
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

############################################################################
#### extra charts for report

create_grouped_bar_plot(loan_data_tb, "Credit_History")

############################################################################
#### deleting columns that could cause multicollinearity

clean_data_tb <- clean_data_tb %>%
  select(-c("Applicant_Income", "Coapplicant_Income", "Loan_Amount", "Full_Loan_Amount"))

############################################################################
#### saving output to .csv
# comment out, uncomment if you want to save it

# folder = "C:\\Users\\richard.smith\\Desktop\\R_Scripts\\loan_data\\"
# file = "clean_data_tb.csv"
# full_path = full_path <- paste0(folder, file)
# 
# write_csv(clean_data_tb, file=full_path)


