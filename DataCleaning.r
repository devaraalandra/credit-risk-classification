# Import libraries

library(dplyr)
library(ggplot2)
library(DataExplorer)

# Set the file path
file_path <- "C://Users//Asus ROG//OneDrive - Asia Pacific University//01 Projects//pfda finals//credit_risk_classification//5. credit_risk_classification.csv"

# Load the data into R
data <- read.csv(file_path)
str(data)

#Cleaning Data check if there is a empty column, change the empty colum into NA and after that search where is the data has NA 
data <- data %>%
  mutate(across(everything(), ~replace(., . == "", NA)))
#print the plot and see the percantage off the missing value
plot_missing(data)

missing_summary <- sapply(data, function(x) sum(is.na(x)))
print(missing_summary)

#search the mode on other payment plan
ggplot(data, aes(x = other_payment_plans)) +
  geom_bar()

# Find the mode of the 'other_payment_plans' column - because on the payment plan there is a missing value
find_mode <- function(x) {
  unique_x <- unique(x[!is.na(x)])  # Remove NA values
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

mode_other_payment_plans <- find_mode(data$other_payment_plans)
print(paste("Mode of 'other_payment_plans':", mode_other_payment_plans))

#Replace NA values with the mode
data$other_payment_plans <- ifelse(is.na(data$other_payment_plans), 
                                   mode_other_payment_plans, 
                                   data$other_payment_plans)

# Check for missing values in 'other_payment_plans'
na_count <- sum(is.na(data$other_payment_plans))
print(paste("Number of missing values in 'other_payment_plans':", na_count))

# Display the updated column
print(table(data$other_payment_plans))





## Preprocessing

# Identify columns with missing values
missing_summary <- sapply(data, function(x) sum(is.na(x)))
print(missing_summary)

# Results: No missing columns, proceed

# Check for unrounded numbers in 'duration' column
unrounded_duration <- any(data$duration %% 1 != 0)
print(paste("Are there unrounded numbers in 'duration'? ", unrounded_duration))

# Check for unrounded numbers in 'residence_since' column
unrounded_residence <- any(data$residence_since %% 1 != 0)
print(paste("Are there unrounded numbers in 'residence_since'? ", unrounded_residence))

# Round up 'ceiling' for duration & residence since

data$duration <- ceiling(data$duration)           # Rounds up
data$residence_since <- round(data$residence_since)  # Rounds to nearest integer

# Check for unrounded numbers in 'duration' column
unrounded_duration <- any(data$duration %% 1 != 0)
print(paste("Are there unrounded numbers in 'duration'? ", unrounded_duration))

# Check for unrounded numbers in 'residence_since' column
unrounded_residence <- any(data$residence_since %% 1 != 0)
print(paste("Are there unrounded numbers in 'residence_since'? ", unrounded_residence))

# Convert categorical variables to factors
data$checking_status <- as.factor(data$checking_status)
data$credit_history <- as.factor(data$credit_history)

# View levels of a factor
levels(data$checking_status)

# Visualize outliers
boxplot(data$credit_amount, main = "Credit Amount")

# Remove outliers (e.g., for `credit_amount`)
Q1 <- quantile(data$credit_amount, 0.25, na.rm = TRUE)
Q3 <- quantile(data$credit_amount, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
data <- data[data$credit_amount >= (Q1 - 1.5 * IQR) & data$credit_amount <= (Q3 + 1.5 * IQR), ]



# Check structure of the data
str(data)

# Check for missing values in both columns
sum(is.na(data$credit_history))
sum(is.na(data$credit_classification))

# Verify the number of rows in the dataset
nrow(data)

# Check data types
str(data$credit_history)
str(data$class)
