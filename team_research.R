install.packages("tidyverse")
# Load necessary libraries
library(tidyverse)

# Load the data (replace 'file_path' with the actual path to your CSV file)
data <- read.csv("/Users/alzamannafisuddinsiddiqui/Desktop/team research and development project/bank-additional-full.csv", sep=";")

# Separate data based on subscription status
subscribed <- data %>% filter(y == "yes") %>% pull(age)
not_subscribed <- data %>% filter(y == "no") %>% pull(age)

# Perform an independent t-test
t_test_result <- t.test(subscribed, not_subscribed, var.equal = FALSE)
print(t_test_result)
# Create a contingency table for housing loan status vs subscription
contingency_table <- table(data$housing, data$y)

# Perform chi-squared test
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

# Calculate Spearman correlation between 'emp.var.rate' and 'euribor3m'
correlation_result <- cor.test(data$emp.var.rate, data$euribor3m, method = "spearman")
print(correlation_result)