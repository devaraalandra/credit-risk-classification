# Import libraries

library(dplyr)
library(ggplot2)

# ---- Renaming columns -----
df <- data
rm(data)

head(df)
colnames(df)

# Rename columns
df <- df %>%
  rename(
    no = X,
    account_status = checking_status,
    loan_duration = duration,
    credit_history = credit_history,
    loan_purpose = purpose,
    loan_amount = credit_amount,
    savings_status = savings_status,
    employment_duration = employment,
    installment_commitment = installment_commitment,
    personal_status = personal_status,
    guarantors = other_parties,
    years_at_residence = residence_since,
    property_type = property_magnitude,
    age = age,
    other_payments = other_payment_plans,
    housing_type = housing,
    num_existing_credits = existing_credits,
    job_type = job,
    num_dependents = num_dependants,
    has_phone = own_telephone,
    is_foreign_worker = foreign_worker,
    credit_risk = class
  )

# check column names
colnames(df)

# Check for unrounded time variables
print(paste("Unrounded loan_duration values?", any(df$loan_duration %% 1 != 0)))
print(paste("Unrounded years_at_residence values?", any(df$years_at_residence %% 1 != 0)))

# If true, use ceiling to round up time variables
df <- df %>%
  mutate(
    loan_duration = ceiling(loan_duration),
    years_at_residence = ceiling(years_at_residence),
  )


# ------ CREDIT HISTORY --------

# ---- Explorative Data Analysis ----

# ---- PRE PROCESSING CREDIT HISTORY ----
summary(df$credit_history)


ggplot(df, aes(x = credit_history, fill = credit_risk)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Credit Risk by Credit History",
    x = "Credit History",
    y = "Proportion",
    fill = "Credit Risk"
  ) +
  theme_minimal()

print("Original credit history categories:")

table(df$credit_history)

# Combine the categories

df <- df %>%
  
  mutate(credit_history = case_when(
    
    credit_history == "all paid" | credit_history == "no credits/all paid" ~ "all_paid",
    credit_history == "critical/order existing credit" ~ "critical_existing",
    credit_history == "delayed previously" ~ "delayed_previously",
    credit_history == "existing paid" ~ "existing_paid",
    
    TRUE ~ as.character(credit_history)
    
  ))

# Verify changes

print("\nUpdated credit history categories:")

table(df$credit_history)

# Analysis
credit_history_table <- table(df$credit_history, df$credit_risk)
print("Contingency Table:")
print(credit_history_table)

# Default Rates by Credit History
prop_table <- prop.table(credit_history_table, margin = 1)
print("Default Rate by Credit History:")
print(prop_table)

# Credit History vs Credit Risk
ggplot(df, aes(x = credit_history, fill = credit_risk)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Credit Risk by Credit History",
    x = "Credit History",
    y = "Proportion",
    fill = "Credit Risk"
  ) +
  theme_minimal()

# ensure factors are properly ordered
df$employment_duration <- factor(df$employment_duration, 
                                 levels = c("<1", "1<=X<4", "4<=X<7", ">=7", "unemployed"))
df$credit_history <- factor(df$credit_history,
                            levels = c("all_paid", "critical_existing", "delayed_previously", "existing_paid"))

# Create the summary data
heatmap_data <- df %>%
  group_by(employment_duration, credit_history) %>%
  summarise(
    risk_proportion = sum(credit_risk == "bad") / n(),
    count = n(),
    .groups = 'drop'
  )

# Create the heatmap
ggplot(heatmap_data, aes(x = credit_history, y = employment_duration)) +
  geom_tile(aes(fill = risk_proportion), color = "white") +  # Added border
  scale_fill_gradient2(
    low = "darkblue",    
    mid = "orange",         
    high = "red",        
    midpoint = 0.5,
    name = "Proportion of\nBad Credit Risk",
    labels = scales::percent,
    limits = c(0, 1)  # Set explicit limits
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_blank(), # Remove grid
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Credit Risk Distribution by Employment Duration and Credit History",
    x = "Credit History",
    y = "Employment Duration"
  )

# Print summary statistics
print("Summary of data used in heatmap:")
print(summary(heatmap_data))
print(heatmap_data)

# Faceted Proportional Bar Chart of Risk Proportion across Credit History Categories seperated by Saving Status
ggplot(df) +
  geom_bar(aes(x = credit_history, fill = credit_risk), position = "fill") +
  facet_wrap(~savings_status, ncol = 1) +
  scale_fill_manual(values = c("bad" = "#f9766c", "good" = "#01bec4")) +  # Light red and light blue
  labs(
    title = "Credit Risk Distribution by Credit History and Savings Status",
    x = "Credit History",
    y = "Proportion",
    fill = "Credit Risk"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

# ----- HYPOTHESIS TESTING ------

# divide credit history into two groups, "all paid" and others"
df$is_all_paid <- ifelse(df$credit_history == "all_paid", "all_paid", "other")
df$is_all_paid <- factor(df$is_all_paid)

# check proportions for both groups
prop_table <- prop.table(contingency_table, margin = 1)
print("\nProportion Table (row proportions):")
print(prop_table)

# 3. Visualization
# Create a proportion bar plot
ggplot(df, aes(x = is_all_paid, fill = credit_risk)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
  labs(title = "Credit Risk Distribution by All_paid Status",
       x = "Credit History Status",
       y = "Proportion",
       fill = "Credit Risk") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 4. Statistical Testing

# Chi-square test
chi_test <- chisq.test(contingency_table)
print("\nChi-square Test Results:")
print(chi_test)

# calculate proportions for all paid and others
all_paid_counts <- c(
  bad = sum(df$is_all_paid == "all_paid" & df$credit_risk == "bad"),
  total = sum(df$is_all_paid == "all_paid")
)

other_counts <- c(
  bad = sum(df$is_all_paid == "other" & df$credit_risk == "bad"),
  total = sum(df$is_all_paid == "other")
)

all_paid_prop <- all_paid_counts["bad"] / all_paid_counts["total"]
other_prop <- other_counts["bad"] / other_counts["total"]
prop_difference <- all_paid_prop - other_prop

# Proportion test
prop_test <- prop.test(
  x = c(all_paid_counts["bad"], other_counts["bad"]),
  n = c(all_paid_counts["total"], other_counts["total"])
)

print("\nProportion Test Results:")
print(prop_test)

print("\nProportion Analysis:")
print(paste("All_paid bad credit risk proportion:", round(all_paid_prop * 100, 2), "%"))
print(paste("Other categories bad credit risk proportion:", round(other_prop * 100, 2), "%"))
print(paste("Difference in proportions:", round(prop_difference * 100, 2), "%"))

# 5. Conclusion
print("\nHypothesis Testing Conclusion:")
print(paste("Observed difference:", round(prop_difference * 100, 2), "%"))
print(paste("Chi-square p-value:", format.pval(chi_test$p.value, digits = 3)))
