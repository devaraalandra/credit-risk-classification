# Import libs

library(dplyr)
library(ggplot2)
library(DataExplorer)

# ------ INSTALLMENT COMMITMENT ANALYSIS --------

# ---- Explorative Data Analysis ----

# Basic summary of installment commitment
summary(df$installment_commitment)
mean(df$installment_commitment)
median(df$installment_commitment)
sd(df$installment_commitment)
