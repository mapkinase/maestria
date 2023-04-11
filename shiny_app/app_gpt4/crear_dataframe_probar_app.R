# This data frame simulates a repeated measures two-way ANOVA dataset 
# with 3 levels for factor 1 (treatment), 2 levels for factor 2 (time), and 10 subjects.

set.seed(42) # For reproducibility

# Create data for factors and dependent variable
Subject <- factor(rep(1:10, each = 6))
Factor1 <- factor(rep(c("A", "B", "C"), each = 2, times = 10))
Factor2 <- factor(rep(c("Pre", "Post"), times = 30))
DependentVar <- round(rnorm(60, mean = 100, sd = 15), 1)

# Combine factors and dependent variable into a data frame
data <- data.frame(Subject, Factor1, Factor2, DependentVar)

# Save the data frame as a CSV file
write.csv(data, "example_data.csv", row.names = FALSE)


# This code creates a data frame called data with 60 observations and 4 variables: Subject, Factor1, Factor2, and DependentVar.
# The data frame is then saved as a CSV file named example_data.csv.
