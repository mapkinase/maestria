# Generate test dataframe
set.seed(42)
n <- 100
example_data <- data.frame(
  variable_1 = rnorm(n, mean = 100, sd = 15),
  variable_2 = rnorm(n, mean = 50, sd = 10),
  variable_3 = rnorm(n, mean = 20, sd = 5),
  variable_4 = rnorm(n, mean = 10, sd = 2),
  Stage = sample(c("some_string_example", "other_stage"), size = n, replace = TRUE),
  Treatment = sample(c("Treatment_A", "Treatment_B"), size = n, replace = TRUE)
)

# Save test dataframe as CSV file
write.csv(example_data, "example.csv", row.names = FALSE)


# This code generates a dataframe example_data with 100 rows and the required columns for the Shiny app: 
# variable_1, variable_2, variable_3, variable_4, Stage, and Treatment.
# The random data is generated with different means and standard deviations for each variable.
# The Stage column contains the desired string "some_string_example" as well as another string "other_stage",
# and the Treatment column includes two treatment types, "Treatment_A" and "Treatment_B"
# The generated dataframe is then saved as "example.csv" to be used for testing the Shiny app 