# load the dataset
library(readr)
googleplaystore <- read_csv("googleplaystore.csv")

#objective 1 

# subset the data into two groups: paid and free apps
paid <- subset(googleplaystore, Type == "Paid")
free <- subset(googleplaystore, Type == "Free")

# calculate the average rating for each group
mean_paid <- mean(paid$Rating, na.rm = TRUE)
mean_free <- mean(free$Rating, na.rm = TRUE)

# calculate the sample size, standard deviation, and standard error for each group
n_paid <- length(paid$Rating)
n_free <- length(free$Rating)
sd_paid <- sd(paid$Rating, na.rm = TRUE)
sd_free <- sd(free$Rating, na.rm = TRUE)
se_paid <- sd_paid / sqrt(n_paid)
se_free <- sd_free / sqrt(n_free)

# perform the two-sample t-test
t_value <- (mean_paid - mean_free) / sqrt(se_paid^2 + se_free^2)
df <- n_paid + n_free - 2 # degrees of freedom
p_value <- pt(t_value, df = df, lower.tail = FALSE) * 2 # two-tailed test

# print the results
cat("t-value =", round(t_value, 2), "\n")
cat("p-value =", format(p_value, scientific = TRUE, digits = 2), "\n")
if (p_value < 0.05) {
  cat("Conclusion: There is a significant difference in the average ratings of paid and free apps.\n")
} else {
  cat("Conclusion: There is no significant difference in the average ratings of paid and free apps.\n")
}


#objective 2 : correlation analysis

# Load the dataset
library(readr)
googleplaystore <- read_csv("googleplaystore.csv")

# Convert "Size" variable to numeric
googleplaystore$Size_numeric <- as.numeric(gsub("M", "", googleplaystore$Size))
# Note: The above line removes the "M" suffix from the "Size" variable, and converts it to a numeric variable

# Subset the dataset to include only non-missing values for "Size" and "Rating"
subset_data <- googleplaystore[!is.na(googleplaystore$Size_numeric) & !is.na(googleplaystore$Rating), ]

# Perform the correlation analysis
correlation <- cor(subset_data$Size_numeric, subset_data$Rating)

# Print the correlation coefficient
cat("Correlation coefficient:", round(correlation, 2))

# Create a scatter plot
plot(subset_data$Size_numeric, subset_data$Rating,
     xlab = "Size", ylab = "Rating",
     main = "Size vs. Rating Scatter Plot")







