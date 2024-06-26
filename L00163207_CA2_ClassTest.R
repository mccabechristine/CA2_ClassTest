## In your assessment, you are tasked with compiling a report investigating 
## whether or not body fat (%) is dependent on subject’s age (years), 
## chest circumference (cm), density (g/cm3), knee circumference (cm) and 
## weight (lbs), based on the given data set. Your report should use R to 
## compile the appropriate tables/graphs/tests/etc. You are expected to explain 
## all concepts and procedures used in the statistical inference on the data. 
## Note: • Report and interpret your findings using 0.05 level of significance; 
##       • The initial multiple regression model will contain 5 explanatory 
##        variables.  
#        • The final model will contain three explanatory variables. 

#  Requirements: In your assessment, you are expected to use RStudio to compile 
#  the appropriate tables/graphs/tests, etc. You are expected to explain all 
#  concepts and procedures used in the analysis of the data. 

# ------------------------------------------------------------------------------
# Null Hypothesis (H0): Body fat percentage is not dependent on age, 
# chest circumference, density, knee circumference, and weight.

# Alternative Hypothesis (H1): Body fat percentage is dependent on age, 
# chest circumference, density, knee circumference, and weight.

# Create the data frame
# Load necessary libraries
library(readxl)
library(ggplot2)
library(car)
library(psych)
library(e1071)
library(nortest)
library(MASS)

# Load the dataset
bodyfat <- "Dataset_2024.xlsx"
data <- read_excel(bodyfat)

# Check for missing values
missing_values <- sum(is.na(data))
print(paste("Number of missing values:", missing_values))

# Handle missing values (if any)
data <- na.omit(data)

# Display the structure and first few rows of the dataset
str(data)
head(data)

# Data Preprocessing

# Convert the dataset to a dataframe.  
bodyfat <- as.data.frame(data)
str(bodyfat)
View(bodyfat)

# Rename variables
names(bodyfat)[names(bodyfat) == "Age (years)"] <- "Age"
names(bodyfat)[names(bodyfat) == "Body fat (%)"] <- "Body_Fat"
names(bodyfat)[names(bodyfat) == "Chest circumference (cm)"] <- "Chest"
names(bodyfat)[names(bodyfat) == "Density (g/cm³)"] <- "Density"
names(bodyfat)[names(bodyfat) == "Knee circumference (cm)"] <- "Knee"
names(bodyfat)[names(bodyfat) == "Weight (lbs)"] <- "Weight"

# Verify the changes
names(bodyfat)

# Descriptive Statistics
desc_stats <- describe(bodyfat)
desc_stats

# Display descriptive statistics
desc_stats_table <- data.frame(
  Variable = c("Age", "Body_Fat", "Chest", "Density", "Knee", "Weight"),
  Mean = sapply(bodyfat, mean),
  SD = sapply(bodyfat, sd),
  Median = sapply(bodyfat, median),
  Q1 = sapply(bodyfat, function(x) quantile(x, 0.25)),
  Q3 = sapply(bodyfat, function(x) quantile(x, 0.75)),
  Min = sapply(bodyfat, min),
  Max = sapply(bodyfat, max)
)
desc_stats_table


# INTRODUCTION -----------------------------------------------------------------
# This experiment investigates whether body fat percentage, dependent on a 
# subject's age, chest circumference, density, knee circumference, and weight. 
# The dataset includes various measurements for each subject, and the analysis 
# aims to identify significant predictors of body fat percentage.

# Type of data: The data consists of continuous numerical measurements.
# Type of study: his is an observational study where existing data on various 
# physical measurements are analyzed to find relationships.
# Sample size: The dataset contains 252 observations, representing the number 
# of subjects in the study.
# ------------------------------------------------------------------------------

# Linearity of data-------------------------------------------------------------
# Using the pairs function to examine linearity
windows(20, 10)
pairs(bodyfat)

# Generate pairs plot for all variables using pairs.panels from the psych package
pairs.panels(bodyfat,
             smooth = FALSE, 
             scale = FALSE,   
             density = TRUE, 
             ellipses = FALSE, 
             method = "spearman",
             pch = 21,           
             lm = FALSE,         
             cor = TRUE,         
             jiggle = FALSE,     
             factor = 2,         
             hist.col = 4,       
             stars = TRUE,       
             ci = TRUE)          

# Scatter plots with smooth lines
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid

scatter.smooth(x = bodyfat$Age,
               y = bodyfat$Body_Fat,
               main = "Correlation of Body Fat - Age",
               xlab = "Age",
               ylab = "Body Fat (%)")

scatter.smooth(x = bodyfat$Chest,
               y = bodyfat$Body_Fat,
               main = "Correlation of Body Fat - Chest Circumference",
               xlab = "Chest Circumference (cm)",
               ylab = "Body Fat (%)")

scatter.smooth(x = bodyfat$Density,
               y = bodyfat$Body_Fat,
               main = "Correlation of Body Fat - Density",
               xlab = "Density (g/cm³)",
               ylab = "Body Fat (%)")

scatter.smooth(x = bodyfat$Knee,
               y = bodyfat$Body_Fat,
               main = "Correlation of Body Fat - Knee Circumference",
               xlab = "Knee Circumference (cm)",
               ylab = "Body Fat (%)")

scatter.smooth(x = bodyfat$Weight,
               y = bodyfat$Body_Fat,
               main = "Correlation of Body Fat - Weight",
               xlab = "Weight (lbs)",
               ylab = "Body Fat (%)")

# Calculate and print the correlation matrix
cor_matrix <- cor(bodyfat[, c("Body_Fat", 
                              "Age", 
                              "Chest", 
                              "Density", 
                              "Knee",
                              "Weight")])

print(cor_matrix)

# The correlation matrix and scatter plot matrix provide an understanding of 
# the relationships between variables. The strong negative correlation between 
# body fat percentage and density indicates that as body fat percentage 
# increases, density decreases.

# Interpretation of Correlation Coefficients
# Age vs. Body Fat (%): Correlation Coefficient: 0.27
# Explanation: There is a weak positive correlation between age and body fat 
# percentage, indicating that as age increases, body fat percentage tends to 
# increase slightly.

# Chest Circumference vs. Body Fat (%): Correlation Coefficient: 0.67
# Explanation: There is a strong positive correlation between chest 
# circumference and body fat percentage, indicating that individuals with 
# larger chest circumferences tend to have higher body fat percentages.

# Density vs. Body Fat (%): Correlation Coefficient: -0.99
# Explanation: There is a very strong negative correlation between density and 
# body fat percentage, indicating that as density decreases, body fat percentage
# increases significantly.

# Knee Circumference vs. Body Fat (%): Correlation Coefficient: 0.49
# Explanation: There is a moderate positive correlation between knee 
# circumference and body fat percentage, suggesting that individuals with 
# larger knee circumferences tend to have higher body fat percentages.

# Weight vs. Body Fat (%): Correlation Coefficient: 0.61
# Explanation: There is a strong positive correlation between weight and body 
# fat percentage, indicating that heavier individuals tend to have higher body 
# fat percentages.

# These scatter plots and correlation coefficients help in understanding the 
# linearity and strength of relationships between the response variable 
# (body fat percentage) and each predictor variable. 

# Boxplots ---------------------------------------------------------------------
# Boxplot for Age
p1 <- ggplot(bodyfat, aes(x = "", y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age", y = "Age (years)") +
  theme_minimal()
print(p1)

# Boxplot for Body Fat
p2 <- ggplot(bodyfat, aes(x = "", y = Body_Fat)) +
  geom_boxplot() +
  labs(title = "Boxplot of Body Fat (%)", y = "Body Fat (%)") +
  theme_minimal()
print(p2)

# Boxplot for Chest Circumference
p3 <- ggplot(bodyfat, aes(x = "", y = Chest)) +
  geom_boxplot() +
  labs(title = "Boxplot of Chest Circumference (cm)", y = "Chest Circumference (cm)") +
  theme_minimal()
print(p3)

# Boxplot for Density
p4 <- ggplot(bodyfat, aes(x = "", y = Density)) +
  geom_boxplot() +
  labs(title = "Boxplot of Density (g/cm³)", y = "Density (g/cm³)") +
  theme_minimal()
print(p4)

# Boxplot for Knee Circumference
p5 <- ggplot(bodyfat, aes(x = "", y = Knee)) +
  geom_boxplot() +
  labs(title = "Boxplot of Knee Circumference (cm)", y = "Knee Circumference (cm)") +
  theme_minimal()
print(p5)

# Boxplot for Weight
p6 <- ggplot(bodyfat, aes(x = "", y = Weight)) +
  geom_boxplot() +
  labs(title = "Boxplot of Weight (lbs)", y = "Weight (lbs)") +
  theme_minimal()
print(p6)

# Identify outliers in Body Fat percentage
bodyfat_outliers <- bodyfat[bodyfat$Body_Fat < (quantile(bodyfat$Body_Fat, 0.25) - 1.5 * IQR(bodyfat$Body_Fat)) | 
                              bodyfat$Body_Fat > (quantile(bodyfat$Body_Fat, 0.75) + 1.5 * IQR(bodyfat$Body_Fat)), ]
print("Outliers in Body Fat:")
print(bodyfat_outliers)

# Optionally remove outliers
bodyfat_cleaned <- bodyfat[!(bodyfat$Body_Fat < (quantile(bodyfat$Body_Fat, 0.25) - 1.5 * IQR(bodyfat$Body_Fat)) | 
                               bodyfat$Body_Fat > (quantile(bodyfat$Body_Fat, 0.75) + 1.5 * IQR(bodyfat$Body_Fat))), ]

# Function to plot histogram, Q-Q plot, and boxplot, and calculate skewness
plot_normality <- function(df, column) {
  par(mfrow = c(2, 2))
  
  # Histogram
  hist(df[[column]], main = paste("Histogram of", column), xlab = column, col = "lightblue", border = "black")
  
  # Q-Q plot
  qqPlot(df[[column]], main = paste("Q-Q Plot of", column), ylab = column)
  
  # Boxplot
  boxplot(df[[column]], main = paste("Boxplot of", column), ylab = column)
  
  # Skewness
  skew <- skewness(df[[column]])
  print(paste("Skewness of", column, ":", skew))
}

# Plot normality for each variable
plot_normality(bodyfat, "Age")
plot_normality(bodyfat, "Body_Fat")
plot_normality(bodyfat, "Chest")
plot_normality(bodyfat, "Density")
plot_normality(bodyfat, "Knee")
plot_normality(bodyfat, "Weight")

# Shapiro-Wilk test for normality
normality_test <- function(df, column) {
  shapiro_test <- shapiro.test(df[[column]])
  print(paste("Shapiro-Wilk test for", column, ":", "W =", shapiro_test$statistic, "p-value =", shapiro_test$p.value))
}

# Perform Shapiro-Wilk test for each variable
normality_test(bodyfat, "Age")
normality_test(bodyfat, "Body_Fat")
normality_test(bodyfat, "Chest")
normality_test(bodyfat, "Density")
normality_test(bodyfat, "Knee")
normality_test(bodyfat, "Weight")

# Develop the full model
full_model <- lm(Body_Fat ~ Age + Chest + Density + Knee + Weight, data = bodyfat)

# Display the summary of the full model
summary(full_model)

# Calculate VIF for each predictor
vif_values <- vif(full_model)
vif_values

# Stepwise selection based on AIC to get the final model
step_model <- stepAIC(full_model, direction = "both")
summary(step_model)

# AIC and BIC for the full model
full_model_aic <- AIC(full_model)
full_model_bic <- BIC(full_model)

# AIC and BIC for the final model
final_model_aic <- AIC(step_model)
final_model_bic <- BIC(step_model)

# Print the results
print(paste("Full Model AIC:", full_model_aic))
print(paste("Full Model BIC:", full_model_bic))
print(paste("Final Model AIC:", final_model_aic))
print(paste("Final Model BIC:", final_model_bic))

# Full Model AIC: 824.23
# Full Model BIC: 847.45
# Final Model AIC: 822.17
# Final Model BIC: 835.61

# The final model has lower AIC and BIC values compared to the full model, suggesting that the final model provides a better fit to the data while also being more parsimonious.
