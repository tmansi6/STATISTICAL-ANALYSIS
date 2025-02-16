#install packages
install.packages("tidyverse")
install.packages('readxl')
install.packages('ggcorrplot')
install.packages('carData')
install.packages("corrplot")


# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(readxl)      # For reading Excel files
library(corrplot)
library(car)
#EDA

# Load the data sets
red_wine <- read_excel("winequality-red.xlsx")
white_wine <- read_excel("winequality-white.xlsx")

# Preview the data sets
head(red_wine)
head(white_wine)

# Red wine dataset overview
str(red_wine)
summary(red_wine)
sum(is.na(red_wine))

# White wine dataset overview
str(white_wine)
summary(white_wine)
sum(is.na(white_wine))

# Add a wine type column and combine datasets
red_wine <- red_wine %>% mutate(wine_type = "Red")
white_wine <- white_wine %>% mutate(wine_type = "White")

combined_wine <- bind_rows(red_wine, white_wine)

# Preview combined data
head(combined_wine)


#VISUALIZATION
# Quality distribution for red and white wines
combined_wine %>%
  ggplot(aes(x = quality, fill = wine_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Wine Quality Distribution", x = "Quality", y = "Count") +
  theme_minimal()
#compare alcohol content
combined_wine %>%
  ggplot(aes(x = alcohol, fill = wine_type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Alcohol Content Distribution", x = "Alcohol (%)",
       y = "Density") +theme_minimal()

# CORELLATION ANALYSIS
# Calculate correlation matrix for red wine
red_corr <- cor(select(red_wine, where(is.numeric)))


# Calculate correlation matrix for white wine
white_corr <- cor(select(white_wine, where(is.numeric)))

# Visualize correlation matrices
corrplot(cor(red_corr))
corrplot(cor(white_corr))

# Extract correlations with 'quality' for red wine
red_quality_corr <- red_corr[, "quality"] %>% sort(decreasing = TRUE)
print("Red Wine Correlations with Quality:")
print(red_quality_corr)

# Extract correlations with 'quality' for white wine
white_quality_corr <- white_corr[, "quality"] %>% sort(decreasing = TRUE)
print("White Wine Correlations with Quality:")
print(white_quality_corr)









#REGRESSION(MULTIPLE)
# Multiple Linear Regression
model_mlr <- lm(quality ~ alcohol + `volatile acidity` 
                + `citric acid` + `residual sugar` + chlorides 
                + `free sulfur dioxide` + `total sulfur dioxide` + density 
                + pH + sulphates + alcohol + wine_type,
                data= combined_wine)

model_mlr1<-lm(quality~ `volatile acidity`+alcohol, data= combined_wine)

model_mlr3 <- lm(quality ~ alcohol + `volatile acidity`  
                + `residual sugar` + chlorides + `free sulfur dioxide` 
                + `total sulfur dioxide` + density + pH + sulphates 
                + alcohol + wine_type, data= combined_wine)

model_mlr2 <- lm(quality ~ alcohol + `volatile acidity`  
                 + `residual sugar` + chlorides + `free sulfur dioxide` 
                 + `total sulfur dioxide`  + pH + sulphates 
                 + alcohol , data= combined_wine)
# Summary of the model
summary(model_mlr)
summary(model_mlr1)
summary(model_mlr3)
summary(model_mlr2)

#ASSUMPTIONS CHECK

#Linearity
# Scatterplots for visualizing linearity
pairs(combined_wine[, c("quality", "alcohol", "volatile acidity", 
                        "citric acid", "residual sugar", "chlorides", 
                        "free sulfur dioxide", "total sulfur dioxide", 
                        "density", "pH", "sulphates")])

# Residual plot for linearity
plot(model_mlr$fitted.values, model_mlr$residuals, 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")



#Normality
# Q-Q plot
qqnorm(model_mlr$residuals)
qqline(model_mlr$residuals, col = "red", lwd = 2)


#Homoscedasity
# Residual plot for homoscedasticity
plot(model_mlr2,3)


#Residual Independence

plot(model_mlr2,1)



#Multicollinearity
# VIF calculation

vif(model_mlr2)




#HYPOTHESIS TESTING
#T-Test for Quality:
t.test(quality ~ wine_type, data = combined_wine)

#F-Test for Variance in Quality:
var.test(red_wine$quality, white_wine$quality)

#T-Tests for Attributes:
# Subset red and white wine data
red_wine <- subset(combined_wine, wine_type == "Red")
white_wine <- subset(combined_wine, wine_type == "White")

# Perform t-tests for all attributes
t_test_results <- list(
  volatile_acidity = t.test(red_wine$`volatile acidity`,
                            white_wine$`volatile acidity`),
  citric_acid = t.test(red_wine$`citric acid`, white_wine$`citric acid`),
  residual_sugar = t.test(red_wine$`residual sugar`, 
                          white_wine$`residual sugar`),
  chlorides = t.test(red_wine$chlorides, white_wine$chlorides),
  free_sulfur_dioxide = t.test(red_wine$`free sulfur dioxide`,
                               white_wine$`free sulfur dioxide`),
  total_sulfur_dioxide = t.test(red_wine$`total sulfur dioxide`,
                                white_wine$`total sulfur dioxide`),
  density = t.test(red_wine$density, white_wine$density),
  pH = t.test(red_wine$pH, white_wine$pH),
  sulphates = t.test(red_wine$sulphates, white_wine$sulphates),
  alcohol = t.test(red_wine$alcohol, white_wine$alcohol))

# Print results for each attribute
for (attribute in names(t_test_results)) {
  cat("\nT-Test for", attribute, "\n")
  print(t_test_results[[attribute]])}






