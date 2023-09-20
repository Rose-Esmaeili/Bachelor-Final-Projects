setwd("C:/Users/A S Computer/Downloads")

data = read.csv("anxiety1.csv")

str(data)

#Pre-Processing

# dropping extra variables
library(dplyr)

data <- data %>% select(-c('S..No.', 'Timestamp'))

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Replace NAs in 'streams' column with its mean
data$streams[is.na(data$streams)] <- as.integer(mean(data$streams, na.rm = TRUE))

# Replace NAs in 'Hours' column with its mean
data$Hours[is.na(data$Hours)] <- as.integer(mean(data$Hours, na.rm = TRUE))

table(data$Gender)

data <- data[data$Gender %in% c("Male", "Female"), ]

table(data$Gender)

# Outlier Detection
boxplot(data$Hours, main="Boxplot for Gaming Hours", ylab="Hours")
boxplot(data$GAD_T, main="Boxplot for GAD_T", ylab="GAD_T")

data <- data[data$Hours <= 50, ]

boxplot(data$Hours, main="Boxplot for Gaming Hours", ylab="Hours")

# Descriptive Statistics

df <- data %>% select(c('Hours', 'streams','GAD_T', 'Age'))
summary(df)

#Graphs

boxplot(data$Hours, main="Boxplot for Gaming Hours", ylab="Hours")
boxplot(data$GAD_T, main="Boxplot for GAD_T", ylab="GAD_T")
hist(data$Age, main="Distribution for Age", xlab="Age")

# Hypotheses 1

# Correlation between Hours and GAD_T
Hours_correlation <- cor.test(data$Hours, data$GAD_T, use="complete.obs", method="pearson")
Hours_correlation

# Scatter plot to visualize this relationship
plot(data$Hours, data$GAD_T, main="Scatterplot of Gaming Hours vs GAD Scores", xlab="Gaming Hours", ylab="GAD Scores", pch=19)

# Hypotheses 2

# ANOVA for different games
anova_result <- aov(GAD_T ~ Game, data=data)
summary(anova_result)

#Hypotheses 3

# ANOVA for different platforms
anova_platform <- aov(GAD_T ~ Platform, data=data)
summary(anova_platform)

# Hypotheses 4

# Correlation between Streams and GAD_T
correlation_stream <- cor.test(data$streams, data$GAD_T, use="complete.obs", method="pearson")
correlation_stream

# Scatter plot to visualize the relationship
plot(data$streams, data$GAD_T, main="Scatterplot of Streaming Hours vs GAD Scores", xlab="Streaming Hours", ylab="GAD Scores", pch=19)

#Hypotheses 5 

# T-test for Gender differences
t_test_gender <- t.test(GAD_T ~ as.factor(Gender), data=data)
t_test_gender

# Correlation between Age and GAD_T
correlation_age <- cor.test(data$Age, data$GAD_T, use="complete.obs", method="pearson")
correlation_age

# Scatter plot for Age vs GAD_T
plot(data$Age, data$GAD_T, main="Scatterplot of Age vs GAD Scores", xlab="Age", ylab="GAD Scores", pch=19)







