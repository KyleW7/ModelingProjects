# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)
library(ggpubr)
library(kableExtra)

# Set seed for reproducibility
set.seed(123)

# Load dataset
dat <- Ks_Strength_Testing_Sample_TripleJump_and_20yd

# Data Cleaning
dat <- dat %>%
  select(-5:-14) %>%
  na.omit()

#---------Using Average SLTJ---------
# plot distribution of 20yd times to check for normality
hist(dat$Twenty_yd, main="Frequency Distribution of 20yd Dash Data", xlab="Time", ylab="Frequency")

# Calculate Kurtosis, or skewness, in the original data set
kurt <- kurtosis(dat$AvgSLTrip)
print(kurt)

# Regression Model
model <- lm(Twenty_yd ~ AvgSLTrip, data = dat)
summary(model)

# Predictions and Residuals
predictions <- predict(model, newdata = dat)
predictions <- as.data.frame(predictions)
residuals <- residuals(model)

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
cat("RMSE:", rmse, "\n")

# Plot distribution of Predicted data and the residuals
hist(predictions$predictions, main="Frequency Distribution of Predicted 20yd Dash Data", xlab="Time", ylab="Frequency")
hist(residuals, main="Residuals Histogram", xlab="Residuals", ylab="Frequency")

# Combine actual and predicted data into data frame
comp <- data.frame(Actual = dat$Twenty_yd, Predicted = predictions$predictions)
comp <- comp %>%
  mutate(Error = comp$Predicted - comp$Actual)
table <- kable(comp, col.names = c("Actual", "Predicted", "Error")) %>%
  kable_styling()

print(table)

# calculate and print correlation between actual and predicted values
correlation <- cor(comp$Actual, comp$Predicted)
print(correlation)

# Visualizations
p1 <- ggplot(comp, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Actual 20yd (s)", y = "Predicted 20yd (s)", title = "Actual vs. Predicted 20yd Dash Times", 
       subtitle = "Using Avg Triple Jump") +
  theme_minimal()
p1 + stat_cor(method = "pearson")

# Data Conversion and Summarization
avg_sd <- comp %>%
  pivot_longer(cols = c("Actual", "Predicted"), names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(Value = mean(value), SD = sd(value))

# Bar Graph
plot <- ggplot(avg_sd, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, position = position_dodge(width = 0.8)) +
  labs(y = "Time(s)", title = "Average Values of Actual vs Predicted 20yd Sprint Times", 
       subtitle = "Using Avg Triple Jump") +
  scale_fill_manual(values = c("Actual" = "grey", "Predicted" = "cyan")) +
  theme_minimal()
plot + scale_y_continuous(breaks = seq(0, 4, by = 0.5), labels = seq(0, 4, by = 0.5))

# Coefficient of Variation
cv <- (avg_sd$SD / avg_sd$Value) * 100
cat("Coefficient of Variation (CV) for Actual Values: ", cv[1], "%\n")
cat("Coefficient of Variation (CV) for Predicted Values: ", cv[2], "%\n")

#---------Using R + L SLTJ Separately---------
# Regression Model
model1 <- lm(dat$Twenty_yd ~ dat$`SL_Triple_(R)` + dat$`SL_Triple_(L)`)
summary(model1)

# Predictions and Residuals
predictions1 <- predict(model1, newdata = dat)
predictions1 <- as.data.frame(predictions1)
residuals1 <- residuals(model1)

# Calculate RMSE
rmse1 <- sqrt(mean(residuals1^2))
cat("RMSE:", rmse1, "\n")

# Plot distribution of Predicted data and the residuals
hist(predictions1$predictions, main="Frequency Distribution of Predicted 20yd Dash Data", xlab="Time", ylab="Frequency")
hist(residuals1, main="Residuals Histogram", xlab="Residuals", ylab="Frequency")

# Combine actual and predicted data into data frame
comp1 <- data.frame(Actual = dat$Twenty_yd, Predicted = predictions1$predictions)

# calculate and print correlation between actual and predicted values
correlation1 <- cor(comp1$Actual, comp1$Predicted)
print(correlation1)

# Visualizations
p2 <- ggplot(comp1, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Actual 20yd (s)", y = "Predicted 20yd (s)", title = "Actual vs. Predicted 20yd Dash Times", 
       subtitle = "Using Each Leg as Separate Predictors") +
  theme_minimal()
p2 + stat_cor(method = "pearson")

# Data Conversion and Summarization
avg_sd1 <- comp1 %>%
  pivot_longer(cols = c("Actual", "Predicted"), names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(Value = mean(value), SD = sd(value))

# Bar Graph
plot1 <- ggplot(avg_sd1, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, position = position_dodge(width = 0.8)) +
  labs(y = "Time(s)", title = "Average Values of Actual vs Predicted 20yd Sprint Times",
       subtitle = "Using Separate Triple Jump Values") +
  scale_fill_manual(values = c("Actual" = "grey", "Predicted" = "magenta")) +
  theme_minimal()
plot1 + scale_y_continuous(breaks = seq(0, 4, by = 0.5), labels = seq(0, 4, by = 0.5))

# Coefficient of Variation
cv1 <- (avg_sd1$SD / avg_sd1$Value) * 100
cat("Coefficient of Variation (CV) for Actual Values: ", cv1[1], "%\n")
cat("Coefficient of Variation (CV) for Predicted Values: ", cv1[2], "%\n")

#---------Comparison of Both Models---------
comp_all <- data.frame(Actual = dat$Twenty_yd, AvgPredicted = predictions$predictions, IndPredicted = predictions1$predictions)

p3 <- ggplot(comp_all, aes(x = AvgPredicted, y = IndPredicted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted from Average SLTJ", y = "Predicted from Individual SLTJ", 
       title = "Comparison of Average vs. Individuall Predicted Times") +
  theme_minimal()
p3 + stat_cor(method = "pearson")

# Data Conversion and Summarization
full_avg_sd <- comp_all %>%
  pivot_longer(cols = c("Actual", "AvgPredicted", "IndPredicted"), names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(Value = mean(value), SD = sd(value))

# Bar Graph
plot2 <- ggplot(full_avg_sd, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, position = position_dodge(width = 0.8)) +
  labs(y = "Time(s)", title = "Average Values of Actual, Average Predicted, & Bilateral Predicted 20yd Sprint Times") +
  scale_fill_manual(values = c("Actual" = "grey", "AvgPredicted" = "cyan", "IndPredicted" = "magenta")) +
  theme_minimal()
plot2 + scale_y_continuous(breaks = seq(0, 4, by = 0.5), labels = seq(0, 4, by = 0.5))