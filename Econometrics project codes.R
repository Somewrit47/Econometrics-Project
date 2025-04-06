#Question
#Does smoking affect your health premium payments
#Does gender play a role in premium charges paid by the customer

#uploading necessary packages
library(lmtest)
library(GGally)
library(ggplot2)
library(MASS)
library(corrplot)
library(dplyr)
library(tidyverse)
library(car)
library(leaps)

#descriptive statistics
x = mean(Premium, data = insurance_data)      
median(Premium, data = insurance_data)    
sigma = sd(Premium)
mean_age = mean(age, data = insurance_data)
mean_bmi = mean(bmi, data = insurance_data)

#uploading file
insurance_data = read.csv(file.choose())
attach(insurance_data)
insurance_data
male_data = insurance_data%>% filter(sex_dummy == "1")
female_data = insurance_data%>% filter(sex_dummy == "0")
smoker = insurance_data%>% filter(Dummy_smoker == "1")
non_smoker = insurance_data%>% filter(Dummy_smoker == "0")
insurance_data$group <- paste(insurance_data$sex, insurance_data$smoker, sep = "_")
premiums = insurance_data$Premium

#Establishing the model
premium_pricing = lm(Premium ~ age + bmi + smoker_dummy + sex_dummy + children, data = insurance_data)
studentized_residuals = rstudent(premium_pricing)

selected_data = insurance_data%>%select(age,bmi,Premium)
ggpairs(selected_data)
boxplot(Premium ~ sex, data = insurance_data,
        col = c("blue", "pink"),
        main = "Box plot of premium charges",
        xlab = "sex",
        ylab = "Premium")
boxplot(Premium ~ smoker, data = insurance_data,
        col = c("red", "blue"),
        main = "Box plot of premium charges",
        xlab = "smoker",
        ylab = "Premium")
ggplot(insurance_data, aes(x = group, y = Premium, fill = group)) +
  geom_boxplot() +
  labs(title = "Premiums by Sex and Smoking Status",
       x = "Sex and Smoking Status",
       y = "Premium") +
  scale_x_discrete(labels = c("female_no" = "Female Non-Smoker",
                              "female_yes" = "Female Smoker",
                              "male_no" = "Male Non-Smoker",
                              "male_yes" = "Male Smoker")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
boxplot(Premium ~ children, data = insurance_data,
        col = c("red", "blue", "yellow", "green", "pink", "purple"),
        main = "Box plot of Premium charges",
        xlab = "Children",
        ylab = "Premium")

#Testing for model assumptions
avPlots(premium_pricing, main = "Partial Regression Plots")
plot(premium_pricing$fitted.values, premium_pricing$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)
bp_test = bptest(premium_pricing)
print(bp_test)
qqnorm(studentized_residuals, main = "QQ Plot of Studentized Residuals")
qqline(studentized_residuals, col ="red", lwd = 2)
test = shapiro.test(premium_pricing)
print(test)

#Testing for log model
premium_log = lm(log(Premium) ~ age + bmi + smoker_dummy + sex_dummy + children, data = insurance_data)
plot(premium_log$fitted.values,premium_log$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)
bp_test_log = bptest(premium_log)
print(bp_test_log)
studentized_residuals_log = rstudent(premium_log)
qqnorm(studentized_residuals_log, main = "QQ plot of Studentized Residuals")
qqline(studentized_residuals_log, col = "red", lwd = 2)
test1 = shapiro.test(log(Premium))
print(test1)

#Testing for boxcox corrected model
bc = boxcox(premium_pricing, lambda =seq(-2,2))
best.lam = bc$x[which(bc$y==max(bc$y))]
#best lambda value = 0.1414141
premium_inv = lm((Premium)^0.1414141 ~ age + bmi + smoker_dummy + sex_dummy + children, data = insurance_data)
plot(premium_inv$fitted.values, premium_inv$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)
bp_test_boxcox = bptest(premium_inv)
print(bp_test_boxcox)
studentized_residuals_inv = rstudent(premium_inv)
qqnorm(studentized_residuals_inv, main = "QQ Plot of Studentized Residuals")
qqline(studentized_residuals_inv, col ="red", lwd = 2)
test = shapiro.test((Premium)^0.14141414)
print(test)


#Testing with bootstrapped samples
set.seed(123)
n_boot = 5000
bootstrap_means = numeric(n_boot)

for( i in 1:n_boot) {
  bootstrap_sample = sample(premiums, size = length(premiums), replace = TRUE)
  bootstrap_means[i] = mean(bootstrap_sample)
}

hist(bootstrap_means, breaks = 50, main = "Bootstrap Distribution", xlab = "Bootstrap Means")
test_bootstrap = shapiro.test(bootstrap_means)
print(test_bootstrap)
mean(bootstrap_means)
quantile(bootstrap_means, c(0.025,0.975))
coef_function = function(insurance_data,indices) {
  bootstrap_sample = insurance_data[indices, ]
  model = lm(premium_pricing, data = bootstrap_sample)
  return(coef(premium_pricing))
}
bootstrap_result = boot(data = insurance_data, statistic = coef_function, R = 1000)
print(bootstrap_result)


#Mallow's CP

leaps_result = regsubsets(response ~ age + sex_dummy + bmi + children + smoker_dummy, data = insurance_data, nbest = 1)
print(leaps_result)
leaps_summary = summary(leaps_result)
print(leaps_summary$cp)
models = leaps_summary$which
cp_values = leaps_summary$cp
result = data.frame(models, Cp = cp_values)
print(result)
plot(leaps_summary$cp,
     type = "b",
     xlab = "Number of Predictors",
     ylab = "Mallow's CP") 

#Cook's Distance

cooks_dist = cooks.distance(premium_pricing)
cooks_dist
threshold = 4/nrow(insurance_data)
influential_points = which(cooks_dist > threshold)
influential_points

#Visualization of Cook's distances
plot(cooks_dist,
     main = "Cook's Distance",
     xlab = "Observation Index",
     ylab = "Cook's Distance")
abline(h = threshold, col = "red", lty = 5)

leveragePlots(premium_pricing)

plot(premium_pricing)

#After removing influential points
insurance_data_cleaned = insurance_data[-influential_points, ]
premium_pricing_cleaned = lm(Premium ~ age + bmi + smoker_dummy + sex_dummy + children, data = insurance_data_cleaned)

#Multicollinearity
vif_values = vif(premium_pricing)
print(vif_values)


#Summary of regression
summary(premium_pricing)
summary(premium_log)
summary(premium_inv)
summary(premium_pricing_cleaned)

#T-test

t_test = t.test(insurance_data, mu = 13270)

#ANOVA Test
null_model = lm(Premium ~ age + bmi, data = insurance_data)
full_model = lm(Premium ~ age + bmi + smoker_dummy + sex_dummy + children, data = insurance_data)
anova_result = anova(null_model, full_model)
print(anova_result)


