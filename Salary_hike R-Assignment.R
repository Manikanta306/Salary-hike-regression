# Salary_hike -> Build a prediction model for Salary_hike

library(readr)
SD <- read_csv("Assignment files/Salary_Data.csv")
View(SD)

names(SD)[names(SD) == "YearsExperience"] <- "Exp"
names(SD)[names(SD) == "Salary"] <- "Sal"
View(SD)

plot(SD$Exp, SD$Sal)
boxplot(SD$Exp, SD$Sal)

# I) First model without transformation
summary(SD)
cor(SD$Exp, SD$Sal)

reg <- lm(SD$Sal ~ SD$Exp)
summary(reg)
confint(reg, level = 0.95)
predict(reg, interval = "predict")

# Here the P-value is less than 0.05. Multiple R-Square value is 0.957. We say that 95.7% this model will predict the correct output.

#II) We transform the variables to check the predicted values are better

reg1 <- lm(SD$Sal ~ log(SD$Exp))
summary(reg1)
confint(reg1, level = 0.95)
predict(reg1, interval = "predict")

# Here the P-value is less than 0.05. Multiple R-Square value is 0.8539. We say that 85.39% this model will predict the correct output.

# III) We transform the variables to check the predicted values are better

reg2 <- lm(log(SD$Sal) ~ SD$Exp)
summary(reg2)
confint(reg2, level = 0.95)
predict(reg2, interval = "predict")

# Here the P-value is less than 0.05. Multiple R-Square value is 0.932. We say that 93.2% this model will predict the correct output.
# By applying transformation we see that the R squared value is decreasing. The model does not need further transformation. Hence multiple R squares 0.957 is the best fit.



