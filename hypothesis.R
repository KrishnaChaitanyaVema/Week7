str(beaver2)
transformed_beaver_data <- transform(beaver2, activ = factor(activ, labels = c("no", "yes")))
library("lattice")
#The histigram uses one sided formula so we don't specify anything left of~
histogram(~temp | activ, data = transformed_beaver_data)
#Quantiles-quantile (QQ) plot allows us to compare the quantiles of both samples
#we use square brackets to select the cases that are required for us

with(transformed_beaver_data, qqplot(temp[activ == "yes"],
                                     temp[activ == "no"],
                                     main = "Comparing two samples",
                                     xlab = "Active temp = yes",
                                     ylab = "Active temp = no" ))

#Using a qq plot to check for normality
# qqn function plots your sample
# aganist a normal distribution
with(transformed_beaver_data, {
  qqnorm(temp[activ == "yes"],
         main = "Inactive")
  qqline(temp[activ == "yes"])
  
})


# Formal test for normaltiy using the Shapiro-wilks test

normality_test <- shapiro.test(transformed_beaver_data$temp)
normality_test$p.value
# p value indicates whether the sample comes from a normal distribution
# p value is clearly lower than 0.05 so it is not normally distributed

with(transformed_beaver_data, tapply(temp, activ, shapiro.test))

#Comparing 2 samples - most widely used test
#eg comparing mileage in cars with manual and auto gearboxes
#R provides  2 tests for comparing numerical data
# which are the t-test and the wilcoxen test
# t-test = normally distributed data
# wilcoxen test = non-normal distributed data

# In this test we are evaluating temp within groups determined by activ
str(transformed_beaver_data)
t.test(temp ~ activ, data= transformed_beaver_data)

# t = test static
# df = degrees of freedom
# p = p value. Small p value means that the mean of both samples differs
# significantly
# Alternative hypothesis = what you can conclude if the p-value
# is lower than the limit of significance (<0.05)
# This shows that the true mean of the difference is not 0
# ie we reject the null hypothesis
# 95 percent confidence interval contains the difference between the 
# means with 85% probability, In this case the difference between the
# means lies probably between 0.72 and 0.89

# we also use two seperate vectors for the samples you want
# to compare and pass both to the function
with(transformed_beaver_data, t.test(temp[activ == "yes"],
                                     temp[activ == "no"]))

# we can use the wolcox.test() function
# for the data taht deviates from the normality
# this test examines whether the centre of the data
# differs for both samples
wilcox.test(temp ~ activ, data = transformed_beaver_data)
# This test is same as the Mann-Whitney test
# so R does not have a seperate test for Mann-Whitney

# Sleep dataset contains data from 10 participants
# who are givem 2 types of sleep medicines
# Researchers record the difference in sleep for
# each person with and without drugs
str(sleep)

# Each participant gets both variants ~ data is therefore paired
# We want to know if both types of drug had effect on
# the length of the sleep

#T test and wilcox test have a paired argument
t.test(extra ~ group, data = sleep, paired = TRUE)

# Example of patients involved in car accidents
# Whether or not they wore a seat belt
survivors <- matrix(c(1781, 1443, 135, 47), ncol = 2)
colnames(survivors) <- c("survived", "died")
rownames(survivors) <- c("no seat belt", "seat belt")
survivors

# A proportion test can examine the propability
# that both proportions are the same

result_prop_test <- prop.test(survivors)
result_prop_test
