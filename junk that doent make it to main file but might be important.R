###Gender and the factors considered when choosing a money market fund
library(MASS)
# Run the logistic regression model
model <- polr(factors ~ gender + invested_before, data = mmf_new, Hess=TRUE)
summary(model)

###
glm(performance_evaluation ~ investing_duration + invested_before, data = mmf_new, family = binomial())

# Perform chi-squared test between age and factors considered when choosing a fund
age_vs_factors <- chisq.test(table(data$`How old are you?`, data$`What factors do you consider when choosing a money market fund to invest in?`))
age_vs_factors

# Perform chi-squared test between gender and factors considered when choosing a fund
gender_vs_factors <- chisq.test(table(data$`What is your gender?`, data$`What factors do you consider when choosing a money market fund to invest in?`))
gender_vs_factors

# Perform chi-squared test between occupation and factors considered when choosing a fund
occupation_vs_factors <- chisq.test(table(investors_clean$occupation, investors_clean$factors))
occupation_vs_factors
