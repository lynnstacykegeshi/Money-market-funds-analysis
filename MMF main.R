rm(list = ls())

library(tidyr)
library(dplyr)
library(gmodels)
library(ggplot2)
library(stats)
library(nnet)

# Read in the data
mmf_data <- read.csv("mmfdata.csv")
View(mmf_data)

#Structure
str(mmf_data)

#Renaming
names(mmf_data) <- c("timestamp", "age", "gender", "occupation", "invested_before", 
                     "motivation", "investing_duration", "factors", "hold_duration", 
                     "performance_evaluation", "gains_or_losses", "belief", "challenges", 
                     "solutions", "strategy_influence", "consideration","prevent_investment", "potential_benefits",
                     "helpful_information","willingness","motivation2", 
                     "other_investments", "recommendation")
#Check for NA's
is.na(mmf_data)

#Convert the "age group" column to factor
mmf_data$age <- factor(mmf_data$age, levels = c("Under 18 years old",
                                                "18-24 years old",
                                                "25-34 years old",
                                                "35-44 years old",
                                                "45-54 years old",
                                                "55 years old or older"))

# Convert the 'gender' column to factor
mmf_data$gender <- factor(mmf_data$gender, levels = c("Male","Female"))

# Convert the 'occupation' column to factor
mmf_data$occupation <- factor(mmf_data$occupation, levels = c("Employed (full-time)", "Employed (part-time)", "Self-employed", "Student", "Unemployed", "Retired") )

# Convert the 'invested_before' column to factor
mmf_data$invested_before <- factor(mmf_data$invested_before, levels = c("Yes", "No"))

# Convert the 'motivation' column to factor
mmf_data$motivation <- factor(mmf_data$motivation, levels = c("Attractive returns", "Low risk", "Recommended by a financial advisor/peer", "Convenient liquidity"))

# Convert the 'investing_duration' column to factor
mmf_data$investing_duration <- factor(mmf_data$investing_duration, levels = c("Less than a year", "1-3 years", "3-5 years", "More than 5 years"))
View(mmf_data)
# Convert the 'factors' column to factor
mmf_data$factors <- factor(mmf_data$factors, levels = c("How the fund has performed in the past", "How much money you'll have to pay for the people who manage the fund", "How safe the companies and governments that the fund invests in are rated", "How big the fund is and whether it can easily turn your investment into cash when you need it"))

# Convert the 'hold_duration' column to factor
mmf_data$hold_duration <- factor(mmf_data$hold_duration, levels = c("Less than 3 months", "3 to 6 months", "6 months to 1 year", "Over 1 year"))

# Convert the 'performance_evaluation' column to factor
mmf_data$performance_evaluation <- factor(mmf_data$performance_evaluation, levels = c("Comparison with benchmark index; how well the market is doing overall", "Comparison with other money market funds", "Comparison with other investments in my portfolio"))

# Convert the 'gains_or_losses' column to factor
mmf_data$gains_or_losses <- factor(mmf_data$gains_or_losses, levels = c("Yes, significant gains", "Yes, significant losses", "No, I have not experienced significant gains or losses"))

# Convert the 'belief' column to factor
mmf_data$belief <- factor(mmf_data$belief, levels = c("Yes, I believe money market funds are a good investment option", "No, I do not believe money market funds are a good investment option", "I am unsure"))

# Convert the 'challenges' column to factor
mmf_data$challenges <- factor(mmf_data$challenges, levels = c("Lack of information about the funds", "Limited diversification", "Liquidity risks"))

# Convert the 'solutions' column to factor
mmf_data$solutions <- factor(mmf_data$solutions, levels=c("Provide more information about the funds", "Offer more diversified investment options", "Enhance liquidity management practices"))

# Convert the 'strategy_influence' column to factor
mmf_data$strategy_influence <- factor(mmf_data$strategy_influence, levels=c("I have increased my allocation to money market funds", "I have decreased my allocation to money market funds", "It has not influenced my overall investment strategy."))

# Convert the 'consideration' column to factor
mmf_data$consideration <- factor(mmf_data$consideration, levels=c("I have thought about it, but not yet.", "I don't know much about them.", "No"))

# Convert the 'prevent_investment' column to factor
mmf_data$prevent_investment <- factor(mmf_data$prevent_investment,levels = c("Lack of money", "Prefer other options", "Uncertain about safety", "Lack of information on money market funds"))

# Convert the 'potential_benefit' column to factor
mmf_data$potential_benefits <- factor(mmf_data$potential_benefits, levels = c("Stable returns", "Potential tax benefits", "No"))

# Convert the 'helpful_information' column to factor
mmf_data$helpful_information <- factor(mmf_data$helpful_information, levels = c("Risks associated with investing", "Historical performance", "Comparison with other options"))

# Convert the 'willingness' column to factor
mmf_data$willingness <- factor(mmf_data$willingness, levels = c("No", "Maybe, if I learn more", "Yes"))

# Convert the 'motivation2' column to factor
mmf_data$motivation2 <- factor(mmf_data$motivation2, levels = c("Less risky", "Easy to invest in", "Allow quick access to money", "Recommendation from peers"))

# Convert the 'other_investments' column to factor
mmf_data$other_investments <- factor(mmf_data$other_investments, levels = c("Stocks", "Bonds", "Real estate", "Mutual funds", "Fixed deposits", "Cryptocurrency", "I don't invest in any other financial products."))

# Convert the 'recommendation' column to factor
mmf_data$recommendation <- factor(mmf_data$recommendation, levels = c("Yes, because they offer a safe and stable investment opportunity with reasonable returns.", "Yes, but only as part of a diversified investment portfolio.", "No, because there are other investment options with better returns.", "No, because I have had negative experiences investing in money market funds.", "I don't know enough about money market funds to give a recommendation."))

str(mmf_data)


# Subset data to only include investors/non-investors in money market funds
investors <- mmf_data %>% filter(invested_before == "Yes") %>% select(-willingness, -consideration, -prevent_investment, -helpful_information, -potential_benefits)
non_investors <- mmf_data %>% filter(invested_before == "No") %>% select(-motivation, -investing_duration, -factors, -hold_duration, -performance_evaluation, -gains_or_losses, -belief, -challenges, -solutions, -strategy_influence)


View(investors)
dim(investors)

View(non_investors)
str(investors)

######
#Cleaning it
##Investors
is.na(investors)
investors_clean <- na.omit(investors)
investors_clean
dim(investors_clean)

##Non investors
is.na(non_investors)
dim(non_investors)
non_investors_clean<-na.omit(non_investors)
non_investors_clean
dim(non_investors_clean)

#Create a new data frame that binds investors clean and non_investors clean.
mmf_new <- bind_rows(investors_clean, non_investors_clean)
View(mmf_new)
tail(mmf_new)

library(ggplot2)

# count the number of males and females
gender_counts <- table(mmf_data$gender)

# create a data frame with gender and count columns
gender_df <- data.frame(gender = names(gender_counts), count = as.numeric(gender_counts))

# create a bar graph with ggplot2
ggplot(data = gender_df, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  ggtitle("Gender Count") +
  xlab("Gender") +
  ylab("Count") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal()

#Gender percentage
mmf_data %>%
  group_by(gender, invested_before) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) -> gender_summary

ggplot(gender_summary, aes(x = gender, y = percent, fill = invested_before)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"), 
                    labels = c("Invested", "Not invested")) +
  labs(x = "Gender", y = "Percentage", 
       title = "Percentage of Women and Men who Invested in MMF")

# Age groups percentage

mmf_data %>%
  group_by(age, invested_before) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) -> age_summary

ggplot(age_summary, aes(x = age, y = percent, fill = invested_before)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"), 
                    labels = c("Invested", "Not invested")) +
  labs(x = "Age Group", y = "Percentage", 
       title = "Percentage of Age Groups who Invested in MMF")

#Occupation
# Define the occupation groups
mmf_data %>%
  group_by(occupation, invested_before) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) -> occ_summary

ggplot(occ_summary, aes(x = occupation, y = percent, fill = invested_before)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"), 
                    labels = c("Invested", "Not invested")) +
  labs(x = "Occupation", y = "Percentage", 
       title = "Percentage of Occupations who Invested in MMF")

# Distribution of how long people typically hold their money market fund investments
## Create a box plot of the investment duration distribution
investors$investing_duration <- as.numeric(investors$investing_duration)

ggplot(investors, aes(x = investing_duration)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Investment Duration Distribution",
       x = "Investment Duration (years)",
       y = "Count")

# Motivation, stacked bar graph
## Create a table of the counts of each motivation factor
motivation_counts <- table(investors$motivation)

# Convert the table to a data frame
motivation_df <- data.frame(motivation = names(motivation_counts),
                            Count = as.vector(motivation_counts))

# Create a stacked bar chart of the motivation factors
ggplot(motivation_df, aes(x = Count, y = motivation, fill = motivation)) +
  geom_col() +
  labs(title = "Motivation to Invest in Money Market Funds",
       x = "Count",
       y = "Motivation") +
  theme(legend.position = "none")  # Hide the legend


# count the number of males and females
hold_counts <- table(mmf_data$hold_duration)

# create a data frame with gender and count columns
hold_df <- data.frame(hold_duration = names(hold_counts), count = as.numeric(hold_counts))

# create a bar graph with ggplot2
ggplot(data = hold_df, aes(x = hold_duration, y = count, fill = hold_duration)) +
  geom_bar(stat = "identity") +
  ggtitle("Hold Count") +
  xlab("Hold") +
  ylab("Count") +
  scale_fill_manual(values = c("pink", "lightblue","lightgreen","gray","yellow")) +
  theme_minimal()

unique(mmf_data$hold_duration)


############
## Omitting NA's 
# Remove NAs from hold_duration column
mmf_cleaned <- na.omit(mmf_data$hold_duration)

# Count the number of each hold duration
hold_counts <- table(mmf_cleaned)

# Create a data frame with hold duration and count columns
hold_df <- data.frame(hold_duration = names(hold_counts), count = as.numeric(hold_counts))

# Create a bar graph with ggplot2
ggplot(data = hold_df, aes(x = hold_duration, y = count, fill = hold_duration)) +
  geom_bar(stat = "identity") +
  ggtitle("Hold Count") +
  xlab("Hold") +
  ylab("Count") +
  scale_fill_manual(values = c("pink", "lightblue", "lightgreen", "purple")) +
  theme_minimal()

###########
#CrossTable gender, invested before
CrossTable(mmf_data$gender, mmf_data$invested_before)

CrossTable(mmf_data$age, mmf_data$invested_before)

CrossTable(mmf_data$occupation, mmf_data$invested_before)

#cross
# Subset the data to exclude "Prefer not to say" category
mmf_data_sub <- mmf_data[mmf_data$gender != "Prefer not to say", ]

# Generate the cross table
cross<-CrossTable(mmf_data_sub$gender, mmf_data_sub$invested_before)

###########

#compare the distribution of investment decisions by occupation
#fix this
ggplot(data = mmf_data, aes(x = occupation, fill = invested_before)) +
  geom_bar() +
  labs(x = "Occupation", y = "Count") +
  scale_fill_discrete(name = "Invested Before", labels = c("Yes", "No")) +
  theme_classic()

#####
#Regression analysis
#logistic regression instead of linear regression since we are dealing with qualitative data, also work on interpretation as we cant use the same interpretation methods as linear regression.
#In logistic regression, the liner score measures the propensity of the event, log odds
#Prob of events= exp(Log odds for event)/1+exp(Log odds for event)

##Age and the likelihood of investing in money market funds:
mmf_new$age <- relevel(mmf_new$age, ref = "18-24 years old")#we can change reference population

age_investedbefore<-glm(invested_before ~ age, data = mmf_data, family = binomial)
age_investedbefore
summary(age_investedbefore)

#***The results indicate that age is a significant predictor of invested_before, as indicated by the p-value of the age group "18-24 years old" (p < 0.001). This suggests that individuals in this age group are more likely to have invested before than the reference group.

#***However, the other age groups (Under 18, 35-44, 45-54, and 55 or older) were not found to be significant predictors of invested_before, as indicated by their non-significant p-values (all p > 0.05). This suggests that there is no statistically significant difference in the likelihood of having invested before between these age groups and the reference group.


##Occupation and the likelihood of investing in money market funds:
mmf_data$occupation <- relevel(mmf_data$occupation, ref = "Student")#we can change reference population

occupation_investedbefore<-glm(invested_before ~ occupation, data = mmf_data, family=binomial, )
occupation_investedbefore
summary(occupation_investedbefore)

### Create a table of proportions
prop.table(table(mmf_new$occupation, mmf_new$invested_before), margin = 1)

##Logistic regression to analyze the relationship between Gender and the likelihood of investing in money market funds:
genin<- glm(invested_before ~ gender, data = mmf_new, family = binomial)

summary(genin)


###A logistic regression model to examine the relationship between the binary variable "Invested_before" (the response variable) and the predictor variables "Age" and "Gender" in your data frame. 
age_gender_investedbefore <- glm(invested_before ~ gender + age + occupation, data = mmf_new, family = binomial)
summary(age_gender_investedbefore)

plot(age_gender_investedbefore$residuals)


## Investigate the relationship between the duration of investing in money market funds and the occupation of the participants.
inoc<- multinom(investing_duration ~ occupation, data = investors_clean, family = multinom)
inoc

###A logistic regression model to examine the relationship between the age group and hold duration
age_holdduration<-multinom(hold_duration ~ age, data = investors_clean, family=multinom)
age_holdduration
summary(age_holdduration)


###A logistic regression model to examine the relationship between the age group, gender and occupation on performance of your money market fund investments?
age_gen_perfomance<- multinom(performance_evaluation ~ age + gender +occupation, data = investors_clean, family = multinom)
age_gen_perfomance
summary(age_gen_perfomance)
plot(age_gen_perfomance$residuals)

###A logistic regression model to examine the relationship between the age group and the factors that have prevented you from investing in money market funds?
age_prevent<-multinom(prevent_investment ~ age, data = non_investors_clean, family=multinom())
age_prevent
summary(age_prevent)
plot(age_prevent$residuals)

#####
#*** not sure if its the correct use of code
#Chi-square

##***Gender and the factors considered when choosing a money market fund
gender_factors <- chisq.test(table(mmf_new$factors, mmf_new$gender)) 
gender_factors

## Compare responses of investors and non-investors to What do you think motivates young investors to invest in money market funds?
chisq.test(mmf_data$motivation2, mmf_data$invested_before)

# Perform chi-squared test between age and investment behavior
age_vs_investment <- chisq.test(table(mmf_new$age, mmf_new$invested_before))
age_vs_investment

# Perform chi-squared test between gender and investment behavior
gender_vs_investment <- chisq.test(table(mmf_new$gender, mmf_new$invested_before))
gender_vs_investment

# Perform chi-squared test between occupation and investment behavior
occupation_vs_investment <-chisq.test(table(mmf_new$occupation, mmf_new$invested_before))
occupation_vs_investment

# Perform chi-squared test between age and duration of investment
age_vs_duration <- chisq.test(table(investors_clean$age, investors_clean$investing_duration))
age_vs_duration

# Perform chi-squared test between gender and duration of investment
gender_vs_duration <- chisq.test(table(investors_clean$gender, investors_clean$investing_duration))
gender_vs_duration

# Perform chi-squared test between occupation and duration of investment
occupation_vs_duration <- chisq.test(table(investors_clean$occupation, investors_clean$investing_duration))
occupation_vs_duration

# Perform chi-squared test between age and factors considered when choosing a fund
age_vs_factors <- chisq.test(table(investors_clean$age, investors_clean$factors))
age_vs_factors

# Perform chi-squared test between gender and factors considered when choosing a fund
gender_vs_factors <- chisq.test(table(investors_clean$gender, investors_clean$factors))
gender_vs_factors

# Perform chi-squared test between occupation and factors considered when choosing a fund
occupation_vs_factors <- chisq.test(table(investors_clean$occupation, investors_clean$factors))
occupation_vs_factors

#we can ignore fecator analysis
#############
#Factor Analysis

##Conduct factor analysis to identify underlying factors that influence investment decisions
library(psych)
investors_clean %>%
  select(investing_duration, motivation, factors) %>%
  mutate(across(everything(), factor)) %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.)))) %>%
  cor() %>%
  psych::principal(factors = 3, rotate = "varimax")

