rm(list = ls())

library(tidyr)
library(dplyr)
library(gmodels)
# Read in the data
mmf_data <- read.csv("mmfdata.csv")

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

# Convert the 'gender' column to factor
mmf_data$gender <- factor(mmf_data$gender, levels = c("Male","Female","Prefer not to say"))

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
  scale_fill_manual(values = c("pink", "lightblue","lightgreen")) +
  theme_minimal()

# count the number of males and females
hold_counts <- table(mmf_clean$hold_duration)

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

#CrossTable
CrossTable(mmf_data$gender, mmf_data$invested_before)


#cross
# Subset the data to exclude "Prefer not to say" category
mmf_data_sub <- mmf_data[mmf_data$gender != "Prefer not to say", ]

# Generate the cross table
cross<-CrossTable(mmf_data_sub$gender, mmf_data_sub$invested_before)

###########

#compare the distribution of investment decisions by occupation
ggplot(data = mmf_data, aes(x = occupation, fill = invested_before)) +
  geom_bar() +
  labs(x = "Occupation", y = "Count") +
  scale_fill_discrete(name = "Invested Before", labels = c("No", "Yes")) +
  theme_classic()

